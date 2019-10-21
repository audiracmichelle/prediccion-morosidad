system("echo '===== Running collapse task ====='")

system("echo '===== Loading libraries ====='")

suppressMessages(library(feather))
suppressMessages(library(aws.s3))
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(lubridate))
suppressMessages(library(xts))

system("echo '===== Loading functions ====='")

# Functions to calculate the decayed weight average

#' @title get_w_with_geomTruncDecay
# decay function
# consists in a truncated version of the geometric distribution
get_w_with_geomTruncDecay <- function(
  T,
  halflife
) {
  median_p <- function(p) (1-(1-p)^halflife) / (1-(1-p)^T) - 0.5
  p <- 1-2^(-1/halflife)
  k <- T:1
  w <- p * (1-p)^{k-1} / (1-(1-p)^T)
  w
}


# This function takes a vector and applies the above
# decay function recursively to get a single vale per row
collapse_ts <- function(
  x,
  halflife = NULL
  ) {
  if(is.null(halflife)) {
    sapply(1:length(x), 
           function(l) sum(x[1:l]/l))
    } else {
      sapply(1:length(x), 
             function(l) sum(x[1:l]*get_w_with_geomTruncDecay(l, halflife)))
    }
}

get_label_prom <- function(
  x, 
  ventana_predict
) {
  lag.xts(rollmean(x, 
                   k = ventana_predict, 
                   na.pad = TRUE, 
                   align = 'left'), 
          k = -1)
}

# lagpad <- function(x, k) {
#   c(rep(NA, k), x)[1 : length(x)] 
# }

# flag_veces_seguidas <- function(hot, veces) {
#   strikes <- rle(hot)
#   strikes$values <- strikes$lengths > veces & strikes$values
#   rep(strikes$values, strikes$lengths)
# }
system("echo '===== Loading params ====='")

params <- list()

# ####
# # usar estos parámetros si se requieren leer los parametros directamente
# params$ymd_preprocess <- 20180907
# params$tag_preprocess <- 'prod'
# params$tag_collapse <- 'prod'
# params$halflife <- 6
# params$min_len_predictors <- 6
# params$bucket_s3 <- 'local'
# ####

args = commandArgs(trailingOnly=TRUE)
params$ymd_preprocess <- args[1]
params$tag_preprocess <- args[2]
params$tag_collapse <- args[3]
params$halflife <- as.numeric(args[4])
params$min_len_predictors <- as.numeric(args[5])
params$bucket_s3 <- as.numeric(args[6])

params$ventana_predict <- 3
params$num_lco_strikes <- 3
params$max_fechas_faltantes_seguidas <- 2

system("echo '===== Fetching data ====='")

if(params$bucket_s3 == "local") {
  limpieza <- read_feather(paste0('home/preprocess/output/limpieza_',
                                  params$ymd_preprocess, '_', 
                                  params$tag_preprocess, '.feather'))
  creditos <- read_feather(paste0('home/preprocess/output/design_matrix_',
                                  params$ymd_preprocess, '_', 
                                  params$tag_preprocess, '.feather'))
} else {
  limpieza <- s3read_using(FUN = read_feather, 
                          bucket = params$bucket_s3, 
                          object = paste0('limpieza_', 
                                          params$ymd_preprocess, '_', 
                                          params$tag_preprocess, '.feather'))
  creditos <- s3read_using(FUN = read_feather, 
                          bucket = params$bucket_s3, 
                          object = paste0('design_matrix_', 
                                          params$ymd_preprocess, '_', 
                                          params$tag_preprocess, '.feather'))
}

creditos %<>% arrange(pkcolocadora, fechaCorteDespues)

system("echo '===== Main body ====='")

limpieza %<>% 
  mutate(historia_saldo_corta_train = if_else(num_indices_preprocess < 
                                                (params$min_len_predictors + params$ventana_predict), 1, 0), 
         ex_train = (ex_preprocess | 
                       historia_saldo_corta_train | 
                       max_fechas_faltantes_seguidas > params$max_fechas_faltantes_seguidas) * 1)
####
system("echo 'sum(limpieza$ex_train == 0)'")
sum(limpieza$ex_train == 0)
system("echo 'sum(limpieza$ex_preprocess == 0)'")
sum(limpieza$ex_preprocess == 0)
####

colocadoras <- limpieza$pkcolocadora[limpieza$ex_train == 0]
# coloc_creditos <- unique(creditos$pkcolocadora)
# length(coloc_creditos)
# length(colocadoras)
# coloc_limpieza <- unique(limpieza$pkcolocadora)
# length(coloc_limpieza)
# sum(colocadoras %in% coloc_creditos)
# colocadoras[!colocadoras %in% coloc_creditos]


creditos %<>%
  filter(pkcolocadora %in% colocadoras) %>% 
  group_by(pkcolocadora) %>% 
  mutate(delta_saldo = saldoActual - lag(saldoActual), 
         delta_prestamoPersonal = prestamoPersonal - lag(prestamoPersonal)) %>% ungroup() %>% 
  mutate(delta_saldo = if_else(is.na(delta_saldo), 0, delta_saldo), 
         delta_prestamoPersonal = if_else(is.na(delta_prestamoPersonal), 0, delta_prestamoPersonal))
# sum(creditos$fechaCorteDespues == ymd(params$ymd_preprocess))

collapse_train <- creditos %>% 
  group_by(pkcolocadora) %>% 
  mutate(saldo_prom_decay = collapse_ts(saldoActual, halflife = params$halflife), 
         prestamoPersonal_prom_decay = collapse_ts(prestamoPersonal, halflife = params$halflife), 
         
         delta_saldo_prom_decay = collapse_ts(delta_saldo, halflife = params$halflife), 
         delta_prestamoPersonal_prom_decay = collapse_ts(delta_prestamoPersonal, halflife = params$halflife), 
         
         delta_saldo_sd_decay = sqrt(collapse_ts((delta_saldo - delta_saldo_prom_decay)^2, 
                                                 halflife = params$halflife)), 
         delta_prestamoPersonal_sd_decay = sqrt(collapse_ts((delta_prestamoPersonal - delta_prestamoPersonal_prom_decay)^2, 
                                                            halflife = params$halflife)),
         
         dias_despues_fechaPago_prom_decay = collapse_ts(dias_despues_fechaPago, halflife = params$halflife), 
         dias_despues_fechaCorte_prom_decay = collapse_ts(dias_despues_fechaCorte, halflife = params$halflife), 
         
         porc_puntual_prom_decay = collapse_ts(porc_puntual, halflife = params$halflife), 
         porc_impuntual_prom_decay = collapse_ts(porc_impuntual, halflife = params$halflife), 
         porc_rescate_prom_decay = collapse_ts(porc_rescate, halflife = params$halflife), 
         
         porc_puntual_sd_decay = sqrt(collapse_ts((porc_puntual - porc_puntual_prom_decay)^2, 
                                                  halflife = params$halflife)), 
         porc_impuntual_sd_decay = sqrt(collapse_ts((porc_impuntual - porc_impuntual_prom_decay)^2, 
                                                    halflife = params$halflife)), 
         porc_rescate_sd_decay = sqrt(collapse_ts((porc_rescate - porc_rescate_prom_decay)^2, 
                                                    halflife = params$halflife)), 
         
         dias_puntual_prop_decay = collapse_ts((dias_despues_fechaPago <= 0), halflife = params$halflife), 
         dias_impuntual_prop_decay = collapse_ts((dias_despues_fechaPago > 0 & dias_despues_fechaCorte < 0), halflife = params$halflife), 
         dias_rescate_prop_decay = collapse_ts((dias_despues_fechaCorte >= 0), halflife = params$halflife) #, 
         
         #valeSum_prom_decay = collapse_ts(valeSum, halflife = params$halflife),
         #valeCuenta_prom_decay = collapse_ts(valeCuenta, halflife = params$halflife), 
         #valeProm_prom_decay = collapse_ts(valeSum / valeCuenta, halflife = params$halflife), 
         #clienteCuenta_prom_decay = collapse_ts(clienteCuenta, halflife = params$halflife)
         )

collapse_train %<>% 
  group_by(pkcolocadora) %>% 
  mutate(label_dias_despues_fechaPago_prom = get_label_prom(dias_despues_fechaPago, 
                                                            ventana_predict = params$ventana_predict), 
         label_dias_despues_fechaCorte_prom = get_label_prom(dias_despues_fechaCorte, 
                                                             ventana_predict = params$ventana_predict), 
         
         label_dias_puntual_prop = get_label_prom((dias_despues_fechaPago <= 0), 
                                                  ventana_predict = params$ventana_predict),
         label_dias_impuntual_prop = get_label_prom((dias_despues_fechaPago > 0 & dias_despues_fechaCorte < 0), 
                                                    ventana_predict = params$ventana_predict), 
         label_dias_rescate_prop = get_label_prom((dias_despues_fechaCorte >= 0), 
                                                  ventana_predict = params$ventana_predict), 
         
         label_lco_mora_prop = get_label_prom((dias_despues_fechaCorte >= 2), 
                                              ventana_predict = params$num_lco_strikes), 
         label_lco_mora = (label_lco_mora_prop >= 1) * 1, 
         
         label_impuntual_2 = (label_dias_despues_fechaPago_prom >= 2) * 1, 
         label_altoRiesgo = lag.xts((dias_despues_fechaCorte >= 15) * 1, k = -1),
         label_lco_mora_2 = (rollmean((dias_despues_fechaPago >= 2), 
                                     params$num_lco_strikes, 
                                     na.pad = TRUE, 
                                     align = 'right') == 1) * 1
         )


# sum(is.na(collapse_train$label_altoRiesgo))
# View(collapse_train %>% filter(pkcolocadora == 1))

# collapse_train %>%
#   group_by(pkcolocadora) %>%
#   mutate(sum_lco = sum(label_lco_mora_2, na.rm = TRUE)) %>%
#   filter(sum_lco > 10) %>%
#   select(pkcolocadora, label_lco_mora, label_lco_mora_2, dias_despues_fechaPago) %>%
#   write_csv('label_lco_mora_2.csv')

collapse_train %<>% 
  filter(indice_preprocess >= params$min_len_predictors, 
         indice_preprocess_desc > params$ventana_predict) %>% 
  rename(fechaCorte = fechaCorteDespues)
 
system("echo '===== Saving processed data ====='")

if(params$bucket_s3 == "local") {
  write_csv(limpieza, paste0('/home/wrangle_train/output/limpieza_train_',
                            params$ymd_preprocess, '_', 
                            params$tag_collapse, '.csv'))
  write_feather(collapse_train, paste0('/home/wrangle_train/output/collapse_train_',
                                      params$ymd_preprocess, '_', 
                                      params$tag_collapse, '.feather'))
} else {
  s3write_using(limpieza, 
                FUN = write_csv, 
                bucket = params$bucket_s3, 
                object = paste0('limpieza_train_',
                                params$ymd_preprocess, '_', 
                                params$tag_collapse, '.csv'))
  s3write_using(collapse_train, 
                FUN = write_feather, 
                bucket = params$bucket_s3, 
                object = paste0('collapse_train_',
                                params$ymd_preprocess, '_', 
                                params$tag_collapse, '.feather'))
}
