system("echo '===== Running collapse task ====='")

system("echo '===== Loading libraries ====='")

suppressMessages(library(feather))
suppressMessages(library(aws.s3))
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(lubridate))
suppressMessages(library(xts))
suppressMessages(library(plotly))

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
# params$ymd_preprocess <- 20180823
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
params$bucket_s3 <- args[6]

params$ventana_predict <- 3
params$num_lco_strikes <- 3
params$max_fechas_faltantes_seguidas <- 2

system("echo '===== Fetching data ====='")

if(params$bucket_s3 == "local") {
  limpieza <- read_feather(paste0('/home/rstudio/pipeline/preprocess/output/limpieza_',
                                  params$ymd_preprocess, '_', 
                                  params$tag_preprocess, '.feather'))
  creditos <- read_feather(paste0('/home/rstudio/pipeline/preprocess/output/design_matrix_',
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
  mutate(porc_mora = porc_impuntual + porc_rescate) 
# sum(creditos$fechaCorteDespues == ymd(params$ymd_preprocess))

system("echo '---transformando saldoActual---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(saldo_prom_decay = collapse_ts(saldoActual, halflife = params$halflife),
         saldo_prom_decay3 = collapse_ts(saldoActual, halflife = 3),
         saldo_prom4 = rollmean(saldoActual, k = 4, na.pad = TRUE, align = 'right'),
         saldo_sd_decay3 = sqrt(collapse_ts((saldoActual - saldo_prom_decay3)^2, 
                                                 halflife = 3))
          ) %>% ungroup()

# ggplotly( creditos %>%
#   filter(pkcolocadora == 175) %>%
#   select(fechaCorteAntes,
#          saldoActual,
#          saldo_prom_decay,
#          saldo_prom_decay3,
#          saldo_prom4) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

creditos %<>%
  group_by(pkcolocadora) %>%
  mutate(diff_saldo = saldoActual - lag(saldoActual),
         #diff_prestamoPersonal = prestamoPersonal - lag(prestamoPersonal)
         diff_saldo = na.locf(diff_saldo, fromLast=TRUE),
         diff_saldo_prom_decay3 = collapse_ts(diff_saldo, halflife = 3),
         diff_saldo_sd_decay3 = sqrt(collapse_ts((diff_saldo - diff_saldo_prom_decay3)^2,
                                                halflife = 3))
  ) %>% ungroup()

# ggplotly(creditos %>%
#   filter(pkcolocadora == 1) %>%
#   select(fechaCorteAntes,
#          diff_saldo,
#          diff_saldo_prom_decay3,
#          diff_saldo_sd_decay3,
#          saldo_sd_decay3) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(delta_saldo = saldoActual / lag(saldoActual) - 1,
         delta_saldo = na.locf(delta_saldo, fromLast=TRUE),
         delta_saldo_prom_decay3 = collapse_ts(delta_saldo, halflife = 3), 
         delta_saldo_sd_decay3 = sqrt(collapse_ts((delta_saldo - delta_saldo_prom_decay3)^2, 
                                                  halflife = 3))
  ) %>% ungroup()

# ggplotly(creditos %>%
#   filter(pkcolocadora == 175) %>%
#   select(fechaCorteAntes,
#          delta_saldo, 
#          delta_saldo_prom_decay3,
#          delta_saldo_sd_decay3) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

system("echo '---transformando dias_despues_fechaPago---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(dias_despues_fechaPago_prom_decay = collapse_ts(dias_despues_fechaPago, halflife = params$halflife), 
         dias_despues_fechaPago_prom_decay3 = collapse_ts(dias_despues_fechaPago, halflife = 3),
         dias_despues_fechaPago_prom4 = rollmean(dias_despues_fechaPago, k = 4, na.pad = TRUE, align = 'right'),
         dias_despues_fechaPago_sd_decay = sqrt(collapse_ts((dias_despues_fechaPago - dias_despues_fechaPago_prom_decay)^2, 
                                                            halflife = params$halflife))
         ) %>% ungroup()

# ggplotly(creditos %>%
#   filter(pkcolocadora == 175) %>%
#   select(fechaCorteAntes,
#          dias_despues_fechaPago,
#          dias_despues_fechaPago_prom_decay,
#          dias_despues_fechaPago_prom_decay3,
#          dias_despues_fechaPago_prom4,
#          dias_despues_fechaPago_sd_decay) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

system("echo '---transformando porc_puntual---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(porc_puntual_prom_decay = collapse_ts(porc_puntual, halflife = params$halflife), 
         porc_puntual_prom_decay3 = collapse_ts(porc_puntual, halflife = 3), 
         porc_puntual_prom4 = rollmean(porc_puntual, k = 4, na.pad = TRUE, align = 'right'),
         porc_puntual_sd_decay = sqrt(collapse_ts((porc_puntual - porc_puntual_prom_decay)^2, 
                                                  halflife = params$halflife))
         ) %>% ungroup()

# ggplotly(creditos %>%
#   filter(pkcolocadora == 175) %>%
#   select(fechaCorteAntes,
#          porc_puntual,
#          porc_puntual_prom_decay,
#          porc_puntual_prom_decay3,
#          porc_puntual_prom4,
#          porc_puntual_sd_decay) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

system("echo '---transformando porc_mora---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(porc_mora_prom_decay = collapse_ts(porc_mora, halflife = params$halflife),
         porc_mora_prom_decay3 = collapse_ts(porc_mora, halflife = 3),
         porc_mora_prom4 = rollmean(porc_mora, k = 4, na.pad = TRUE, align = 'right'),
         porc_mora_sd_decay3 = sqrt(collapse_ts((porc_mora - porc_mora_prom_decay3)^2, 
                                                  halflife = 3))
         ) %>% ungroup()

# ggplotly(creditos %>%
#   filter(pkcolocadora == 175) %>%
#   select(fechaCorteAntes,
#          porc_mora,
#          porc_mora_prom_decay,
#          porc_mora_prom_decay3,
#          porc_mora_prom4,
#          porc_mora_sd_decay3) %>%
#   rename(date = fechaCorteAntes) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line())

system("echo '---transformando dias_puntual_prop---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(dias_puntual_prop_decay = collapse_ts((dias_despues_fechaPago <= 0), halflife = params$halflife), 
         dias_puntual_prop_decay3 = collapse_ts((dias_despues_fechaPago <= 0), halflife = 3),
         dias_puntual_prop4 = rollmean((dias_despues_fechaPago <= 0), k = 4, na.pad = TRUE, align = 'right'),
         dias_puntual_sd_decay = sqrt(collapse_ts(((dias_despues_fechaPago <= 0) - dias_puntual_prop_decay)^2, 
                                               halflife = params$halflife))
         ) %>% ungroup()

# ggplotly(creditos %>%
#            filter(pkcolocadora == 175) %>%
#            select(fechaCorteAntes, 
#                   dias_despues_fechaPago,
#                   dias_puntual_prop_decay,
#                   dias_puntual_prop_decay3,
#                   dias_puntual_prop4,
#                   dias_puntual_sd_decay) %>%
#            rename(date = fechaCorteAntes) %>%
#            gather(key, value,-date) %>%
#            ggplot(aes(x = date,
#                       y = value,
#                       colour = key)) +
#            geom_line())

system("echo '---transformando dias_mora_prop---'")

creditos %<>% 
  group_by(pkcolocadora) %>% 
  mutate(dias_mora_prop_decay = collapse_ts((dias_despues_fechaPago > 0), halflife = params$halflife), 
         dias_mora_prop_decay3 = collapse_ts((dias_despues_fechaPago > 0), halflife = 3),
         dias_mora_prop4 = rollmean((dias_despues_fechaPago > 0), k = 4, na.pad = TRUE, align = 'right'),
         dias_mora_sd_decay3 = sqrt(collapse_ts(((dias_despues_fechaPago > 0) - dias_mora_prop_decay3)^2, 
                                                  halflife = 3))
  ) %>% ungroup()

# ggplotly(creditos %>%
#            filter(pkcolocadora == 175) %>%
#            select(fechaCorteAntes,
#                   dias_despues_fechaPago,
#                   dias_mora_prop_decay,
#                   dias_mora_prop_decay3,
#                   dias_mora_prop4,
#                   dias_mora_sd_decay3) %>%
#            rename(date = fechaCorteAntes) %>%
#            gather(key, value,-date) %>%
#            ggplot(aes(x = date,
#                       y = value,
#                       colour = key)) +
#            geom_line())

##valeSum_prom_decay = collapse_ts(valeSum, halflife = params$halflife),
##valeCuenta_prom_decay = collapse_ts(valeCuenta, halflife = params$halflife), 
##valeProm_prom_decay = collapse_ts(valeSum / valeCuenta, halflife = params$halflife), 
##clienteCuenta_prom_decay = collapse_ts(clienteCuenta, halflife = params$halflife)

creditos %<>% 
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
         ) %>% 
  ungroup()

# sum(is.na(creditos$label_altoRiesgo))
# View(creditos %>% filter(pkcolocadora == 1))

# creditos %>%
#   group_by(pkcolocadora) %>%
#   mutate(sum_lco = sum(label_lco_mora_2, na.rm = TRUE)) %>%
#   filter(sum_lco > 10) %>%
#   select(pkcolocadora, label_lco_mora, label_lco_mora_2, dias_despues_fechaPago) %>%
#   write_csv('label_lco_mora_2.csv')

creditos %<>%
  filter(indice_preprocess >= params$min_len_predictors,
         indice_preprocess_desc > params$ventana_predict) %>%
  rename(fechaCorte = fechaCorteDespues)

# creditos %>% 
#   summarize(n = n(), 
#             impuntual = sum(label_impuntual_2) / n(), 
#             puntual = sum(label_impuntual_2 == 0) / n())
# 
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15) %>% 
#   summarize(n = n(), 
#             impuntual = sum(label_impuntual_2) / n(), 
#             puntual = sum(label_impuntual_2 == 0) / n())
 
# creditos %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_porc_mora = mean(porc_mora, na.rm = TRUE), 
#             portfolio_porc_mora_prom_decay = mean(porc_mora_prom_decay, na.rm = TRUE), 
#             portfolio_porc_mora_prom_decay3 = mean(porc_mora_prom_decay3, na.rm = TRUE),
#             portfolio_porc_mora_prom4 = mean(porc_mora_prom4, na.rm = TRUE), 
#             portfolio_porc_mora_sd_decay3 = mean(porc_mora_sd_decay3, na.rm = TRUE)) %>%
#   ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()
# 
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_porc_mora = mean(porc_mora, na.rm = TRUE), 
#             portfolio_porc_mora_prom_decay = mean(porc_mora_prom_decay, na.rm = TRUE), 
#             portfolio_porc_mora_prom_decay3 = mean(porc_mora_prom_decay3, na.rm = TRUE),
#             portfolio_porc_mora_prom4 = mean(porc_mora_prom4, na.rm = TRUE), 
#             portfolio_porc_mora_sd_decay3 = mean(porc_mora_sd_decay3, na.rm = TRUE)) %>%
#   ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()

# # los datos agregados se ven sospechosos antes de 2015
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_porc_puntual = mean(porc_puntual, na.rm = TRUE), 
#             portfolio_porc_puntual_prom_decay = mean(porc_puntual_prom_decay, na.rm = TRUE), 
#             portfolio_porc_puntual_prom_decay3 = mean(porc_puntual_prom_decay3, na.rm = TRUE),
#             portfolio_porc_puntual_prom4 = mean(porc_puntual_prom4, na.rm = TRUE), 
#             portfolio_porc_puntual_sd_decay = mean(porc_puntual_sd_decay, na.rm = TRUE)) %>%
#   ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()
# 
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15, 
#          fechaCorte >= as.Date('2014-12-31')) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_porc_puntual = mean(porc_puntual, na.rm = TRUE), 
#             portfolio_porc_puntual_prom_decay = mean(porc_puntual_prom_decay, na.rm = TRUE), 
#             portfolio_porc_puntual_prom_decay3 = mean(porc_puntual_prom_decay3, na.rm = TRUE),
#             portfolio_porc_puntual_prom4 = mean(porc_puntual_prom4, na.rm = TRUE)) %>%
#   ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()

# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15, 
#          fechaCorte >= as.Date('2014-12-31')) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_dias_puntual_prop_decay = mean(dias_puntual_prop_decay, na.rm = TRUE), 
#             portfolio_dias_puntual_prop_decay3 = mean(dias_puntual_prop_decay3, na.rm = TRUE), 
#             portfolio_dias_puntual_prop4 = mean(dias_puntual_prop4, na.rm = TRUE), 
#             ) %>% ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()
# 
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15, 
#          fechaCorte >= as.Date('2014-12-31')) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_dias_mora_prop_decay = mean(dias_mora_prop_decay, na.rm = TRUE), 
#             portfolio_dias_mora_prop_decay3 = mean(dias_mora_prop_decay3, na.rm = TRUE), 
#             portfolio_dias_mora_prop4 = mean(dias_mora_prop4, na.rm = TRUE), 
#   ) %>% ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()
# 
# creditos %>%
#   filter(dias_despues_fechaPago <= 15, 
#          dias_despues_fechaPago_sd_decay <= 15, 
#          fechaCorte >= as.Date('2014-12-31')) %>% 
#   group_by(fechaPago) %>%
#   summarize(portfolio_dias_puntual_sd_decay = mean(dias_puntual_sd_decay, na.rm = TRUE), 
#             portfolio_dias_mora_sd_decay3 = mean(dias_mora_sd_decay3, na.rm = TRUE)
#   ) %>% ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()

## Evaluar si se debe crear una bandera en preprocess para
## identificar cuando dias_despues_fechaPago rebasa 15 días 
## por primera vez y eliminar las entradas futuras
creditos %<>%
  filter(dias_despues_fechaPago <= 15, 
         dias_despues_fechaPago_sd_decay <= 15, 
         fechaCorte >= as.Date('2014-12-31'))

creditos %<>% 
  group_by(fechaCorte) %>%
  mutate(portfolio_porc_mora_prom_decay3 = mean(porc_mora_prom_decay3, na.rm = TRUE),
         portfolio_porc_mora_sd_decay3 = mean(porc_mora_sd_decay3, na.rm = TRUE),
         portfolio_dias_mora_prop_decay3 = mean(dias_mora_prop_decay3, na.rm = TRUE),
         portfolio_dias_mora_sd_decay3 = mean(dias_mora_sd_decay3, na.rm = TRUE)) %>%
  ungroup()

# creditos %>%
#   filter(dias_despues_fechaPago <= 15,
#          dias_despues_fechaPago_sd_decay <= 15,
#          fechaCorte >= as.Date('2014-12-31')) %>%
#   group_by(fechaPago) %>%
#   summarize(portfolio_porc_mora_prom_decay3 = mean(porc_mora_prom_decay3, na.rm = TRUE),
#             portfolio_porc_mora_sd_decay3 = mean(porc_mora_sd_decay3, na.rm = TRUE), 
#             portfolio_dias_mora_prop_decay3 = mean(dias_mora_prop_decay3, na.rm = TRUE), 
#             portfolio_dias_mora_sd_decay3 = mean(dias_mora_sd_decay3, na.rm = TRUE)
#   ) %>% ungroup() %>%
#   rename(date = fechaPago) %>%
#   gather(key, value,-date) %>%
#   ggplot(aes(x = date,
#              y = value,
#              colour = key)) +
#   geom_line()
              
system("echo '===== Saving processed data ====='")

if(params$bucket_s3 == "local") {
  write_csv(limpieza, paste0('/home/wrangle_train/output/limpieza_train_',
                            params$ymd_preprocess, '_', 
                            params$tag_collapse, '.csv'))
  write_feather(creditos, paste0('/home/wrangle_train/output/collapse_train_',
                                      params$ymd_preprocess, '_', 
                                      params$tag_collapse, '.feather'))
} else {
  s3write_using(limpieza, 
                FUN = write_csv, 
                bucket = params$bucket_s3, 
                object = paste0('limpieza_train_',
                                params$ymd_preprocess, '_', 
                                params$tag_collapse, '.csv'))
  s3write_using(creditos, 
                FUN = write_feather, 
                bucket = params$bucket_s3, 
                object = paste0('collapse_train_',
                                params$ymd_preprocess, '_', 
                                params$tag_collapse, '.feather'))
}
