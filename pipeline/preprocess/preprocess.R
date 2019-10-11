system("echo '===== Running preprocess task ====='")

system("echo '===== Loading libraries ====='")

suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(feather))
suppressMessages(library(magrittr))
suppressMessages(library(Rcpp))
suppressMessages(library(data.table))
suppressMessages(library(aws.s3))

system("echo '===== Loading functions ====='")

max_corte_antes <- function(
  x,
  min_calendar_corte,
  calendar_corte
  ) {
  if(x < min_calendar_corte) {
    NA
    } else {
    max(calendar_corte[calendar_corte <= x])
    }
}

min_corte_despues <- function(
  x,
  max_calendar_corte,
  calendar_corte
  ) {
  if(x >= max_calendar_corte) {
    NA
  } else {
    min(calendar_corte[calendar_corte > x])
  }
}

get_fechaCorteDespues_faltante <- function(fechaCorteDespues, fechas = calendar_corte) {
  primer_fecha <- min(fechaCorteDespues)
  ultima_fecha <- max(fechaCorteDespues)
  
  !fechas[fechas >= primer_fecha & fechas <= ultima_fecha] %in% fechaCorteDespues
}

get_fechas_faltantes <- function(fechaCorteDespues) {
  fechaCorteDespues_faltante <- get_fechaCorteDespues_faltante(fechaCorteDespues)
  sum(fechaCorteDespues_faltante)
}

get_max_fechas_faltantes_seguidas <- function(fechaCorteDespues) {
  fechaCorteDespues_faltante <- get_fechaCorteDespues_faltante(fechaCorteDespues)
  strikes_faltante <- rle(fechaCorteDespues_faltante)
  max(strikes_faltante$lengths * strikes_faltante$values)
}

get_fecha_despues_max_faltantes_seguidos <- function(fechaCorteDespues) {
  fechaCorteDespues_faltante <- get_fechaCorteDespues_faltante(fechaCorteDespues)
  strikes_faltante <- rle(fechaCorteDespues_faltante)
  index <- which.max(strikes_faltante$lengths * strikes_faltante$values)
  index <- sum(strikes_faltante$lengths[1:index] * !strikes_faltante$values[1:index]) + 1
  fechaCorteDespues[index]
}

sourceCpp("./buscar_indice_primer_positivo.cpp")
sourceCpp("./buscar_indice_ultimo_positivo.cpp")

get_num_na_final <- function(x) {
  strikes_na <- rle(is.na(x))
  values <- strikes_na$values
  values[-length(values)] <- FALSE
  na_seguidos <- as.numeric(values * strikes_na$lengths)
  rep(na_seguidos, strikes_na$lengths)
}

get_num_na_antes <- function(x) {
  strikes_na <- rle(is.na(x))
  values <- strikes_na$values
  values[length(values)] <- FALSE
  na_seguidos <- as.numeric(values * strikes_na$lengths)
  rep(na_seguidos, strikes_na$lengths)
}

# get_na_seguidos <- function(x) {
#   strikes_na <- rle(is.na(x))
#   strikes_na$values * strikes_na$lengths
# }
# 
# get_na_seguidos_al_final <- function(x) {
#   na_seguidos <- get_na_seguidos(x)
#   na_seguidos[length(na_seguidos)]
# }
# 
# get_num_na_antes <- function(x) {
#   na_seguidos <- get_na_seguidos(x)
#   sum(na_seguidos[-length(na_seguidos)])
# }

get_indice_bool <- function(bool) {
  indice <- 1:sum(bool)
  indice_bool <- rep(NA, length(bool))
  indice_bool[bool] <- indice
  indice_bool
}

system("echo '===== Loading params ====='")

params <- list()

# ####
# # usar estos parámetros si se requieren leer los parametros directamente
# params$ymd_sources <- 20180907
# params$tag_sources <- "prod"
# params$ymd_preprocess <- 20180907
# params$tag_preprocess <- "prod"
# ####

args = commandArgs(trailingOnly=TRUE)
params$ymd_sources <- args[1]
params$tag_sources <- args[2]
params$ymd_preprocess <- args[3]
params$tag_preprocess <- args[4]

system("echo '===== Fetching data ====='")

# # ####
# # # usar estas líneas si se quieren leer los archivos directamente de la carpeta local
# calendario <- read_feather(paste0('../sources/output/calendario_',
#                                   params$ymd_sources, '_', 
#                                   params$tag_sources, '.feather'))
# cobranza <- read_feather(paste0('../sources/output/cobranza_',
#                                 params$ymd_sources, '_', 
#                                 params$tag_sources, '.feather'))
# movimientos <- read_feather(paste0('../sources/output/movimientos_',
#                                    params$ymd_sources, '_', 
#                                    params$tag_sources, '.feather'))
# activas <- read_feather(paste0('../sources/output/activas_', 
#                                params$ymd_sources, '_', 
#                                params$tag_sources, '.feather'))
# bajas <- read_feather(paste0('../sources/output/bajas_', 
#                              params$ymd_sources, '_', 
#                              params$tag_sources, '.feather'))
# convenios <- read_feather(paste0('../sources/output/convenios_',
#                                  params$ymd_sources, '_',
#                                  params$tag_sources, '.feather'))
# 
# # clientes_comfu <- read_feather(paste0('../sources/output/clientes_comfu_',
# #                                       params$ymd_sources, '_', 
# #                                       params$tag_sources, '.feather'))
# # voucher_companies <- read_feather(paste0('../sources/output/voucher_companies_',
# #                                          params$ymd_sources, '_', 
# #                                          params$tag_sources, '.feather'))

# ####

calendario <- s3read_using(FUN = read_feather,
                           bucket = 'datank-concredito',
                           object = paste0('/data/sources/output/calendario_',
                                           params$ymd_sources, '_',
                                           params$tag_sources, '.feather'))
cobranza <- s3read_using(FUN = read_feather,
                         bucket = 'datank-concredito',
                         object = paste0('/data/sources/output/cobranza_',
                                         params$ymd_sources, '_',
                                         params$tag_sources, '.feather'))
movimientos <- s3read_using(FUN = read_feather,
                            bucket = 'datank-concredito',
                            object = paste0('/data/sources/output/movimientos_',
                                            params$ymd_sources, '_',
                                            params$tag_sources, '.feather'))
activas <- s3read_using(FUN = read_feather,
                            bucket = 'datank-concredito',
                            object = paste0('/data/sources/output/activas_',
                                            params$ymd_sources, '_',
                                            params$tag_sources, '.feather'))
bajas <- s3read_using(FUN = read_feather,
                            bucket = 'datank-concredito',
                            object = paste0('/data/sources/output/bajas_',
                                            params$ymd_sources, '_',
                                            params$tag_sources, '.feather'))
convenios <- s3read_using(FUN = read_feather,
                            bucket = 'datank-concredito',
                            object = paste0('/data/sources/output/convenios_',
                                            params$ymd_sources, '_',
                                            params$tag_sources, '.feather'))
# 
# # clientes_comfu <- s3read_using(FUN = read_feather,
# #                                bucket = 'datank-concredito',
# #                                object = paste0('/data/sources/output/clientes_comfu_',
# #                                                params$ymd_sources, '_',
# #                                                params$tag_sources, '.feather'))
# # voucher_companies <- s3read_using(FUN = read_feather,
# #                                   bucket = 'datank-concredito',
# #                                   object = paste0('/data/sources/output/voucher_companies_',
# #                                                   params$ymd_sources, '_',
# #                                                   params$tag_sources, '.feather'))

system("echo '===== Main body ====='")

system("echo '===== Creating catalogo ====='")

calendar_corte <- calendario$fechaCorte
min_calendar_corte <- min(calendar_corte)
max_calendar_corte <- max(calendar_corte)

# min_calendar_corte
# min(cobranza$fechaCorte)

catalogo <- data_frame(fechaCredito = seq(from = ymd('20080101'), to = max_calendar_corte, by='1 day')) %>%
  group_by(fechaCredito) %>% 
  mutate(fechaCorteDespues = min_corte_despues(fechaCredito, 
                                               max_calendar_corte, 
                                               calendar_corte),
         fechaCorteAntes = max_corte_antes(fechaCredito, 
                                           min_calendar_corte, 
                                           calendar_corte)) %>% ungroup()

# ####
# # explora
# fechaCorte_en_cobranza <- unique(cobranza$fechaCorte)
# fechaCorte_en_cobranza[!fechaCorte_en_cobranza %in% catalogo$fechaCorteDespues]
# # existen fechas de corte en cobranza que no son fechas de corte en el catalogo
# # al parecer antes de 2010 fechaCorte en cobranza era el 15 y 30 de cada mes 
# ####

system("echo '===== Creating payments ====='")

cobranza %<>% 
  left_join(catalogo, by = c("fechaPago" = "fechaCredito")) %>% # para que todas las colocadoras tengan las mismas fechas de corte se hace el join por fechaPago
  filter(fechaCorteAntes < ymd(params$ymd_preprocess)) %>% # eliminamos las entradas con fechaCorteAntes depues a la fecha de corte evaludada
  arrange(pkcolocadora, fechaCorteAntes)

# unique(cobranza$fechaCorte[cobranza$fechaCorte != cobranza$fechaCorteAntes])
# distinct <- cobranza %>%
#   group_by(fechaCorteAntes) %>%
#   summarize(fechaCorteDespues = n_distinct(fechaCorteDespues),
#             fechaPago = n_distinct(fechaPago))
# table(distinct$fechaCorteDespues)
# table(distinct$fechaPago)
# distinct %>% filter(fechaPago > 1)

# distinct <- cobranza %>% 
#   group_by(pkcolocadora) %>% 
#   summarize(min_fecha = n_distinct(min_fecha)) 
# table(distinct$min_fecha)
  
min_fecha_movimientos <- min(movimientos$fechaMovimiento[movimientos$tipoMovimiento == 'ABONO'])
limpieza_1 <- cobranza %>% 
  group_by(pkcolocadora, min_fecha) %>% 
  summarize(min_fechaCorte = min(fechaCorte), #se busca el min en fechaCorte y no en fechaCorteAntes porque el catalogo tiene na's
            max_fechaCorteDespues = max(fechaCorteDespues), 
            num_indices_total = n()) %>% ungroup() %>% 
  mutate(min_fecha_na = is.na(min_fechaCorte) * 1, 
         fin_antes_movimientos = (max_fechaCorteDespues <= min_fecha_movimientos) * 1)
# sum(limpieza_1$min_fecha_na)
# length(unique(limpieza_1$pkcolocadora[limpieza_1$fin_antes_movimientos == 1]))

# distinct <- cobranza %>%
#   group_by(pkcolocadora,
#            fechaPago,
#            fechaCorteAntes,
#            fechaCorteDespues) %>%
#   summarize(num_pktipodevale = n_distinct(pktipodevale),
#             num_fechaCorte = n_distinct(fechaCorte),
#             num_fechaAbono = n_distinct(fechaAbono),
#             num_nivelColocadora = n_distinct(nivelColocadora),
#             num_min_fecha = n_distinct(min_fecha))
# table(distinct$num_pktipodevale) # 3
# table(distinct$num_fechaCorte) # 1
# table(distinct$num_fechaAbono) # 2
# table(distinct$num_nivelColocadora) # 2
# table(distinct$num_min_fecha) # 1
# table(distinct$num_pktipodevale[distinct$num_fechaAbono > 1])
# table(distinct$num_pktipodevale[distinct$num_nivelColocadora > 1])

# cobranza %>%
#   left_join(distinct) %>%
#   filter(num_pktipodevale >1) %>%
#   select(pkcolocadora,
#          pktipodevale,
#          fechaCorteDespues,
#          prestamoPersonal) %>% View()

# dim(cobranza)
payments <- cobranza %>%
  filter(fechaCorteDespues > min_fecha_movimientos) %>% # eliminamos las entradas previas a la primer fecha donde tenemos movimientos. Usamos fechaCorte porque fechaCorteAntes tiene NA's
  group_by(pkcolocadora, 
           fechaPago, 
           fechaCorteAntes, 
           fechaCorteDespues, 
           min_fecha) %>%
  summarise(fechaAbono = max(fechaAbono), 
            minimoPagar = sum(minimoPagar), 
            saldoActual = sum(saldoActual), 
            vencidoActual = sum(vencidoActual), 
            pagoQuincenal = sum(pagoQuincenal), 
            importeAbonado = sum(importeAbonado), 
            prestamoPersonal = sum(prestamoPersonal), 
            nivelColocadora = paste(nivelColocadora, collapse = ' ')) %>% ungroup()
# min(payments$fechaCorteDespues)
# dim(payments)

catalogoFechas <- unique(payments[c('fechaCorteAntes', 'fechaPago', 'fechaCorteDespues')])

limpieza_2 <- payments %>% 
  group_by(pkcolocadora) %>% 
  summarize(fechas_faltantes = get_fechas_faltantes(fechaCorteDespues), 
            fechas_repetidas = sum(duplicated(fechaCorteDespues)), #teniamos fechas repetidas en el release 0. No tenemos fechas repetidas desde el release 1
            max_fechas_faltantes_seguidas = get_max_fechas_faltantes_seguidas(fechaCorteDespues), 
            fecha_despues_max_faltantes_seguidos = get_fecha_despues_max_faltantes_seguidos(fechaCorteDespues), 
            indice_primer_saldo = buscar_indice_primer_positivo(saldoActual, 199), 
            indice_ultimo_saldo = buscar_indice_ultimo_positivo(saldoActual, 199), 
            num_indices_desde_movimientos = n()) %>% ungroup() %>% 
  mutate(num_indices_saldo = indice_ultimo_saldo - indice_primer_saldo + 1, 
         ningun_saldo_positivo = if_else((indice_primer_saldo == 0), 1, 0), 
         un_saldo_positivo = if_else((indice_primer_saldo != 0 & indice_primer_saldo == indice_ultimo_saldo), 1, 0), 
         historia_saldo_corta_preprocess = if_else((num_indices_saldo <= 6), 1, 0))
# table(limpieza_2$fechas_faltantes[limpieza_2$fechas_faltantes > 0])
# sum(limpieza_2$fechas_repetidas) # ya no hay fechas repetidas
# table(limpieza_2$max_fechas_faltantes_seguidas[limpieza_2$max_fechas_faltantes_seguidas > 0])

# table(limpieza_2$indice_primer_saldo)
# hist(limpieza_2$num_indices_desde_movimientos)
# hist(limpieza_2$num_indices_saldo)
# hist(limpieza_2$indice_ultimo_saldo)
# table(limpieza_2$ningun_saldo_positivo)
# table(limpieza_2$un_saldo_positivo)
# sum(limpieza_2$historia_saldo_corta_preprocess)

# limpieza_2 %>%
#   select(max_fechas_faltantes_seguidas, fecha_despues_max_faltantes_seguidos) %>%
#   filter(max_fechas_faltantes_seguidas > 1) %>%
#   pull(fecha_despues_max_faltantes_seguidos) %>%
#   table()

# View(limpieza_2 %>%
#        select(pkcolocadora,
#               max_fechas_faltantes_seguidas,
#               fecha_despues_max_faltantes_seguidos))

payments %<>%
  left_join(limpieza_2 %>% 
              select(pkcolocadora, 
                     indice_primer_saldo, 
                     indice_ultimo_saldo)) %>% 
  group_by(pkcolocadora) %>% 
  mutate(indice = row_number()) %>% ungroup() %>% 
  filter(indice >= indice_primer_saldo, 
         indice <= indice_ultimo_saldo) 
# dim(payments)

limpieza_3 <- payments %>% 
  group_by(pkcolocadora) %>% 
  summarize(fecha_ultimo_saldo = max(fechaCorteDespues)) %>% ungroup() %>% 
  mutate(predict_sin_saldo = (fecha_ultimo_saldo < ymd(params$ymd_preprocess)) * 1)
# summary(limpieza_3$fecha_ultimo_saldo)
# sum(limpieza_3$predict_sin_saldo)

limpieza <- limpieza_1 %>% 
  left_join(limpieza_2) %>% 
  left_join(limpieza_3)

limpieza %<>% 
  mutate(ex_preprocess = (min_fecha_na | 
                            fin_antes_movimientos | 
                            ningun_saldo_positivo |
                            un_saldo_positivo | 
                            historia_saldo_corta_preprocess) * 1) 

# sum(is.na(limpieza$ex_preprocess))

# valida <- payments %>%
#   left_join(limpieza)
# ningun_saldo_positivo se elimina al filtrar payments con limpieza_2
# sum(valida$un_saldo_positivo)
# valida %>% filter(max_fechas_faltantes_seguidas > 50) %>% View()
# valida %>% filter(un_saldo_positivo == 1) %>% View()

payments %<>%
  left_join(limpieza %>% 
              select(pkcolocadora, 
                     max_fechaCorteDespues, 
                     ex_preprocess)) %>% 
  filter(ex_preprocess == 0)
# dim(payments)

payments %<>% 
  mutate(fechaAbono_na = is.na(fechaAbono) * 1) %>% 
  group_by(pkcolocadora) %>% 
  mutate(num_fechaAbono_na = sum(is.na(fechaAbono)), 
         fechaAbono_num_na_final = get_num_na_final(fechaAbono), 
         fechaAbono_num_na_antes = get_num_na_antes(fechaAbono)) %>% ungroup()
# table(payments$num_fechaAbono_na)
# payments %>% filter(pkcolocadora == 1) %>% View()

# payments %>%
#   mutate(dias_despues_fechaCorte = as.numeric(fechaAbono - fechaCorteDespues)) %>%
#   filter(dias_despues_fechaCorte > 0) %>%
#   pull(dias_despues_fechaCorte) %>% summary()
payments %<>%
  mutate(fechaAbono = if_else((fechaAbono_num_na_antes > 0), 
                              fechaPago, fechaAbono), 
         fechaAbono = if_else((fechaAbono_num_na_final > 0), 
                              max_fechaCorteDespues + 15, fechaAbono), 
         dias_despues_fechaPago = as.numeric(fechaAbono - fechaPago), 
         dias_despues_fechaCorte = as.numeric(fechaAbono - fechaCorteDespues))
# payments %>%
#   filter(dias_despues_fechaCorte > 0) %>%
#   pull(dias_despues_fechaCorte) %>% summary()
# payments %>% group_by(fechaCorteDespues) %>% summarize(num = n_distinct(pkcolocadora)) %>% View()

system("echo '===== Creating pay_expanded ====='")

catalogoPagos <-
  payments %>%
  select(pkcolocadora,
         fechaCorteDespues,
         fechaAbono) %>%
  rename(fechaCorteAntes = fechaCorteDespues, 
         fechaAbonoAntes = fechaAbono) %>% 
  right_join(
    payments %>%
      select(pkcolocadora,
             fechaCorteAntes,
             fechaCorteDespues,
             fechaAbono,
             fechaPago)) 
# sum(is.na(catalogoPagos$fechaAbonoAntes)) #aparecen NAs pues la primer observacion por colocadora no tiene abonoAntes al hacer el join

catalogoPagos %<>% 
  mutate(fechaAbonoAntes = if_else(is.na(fechaAbonoAntes), 
                                   fechaAbono,
                                   fechaAbonoAntes)) %>% 
  mutate(fechaAbonoAntes = if_else(fechaAbonoAntes == fechaAbono,
                                   fechaAbonoAntes,
                                   fechaAbonoAntes + 1)) %>% 
  filter(fechaAbonoAntes <= fechaAbono) #eliminamos aquellos intervalos donde fechaAbonoAntes es mayor a fechaAbono
#View(catalogoPagos)

pay_expanded <-
  setDT(catalogoPagos)[,list(idnum = pkcolocadora,
                            fechaCorteAntes = fechaCorteAntes,
                            fechaMovimiento = seq(fechaAbonoAntes,
                                                  fechaAbono,
                                                  by = "day")), by = 1:nrow(catalogoPagos)] %>% 
  rename(pkcolocadora=idnum)

# # pay_expanded %>% write_feather(paste0('output/pay_expanded_',
# #                                   params$ymd_preprocess,
# #                                   '.feather'))
# # pay_expanded <- read_feather(paste0('output/pay_expanded_',
# #                                     params$ymd_preprocess,
# #                                     '.feather'))

#dim(movimientos)
#sum(movimientos$pkcolocadora == 0)
#table(movimientos$tipoMovimiento)
movimientos %<>% 
  filter(tipoMovimiento == 'ABONO') %>% 
  select(-tipoMovimiento) 

# movimientos %>%
#   group_by(pkcolocadora, fechaMovimiento) %>%
#   summarize(num = n()) %>% pull(num) %>% table()
movimientos %<>% 
  group_by(pkcolocadora, fechaMovimiento) %>% 
  summarize(importeMovimiento = sum(importeMovimiento)) %>% ungroup() %>% 
  mutate(idMovimiento = row_number())
# movimientos %>%
#   group_by(pkcolocadora, fechaMovimiento) %>%
#   summarize(num = n()) %>% pull(num) %>% table()
# dim(movimientos)

pay_expanded %<>% 
  left_join(as.data.table(movimientos))

# idPay <- unique(pay_expanded$idMovimiento)
# valida <- movimientos %>%
#   filter(!idMovimiento %in% idPay)
# valida %>%
#   group_by(pkcolocadora) %>%
#   summarize(num = n()) %>% arrange(desc(num))
# quedan fuera los movimientos de colocadoras por indices antes de primer saldo y ultimo saldo por limpieza 

# sum(is.na(pay_expanded$importeMovimiento))
# sum(!is.na(pay_expanded$importeMovimiento))
pay_expanded %<>% 
  filter(!is.na(importeMovimiento)) %>%
  left_join(as.data.table(catalogoFechas)) %>% 
  mutate(periodo = if_else(fechaMovimiento <= fechaPago,
                          "puntual",
                          if_else(fechaMovimiento <= fechaCorteDespues,
                                  "impuntual", "rescate")))
# dim(pay_expanded)

# se asigna un mismo pago en diferentes periodos cuando los periodos tienen intersecciones
pay_expanded %<>% 
  group_by(idMovimiento) %>% 
  mutate(veces_idMovimiento = n()) %>% ungroup() %>% 
  mutate(importeMovimiento = importeMovimiento / veces_idMovimiento)

pay_expanded %<>% 
  group_by(pkcolocadora, fechaCorteAntes, periodo) %>% 
  summarise(abono = sum(importeMovimiento)) %>% ungroup() %>% 
  spread(periodo, abono, fill = 0) %>% 
  select(pkcolocadora, fechaCorteAntes, puntual, impuntual, rescate)

# dim(pay_expanded)
# ultimocorte <- pay_expanded[pay_expanded$fechaCorteAntes == max(pay_expanded$fechaCorteAntes), ]
# View(ultimocorte)
# dim(ultimocorte)

system("echo '===== Creating design_matrix ====='")

design_matrix <- payments %>% 
  mutate(antiguedad = fechaCorteDespues - min_fecha)%>% 
  left_join(pay_expanded)%>% 
  mutate(importeMovimiento = puntual + impuntual + rescate, 
         porc_puntual = puntual / minimoPagar, 
         porc_impuntual = impuntual / minimoPagar, 
         porc_rescate = rescate / minimoPagar)
# dim(design_matrix)
# ultimocorte <- design_matrix[design_matrix$fechaCorteDespues == ymd(params$ymd_preprocess), ]
# dim(ultimocorte)

design_matrix %<>% 
  mutate(importeMovimiento_na = is.na(importeMovimiento) * 1) %>% 
  group_by(pkcolocadora) %>% 
  mutate(num_importeMovimiento_na = sum(is.na(importeMovimiento)), 
         importeMovimiento_num_na_final = get_num_na_final(importeMovimiento), 
         importeMovimiento_num_na_antes = get_num_na_antes(importeMovimiento)) %>% ungroup()
# table(design_matrix$num_importeMovimiento_na)
# table(design_matrix$importeMovimiento_num_na_final)
# table(design_matrix$importeMovimiento_num_na_antes)

# sum(design_matrix$importeMovimiento_na)
# sum(design_matrix$fechaAbono_na)
# sum(design_matrix$fechaAbono_na > 0 &
#       design_matrix$importeMovimiento_na > 0)
# sum(design_matrix$fechaAbono_num_na_antes > 0 &
#       design_matrix$importeMovimiento_num_na_antes > 0)
# sum(design_matrix$fechaAbono_num_na_final > 0 &
#       design_matrix$importeMovimiento_num_na_final > 0)
# sum(design_matrix$fechaAbono_num_na_final > 0)
# sum(design_matrix$importeMovimiento_num_na_final > 0)
# sum(design_matrix$fechaAbono_num_na_antes > 0)
# sum(design_matrix$importeMovimiento_num_na_antes > 0)

design_matrix %<>% 
  group_by(pkcolocadora) %>% 
  mutate(indice_fechaAbono_num_na_final = get_indice_bool(fechaAbono_num_na_final > 0), 
         saldo_prom_fechaAbono_num_na_final = mean(saldoActual[fechaAbono_num_na_final > 0]), 
         minimoPagar_prom_fechaAbono_num_na_final = mean(minimoPagar[fechaAbono_num_na_final > 0])) %>% 
  ungroup()

# pueden haber pagos anticipado y ningun movimiento registrado antes de la salida sin default
design_matrix %<>% 
  mutate(row_salida_sin_default = (importeMovimiento_num_na_final & 
                                     fechaAbono_num_na_final & 
                                     saldo_prom_fechaAbono_num_na_final < 5000 &
                                     max_fechaCorteDespues != ymd(params$ymd_preprocess)), 
         row_default = (importeMovimiento_num_na_final & 
                          fechaAbono_num_na_final & 
                          !row_salida_sin_default)) %>% 
  group_by(pkcolocadora) %>% 
  mutate(salida_sin_default = (sum(row_salida_sin_default, na.rm = TRUE) > 0) * 1, 
         default = (sum(row_default, na.rm = TRUE) > 0) * 1) %>% 
  ungroup()

limpieza_4 <- design_matrix %>% 
  group_by(pkcolocadora, 
           num_fechaAbono_na, 
           num_importeMovimiento_na, 
           saldo_prom_fechaAbono_num_na_final, 
           minimoPagar_prom_fechaAbono_num_na_final, 
           salida_sin_default,
           default) %>% 
  summarize(fechaAbono_num_na_final = max(fechaAbono_num_na_final),  
            importeMovimiento_num_na_final = max(importeMovimiento_num_na_final)) %>% 
  mutate(fechaAbono_num_na_antes = num_fechaAbono_na - fechaAbono_num_na_final, 
         importeMovimiento_num_na_antes = num_importeMovimiento_na - importeMovimiento_num_na_final)

design_matrix %<>% 
  group_by(pkcolocadora) %>% 
  filter(!row_salida_sin_default, 
         !(row_default & 
             !indice_fechaAbono_num_na_final %in% 
             c(1,2,max(indice_fechaAbono_num_na_final, na.rm = TRUE)))) %>% 
  ungroup() %>% 
  select(-row_salida_sin_default, -row_default)
# sum(design_matrix$importeMovimiento_num_na_final > 0)
# sum(design_matrix$importeMovimiento_na)

design_matrix %<>% 
  group_by(pkcolocadora) %>% 
  mutate(indice_preprocess = row_number(), 
         indice_preprocess_desc = sort(indice_preprocess, 
                                  decreasing = TRUE)) %>% ungroup()

limpieza_5 <- design_matrix %>% 
  group_by(pkcolocadora) %>% 
  summarize(num_indices_preprocess = n())
  

# bool_mov_na_puntual
bool_mov_na_puntual <- design_matrix$importeMovimiento_na & 
  design_matrix$dias_despues_fechaPago <= 0
# sum(bool_mov_na_puntual)
# sum(is.na(bool_mov_na_puntual))

# bool_impuntual
bool_mov_na_impuntual <- design_matrix$importeMovimiento_na & 
  design_matrix$dias_despues_fechaPago > 0 & 
  design_matrix$dias_despues_fechaCorte <= 0
# sum(bool_mov_na_impuntual)
# sum(is.na(bool_mov_na_impuntual))

# bool_mov_na_rescate
bool_mov_na_rescate <- design_matrix$importeMovimiento_na & 
  design_matrix$dias_despues_fechaCorte > 0
# sum(bool_mov_na_rescate)
# sum(is.na(bool_mov_na_rescate))

design_matrix$porc_puntual[bool_mov_na_puntual] <- 1.0
design_matrix$porc_impuntual[bool_mov_na_puntual] <- 0.0
design_matrix$porc_rescate[bool_mov_na_puntual] <- 0.0

design_matrix$porc_puntual[bool_mov_na_impuntual] <- 0.0
design_matrix$porc_impuntual[bool_mov_na_impuntual] <- 1.0
design_matrix$porc_rescate[bool_mov_na_impuntual] <- 0.0

design_matrix$porc_puntual[bool_mov_na_rescate] <- 0.0
design_matrix$porc_impuntual[bool_mov_na_rescate] <- 0.0
design_matrix$porc_rescate[bool_mov_na_rescate] <- 1.0

# sum(is.na(design_matrix$porc_puntual))
# sum(is.na(design_matrix$porc_impuntual))
# sum(is.na(design_matrix$porc_rescate))
# sum(design_matrix$porc_puntual < 0 | 
#       design_matrix$porc_impuntual < 0 | 
#       design_matrix$porc_rescate < 0 )
# sum(design_matrix$porc_puntual == 0 & 
#       design_matrix$porc_impuntual == 0 & 
#       design_matrix$porc_rescate == 0)

# design_matrix %>% filter(pkcolocadora == 1) %>% View()

# bool_error 
bool_error <- design_matrix$porc_puntual >= 2 | 
  design_matrix$porc_impuntual >= 2 | 
  design_matrix$porc_rescate >= 2
# sum(bool_error)
# sum(is.na(bool_error))

# bool_puntual
bool_puntual <- bool_error & 
  design_matrix$dias_despues_fechaPago <= 0
# sum(bool_puntual)
# sum(is.na(bool_puntual))

# bool_impuntual
bool_impuntual <- bool_error & 
  design_matrix$dias_despues_fechaPago > 0 & 
  design_matrix$dias_despues_fechaCorte <= 0
# sum(bool_impuntual)
# sum(is.na(bool_impuntual))

# bool_rescate
bool_rescate <- bool_error & 
  design_matrix$dias_despues_fechaCorte > 0
# sum(bool_rescate)
# sum(is.na(bool_rescate))

design_matrix$porc_puntual[bool_puntual] <- 1
design_matrix$porc_impuntual[bool_puntual] <- 0
design_matrix$porc_rescate[bool_puntual] <- 0

design_matrix$porc_puntual[bool_impuntual] <- 0.5
design_matrix$porc_impuntual[bool_impuntual] <- 0.5
design_matrix$porc_rescate[bool_impuntual] <- 0

design_matrix$porc_puntual[bool_rescate] <- 0.3
design_matrix$porc_impuntual[bool_rescate] <- 0.3
design_matrix$porc_rescate[bool_rescate] <- 0.4

# valida <- design_matrix %>%
#   filter(pkcolocadora %in% bajas$pkcolocadora)
# sum(valida$salida_sin_default == 0 & valida$default == 0)
# View(valida %>% filter(salida_sin_default == 0, default == 0))

system("echo '===== Creating limpieza ====='")

# sum(limpieza$ex_preprocess == 0)
limpieza %<>%
  left_join(limpieza_4) %>% 
  left_join(limpieza_5) %>% 
  mutate(ex_preprocess = (!pkcolocadora %in% design_matrix$pkcolocadora) * 1)
# sum(limpieza$ex_preprocess == 0)

activas %<>% 
  mutate(activa = (statusColocadora == 'A') * 1)
# View(activas)
# sum(activas$activa)

bajas %<>% 
  mutate(baja = 1) %>% 
  rename(tipoBaja = descripcionTipoConcepto) %>% 
  filter(fechaBaja <= ymd(params$ymd_preprocess))
# max(bajas$fechaBaja)
# table(bajas$tipoBaja)
# sum(is.na(bajas$tipoBaja))
# View(bajas)

limpieza %<>% 
  left_join(activas %>% 
              select(pkcolocadora, 
                     activa)) %>% 
  left_join(bajas %>% 
              select(pkcolocadora, 
                     baja, 
                     tipoBaja))

limpieza$baja[is.na(limpieza$baja)] <- 0

# dim(convenios)
convenios %<>% 
  rename(fechaInicio_convenio_vigente = fechaAutorizacion, 
         fechaFin_convenio_vigente = fechaLimiteLiquidacion) %>% 
  filter((fechaInicio_convenio_vigente <= ymd(params$ymd_preprocess) & 
            fechaFin_convenio_vigente >= ymd(params$ymd_preprocess))) %>% 
  mutate(convenio = 1)
convenios <- convenios[!duplicated(convenios$pkcolocadora), ]
# dim(convenios)

limpieza %<>% 
  left_join(convenios %>% 
              select(pkcolocadora, 
                     convenio, 
                     fechaInicio_convenio_vigente, 
                     fechaFin_convenio_vigente))

limpieza$convenio[is.na(limpieza$convenio)] <- 0

# View(limpieza)

# # system("echo '===== Creating client_data ====='")
# # 
# # client_data <-
# #   clientes_comfu %>% 
# #   mutate(fechaCredito = ymd(fechaCredito)) %>%
# #   left_join(catalogo) %>% 
# #   arrange(fechaCorteDespues) %>% 
# #   group_by(pkcolocadora, fechaCorteDespues) %>% 
# #   summarise(valeSum = sum(capital),
# #             valeCuenta = n(),
# #             clienteCuenta = n_distinct(pkcliente)) %>% 
# #   ungroup %>% 
# #   left_join(
# #     voucher_companies %>%
# #       filter(tipo == 'VALES') %>% 
# #       group_by(pkcolocadora) %>% 
# #       summarise(companies_worked = n())
# #   )
# # 
# # client_data$companies_worked[is.na(client_data$companies_worked)] <- 0

system("echo '===== Saving processed data ====='")

# ####
# # usar estas lineas si se quiere escribir el resultado directamente en la carpeta local
# write_feather(limpieza, paste0('./output/limpieza_',
#                                params$ymd_preprocess, '_', 
#                                params$tag_preprocess, '.feather'))
# write_feather(design_matrix, paste0('./output/design_matrix_',
#                                     params$ymd_preprocess, '_', 
#                                     params$tag_preprocess, '.feather'))
# ####

s3write_using(limpieza,
              FUN = write_feather,
              bucket = 'datank-concredito',
              object = paste0('/data/preprocess/output/limpieza_',
                              params$ymd_preprocess, '_', 
                              params$tag_preprocess, '.feather'))
s3write_using(design_matrix,
              FUN = write_feather,
              bucket = 'datank-concredito',
              object = paste0('/data/preprocess/output/design_matrix_',
                              params$ymd_preprocess, '_', 
                              params$tag_preprocess, '.feather'))
