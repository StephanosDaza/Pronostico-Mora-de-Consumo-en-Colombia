# Tendencia, estacionalidad y relación con la tasa

# tendencia: media móvil centrada 2x12
tendencia_2x12 <- function(x) {
  as.numeric(stats::filter(x, c(0.5, rep(1, 11), 0.5) / 12, sides = 2))
}

# fuerza estacional, descomposición clásica (se usa 2015-2019)
fuerza_estacional <- function(x, mes) {
  tend <- tendencia_2x12(x)
  sin_tend <- x - tend
  indice <- tapply(sin_tend, mes, mean, na.rm = TRUE)
  indice <- indice - mean(indice)
  estacional <- indice[as.character(mes)]
  resto <- x - tend - estacional
  ok <- !is.na(resto)
  max(0, 1 - stats::var(resto[ok]) / stats::var(estacional[ok] + resto[ok]))
}

# sumar meses a una fecha
sumar_meses <- function(fecha, n) {
  lubridate::add_with_rollback(fecha, lubridate::period(n, units = "month"))
}

# correlación entre el cambio mensual de la mora y el de la tasa rezagada, sin 2020-2021
correlacion_tasa <- function(d, tibc, rezagos = 6:12) {
  d <- dplyr::arrange(d, fecha)
  anio <- lubridate::year(d$fecha)
  dm <- c(NA, diff(d$mora_pct))
  tasa_en <- function(L) {
    dplyr::left_join(dplyr::select(d, fecha),
                     dplyr::mutate(tibc, fecha = sumar_meses(fecha, L)), by = "fecha")$tibc_consumo
  }
  tibble::tibble(
    rezago = rezagos,
    correlacion = sapply(rezagos, function(L) {
      dt <- tasa_en(L) - tasa_en(L + 1)
      ok <- !is.na(dm) & !is.na(dt) & !(anio %in% 2020:2021)
      stats::cor(dm[ok], dt[ok])
    })
  )
}

# resumen de la serie
caracteristicas_serie <- function(d, tibc) {
  d <- dplyr::arrange(d, fecha)
  x <- d$mora_pct
  mes <- lubridate::month(d$fecha)
  anio <- lubridate::year(d$fecha)
  valor <- function(a, m) x[anio == a & mes == m]

  pre <- anio <= 2019
  dev <- x - tendencia_2x12(x)
  fuera <- !(anio %in% 2020:2021) & !is.na(dev)
  efecto <- tapply(dev[fuera], mes[fuera], mean)

  anios_dic <- unique(anio[mes == 12])
  dic_menor <- sum(sapply(anios_dic, function(a) valor(a, 12) < valor(a, 11)))

  tasa <- correlacion_tasa(d, tibc)
  i_min <- which.min(x)
  i_max <- which.max(x)

  tibble::tibble(
    meses               = nrow(d),
    desde               = min(d$fecha),
    hasta               = max(d$fecha),
    entidades_min       = min(d$n_entidades),
    entidades_max       = max(d$n_entidades),
    entidades_ultimo    = d$n_entidades[nrow(d)],
    mora_min            = x[i_min],
    fecha_min           = d$fecha[i_min],
    mora_max            = x[i_max],
    fecha_max           = d$fecha[i_max],
    mora_ultima         = x[nrow(d)],
    volatilidad_pp      = stats::sd(diff(x)),
    fuerza_estacional   = fuerza_estacional(x[pre], mes[pre]),
    efecto_diciembre_pp = unname(efecto["12"]),
    dic_menor_nov       = paste0(dic_menor, "/", length(anios_dic)),
    caida_alivios_pp    = min(x[anio == 2020 & mes >= 3]) - valor(2020, 2),
    rezago_tasa         = tasa$rezago[which.max(tasa$correlacion)],
    corr_tasa           = max(tasa$correlacion)
  )
}
