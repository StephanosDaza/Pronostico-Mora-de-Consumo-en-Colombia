# Métricas para comparar las series

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

# métricas de una serie
metricas_serie <- function(d, tibc, total) {
  d <- dplyr::arrange(d, fecha)
  x <- d$mora_pct
  mes <- lubridate::month(d$fecha)
  anio <- lubridate::year(d$fecha)
  valor <- function(a, m) x[anio == a & mes == m]

  pre <- anio <= 2019
  dev <- x - tendencia_2x12(x)
  fuera <- !(anio %in% 2020:2021) & !is.na(dev)
  efecto <- tapply(dev[fuera], mes[fuera], mean)

  # correlación con la tasa rezagada 6 a 12 meses, sin 2020-2021
  cor_rezago <- sapply(6:12, function(L) {
    tasa_rez <- dplyr::left_join(
      dplyr::select(d, fecha),
      dplyr::mutate(tibc, fecha = sumar_meses(fecha, L)), by = "fecha")$tibc_consumo
    tasa_rez_ant <- dplyr::left_join(
      dplyr::select(d, fecha),
      dplyr::mutate(tibc, fecha = sumar_meses(fecha, L + 1)), by = "fecha")$tibc_consumo
    dm <- c(NA, diff(x))
    dt <- tasa_rez - tasa_rez_ant
    ok <- !is.na(dm) & !is.na(dt) & !(anio %in% 2020:2021)
    stats::cor(dm[ok], dt[ok])
  })

  part <- dplyr::left_join(dplyr::select(d, fecha, saldo),
                           dplyr::select(total, fecha, saldo_total = saldo), by = "fecha")

  tibble::tibble(
    participacion_saldo_pct = 100 * mean(part$saldo / part$saldo_total),
    entidades_min = min(d$n_entidades),
    entidades_max = max(d$n_entidades),
    mora_min = min(x),
    mora_max = max(x),
    volatilidad_pp = stats::sd(diff(x)),
    fuerza_estacional = fuerza_estacional(x[pre], mes[pre]),
    efecto_diciembre_pp = unname(efecto["12"]),
    efecto_ene_mar_pp = mean(efecto[c("1", "2", "3")]),
    dic_menor_nov = paste0(sum(sapply(2015:2025, function(a) valor(a, 12) < valor(a, 11))), "/11"),
    caida_alivios_pp = min(x[anio == 2020 & mes >= 3]) - valor(2020, 2),
    rezago_tasa = (6:12)[which.max(cor_rezago)],
    corr_tasa = max(cor_rezago)
  )
}

# tabla comparativa
comparar_series <- function(series, tibc) {
  total <- dplyr::filter(series, serie == "Consumo total")
  series |>
    dplyr::group_by(serie) |>
    dplyr::group_modify(~ metricas_serie(.x, tibc, total)) |>
    dplyr::ungroup()
}
