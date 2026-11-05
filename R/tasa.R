# Tasa de interés como regresora

# Tasa de interés rezagada.
# Recibe: fechas de la serie, la tasa mensual (fecha, tibc_consumo) y los rezagos en meses.
# Devuelve: tabla con la fecha y, para cada rezago L,
#   tasa_lL     tasa de hace L meses
#   tasa_p3_lL  promedio de la tasa en los meses t-L, t-L-1 y t-L-2
# El promedio de tres meses suaviza la tasa trimestral anterior a sep-2017.
tasa_rezagada <- function(fecha, tasa, rezagos) {
  tasa <- dplyr::arrange(tasa, fecha) |>
    dplyr::mutate(p3 = as.numeric(stats::filter(tibc_consumo, rep(1 / 3, 3), sides = 1)))
  salida <- tibble::tibble(fecha = fecha)
  for (L in rezagos) {
    t <- dplyr::transmute(tasa, fecha = sumar_meses(fecha, L), valor = tibc_consumo, p3)
    names(t)[2:3] <- c(paste0("tasa_l", L), paste0("tasa_p3_l", L))
    salida <- dplyr::left_join(salida, t, by = "fecha")
  }
  salida
}

# Correlación cruzada preblanqueada entre la tasa y la mora.
# Recibe: la serie (fecha, mora_pct), la tasa mensual, los rezagos y un vector lógico con los
# meses de mora que se excluyen del cálculo (los alivios).
# Devuelve: una fila por rezago L con la correlación entre la tasa filtrada de hace L meses y la
# mora filtrada, el número de pares y la banda de ±1,96/raíz(n). El filtro es el ARIMA que se
# ajusta a la tasa; aplicarlo a las dos series quita la persistencia propia de la tasa, de modo
# que la correlación que queda refleja la relación entre ellas (Box y Jenkins, funciones de
# transferencia). Se omiten los primeros 12 meses de la mora filtrada, que dependen del arranque.
correlacion_preblanqueada <- function(serie, tasa, rezagos = 0:18, excluir = NULL) {
  tasa <- dplyr::arrange(tasa, fecha)
  serie <- dplyr::arrange(serie, fecha)
  inicio <- function(f) c(lubridate::year(f[1]), lubridate::month(f[1]))
  x <- stats::ts(tasa$tibc_consumo, start = inicio(tasa$fecha), frequency = 12)
  fit_x <- forecast::auto.arima(x, allowdrift = FALSE, allowmean = FALSE)
  y <- stats::ts(serie$mora_pct, start = inicio(serie$fecha), frequency = 12)
  fit_y <- forecast::Arima(y, model = fit_x)
  ex <- tibble::tibble(fecha = tasa$fecha, ex = as.numeric(stats::residuals(fit_x)))
  ey <- tibble::tibble(fecha = serie$fecha, ey = as.numeric(stats::residuals(fit_y)),
                       excluir = if (is.null(excluir)) FALSE else excluir)[-(1:12), ]
  correlaciones <- purrr::map_dfr(rezagos, function(L) {
    d <- dplyr::left_join(ey, dplyr::mutate(ex, fecha = sumar_meses(fecha, L)), by = "fecha") |>
      dplyr::filter(!excluir, !is.na(ex))
    tibble::tibble(rezago = L, correlacion = stats::cor(d$ex, d$ey), n = nrow(d),
                   banda = 1.96 / sqrt(nrow(d)))
  })
  o <- forecast::arimaorder(fit_x)
  attr(correlaciones, "filtro") <- if (length(o) > 3) {
    sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)12", o[1], o[2], o[3], o[4], o[5], o[6])
  } else {
    sprintf("ARIMA(%d,%d,%d)", o[1], o[2], o[3])
  }
  correlaciones
}

# Versiones de la tasa que se comparan.
# Recibe: los rezagos.
# Devuelve: lista con el nombre de cada versión y la columna que usa ("Sin tasa" no usa ninguna).
versiones_tasa <- function(rezagos) {
  c(list("Sin tasa" = character(0)),
    setNames(as.list(paste0("tasa_l", rezagos)), paste("Tasa con rezago de", rezagos, "meses")),
    setNames(as.list(paste0("tasa_p3_l", rezagos)),
             paste("Promedio de 3 meses con rezago de", rezagos, "meses")))
}
