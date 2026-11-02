# Transformación de Box-Cox

# Lambda de Box-Cox por el método de Guerrero.
# Recibe: una serie mensual (fecha, mora_pct).
# Devuelve: el lambda que hace más pareja la relación entre nivel y variabilidad de cada año.
lambda_guerrero <- function(d) {
  d |>
    dplyr::mutate(mes = tsibble::yearmonth(fecha)) |>
    tsibble::as_tsibble(index = mes) |>
    fabletools::features(mora_pct, feasts::guerrero) |>
    dplyr::pull(lambda_guerrero)
}

# Nivel y variabilidad de cada año completo.
# Recibe: una serie mensual (fecha, mora_pct).
# Devuelve, solo para años con 12 meses: año, media, desviación estándar del nivel, rango y
# desviación estándar del cambio mensual. Esta última no se infla cuando la serie sube o baja
# durante el año, así que mide mejor la variabilidad propiamente dicha.
media_desviacion_anual <- function(d) {
  d |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(anio = lubridate::year(fecha), cambio = mora_pct - dplyr::lag(mora_pct)) |>
    dplyr::group_by(anio) |>
    dplyr::summarise(meses = dplyr::n(), media = mean(mora_pct), desviacion = stats::sd(mora_pct),
                     rango = diff(range(mora_pct)),
                     desviacion_cambio = stats::sd(cambio, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(meses == 12) |>
    dplyr::select(-meses)
}
