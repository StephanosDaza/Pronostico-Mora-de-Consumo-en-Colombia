# Índice de mora de consumo y tasa mensual

# productos de consumo (UNICAP): crédito rotativo, tarjetas, libre inversión, libranza,
# vehículo, otros portafolios, bajo monto y empleados
UNICAP_CONSUMO <- c(1:6, 29, 30)

# mora = (saldo - vigente) / saldo, sumando productos y entidades por mes
construir_serie_mora <- function(cartera) {
  cartera |>
    dplyr::filter(unicap %in% UNICAP_CONSUMO) |>
    dplyr::group_by(fecha) |>
    dplyr::summarise(
      # antes de sumar el saldo
      n_entidades = dplyr::n_distinct(paste(tipo_entidad, codigo_entidad)[saldo > 0]),
      saldo       = sum(saldo, na.rm = TRUE),
      vigente     = sum(vigente, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(vencida = saldo - vigente,
                  mora_pct = 100 * vencida / saldo) |>
    dplyr::arrange(fecha)
}

# TIBC mensual (trimestral hasta ago-2017); si hay dos resoluciones en un mes queda la última
tibc_mensual <- function(tibc) {
  tibc |>
    dplyr::arrange(fecha_resolucion) |>
    dplyr::mutate(fecha = purrr::map2(
      lubridate::floor_date(vigencia_desde, "month"),
      lubridate::floor_date(vigencia_hasta, "month"),
      ~ seq(.x, .y, by = "month"))) |>
    tidyr::unnest(fecha) |>
    dplyr::group_by(fecha) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(fecha, tibc_consumo = tibc)
}
