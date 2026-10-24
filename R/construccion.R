# Índice de mora y tasa mensual

# productos de consumo (columna UNICAP)
PRODUCTOS_CONSUMO <- tibble::tribble(
  ~unicap, ~serie,
  1L,  "Crédito rotativo",
  2L,  "Tarjetas de crédito",
  3L,  "Libre inversión",
  4L,  "Libranza",
  5L,  "Vehículo",
  6L,  "Otros portafolios",
  29L, "Bajo monto",
  30L, "Empleados"
)

# mora = (saldo - vigente) / saldo, por mes
indice_mora <- function(cartera) {
  cartera |>
    dplyr::group_by(fecha) |>
    dplyr::summarise(
      # antes de sumar el saldo
      n_entidades = dplyr::n_distinct(paste(tipo_entidad, codigo_entidad)[saldo > 0]),
      saldo       = sum(saldo, na.rm = TRUE),
      vigente     = sum(vigente, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(vencida = saldo - vigente,
                  mora_pct = 100 * vencida / saldo)
}

# serie total y una serie por producto
construir_series_mora <- function(cartera) {
  total <- indice_mora(cartera) |> dplyr::mutate(serie = "Consumo total")
  por_producto <- cartera |>
    dplyr::inner_join(PRODUCTOS_CONSUMO, by = "unicap") |>
    dplyr::group_by(serie) |>
    dplyr::group_modify(~ indice_mora(.x)) |>
    dplyr::ungroup()
  dplyr::bind_rows(total, por_producto) |>
    dplyr::mutate(serie = factor(serie, levels = c("Consumo total", PRODUCTOS_CONSUMO$serie))) |>
    dplyr::arrange(serie, fecha) |>
    dplyr::select(fecha, serie, saldo, vigente, vencida, mora_pct, n_entidades)
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
