# Partición de la muestra en entrenamiento y evaluación

# Último mes de entrenamiento.
# Recibe: fecha de corte, horizonte y número de orígenes.
# Devuelve: el mes que deja al final de la serie los orígenes + horizonte meses de evaluación,
# es decir, uno más de los orígenes + horizonte - 1 estrictamente necesarios.
fin_entrenamiento <- function(fecha_corte, horizonte, n_origenes) {
  sumar_meses(lubridate::floor_date(fecha_corte, "month"), -(n_origenes + horizonte))
}

# Marca cada mes como entrenamiento o evaluación.
# Recibe: la serie (fecha, mora_pct) y el último mes de entrenamiento.
# Devuelve: fecha, mora_pct y muestra ("entrenamiento" o "evaluacion").
partir_serie <- function(serie, fin_train) {
  serie |>
    dplyr::arrange(fecha) |>
    dplyr::select(fecha, mora_pct) |>
    dplyr::mutate(muestra = dplyr::if_else(fecha <= fin_train, "entrenamiento", "evaluacion"))
}

# Resumen de la partición y verificación del origen móvil.
# Recibe: la serie partida, el horizonte y el número de orígenes.
# Devuelve: desde, hasta y meses de cada parte, con el primer y el último origen y el último
# mes pronosticado. Se detiene si la evaluación no alcanza para los orígenes pedidos.
resumen_particion <- function(serie_partida, horizonte, n_origenes) {
  partes <- serie_partida |>
    dplyr::group_by(muestra) |>
    dplyr::summarise(desde = min(fecha), hasta = max(fecha), meses = dplyr::n(), .groups = "drop")
  n_eval <- partes$meses[partes$muestra == "evaluacion"]
  necesarios <- n_origenes + horizonte - 1
  if (length(n_eval) == 0 || n_eval < necesarios) {
    stop(sprintf("La evaluación tiene %d meses y se necesitan %d para %d orígenes a %d meses.",
                 if (length(n_eval) == 0) 0L else n_eval, necesarios, n_origenes, horizonte))
  }
  fin_train <- partes$hasta[partes$muestra == "entrenamiento"]
  tibble::tibble(
    entrenamiento_desde = partes$desde[partes$muestra == "entrenamiento"],
    entrenamiento_hasta = fin_train,
    meses_entrenamiento = partes$meses[partes$muestra == "entrenamiento"],
    evaluacion_desde    = partes$desde[partes$muestra == "evaluacion"],
    evaluacion_hasta    = partes$hasta[partes$muestra == "evaluacion"],
    meses_evaluacion    = n_eval,
    meses_necesarios    = necesarios,
    primer_origen       = fin_train,
    ultimo_origen       = sumar_meses(fin_train, n_origenes - 1),
    ultimo_pronostico   = sumar_meses(fin_train, n_origenes - 1 + horizonte)
  )
}
