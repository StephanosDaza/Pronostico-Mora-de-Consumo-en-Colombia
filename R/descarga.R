# Descarga de datos.gov.co

# descarga por páginas desde la API de datos.gov.co
descargar_socrata <- function(id, where = NULL, limite = 50000) {
  base <- paste0("https://www.datos.gov.co/resource/", id, ".csv")
  partes <- list()
  offset <- 0
  repeat {
    q <- list(`$limit` = limite, `$offset` = offset, `$order` = ":id")
    if (!is.null(where)) q$`$where` <- where
    consulta <- paste(names(q),
                      vapply(q, function(v) utils::URLencode(as.character(v), reserved = TRUE), ""),
                      sep = "=", collapse = "&")
    lote <- readr::read_csv(paste0(base, "?", consulta),
                            col_types = readr::cols(.default = "c"),
                            show_col_types = FALSE, progress = FALSE)
    if (nrow(lote) == 0) break
    partes[[length(partes) + 1]] <- lote
    if (nrow(lote) < limite) break
    offset <- offset + limite
  }
  dplyr::bind_rows(partes)
}

# fechas en formato ISO (API) o dd/mm/aaaa (CSV del portal)
leer_fecha <- function(x) {
  as.Date(lubridate::parse_date_time(x, orders = c("Ymd HMS", "Ymd", "dmY"), quiet = TRUE))
}

# números con % o separador de miles
leer_numero <- function(x) {
  readr::parse_number(as.character(x), locale = readr::locale(decimal_mark = "."))
}

# cartera de consumo (rvii-eis8), solo renglón 5 para no duplicar subtotales
descargar_cartera_consumo <- function(fecha_corte) {
  where <- paste0("unicap in(1,2,3,4,5,6,29,30) AND renglon = 5 AND fecha_corte <= '",
                  format(fecha_corte, "%Y-%m-%d"), "T00:00:00'")
  crudo <- descargar_socrata("rvii-eis8", where = where)
  estandarizar_cartera(crudo)
}

# misma cartera desde un CSV del portal
leer_cartera_csv <- function(ruta, fecha_corte) {
  crudo <- readr::read_csv(ruta, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  names(crudo) <- tolower(names(crudo))
  names(crudo)[grepl("^\\(1\\)", names(crudo))] <- "_1_saldo_de_la_cartera_a"
  names(crudo)[grepl("^\\(2\\)", names(crudo))] <- "_2_vigente"
  estandarizar_cartera(crudo) |>
    dplyr::filter(unicap %in% UNICAP_CONSUMO, renglon == 5, fecha <= fecha_corte)
}

# nombres y tipos de columnas
estandarizar_cartera <- function(crudo) {
  crudo |>
    dplyr::transmute(
      tipo_entidad   = as.integer(tipo_entidad),
      codigo_entidad = as.integer(codigo_entidad),
      nombre_entidad = nombreentidad,
      fecha          = lubridate::floor_date(leer_fecha(fecha_corte), "month"),
      unicap         = as.integer(unicap),
      renglon        = as.integer(renglon),
      saldo          = leer_numero(`_1_saldo_de_la_cartera_a`),
      vigente        = leer_numero(`_2_vigente`)
    )
}

# TIBC consumo y ordinario (pare-7x5i)
descargar_tibc <- function(ruta_csv = NULL) {
  crudo <- if (is.null(ruta_csv)) {
    descargar_socrata("pare-7x5i")
  } else {
    readr::read_csv(ruta_csv, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  }
  names(crudo) <- tolower(names(crudo))
  crudo |>
    dplyr::filter(toupper(trimws(modalidad)) == "CONSUMO Y ORDINARIO") |>
    dplyr::transmute(
      fecha_resolucion = leer_fecha(fecha_resolucion),
      vigencia_desde   = leer_fecha(vigencia_desde),
      vigencia_hasta   = leer_fecha(vigencia_hasta),
      tibc             = leer_numero(interes_bancario_corriente)
    ) |>
    # por si la tasa viene como fracción
    dplyr::mutate(tibc = if (max(tibc, na.rm = TRUE) < 1) 100 * tibc else tibc)
}
