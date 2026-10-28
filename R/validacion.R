# Validación contra el ICV oficial

# ICV de consumo, total establecimientos de crédito (sirve con y sin castigos)
leer_icv_consumo <- function(ruta) {
  crudo <- readxl::read_excel(ruta, sheet = 1, col_names = FALSE, col_types = "text",
                              .name_repair = function(n) paste0("c", seq_along(n)))
  m <- as.matrix(crudo)
  # se busca por contenido porque read_excel salta filas vacías
  pos <- which(matrix(grepl("Cartera Consumo", m), nrow(m)), arr.ind = TRUE)[1, ]
  col_etiqueta <- pos[["col"]]
  etiquetas <- trimws(m[, col_etiqueta])
  debajo <- (pos[["row"]] + 1):nrow(m)
  fila <- debajo[which(etiquetas[debajo] == "Establecimientos de Crédito")[1]]
  # fila de fechas (serial de Excel o texto)
  serial <- suppressWarnings(as.numeric(m))
  dias_serial <- ifelse(!is.na(serial) & serial > 36000 & serial < 60000, serial - 25569, NA)
  dias_texto <- as.numeric(as.Date(substr(m, 1, 10), format = "%Y-%m-%d"))
  dias <- matrix(dplyr::coalesce(dias_serial, dias_texto), nrow(m))
  fila_fechas <- which.max(rowSums(!is.na(dias)))
  columnas <- which(!is.na(dias[fila_fechas, ]))
  fechas <- as.Date(dias[fila_fechas, columnas], origin = "1970-01-01")
  valores <- suppressWarnings(as.numeric(m[fila, columnas])) * 100
  tibble::tibble(fecha = lubridate::floor_date(fechas, "month"), valor = valores) |>
    dplyr::filter(!is.na(valor))
}

# serie construida frente al ICV
validar_contra_icv <- function(serie, ruta_icv, ruta_icv_castigos) {
  icv  <- leer_icv_consumo(ruta_icv) |> dplyr::rename(icv_oficial = valor)
  icvc <- leer_icv_consumo(ruta_icv_castigos) |> dplyr::rename(icv_con_castigos = valor)
  serie |>
    dplyr::select(fecha, mora_construida = mora_pct) |>
    dplyr::inner_join(icv, by = "fecha") |>
    dplyr::inner_join(icvc, by = "fecha") |>
    dplyr::mutate(diferencia = mora_construida - icv_oficial)
}
