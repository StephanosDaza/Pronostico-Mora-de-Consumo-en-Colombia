# Serie de mora, tasa, validación y características
# Correr desde la raíz del proyecto

library(dplyr)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

# Parámetros
fecha_corte <- as.Date("2026-07-31")
usar_api    <- TRUE   # FALSE: leer los CSV de datos/crudos
ruta_cartera_csv <- "datos/crudos/Distribucion_de_cartera_por_producto.csv"
ruta_tibc_csv    <- "datos/crudos/Tasa_de_interes_bancario_corriente_TIBC.csv"

# Datos
if (usar_api) {
  cartera <- descargar_cartera_consumo(fecha_corte)
  tibc    <- descargar_tibc()
} else {
  cartera <- leer_cartera_csv(ruta_cartera_csv, fecha_corte)
  tibc    <- descargar_tibc(ruta_tibc_csv)
}

# Serie y tasa
serie <- construir_serie_mora(cartera)
tasa  <- tibc_mensual(tibc)

validacion <- validar_contra_icv(serie,
                                 "datos/crudos/icv_historia.xlsx",
                                 "datos/crudos/icv_cas_historia.xlsx")
message(sprintf("Validación: %d meses, diferencia máxima %.4f puntos porcentuales",
                nrow(validacion), max(abs(validacion$diferencia))))

caracteristicas <- caracteristicas_serie(serie, tasa)

print(serie, n = 12)
print(tasa, n = 12)
print(validacion, n = 12)
print(caracteristicas, width = Inf)

dir.create("datos/procesados", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(serie,           "datos/procesados/serie_mora.csv")
readr::write_csv(tasa,            "datos/procesados/tibc_mensual.csv")
readr::write_csv(validacion,      "datos/procesados/validacion_icv.csv")
readr::write_csv(caracteristicas, "datos/procesados/caracteristicas_serie.csv")
