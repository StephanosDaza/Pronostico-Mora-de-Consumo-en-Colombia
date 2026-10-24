# Tabla, perfil y gráficas de cada serie
library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

series <- readr::read_csv("datos/procesados/series_mora.csv", show_col_types = FALSE) |>
  mutate(serie = factor(serie, levels = c("Consumo total", PRODUCTOS_CONSUMO$serie)))

dir.create("datos/procesados/individuales", showWarnings = FALSE, recursive = TRUE)
dir.create("figuras/individuales", showWarnings = FALSE, recursive = TRUE)

resumenes <- list()

for (nombre in levels(series$serie)) {
  d       <- filter(series, serie == nombre)
  archivo <- nombre_archivo(nombre)

  resumen <- resumen_serie(d)
  tabla   <- tabla_anio_mes(d)
  perfil  <- perfil_mensual(d)
  g_serie  <- grafico_serie(d, nombre)
  g_perfil <- grafico_perfil(perfil, nombre)

  cat("\n==============================\n", nombre, "\n==============================\n")
  print(resumen, width = Inf)
  print(tabla, n = Inf, width = Inf)
  print(perfil, n = 12)
  print(g_serie)
  print(g_perfil)

  readr::write_csv(tabla,  file.path("datos/procesados/individuales", paste0("tabla_", archivo, ".csv")))
  readr::write_csv(perfil, file.path("datos/procesados/individuales", paste0("perfil_", archivo, ".csv")))
  ggsave(file.path("figuras/individuales", paste0("serie_", archivo, ".png")),
         g_serie, width = 9, height = 4.5, dpi = 160, bg = "white")
  ggsave(file.path("figuras/individuales", paste0("perfil_", archivo, ".png")),
         g_perfil, width = 8, height = 4, dpi = 160, bg = "white")

  resumenes[[nombre]] <- mutate(resumen, serie = nombre, .before = 1)
}

# resumen de las nueve series
resumen_series <- bind_rows(resumenes)
readr::write_csv(resumen_series, "datos/procesados/resumen_series.csv")
print(resumen_series, width = Inf)

# Ejemplo: ver_serie(series, "Libranza")
