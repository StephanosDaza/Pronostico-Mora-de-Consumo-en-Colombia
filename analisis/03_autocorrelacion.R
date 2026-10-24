# ACF y PACF de cada serie
library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

series <- readr::read_csv("datos/procesados/series_mora.csv", show_col_types = FALSE) |>
  mutate(serie = factor(serie, levels = c("Consumo total", PRODUCTOS_CONSUMO$serie)))

dir.create("figuras/autocorrelacion", showWarnings = FALSE, recursive = TRUE)

for (nombre in levels(series$serie)) {
  d       <- filter(series, serie == nombre)
  archivo <- nombre_archivo(nombre)

  g_niveles <- grafico_acf_pacf(d, nombre, diferencia = "ninguna")
  g_dif     <- grafico_acf_pacf(d, nombre, diferencia = "ambas")

  print(g_niveles)
  print(g_dif)

  ggsave(file.path("figuras/autocorrelacion", paste0("acf_niveles_", archivo, ".png")),
         g_niveles, width = 9, height = 4, dpi = 160, bg = "white")
  ggsave(file.path("figuras/autocorrelacion", paste0("acf_diferenciada_", archivo, ".png")),
         g_dif, width = 9, height = 4, dpi = 160, bg = "white")
}

# grafico_acf_pacf(filter(series, serie == "Consumo total"), "Consumo total", diferencia = "regular")
