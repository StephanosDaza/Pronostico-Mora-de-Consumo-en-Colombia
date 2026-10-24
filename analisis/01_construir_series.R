# Series, tasa, validación y comparación
library(dplyr)
library(ggplot2)

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

# Series
series <- construir_series_mora(cartera)
tasa   <- tibc_mensual(tibc)

validacion <- validar_contra_icv(series,
                                 "datos/crudos/icv_historia.xlsx",
                                 "datos/crudos/icv_cas_historia.xlsx")
message(sprintf("Validación: %d meses, diferencia máxima %.4f puntos porcentuales",
                nrow(validacion), max(abs(validacion$diferencia))))

comparacion <- comparar_series(series, tasa)

print(series, n = 12)
print(tasa, n = 12)
print(validacion, n = 12)

dir.create("datos/procesados", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(series,      "datos/procesados/series_mora.csv")
readr::write_csv(tasa,        "datos/procesados/tibc_mensual.csv")
readr::write_csv(validacion,  "datos/procesados/validacion_icv.csv")
readr::write_csv(comparacion, "datos/procesados/comparacion_series.csv")

# Figuras
dir.create("figuras", showWarnings = FALSE)
azul <- "#2a78d6"
coma <- function(x) format(x, decimal.mark = ",", trim = TRUE)

etiquetas <- comparacion |>
  mutate(titulo = if_else(serie == "Consumo total", "Consumo total",
                          paste0(serie, " (", coma(round(participacion_saldo_pct, 1)), " % del saldo)")))
series_fig <- series |>
  left_join(select(etiquetas, serie, titulo), by = "serie") |>
  mutate(titulo = factor(titulo, levels = etiquetas$titulo))

g1 <- ggplot(series_fig, aes(fecha, mora_pct)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_line(colour = azul, linewidth = 0.7) +
  facet_wrap(~ titulo, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(coma(x), " %")) +
  labs(title = "Índice de mora de consumo: total y ocho productos",
       subtitle = "Porcentaje del saldo con más de 30 días de atraso. Cada panel tiene su propia escala. Franja gris: alivios (mar-2020 a jun-2021).",
       x = NULL, y = NULL,
       caption = "Fuente: Superintendencia Financiera, Distribución de cartera por producto (datos.gov.co, rvii-eis8). Cálculos propios.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(hjust = 0), plot.title.position = "plot")
ggsave("figuras/fig1_series_mora.png", g1, width = 11, height = 8.2, dpi = 160, bg = "white")
print(g1)

g2 <- comparacion |>
  filter(serie != "Bajo monto") |>
  mutate(serie = reorder(as.character(serie), fuerza_estacional),
         total = serie == "Consumo total") |>
  ggplot(aes(fuerza_estacional, serie, fill = total)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = paste0(coma(round(fuerza_estacional, 2)), "   (diciembre ",
                               gsub(".", ",", sprintf("%+.2f", efecto_diciembre_pp), fixed = TRUE), " pp)")),
            hjust = -0.05, size = 3) +
  scale_fill_manual(values = c(`TRUE` = azul, `FALSE` = "#9cc1ec"), guide = "none") +
  scale_x_continuous(limits = c(0, 1), labels = coma) +
  labs(title = "Fuerza estacional de cada serie, 2015-2019",
       x = "Fuerza estacional (0 = ninguna, 1 = dominante)", y = NULL,
       caption = "Descomposición clásica aditiva. Entre paréntesis: desviación media de diciembre frente a la tendencia, sin 2020-2021.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot")
ggsave("figuras/fig2_estacionalidad.png", g2, width = 7.5, height = 3.9, dpi = 160, bg = "white")
print(g2)

print(comparacion, width = Inf)
