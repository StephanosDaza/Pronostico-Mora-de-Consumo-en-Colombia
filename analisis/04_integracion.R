# Paso 3 de la solución: orden de integración
# Correr después de 03_transformacion.R. Solo usa la muestra de entrenamiento, en escala original.

library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")
pre   <- filter(train, fecha < as.Date("2020-03-01"))   # antes de los alivios

muestras <- list("Entrenamiento (2015 a 2023)" = train$mora_pct,
                 "Antes de los alivios (2015 a feb-2020)" = pre$mora_pct)

# 1. Varianza y autocorrelación de cada diferenciación
resumen <- purrr::imap_dfr(muestras, ~ mutate(resumen_diferencias(.x), muestra = .y, .before = 1))
print(resumen, n = Inf, width = Inf)

# 2. KPSS y ADF sobre cada diferenciación
pruebas <- purrr::imap_dfr(muestras, ~ mutate(pruebas_integracion(.x), muestra = .y, .before = 1))
print(pruebas, n = Inf, width = Inf)

# 3. Diferencias sugeridas por las pruebas automáticas
sugeridas <- purrr::imap_dfr(muestras, ~ mutate(diferencias_sugeridas(.x), muestra = .y, .before = 1))
print(sugeridas, n = Inf, width = Inf)

# Las cuatro diferenciaciones del entrenamiento
dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
g_diferencias <- purrr::map_dfr(names(ETIQUETAS_DIFERENCIA), function(dif) {
  z <- aplicar_diferencia(train$mora_pct, dif)
  tibble(fecha = tail(train$fecha, length(z)), valor = z,
         diferencia = factor(ETIQUETAS_DIFERENCIA[[dif]], levels = ETIQUETAS_DIFERENCIA))
}) |>
  ggplot(aes(fecha, valor)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "#2a78d6", linewidth = 0.7) +
  facet_wrap(~ diferencia, ncol = 2, scales = "free_y") +
  labs(title = "Diferenciaciones candidatas, entrenamiento",
       subtitle = "Puntos porcentuales. Franja gris: alivios (mar-2020 a jun-2021).",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0),
        plot.title.position = "plot")
print(g_diferencias)
ggsave("figuras/modelo/diferencias.png", g_diferencias, width = 10, height = 6, dpi = 160, bg = "white")

readr::write_csv(resumen,   "datos/procesados/integracion_resumen.csv")
readr::write_csv(pruebas,   "datos/procesados/integracion_pruebas.csv")
readr::write_csv(sugeridas, "datos/procesados/integracion_sugeridas.csv")
