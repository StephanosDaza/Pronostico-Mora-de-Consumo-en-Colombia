library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_mora.csv", show_col_types = FALSE) |>
  filter(fecha <= FECHA_CORTE)

fin_train <- fin_entrenamiento(FECHA_CORTE, HORIZONTE, N_ORIGENES)
serie_partida <- partir_serie(serie, fin_train)

particion <- resumen_particion(serie_partida, HORIZONTE, N_ORIGENES)
print(tidyr::pivot_longer(mutate(particion, across(everything(), as.character)),
                          everything(), names_to = "concepto", values_to = "valor"), n = Inf)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
g_particion <- ggplot(serie_partida, aes(fecha, mora_pct)) +
  annotate("rect", xmin = particion$evaluacion_desde, xmax = particion$evaluacion_hasta,
           ymin = -Inf, ymax = Inf, fill = "#eb6834", alpha = 0.12) +
  geom_line(aes(colour = muestra), linewidth = 0.8) +
  scale_colour_manual(
    values = c(entrenamiento = "#2a78d6", evaluacion = "#eb6834"),
    labels = c(sprintf("Entrenamiento (%s a %s)", format(particion$entrenamiento_desde, "%m-%Y"),
                       format(particion$entrenamiento_hasta, "%m-%Y")),
               sprintf("Evaluación (%s a %s)", format(particion$evaluacion_desde, "%m-%Y"),
                       format(particion$evaluacion_hasta, "%m-%Y")))) +
  scale_y_continuous(labels = function(x) paste0(format(x, decimal.mark = ","), " %")) +
  labs(title = "Partición de la muestra",
       subtitle = sprintf("Fecha de corte: %s. Franja naranja: evaluación con %d orígenes y horizonte de %d meses.",
                          format(FECHA_CORTE, "%m-%Y"), N_ORIGENES, HORIZONTE),
       x = NULL, y = "Índice de mora", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_particion)
ggsave("figuras/modelo/particion.png", g_particion, width = 9, height = 4.5, dpi = 160, bg = "white")

readr::write_csv(serie_partida, "datos/procesados/serie_partida.csv")
readr::write_csv(particion,     "datos/procesados/particion.csv")
