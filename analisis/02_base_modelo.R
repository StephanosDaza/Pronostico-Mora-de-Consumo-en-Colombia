# Base para el modelo: partición de la muestra, intervención y tasa rezagada
# Correr después de 01_construir_serie.R

library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

# Parámetros
fin_entrenamiento <- as.Date("2023-12-01")   # último mes de entrenamiento
horizonte         <- 6                       # meses que se pronostican desde cada origen
n_origenes        <- 25                      # mínimo que pide el enunciado
rezagos_tasa      <- 6:12                    # rezago mínimo igual al horizonte

serie <- readr::read_csv("datos/procesados/serie_mora.csv", show_col_types = FALSE)
tasa  <- readr::read_csv("datos/procesados/tibc_mensual.csv", show_col_types = FALSE)

base <- construir_base_modelo(serie, tasa, fin_entrenamiento, rezagos_tasa)

# Partición
particion <- resumen_particion(base, horizonte, n_origenes)
print(particion, width = Inf)

# Revisión de las variables de intervención
print(filter(base, fecha >= as.Date("2020-01-01"), fecha <= as.Date("2021-08-01")) |>
        select(fecha, mora_pct, alivio, gracia, p_mar20, p_oct20, caida, rebote), n = Inf)

# Ninguna columna de tasa debe tener faltantes
faltantes <- colSums(is.na(select(base, starts_with("tasa"))))
print(faltantes)
stopifnot(all(faltantes == 0))

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
g_particion <- ggplot(base, aes(fecha, mora_pct)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("rect", xmin = min(base$fecha[base$muestra == "evaluacion"]), xmax = max(base$fecha),
           ymin = -Inf, ymax = Inf, fill = "#eb6834", alpha = 0.12) +
  geom_line(aes(colour = muestra), linewidth = 0.8) +
  scale_colour_manual(values = c(entrenamiento = "#2a78d6", evaluacion = "#eb6834"),
                      labels = c("Entrenamiento (ene-2015 a dic-2023)", "Evaluación (desde ene-2024)")) +
  scale_y_continuous(labels = function(x) paste0(format(x, decimal.mark = ","), " %")) +
  labs(title = "Partición de la muestra",
       subtitle = "Franja gris: alivios (mar-2020 a jun-2021). Franja naranja: evaluación con origen móvil.",
       x = NULL, y = "Índice de mora", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_particion)
ggsave("figuras/modelo/particion.png", g_particion, width = 9, height = 4.5, dpi = 160, bg = "white")

readr::write_csv(base,      "datos/procesados/base_modelo.csv")
readr::write_csv(particion, "datos/procesados/particion.csv")
