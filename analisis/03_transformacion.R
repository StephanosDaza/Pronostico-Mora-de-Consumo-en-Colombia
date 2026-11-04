library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE)
train <- filter(serie, muestra == "entrenamiento")
pre   <- filter(train, fecha < as.Date("2020-03-01"))   # antes de los alivios

# Lambda de Guerrero
# cerca de 1: la serie se deja igual; cerca de 0: logaritmo
lambdas <- tibble(
  muestra = c("Entrenamiento (2015 a 2023)", "Antes de los alivios (2015 a feb-2020)"),
  lambda  = c(lambda_guerrero(train), lambda_guerrero(pre))
)
print(lambdas)

# Relación entre nivel y variabilidad por año
anual <- media_desviacion_anual(train)
print(anual, n = Inf)

correlacion_con <- function(d, etiqueta) {
  tibble(años = etiqueta,
         correlacion_desviacion_nivel  = cor(d$media, d$desviacion),
         correlacion_desviacion_cambio = cor(d$media, d$desviacion_cambio))
}
correlaciones <- bind_rows(
  correlacion_con(anual, "Todos los años de entrenamiento"),
  correlacion_con(filter(anual, !anio %in% 2020:2021), "Sin 2020 y 2021"),
  correlacion_con(filter(anual, !anio %in% c(2020, 2021, 2023)), "Sin 2020, 2021 y 2023")
)
print(correlaciones)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)

g_media_desv <- ggplot(anual, aes(media, desviacion_cambio)) +
  geom_point(aes(colour = anio %in% 2020:2021), size = 2.5) +
  geom_text(aes(label = anio), vjust = -0.8, size = 3.2) +
  scale_colour_manual(values = c(`FALSE` = "#2a78d6", `TRUE` = "#eb6834"),
                      labels = c("Años sin alivios", "2020 y 2021"), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(format(x, decimal.mark = ","), " %")) +
  scale_y_continuous(labels = function(x) format(x, decimal.mark = ",")) +
  labs(title = "Nivel y variabilidad de la mora por año, entrenamiento",
       subtitle = "Variabilidad medida con la desviación estándar del cambio mensual",
       x = "Media anual del índice de mora", y = "Desviación del cambio mensual (pp)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_media_desv)
ggsave("figuras/modelo/media_desviacion.png", g_media_desv, width = 7.5, height = 5, dpi = 160, bg = "white")

# Serie original Vs. la transformada con el lambda del entrenamiento
lambda_train <- lambdas$lambda[1]
g_comparacion <- train |>
  mutate(`Original (%)` = mora_pct,
         Transformada = fabletools::box_cox(mora_pct, lambda_train)) |>
  tidyr::pivot_longer(c(`Original (%)`, Transformada), names_to = "serie", values_to = "valor") |>
  mutate(serie = if_else(serie == "Transformada",
                         sprintf("Box-Cox con lambda = %s", format(round(lambda_train, 2), decimal.mark = ",")),
                         serie)) |>
  ggplot(aes(fecha, valor)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_line(colour = "#2a78d6", linewidth = 0.8) +
  facet_wrap(~ serie, ncol = 1, scales = "free_y") +
  labs(title = "Serie de entrenamiento original y transformada", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0),
        plot.title.position = "plot")
print(g_comparacion)
ggsave("figuras/modelo/transformacion.png", g_comparacion, width = 9, height = 5.5, dpi = 160, bg = "white")

readr::write_csv(lambdas,       "datos/procesados/transformacion_lambda.csv")
readr::write_csv(anual,         "datos/procesados/transformacion_anual.csv")
readr::write_csv(correlaciones, "datos/procesados/transformacion_correlacion.csv")
