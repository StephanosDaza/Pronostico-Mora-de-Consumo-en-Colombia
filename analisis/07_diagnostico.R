library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")
seleccion <- readr::read_csv("datos/procesados/estimacion_seleccion.csv", show_col_types = FALSE)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
fuera_alivios <- function(fecha) fecha < as.Date("2020-03-01") | fecha > as.Date("2021-06-01")

resultados <- purrr::pmap(seleccion, function(modelo, p, q, P, Q, ...) {
  fit <- estimar_sarima(train$mora_pct, p, q, P, Q)
  res <- residuos_modelo(fit, train$fecha)
  k   <- p + q + P + Q
  list(
    modelo = modelo, codigo = codigo_modelo(p, q, P, Q), residuos = res,
    ljung_box = mutate(ljung_box(res$residuo, k), modelo = modelo, .before = 1),
    acf = mutate(tabla_acf_pacf(res$residuo), modelo = modelo, .before = 1),
    normalidad = bind_rows(
      normalidad(res$residuo, "Todo el entrenamiento"),
      normalidad(res$residuo[fuera_alivios(res$fecha)], "Sin los meses de alivios")
    ) |> mutate(modelo = modelo, .before = 1),
    grandes = mutate(residuos_grandes(res), modelo = modelo, .before = 1),
    varianza = mutate(varianza_residuos(res, train), modelo = modelo, .before = 1),
    sobreajuste = sobreajuste(train$mora_pct, p, q, P, Q)
  )
}) |>
  setNames(seleccion$modelo)

juntar <- function(campo) purrr::map_dfr(resultados, campo)

# Ljung-Box en los rezagos 12, 24 y 36
ljung <- juntar("ljung_box")
print(ljung, n = Inf)

# ACF y PACF de los residuos
acf_res <- juntar("acf")
print(acf_res, n = Inf)
purrr::walk(resultados, function(r) {
  g <- grafico_acf_residuos(r$residuos$residuo, r$modelo)
  print(g)
  ggsave(file.path("figuras/modelo", paste0("acf_residuos_", r$codigo, ".png")),
         g, width = 9, height = 4, dpi = 160, bg = "white")
})

# Normalidad, con todo el entrenamiento y sin los meses de alivios
norm <- juntar("normalidad")
print(norm, width = Inf)

# Meses con los residuos más grandes
grandes <- juntar("grandes")
print(grandes, n = Inf)

# Variabilidad de los residuos frente al nivel de la mora, por año
varianza <- juntar("varianza")
print(varianza, n = Inf)
corr_varianza <- varianza |>
  group_by(modelo) |>
  summarise(correlacion_todos = cor(media_mora, desviacion_residuo),
            correlacion_sin_alivios = cor(media_mora[!anio %in% 2020:2021],
                                          desviacion_residuo[!anio %in% 2020:2021]),
            .groups = "drop")
print(corr_varianza)

# Sobreajuste: agregar un término a cada modelo
sobre <- juntar("sobreajuste")
print(sobre, n = Inf, width = Inf)

# Residuos en el tiempo, estandarizados
residuos <- purrr::map_dfr(resultados, ~ mutate(.x$residuos, modelo = .x$modelo))
g_residuos <- ggplot(residuos, aes(fecha, estandarizado)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "#eb6834") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(colour = "#2a78d6", linewidth = 0.6) +
  geom_point(data = filter(residuos, abs(estandarizado) > 3), colour = "#eb6834", size = 2) +
  facet_wrap(~ modelo, ncol = 1) +
  labs(title = "Residuos estandarizados, entrenamiento",
       subtitle = "Líneas punteadas: ±3 desviaciones. Puntos: meses fuera de ese rango. Franja gris: alivios.",
       x = NULL, y = "Residuo / desviación") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0),
        plot.title.position = "plot")
print(g_residuos)
ggsave("figuras/modelo/residuos.png", g_residuos, width = 9, height = 6, dpi = 160, bg = "white")

# Histograma y gráfico Q-Q de los residuos estandarizados
g_hist <- ggplot(residuos, aes(estandarizado)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "#9cc1ec", colour = "white") +
  stat_function(fun = dnorm, colour = "#eb6834", linewidth = 0.8) +
  facet_wrap(~ modelo, ncol = 2) +
  labs(title = "Distribución de los residuos estandarizados",
       subtitle = "Curva naranja: normal estándar.", x = "Residuo / desviación", y = "Densidad") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0),
        plot.title.position = "plot")
g_qq <- ggplot(residuos, aes(sample = estandarizado)) +
  stat_qq(colour = "#2a78d6", size = 1.3) +
  stat_qq_line(colour = "#eb6834") +
  facet_wrap(~ modelo, ncol = 2) +
  labs(title = "Gráfico Q-Q de los residuos estandarizados",
       x = "Cuantiles de la normal", y = "Cuantiles de los residuos") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0),
        plot.title.position = "plot")
print(g_hist)
print(g_qq)
ggsave("figuras/modelo/residuos_histograma.png", g_hist, width = 9, height = 4, dpi = 160, bg = "white")
ggsave("figuras/modelo/residuos_qq.png", g_qq, width = 9, height = 4, dpi = 160, bg = "white")

# Variabilidad de los residuos frente al nivel
g_var <- ggplot(varianza, aes(media_mora, desviacion_residuo)) +
  geom_point(aes(colour = anio %in% 2020:2021), size = 2.5) +
  geom_text(aes(label = anio), vjust = -0.8, size = 3) +
  scale_colour_manual(values = c(`FALSE` = "#2a78d6", `TRUE` = "#eb6834"),
                      labels = c("Años sin alivios", "2020 y 2021"), name = NULL) +
  facet_wrap(~ modelo, ncol = 2) +
  labs(title = "Variabilidad de los residuos frente al nivel de la mora, por año",
       x = "Media anual del índice de mora (%)", y = "Desviación de los residuos (pp)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(hjust = 0), plot.title.position = "plot")
print(g_var)
ggsave("figuras/modelo/residuos_varianza.png", g_var, width = 9, height = 4.5, dpi = 160, bg = "white")

readr::write_csv(ljung,         "datos/procesados/diagnostico_ljung_box.csv")
readr::write_csv(acf_res,       "datos/procesados/diagnostico_acf.csv")
readr::write_csv(norm,          "datos/procesados/diagnostico_normalidad.csv")
readr::write_csv(grandes,       "datos/procesados/diagnostico_residuos_grandes.csv")
readr::write_csv(varianza,      "datos/procesados/diagnostico_varianza.csv")
readr::write_csv(corr_varianza, "datos/procesados/diagnostico_varianza_correlacion.csv")
readr::write_csv(sobre,         "datos/procesados/diagnostico_sobreajuste.csv")
readr::write_csv(residuos,      "datos/procesados/diagnostico_residuos.csv")
