# Paso 5 de la solución: estimación y selección
# Correr después de 05_identificacion.R. Solo usa la muestra de entrenamiento.
# Los candidatos se estiman sin intervención; el diagnóstico del paso 6 dice si hace falta.
# Todos tienen d = 1, D = 0 y la misma muestra, así que sus AICc son comparables.

library(dplyr)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")
candidatos <- readr::read_csv("datos/procesados/identificacion_candidatos.csv", show_col_types = FALSE)

# Estimación por máxima verosimilitud
ajustes <- purrr::pmap(candidatos, function(modelo, p, q, P, Q, ...) {
  estimar_sarima(train$mora_pct, p, q, P, Q)
}) |>
  setNames(candidatos$modelo)

# Coeficientes con error estándar, z y valor p
coeficientes <- purrr::imap_dfr(ajustes, coeficientes_modelo)
print(coeficientes, n = Inf, width = Inf)

# Comparación de los candidatos, de menor a mayor AICc
comparacion <- purrr::imap_dfr(ajustes, resumen_modelo) |>
  arrange(aicc) |>
  mutate(delta_aicc = aicc - min(aicc), .after = aicc)
print(comparacion, width = Inf)

# Selección provisional entre los modelos estacionarios e invertibles: el de menor AICc y el de
# menor BIC. Si no coinciden, los dos pasan al diagnóstico y la selección se repite con la
# intervención.
validos <- filter(comparacion, estacionario, invertible)
seleccion <- bind_rows(
  mutate(slice_min(validos, aicc, n = 1), criterio = "Menor AICc"),
  mutate(slice_min(validos, bic, n = 1),  criterio = "Menor BIC")
) |>
  group_by(modelo) |>
  summarise(criterio = paste(criterio, collapse = " y "),
            parametros = first(parametros), aicc = first(aicc), bic = first(bic), .groups = "drop") |>
  left_join(candidatos, by = "modelo")
print(seleccion, width = Inf)

readr::write_csv(coeficientes, "datos/procesados/estimacion_coeficientes.csv")
readr::write_csv(comparacion,  "datos/procesados/estimacion_comparacion.csv")
readr::write_csv(seleccion,    "datos/procesados/estimacion_seleccion.csv")
