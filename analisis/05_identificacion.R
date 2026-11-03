# Paso 4 de la solución: identificación
# Correr después de 04_integracion.R. Solo usa la muestra de entrenamiento.
# Diferenciación definida en el paso 3: d = 1, D = 0.

library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")
pre   <- filter(train, fecha < as.Date("2020-03-01"))   # antes de los alivios

# 1. ACF y PACF de la serie con diferencia regular
acf_train <- tabla_acf_pacf(diferenciar(train$mora_pct, "regular"))
acf_pre   <- tabla_acf_pacf(diferenciar(pre$mora_pct, "regular"))
print(acf_train, n = Inf)
print(acf_pre, n = Inf)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
g_train <- grafico_acf_pacf(train, diferencia = "regular") +
  labs(subtitle = "Entrenamiento, 2015 a 2023. Líneas punteadas: banda aproximada al 95 %.")
g_pre <- grafico_acf_pacf(pre, diferencia = "regular") +
  labs(subtitle = "Antes de los alivios, 2015 a feb-2020. Líneas punteadas: banda aproximada al 95 %.")
print(g_train)
print(g_pre)
ggsave("figuras/modelo/acf_regular_entrenamiento.png", g_train, width = 9, height = 4, dpi = 160, bg = "white")
ggsave("figuras/modelo/acf_regular_pre_alivios.png", g_pre, width = 9, height = 4, dpi = 160, bg = "white")

# 2. Búsqueda automática como referencia (d = 1 y D = 0 fijos, sin regresoras)
automatica <- bind_rows(
  mutate(busqueda_automatica(train$mora_pct), muestra = "Entrenamiento (2015 a 2023)", .before = 1),
  mutate(busqueda_automatica(pre$mora_pct),   muestra = "Antes de los alivios (2015 a feb-2020)", .before = 1)
)
print(automatica, width = Inf)

# 3. Candidatos para el paso 5
# Antes de los alivios: sin estructura regular (rezagos 1 a 6 dentro de la banda) y AR estacional
# (ACF 0,52, 0,34 y 0,19 en 12, 24 y 36; PACF con un solo pico en 12).
# Entrenamiento completo: AR(1) regular (ACF que decae desde 0,48 y PACF que se corta en 1) y
# estructura estacional tapada por la pandemia (PACF de -0,32 en el rezago 13).
candidatos <- tribble(
  ~p, ~q, ~P, ~Q, ~razon,
  1, 0, 1, 0, "AR(1) por la ACF y la PACF del entrenamiento; AR estacional por el patrón de antes de los alivios",
  1, 0, 0, 1, "MA estacional como alternativa al AR estacional (la ACF de antes de los alivios favorece el AR)"
) |>
  bind_rows(transmute(automatica, p, q, P, Q, razon = paste("Búsqueda automática,", tolower(muestra)))) |>
  distinct(p, q, P, Q, .keep_all = TRUE) |>
  mutate(modelo = etiqueta_sarima(p, 1, q, P, 0, Q), .before = 1)
print(candidatos, width = Inf)

descartados <- tribble(
  ~p, ~q, ~P, ~Q, ~razon,
  2, 0, 1, 0, "La PACF en el rezago 2 es 0,08: no hay evidencia de un segundo término AR",
  1, 1, 1, 0, "La PACF se corta en el rezago 1, así que el MA regular sobra; se usa en el diagnóstico como prueba de sobreajuste"
) |>
  mutate(modelo = etiqueta_sarima(p, 1, q, P, 0, Q), .before = 1)
print(descartados, width = Inf)

readr::write_csv(acf_train,  "datos/procesados/identificacion_acf_entrenamiento.csv")
readr::write_csv(acf_pre,    "datos/procesados/identificacion_acf_pre_alivios.csv")
readr::write_csv(automatica, "datos/procesados/identificacion_automatica.csv")
readr::write_csv(candidatos,  "datos/procesados/identificacion_candidatos.csv")
readr::write_csv(descartados, "datos/procesados/identificacion_descartados.csv")
