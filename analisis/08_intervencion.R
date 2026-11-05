# Paso 7 de la solución: atípicos e intervención
# Correr después de 07_diagnostico.R. Solo usa la muestra de entrenamiento.
# El diagnóstico mostró residuos de 5 a 6 desviaciones en 2020 y normalidad solo sin esos meses,
# así que se incorporan variables para los alivios. También mostró que un MA estacional cercano
# a -1 mejora el ajuste, señal de estacionalidad fija, así que se comparan el AR estacional y las
# variables ficticias por mes. Todos los modelos tienen d = 1 y la misma muestra.
#
# Segunda vuelta: en la primera, las rampas fueron la mejor forma en las tres estructuras, pero
# dejaron residuos grandes en nov-2020 y Ljung-Box falló con las ficticias. El rebote no fue
# parejo durante seis meses: se concentró entre agosto y octubre de 2020 (cambios de +1,04, +0,82
# y +1,21 puntos) y casi se detuvo después. Por eso se agrega una rampa de rebote corta, de
# agosto a octubre, como forma adicional.

library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")

intervencion <- variables_intervencion(train$fecha)
ficticias    <- ficticias_mensuales(train$fecha)
en_alivios   <- intervencion$alivio == 1

estructuras <- tribble(
  ~estructura,                          ~p, ~q, ~P, ~Q, ~con_ficticias,
  "SARIMA(1,1,0)(1,0,0)12",              1,  0,  1,  0, FALSE,
  "SARIMA(0,1,3)(2,0,0)12",              0,  3,  2,  0, FALSE,
  "ARIMA(1,1,0) con ficticias por mes",  1,  0,  0,  0, TRUE
)

# 1. Estimación de cada estructura con cada forma de intervención
ajustes <- tidyr::expand_grid(estructuras, forma = names(FORMAS_INTERVENCION)) |>
  mutate(ajuste = purrr::pmap(list(p, q, P, Q, con_ficticias, forma), function(p, q, P, Q, fict, forma) {
    vars <- FORMAS_INTERVENCION[[forma]]
    xreg <- bind_cols(intervencion[, vars, drop = FALSE], if (fict) ficticias else NULL)
    xreg <- if (ncol(xreg) == 0) NULL else as.matrix(xreg)
    tryCatch(estimar_con_regresoras(train$mora_pct, xreg, p, q, P, Q), error = function(e) NULL)
  }))

comparacion <- ajustes |>
  filter(!purrr::map_lgl(ajuste, is.null)) |>
  mutate(resumen = purrr::pmap(list(ajuste, p, q, P, Q),
                               ~ resumen_con_regresoras(..1, ..2, ..3, ..4, ..5, train$fecha, en_alivios))) |>
  tidyr::unnest(resumen) |>
  arrange(aicc) |>
  mutate(delta_aicc = aicc - min(aicc), .after = aicc)
print(select(comparacion, -ajuste), n = Inf, width = Inf)

# 2. Coeficientes de las variables de intervención y de la parte estacional
coeficientes <- comparacion |>
  mutate(coefs = purrr::map(ajuste, function(fit) {
    est <- stats::coef(fit); ee <- sqrt(diag(fit$var.coef))
    tibble(termino = names(est), estimacion = unname(est), error_estandar = unname(ee),
           z = unname(est / ee))
  })) |>
  select(estructura, forma, coefs) |>
  tidyr::unnest(coefs)
print(filter(coeficientes, !grepl("^mes_", termino)), n = Inf, width = Inf)

# 3. Selección: menor AICc entre los modelos estacionarios, invertibles y sin autocorrelación
#    en los residuos (Ljung-Box en el rezago 24 con p > 0,05). También se reporta el menor BIC.
validos <- filter(comparacion, estacionario, invertible, ljung_box_p > 0.05)
seleccion <- bind_rows(
  mutate(slice_min(validos, aicc, n = 1), criterio = "Menor AICc"),
  mutate(slice_min(validos, bic, n = 1),  criterio = "Menor BIC")
) |>
  select(criterio, estructura, forma, p, q, P, Q, con_ficticias, parametros, aicc, bic)
print(seleccion, width = Inf)

# 4. Diagnóstico del modelo elegido por AICc
elegido <- filter(comparacion, estructura == seleccion$estructura[1], forma == seleccion$forma[1])
fit_final <- elegido$ajuste[[1]]
k_final   <- elegido$p + elegido$q + elegido$P + elegido$Q
res_final <- residuos_modelo(fit_final, train$fecha)

final_ljung <- ljung_box(res_final$residuo, k_final)
final_norm  <- bind_rows(
  normalidad(res_final$residuo, "Todo el entrenamiento"),
  normalidad(res_final$residuo[!en_alivios[-1]], "Sin los meses de alivios")
)
atipicos <- filter(res_final, abs(estandarizado) > 3) |> select(fecha, residuo, estandarizado)
print(final_ljung)
print(final_norm, width = Inf)
print(atipicos)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
nombre_final <- paste(elegido$estructura, "con", tolower(elegido$forma))

g_acf <- grafico_acf_residuos(res_final$residuo, nombre_final)
print(g_acf)
ggsave("figuras/modelo/acf_residuos_final.png", g_acf, width = 9, height = 4, dpi = 160, bg = "white")

g_res <- ggplot(res_final, aes(fecha, estandarizado)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "#eb6834") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(colour = "#2a78d6", linewidth = 0.6) +
  geom_point(data = filter(res_final, abs(estandarizado) > 3), colour = "#eb6834", size = 2) +
  labs(title = paste("Residuos estandarizados:", nombre_final),
       subtitle = "Líneas punteadas: ±3 desviaciones. Franja gris: alivios.",
       x = NULL, y = "Residuo / desviación") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_res)
ggsave("figuras/modelo/residuos_final.png", g_res, width = 9, height = 4, dpi = 160, bg = "white")

# 5. Efecto estimado de la intervención sobre la mora
vars_final <- FORMAS_INTERVENCION[[elegido$forma]]
xreg_final <- as.matrix(bind_cols(intervencion, ficticias))
efecto <- efecto_intervencion(fit_final, xreg_final, vars_final, train$fecha)
g_efecto <- efecto |>
  filter(fecha >= as.Date("2019-01-01"), fecha <= as.Date("2022-12-01")) |>
  ggplot(aes(fecha, efecto)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_step(colour = "#2a78d6", linewidth = 0.9) +
  labs(title = paste("Efecto estimado de la intervención:", tolower(elegido$forma)),
       subtitle = "Contribución de las variables de intervención al índice de mora.",
       x = NULL, y = "Puntos porcentuales") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_efecto)
ggsave("figuras/modelo/efecto_intervencion.png", g_efecto, width = 9, height = 4, dpi = 160, bg = "white")

readr::write_csv(select(comparacion, -ajuste), "datos/procesados/intervencion_comparacion.csv")
readr::write_csv(coeficientes,                 "datos/procesados/intervencion_coeficientes.csv")
readr::write_csv(seleccion,                    "datos/procesados/intervencion_seleccion.csv")
readr::write_csv(final_ljung,                  "datos/procesados/intervencion_final_ljung_box.csv")
readr::write_csv(final_norm,                   "datos/procesados/intervencion_final_normalidad.csv")
readr::write_csv(atipicos,                     "datos/procesados/intervencion_final_atipicos.csv")
readr::write_csv(efecto,                       "datos/procesados/intervencion_efecto.csv")
