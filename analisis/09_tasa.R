library(dplyr)
library(ggplot2)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

serie <- readr::read_csv("datos/procesados/serie_partida.csv", show_col_types = FALSE) |> arrange(fecha)
tasa  <- readr::read_csv("datos/procesados/tibc_mensual.csv", show_col_types = FALSE) |> arrange(fecha)
train <- filter(serie, muestra == "entrenamiento")
rezagos <- 6:12

intervencion <- variables_intervencion(train$fecha)[, c("caida", "rebote_corto")]
ficticias    <- ficticias_mensuales(train$fecha)
tasas        <- tasa_rezagada(train$fecha, tasa, rezagos)
en_alivios   <- variables_intervencion(train$fecha)$alivio == 1
stopifnot(!anyNA(tasas))

# Correlación cruzada preblanqueada, solo con datos hasta el fin del entrenamiento
cc <- correlacion_preblanqueada(train, filter(tasa, fecha <= max(train$fecha)), 0:18,
                                excluir = en_alivios)
cat("Filtro de preblanqueo (ARIMA de la tasa):", attr(cc, "filtro"), "\n")
print(cc, n = Inf)

dir.create("figuras/modelo", showWarnings = FALSE, recursive = TRUE)
g_cc <- ggplot(cc, aes(rezago, correlacion)) +
  annotate("rect", xmin = 5.5, xmax = 12.5, ymin = -Inf, ymax = Inf, fill = "#9cc1ec", alpha = 0.25) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_hline(aes(yintercept = banda), linetype = "dashed", colour = "#eb6834") +
  geom_hline(aes(yintercept = -banda), linetype = "dashed", colour = "#eb6834") +
  geom_segment(aes(xend = rezago, yend = 0), colour = "#2a78d6", linewidth = 0.9) +
  scale_x_continuous(breaks = 0:18) +
  labs(title = "Correlación cruzada preblanqueada entre la tasa y la mora",
       subtitle = paste0("Tasa de hace L meses frente a la mora, filtradas con ", attr(cc, "filtro"),
                         ". Franja azul: rezagos candidatos (6 a 12). Sin los meses de alivios."),
       x = "Rezago de la tasa (meses)", y = "Correlación") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_cc)
ggsave("figuras/modelo/correlacion_preblanqueada.png", g_cc, width = 9, height = 4, dpi = 160, bg = "white")

# Cada estructura con cada versión de la tasa y todas con la misma intervención
estructuras <- tribble(
  ~estructura,                          ~p, ~q, ~P, ~Q, ~con_ficticias,
  "SARIMA(1,1,0)(1,0,0)12",              1,  0,  1,  0, FALSE,
  "SARIMA(0,1,3)(2,0,0)12",              0,  3,  2,  0, FALSE,
  "ARIMA(1,1,0) con ficticias por mes",  1,  0,  0,  0, TRUE
)
versiones <- versiones_tasa(rezagos)

ajustes <- tidyr::expand_grid(estructuras, version = names(versiones)) |>
  mutate(ajuste = purrr::pmap(list(p, q, P, Q, con_ficticias, version), function(p, q, P, Q, fict, version) {
    xreg <- bind_cols(intervencion, tasas[, versiones[[version]], drop = FALSE],
                      if (fict) ficticias else NULL)
    tryCatch(estimar_con_regresoras(train$mora_pct, as.matrix(xreg), p, q, P, Q),
             error = function(e) NULL)
  })) |>
  filter(!purrr::map_lgl(ajuste, is.null))

comparacion <- ajustes |>
  mutate(
    resumen = purrr::pmap(list(ajuste, p, q, P, Q),
                          ~ resumen_con_regresoras(..1, ..2, ..3, ..4, ..5, train$fecha, en_alivios)),
    ljung_box_p12 = purrr::pmap_dbl(list(ajuste, p, q, P, Q), function(fit, p, q, P, Q) {
      stats::Box.test(as.numeric(stats::residuals(fit))[-1], lag = 12, type = "Ljung-Box",
                      fitdf = p + q + P + Q)$p.value
    }),
    coef_tasa = purrr::map2_dbl(ajuste, version, function(fit, v) {
      if (length(versiones[[v]]) == 0) NA_real_ else unname(stats::coef(fit)[versiones[[v]]])
    }),
    z_tasa = purrr::map2_dbl(ajuste, version, function(fit, v) {
      if (length(versiones[[v]]) == 0) return(NA_real_)
      unname(stats::coef(fit)[versiones[[v]]] / sqrt(diag(fit$var.coef))[versiones[[v]]])
    })
  ) |>
  tidyr::unnest(resumen) |>
  arrange(aicc) |>
  mutate(delta_aicc = aicc - min(aicc), .after = aicc)
print(select(comparacion, estructura, version, parametros, aicc, delta_aicc, bic, coef_tasa, z_tasa,
             ljung_box_p12, ljung_box_p, max_residuo, mes_max_residuo), n = Inf, width = Inf)

# Mejor versión de cada estructura frente a la misma estructura sin tasa
por_estructura <- comparacion |>
  group_by(estructura) |>
  summarise(aicc_sin_tasa = aicc[version == "Sin tasa"],
            mejor_version = version[which.min(aicc)], aicc_mejor = min(aicc),
            mejora_aicc = aicc_sin_tasa - aicc_mejor, .groups = "drop")
print(por_estructura, width = Inf)

# Selección: menor AICc y menor BIC entre los modelos estacionarios, invertibles y con
#    Ljung-Box p > 0,05 en los rezagos 12 y 24
validos <- filter(comparacion, estacionario, invertible, ljung_box_p > 0.05, ljung_box_p12 > 0.05)
if (nrow(validos) == 0) {
  message("Ningún modelo pasa Ljung-Box en los rezagos 12 y 24; se usa solo el rezago 24.")
  validos <- filter(comparacion, estacionario, invertible, ljung_box_p > 0.05)
}
seleccion <- bind_rows(
  mutate(slice_min(validos, aicc, n = 1), criterio = "Menor AICc"),
  mutate(slice_min(validos, bic, n = 1),  criterio = "Menor BIC")
) |>
  select(criterio, estructura, version, p, q, P, Q, con_ficticias, parametros, aicc, bic)
print(seleccion, width = Inf)

# Diagnóstico del modelo elegido por AICc
elegido <- filter(comparacion, estructura == seleccion$estructura[1], version == seleccion$version[1])
fit_final <- elegido$ajuste[[1]]
k_final   <- elegido$p + elegido$q + elegido$P + elegido$Q
res_final <- residuos_modelo(fit_final, train$fecha)

coef_final <- tibble(termino = names(coef(fit_final)), estimacion = unname(coef(fit_final)),
                     error_estandar = unname(sqrt(diag(fit_final$var.coef)))) |>
  mutate(z = estimacion / error_estandar)
final_ljung <- ljung_box(res_final$residuo, k_final)
final_norm  <- bind_rows(
  normalidad(res_final$residuo, "Todo el entrenamiento"),
  normalidad(res_final$residuo[!en_alivios[-1]], "Sin los meses de alivios")
)
atipicos <- filter(res_final, abs(estandarizado) > 3) |> select(fecha, residuo, estandarizado)
print(coef_final, n = Inf)
print(final_ljung)
print(final_norm, width = Inf)
print(atipicos)

nombre_final <- paste(elegido$estructura, "con intervención y", tolower(elegido$version))
g_acf <- grafico_acf_residuos(res_final$residuo, nombre_final)
print(g_acf)
ggsave("figuras/modelo/acf_residuos_tasa.png", g_acf, width = 9, height = 4, dpi = 160, bg = "white")

g_res <- ggplot(res_final, aes(fecha, estandarizado)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "#eb6834") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(colour = "#2a78d6", linewidth = 0.6) +
  labs(title = paste("Residuos estandarizados:", nombre_final),
       subtitle = "Líneas punteadas: ±3 desviaciones. Franja gris: alivios.",
       x = NULL, y = "Residuo / desviación") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
print(g_res)
ggsave("figuras/modelo/residuos_tasa.png", g_res, width = 9, height = 4, dpi = 160, bg = "white")

readr::write_csv(cc, "datos/procesados/tasa_correlacion_preblanqueada.csv")
readr::write_csv(select(comparacion, -ajuste), "datos/procesados/tasa_comparacion.csv")
readr::write_csv(por_estructura, "datos/procesados/tasa_por_estructura.csv")
readr::write_csv(seleccion,      "datos/procesados/tasa_seleccion.csv")
readr::write_csv(coef_final,     "datos/procesados/tasa_final_coeficientes.csv")
readr::write_csv(final_ljung,    "datos/procesados/tasa_final_ljung_box.csv")
readr::write_csv(final_norm,     "datos/procesados/tasa_final_normalidad.csv")
readr::write_csv(atipicos,       "datos/procesados/tasa_final_atipicos.csv")
