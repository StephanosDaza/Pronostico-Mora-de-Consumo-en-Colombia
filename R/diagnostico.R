# Diagnóstico de residuos de los SARIMA

# Residuos de un modelo ajustado.
# Recibe: el modelo y las fechas de la muestra con que se estimó.
# Devuelve: fecha, residuo, valor ajustado y residuo estandarizado (residuo / desviación).
# Se omite el primer mes, que solo sirve para arrancar la diferencia regular.
residuos_modelo <- function(fit, fechas) {
  r <- as.numeric(stats::residuals(fit))
  a <- as.numeric(stats::fitted(fit))
  tibble::tibble(fecha = fechas, residuo = r, ajustado = a)[-1, ] |>
    dplyr::mutate(estandarizado = residuo / stats::sd(residuo))
}

# Prueba de Ljung-Box.
# Recibe: los residuos, el número de parámetros ARMA estimados y los rezagos a probar.
# Devuelve: una fila por rezago con el estadístico Q, los grados de libertad (rezago menos
# parámetros) y el valor p. H0: los residuos no tienen autocorrelación hasta ese rezago.
ljung_box <- function(residuo, parametros, rezagos = c(12, 24, 36)) {
  purrr::map_dfr(rezagos, function(m) {
    lb <- stats::Box.test(residuo, lag = m, type = "Ljung-Box", fitdf = parametros)
    tibble::tibble(rezago = m, q = unname(lb$statistic), gl = m - parametros, valor_p = lb$p.value)
  })
}

# Normalidad de los residuos.
# Recibe: los residuos y una etiqueta.
# Devuelve: n, asimetría, curtosis, Jarque-Bera y Shapiro-Wilk con sus valores p.
# Jarque-Bera: JB = n/6 (S^2 + (K - 3)^2 / 4), con H0 de normalidad y distribución chi cuadrado
# con 2 grados de libertad.
normalidad <- function(residuo, etiqueta) {
  n <- length(residuo)
  z <- residuo - mean(residuo)
  s <- mean(z^3) / mean(z^2)^1.5
  k <- mean(z^4) / mean(z^2)^2
  jb <- n / 6 * (s^2 + (k - 3)^2 / 4)
  sw <- stats::shapiro.test(residuo)
  tibble::tibble(muestra = etiqueta, n = n, asimetria = s, curtosis = k,
                 jarque_bera = jb, jb_valor_p = stats::pchisq(jb, 2, lower.tail = FALSE),
                 shapiro_w = unname(sw$statistic), shapiro_valor_p = sw$p.value)
}

# Meses con los residuos más grandes.
# Recibe: la tabla de residuos y cuántos meses mostrar.
# Devuelve: los meses ordenados de mayor a menor residuo en valor absoluto.
residuos_grandes <- function(res, n = 8) {
  res |>
    dplyr::arrange(dplyr::desc(abs(estandarizado))) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(fecha, residuo, estandarizado)
}

# Variabilidad de los residuos frente al nivel de la serie, por año.
# Recibe: la tabla de residuos y la serie (fecha, mora_pct).
# Devuelve: año, media de la mora y desviación estándar de los residuos de ese año.
varianza_residuos <- function(res, serie) {
  res |>
    dplyr::left_join(dplyr::select(serie, fecha, mora_pct), by = "fecha") |>
    dplyr::mutate(anio = lubridate::year(fecha)) |>
    dplyr::group_by(anio) |>
    dplyr::summarise(meses = dplyr::n(), media_mora = mean(mora_pct),
                     desviacion_residuo = stats::sd(residuo), .groups = "drop") |>
    dplyr::filter(meses >= 11)
}

# Prueba de sobreajuste.
# Recibe: la serie y los órdenes del modelo base.
# Devuelve: una fila por cada modelo ampliado con un término más (AR, MA, AR estacional o MA
# estacional), con la estimación y el z del término agregado y el cambio en AICc frente al base.
# Si ningún término agregado es significativo y el AICc no mejora, el modelo base es suficiente.
sobreajuste <- function(x, p, q, P, Q) {
  base <- estimar_sarima(x, p, q, P, Q)
  ampliaciones <- list(
    list(p = p + 1, q = q, P = P, Q = Q, termino = paste0("ar", p + 1)),
    list(p = p, q = q + 1, P = P, Q = Q, termino = paste0("ma", q + 1)),
    list(p = p, q = q, P = P + 1, Q = Q, termino = paste0("sar", P + 1)),
    list(p = p, q = q, P = P, Q = Q + 1, termino = paste0("sma", Q + 1))
  )
  purrr::map_dfr(ampliaciones, function(a) {
    fit <- tryCatch(estimar_sarima(x, a$p, a$q, a$P, a$Q), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    co <- stats::coef(fit)
    ee <- sqrt(diag(fit$var.coef))
    tibble::tibble(
      modelo_base   = etiqueta_sarima(p, 1, q, P, 0, Q),
      modelo        = etiqueta_sarima(a$p, 1, a$q, a$P, 0, a$Q),
      termino       = a$termino,
      estimacion    = unname(co[a$termino]),
      z             = unname(co[a$termino] / ee[a$termino]),
      aicc          = fit$aicc,
      delta_aicc    = fit$aicc - base$aicc
    )
  })
}

# Gráfica de la ACF y la PACF de los residuos.
# Recibe: los residuos, el nombre del modelo y el número de rezagos.
# Devuelve: la gráfica con la banda de ±1,96/raíz(n).
grafico_acf_residuos <- function(residuo, modelo, rezagos = 36) {
  datos <- calcular_acf_pacf(residuo, rezagos)
  ggplot2::ggplot(datos, ggplot2::aes(rezago, valor)) +
    ggplot2::geom_vline(xintercept = c(12, 24, 36), colour = "grey88") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = banda), linetype = "dashed", colour = "#eb6834") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = -banda), linetype = "dashed", colour = "#eb6834") +
    ggplot2::geom_segment(ggplot2::aes(xend = rezago, yend = 0), colour = "#2a78d6", linewidth = 0.7) +
    ggplot2::facet_wrap(~ funcion, ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, rezagos, 6)) +
    ggplot2::labs(title = paste("ACF y PACF de los residuos:", modelo),
                  subtitle = "Líneas punteadas: banda aproximada al 95 %. Líneas verticales: rezagos 12, 24 y 36.",
                  x = "Rezago (meses)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title.position = "plot")
}

# Código corto de un modelo para nombrar archivos, por ejemplo "110_100".
codigo_modelo <- function(p, q, P, Q, d = 1, D = 0) paste0(p, d, q, "_", P, D, Q)
