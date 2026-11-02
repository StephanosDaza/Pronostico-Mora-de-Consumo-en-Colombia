# Estimación de los SARIMA candidatos

# Estima un SARIMA(p,1,q)(P,0,Q)12 por máxima verosimilitud.
# Recibe: vector numérico mensual en niveles y los órdenes p, q, P, Q.
# Devuelve: el modelo ajustado (objeto de forecast::Arima), sin deriva.
estimar_sarima <- function(x, p, q, P, Q, d = 1, D = 0) {
  y <- stats::ts(x, frequency = 12)
  forecast::Arima(y, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = 12),
                  method = "ML")
}

# Coeficientes con su incertidumbre.
# Recibe: un modelo ajustado y su nombre.
# Devuelve: una fila por coeficiente con la estimación, el error estándar, el estadístico z
# (estimación / error estándar) y el valor p bilateral con la normal.
coeficientes_modelo <- function(fit, modelo) {
  est <- stats::coef(fit)
  ee  <- sqrt(diag(fit$var.coef))
  tibble::tibble(modelo = modelo, termino = names(est), estimacion = unname(est),
                 error_estandar = unname(ee), z = unname(est / ee),
                 valor_p = 2 * stats::pnorm(-abs(unname(est / ee))))
}

# Módulo de la raíz más pequeña de un polinomio de rezago.
# Recibe: los coeficientes del modelo, el prefijo ("ar", "ma", "sar" o "sma") y el signo con que
# entran al polinomio (-1 para la parte AR, +1 para la MA).
# Devuelve: el menor módulo de las raíces, o NA si el modelo no tiene esa parte.
# Para la parte estacional se calcula en B^12, y la condición de estar fuera del círculo
# unitario es la misma.
raiz_minima <- function(coefs, prefijo, signo) {
  phi <- coefs[grepl(paste0("^", prefijo, "[0-9]+$"), names(coefs))]
  if (length(phi) == 0) return(NA_real_)
  min(Mod(polyroot(c(1, signo * phi))))
}

# Resumen de un modelo ajustado.
# Recibe: un modelo ajustado y su nombre.
# Devuelve: número de parámetros, observaciones efectivas, log-verosimilitud, AIC, AICc, BIC,
# varianza del error, si todos los coeficientes son significativos al 5 % y las raíces mínimas.
# Estacionario e invertible exigen que todas las raíces estén fuera del círculo unitario.
resumen_modelo <- function(fit, modelo) {
  co <- stats::coef(fit)
  z  <- co / sqrt(diag(fit$var.coef))
  raices <- c(ar = raiz_minima(co, "ar", -1), sar = raiz_minima(co, "sar", -1),
              ma = raiz_minima(co, "ma", 1),  sma = raiz_minima(co, "sma", 1))
  tibble::tibble(
    modelo = modelo, parametros = length(co), n = fit$nobs,
    log_verosimilitud = fit$loglik, aic = fit$aic, aicc = fit$aicc, bic = fit$bic,
    sigma2 = fit$sigma2,
    todos_significativos = all(abs(z) > 1.96),
    raiz_min_ar = raices[["ar"]], raiz_min_sar = raices[["sar"]],
    raiz_min_ma = raices[["ma"]], raiz_min_sma = raices[["sma"]],
    estacionario = all(c(raices[["ar"]], raices[["sar"]]) > 1, na.rm = TRUE),
    invertible   = all(c(raices[["ma"]], raices[["sma"]]) > 1, na.rm = TRUE)
  )
}
