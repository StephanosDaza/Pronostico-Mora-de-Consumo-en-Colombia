# Identificación del SARIMA: ACF, PACF y búsqueda automática

# Nombre de un modelo SARIMA mensual.
# Recibe: los órdenes p, d, q, P, D, Q.
# Devuelve: texto como "SARIMA(1,1,0)(1,0,0)12".
etiqueta_sarima <- function(p, d, q, P, D, Q) {
  sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)12", p, d, q, P, D, Q)
}

# Valores de la ACF y la PACF en rezagos seleccionados.
# Recibe: vector numérico ya diferenciado y los rezagos a mostrar.
# Devuelve: una fila por rezago con la ACF, la PACF, la banda de ±1,96/raíz(n) y si cada una
# se sale de la banda.
tabla_acf_pacf <- function(x, rezagos = c(1:6, 12, 13, 24, 36)) {
  calcular_acf_pacf(x, max(rezagos)) |>
    dplyr::filter(rezago %in% rezagos) |>
    tidyr::pivot_wider(names_from = funcion, values_from = valor) |>
    dplyr::mutate(acf_fuera = abs(ACF) > banda, pacf_fuera = abs(PACF) > banda) |>
    dplyr::select(rezago, ACF, PACF, banda, acf_fuera, pacf_fuera)
}

# Búsqueda automática del SARIMA con las diferencias fijas.
# Recibe: vector numérico mensual en niveles, d y D.
# Devuelve: los órdenes del modelo con menor AICc entre los que prueba auto.arima
# (búsqueda completa, sin atajos), con su AICc.
busqueda_automatica <- function(x, d = 1, D = 0) {
  y <- stats::ts(x, frequency = 12)
  fit <- forecast::auto.arima(y, d = d, D = D, seasonal = TRUE,
                              stepwise = FALSE, approximation = FALSE)
  o <- forecast::arimaorder(fit)
  P <- if (length(o) > 3) o[["P"]] else 0L
  Q <- if (length(o) > 3) o[["Q"]] else 0L
  tibble::tibble(p = o[["p"]], q = o[["q"]], P = P, Q = Q,
                 modelo = etiqueta_sarima(o[["p"]], d, o[["q"]], P, D, Q),
                 aicc = fit$aicc)
}
