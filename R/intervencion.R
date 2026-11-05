# Análisis de intervención y estacionalidad con variables ficticias

# Meses transcurridos entre dos fechas mensuales (b - a)
meses_entre <- function(a, b) {
  (lubridate::year(b) - lubridate::year(a)) * 12 + lubridate::month(b) - lubridate::month(a)
}

# Variables candidatas para los alivios de 2020-2021.
# Recibe: vector de fechas mensuales.
# Devuelve: tabla con
#   alivio   1 entre mar-2020 y jun-2021, todo el periodo de alivios
#   gracia   1 entre mar-2020 y ago-2020, mientras corrían los periodos de gracia
#   p_mar20  pulso en mar-2020, inicio de los alivios
#   p_ago20  pulso en ago-2020, cuando empiezan a vencerse las gracias
#   p_oct20  pulso en oct-2020, el salto más grande del rebote
#   caida    rampa de 1 a 5 entre mar-2020 y jul-2020; después vale 5
#   rebote   rampa de 1 a 6 entre ago-2020 y ene-2021; después vale 6
#   rebote_corto  rampa de 1 a 3 entre ago-2020 y oct-2020; después vale 3
variables_intervencion <- function(fecha) {
  tibble::tibble(
    alivio  = as.integer(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2021-06-01")),
    gracia  = as.integer(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-08-01")),
    p_mar20 = as.integer(fecha == as.Date("2020-03-01")),
    p_ago20 = as.integer(fecha == as.Date("2020-08-01")),
    p_oct20 = as.integer(fecha == as.Date("2020-10-01")),
    caida   = pmin(5, pmax(0, meses_entre(as.Date("2020-02-01"), fecha))),
    rebote  = pmin(6, pmax(0, meses_entre(as.Date("2020-07-01"), fecha))),
    rebote_corto = pmin(3, pmax(0, meses_entre(as.Date("2020-07-01"), fecha)))
  )
}

# Formas de intervención que se comparan, con las variables que usa cada una.
FORMAS_INTERVENCION <- list(
  "Sin intervención"              = character(0),
  "Indicadora de alivios"         = "alivio",
  "Pulsos de inicio y rebote"     = c("p_mar20", "p_oct20"),
  "Pulsos de marzo, agosto y octubre" = c("p_mar20", "p_ago20", "p_oct20"),
  "Rampas de caída y rebote"      = c("caida", "rebote"),
  "Rampas de caída y rebote corto" = c("caida", "rebote_corto")
)

# Variables ficticias por mes, con enero como referencia.
# Recibe: vector de fechas mensuales.
# Devuelve: tabla con 11 columnas (mes_02 a mes_12) que valen 1 en su mes.
ficticias_mensuales <- function(fecha) {
  m <- lubridate::month(fecha)
  salida <- sapply(2:12, function(k) as.integer(m == k))
  colnames(salida) <- sprintf("mes_%02d", 2:12)
  tibble::as_tibble(salida)
}

# Estima una regresión con errores SARIMA(p,1,q)(P,0,Q)12 por máxima verosimilitud.
# Recibe: la serie, la matriz de regresoras (o NULL) y los órdenes.
# Devuelve: el modelo ajustado. Con d = 1, R diferencia también las regresoras.
estimar_con_regresoras <- function(x, xreg, p, q, P, Q) {
  y <- stats::ts(x, frequency = 12)
  if (!is.null(xreg) && ncol(xreg) == 0) xreg <- NULL
  forecast::Arima(y, order = c(p, 1, q), seasonal = list(order = c(P, 0, Q), period = 12),
                  xreg = xreg, method = "ML")
}

# Resumen de un modelo con regresoras.
# Recibe: el modelo, los órdenes, las fechas y un vector lógico que marca los meses de alivios.
# Devuelve: parámetros, AICc, BIC, Ljung-Box en el rezago 24 (con gl descontando los términos
# ARMA), Jarque-Bera con y sin los meses de alivios, el mayor residuo estandarizado y si el
# modelo es estacionario e invertible.
resumen_con_regresoras <- function(fit, p, q, P, Q, fechas, en_alivios) {
  co  <- stats::coef(fit)
  res <- as.numeric(stats::residuals(fit))[-1]
  est <- res / stats::sd(res)
  alv <- en_alivios[-1]
  raices <- c(raiz_minima(co, "ar", -1), raiz_minima(co, "sar", -1),
              raiz_minima(co, "ma", 1), raiz_minima(co, "sma", 1))
  tibble::tibble(
    parametros   = length(co),
    aicc         = fit$aicc,
    bic          = fit$bic,
    ljung_box_p  = stats::Box.test(res, lag = 24, type = "Ljung-Box", fitdf = p + q + P + Q)$p.value,
    jb_p         = normalidad(res, "")$jb_valor_p,
    jb_p_sin_alivios = normalidad(res[!alv], "")$jb_valor_p,
    max_residuo  = max(abs(est)),
    mes_max_residuo = fechas[-1][which.max(abs(est))],
    estacionario = all(raices[1:2] > 1, na.rm = TRUE),
    invertible   = all(raices[3:4] > 1, na.rm = TRUE)
  )
}

# Contribución de la intervención al nivel de la mora.
# Recibe: el modelo, la matriz de regresoras, las variables de intervención usadas y las fechas.
# Devuelve: fecha y efecto en puntos porcentuales (suma de coeficiente por variable).
efecto_intervencion <- function(fit, xreg, variables, fechas) {
  co <- stats::coef(fit)
  efecto <- if (length(variables) == 0) 0 else
    as.numeric(as.matrix(xreg[, variables, drop = FALSE]) %*% co[variables])
  tibble::tibble(fecha = fechas, efecto = efecto)
}
