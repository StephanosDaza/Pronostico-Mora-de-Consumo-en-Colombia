# Orden de integración: diferencias candidatas y pruebas de raíz unitaria

# Aplica una diferenciación a una serie mensual.
# Recibe: vector numérico ordenado en el tiempo y la diferenciación ("ninguna", "regular",
# "estacional" o "ambas").
# Devuelve: la serie diferenciada, sin los valores que se pierden al inicio.
aplicar_diferencia <- function(x, diferencia) {
  switch(diferencia,
         ninguna    = x,
         regular    = diff(x, lag = 1),
         estacional = diff(x, lag = 12),
         ambas      = diff(diff(x, lag = 12), lag = 1))
}

ETIQUETAS_DIFERENCIA <- c(ninguna = "Sin diferenciar", regular = "Regular (∇)",
                          estacional = "Estacional (∇12)", ambas = "Regular y estacional (∇∇12)")

# Varianza y autocorrelación de cada diferenciación candidata.
# Recibe: vector numérico ordenado en el tiempo.
# Devuelve: una fila por diferenciación con n, varianza y ACF en los rezagos 1 y 12.
# Si una diferencia sobra, la varianza sube frente a la diferenciación anterior.
resumen_diferencias <- function(x) {
  purrr::map_dfr(names(ETIQUETAS_DIFERENCIA), function(dif) {
    z <- aplicar_diferencia(x, dif)
    a <- stats::acf(z, lag.max = 12, plot = FALSE)$acf
    tibble::tibble(diferencia = ETIQUETAS_DIFERENCIA[[dif]], n = length(z),
                   varianza = stats::var(z), acf_1 = a[2], acf_12 = a[13])
  })
}

# KPSS y ADF sobre cada diferenciación candidata.
# Recibe: vector numérico ordenado en el tiempo.
# Devuelve: una fila por diferenciación con el estadístico y el valor crítico al 5 % de cada prueba.
#   KPSS, H0: la serie es estacionaria. Se rechaza si el estadístico supera el crítico.
#   ADF con constante, H0: hay raíz unitaria. Se rechaza si el estadístico es menor que el crítico.
pruebas_integracion <- function(x) {
  purrr::map_dfr(names(ETIQUETAS_DIFERENCIA), function(dif) {
    z <- aplicar_diferencia(x, dif)
    kpss <- urca::ur.kpss(z, type = "mu", lags = "short")
    adf  <- urca::ur.df(z, type = "drift", lags = 12, selectlags = "AIC")
    tibble::tibble(
      diferencia      = ETIQUETAS_DIFERENCIA[[dif]],
      kpss_estad      = kpss@teststat[1],
      kpss_critico_5  = kpss@cval[1, "5pct"],
      kpss_rechaza    = kpss@teststat[1] > kpss@cval[1, "5pct"],
      adf_estad       = adf@teststat[1],
      adf_critico_5   = adf@cval[1, "5pct"],
      adf_rechaza     = adf@teststat[1] < adf@cval[1, "5pct"]
    )
  })
}

# Número de diferencias que sugieren las pruebas automáticas.
# Recibe: vector numérico mensual ordenado en el tiempo.
# Devuelve: una fila por prueba con las diferencias sugeridas.
#   ndiffs (KPSS y ADF): diferencias regulares sobre la serie.
#   nsdiffs: diferencias estacionales, sobre la serie y sobre la serie con diferencia regular.
#     seas mide la fuerza de la estacionalidad (sugiere 1 si es mayor que 0,64);
#     OCSB (H0: raíz unitaria estacional) y Canova-Hansen (H0: estacionalidad estable, requiere uroot)
#     prueban si la estacionalidad es estocástica, que es lo que justifica la diferencia estacional.
diferencias_sugeridas <- function(x) {
  y  <- stats::ts(x, frequency = 12)
  dy <- diff(y)
  tibble::tribble(
    ~prueba,                                   ~sobre,                 ~diferencias,
    "ndiffs con KPSS (regular)",               "serie",                forecast::ndiffs(y, test = "kpss"),
    "ndiffs con ADF (regular)",                "serie",                forecast::ndiffs(y, test = "adf"),
    "nsdiffs por fuerza estacional",           "serie",                forecast::nsdiffs(y, test = "seas"),
    "nsdiffs con OCSB",                        "serie",                forecast::nsdiffs(y, test = "ocsb"),
    "nsdiffs con Canova-Hansen",               "serie",                forecast::nsdiffs(y, test = "ch"),
    "nsdiffs por fuerza estacional",           "diferencia regular",   forecast::nsdiffs(dy, test = "seas"),
    "nsdiffs con OCSB",                        "diferencia regular",   forecast::nsdiffs(dy, test = "ocsb"),
    "nsdiffs con Canova-Hansen",               "diferencia regular",   forecast::nsdiffs(dy, test = "ch")
  )
}
