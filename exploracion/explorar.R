# Ejecutar desde la raíz del repositorio: source("exploracion/explorar.R")
# Cambiamos este archivo libremente para responder cada pregunta.
library(ggplot2)

# La franja cubre todas las fechas de marzo de 2020 a junio de 2021.
franja_intervencion <- function() {
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-07-01"),
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5)
}

ruta <- "datos/procesados/serie_mora.csv"
if (!file.exists(ruta)) {
  stop("Falta la serie. Ejecuta primero source('analisis/01_construir_serie.R', encoding = 'UTF-8').")
}
serie <- readr::read_csv(ruta, show_col_types = FALSE)

meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
serie$mes <- factor(meses[as.integer(format(serie$fecha, "%m"))], levels = meses)
cajas <- ggplot(serie, aes(mes, mora_pct)) +
  geom_boxplot(fill = "#acd3ee", color = "#1768ac", outlier.color = "#d55e00") +
  labs(title = "Distribución del índice de mora por mes",
       subtitle = "Observaciones mensuales de enero de 2015 a julio de 2026",
       x = "Mes del año", y = "Índice de mora (%)",
       caption = "Fuente: índice construido por el proyecto con datos.gov.co") +
  theme_minimal(base_size = 13)
ggsave("exploracion/cajas_por_mes.png", cajas, width = 11, height = 5.5, dpi = 150)

serie <- serie[order(serie$fecha), ]
serie$diferencia <- c(NA_real_, diff(serie$mora_pct))
serie_diferenciada <- serie[!is.na(serie$diferencia), ]
grafica_diferencia <- ggplot(serie_diferenciada, aes(fecha, diferencia)) +
  franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Primera diferencia del índice de mora",
       subtitle = "Cambio respecto al mes anterior",
       x = NULL, y = "Cambio mensual (puntos porcentuales)",
       caption = "Fuente: índice construido por el proyecto con datos.gov.co") +
  theme_minimal(base_size = 13)
ggsave("exploracion/serie_primera_diferencia.png", grafica_diferencia, width = 11, height = 5.5, dpi = 150)
cajas_diferencia <- ggplot(serie_diferenciada, aes(mes, diferencia)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_boxplot(fill = "#acd3ee", color = "#1768ac", outlier.color = "#d55e00") +
  labs(title = "Primera diferencia de la mora por mes",
       subtitle = "Cambio respecto al mes anterior, febrero de 2015 a julio de 2026",
       x = "Mes del año", y = "Cambio mensual (puntos porcentuales)",
       caption = "Fuente: índice construido por el proyecto con datos.gov.co") +
  theme_minimal(base_size = 13)
ggsave("exploracion/cajas_primera_diferencia.png", cajas_diferencia, width = 11, height = 5.5, dpi = 150)

# Diferencia estacional directamente desde la serie original, con rezago de 12 meses.
serie$diferencia_estacional <- c(rep(NA_real_, 12), diff(serie$mora_pct, lag = 12))
estacional <- serie[!is.na(serie$diferencia_estacional), ]
grafica_estacional <- ggplot(estacional, aes(fecha, diferencia_estacional)) +
  franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Diferencia estacional del índice de mora", subtitle = "Cambio frente al mismo mes del año anterior",
       x = NULL, y = "Cambio anual (puntos porcentuales)",
       caption = "Fuente: índice construido por el proyecto con datos.gov.co") +
  theme_minimal(base_size = 13)
ggsave("exploracion/serie_diferencia_estacional.png", grafica_estacional, width = 11, height = 5.5, dpi = 150)
cajas_estacional <- ggplot(estacional, aes(mes, diferencia_estacional)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_boxplot(fill = "#acd3ee", color = "#1768ac", outlier.color = "#d55e00") +
  labs(title = "Diferencia estacional de la mora por mes", subtitle = "Cambio frente al mismo mes del año anterior",
       x = "Mes del año", y = "Cambio anual (puntos porcentuales)",
       caption = "Fuente: índice construido por el proyecto con datos.gov.co") +
  theme_minimal(base_size = 13)
ggsave("exploracion/cajas_diferencia_estacional.png", cajas_estacional, width = 11, height = 5.5, dpi = 150)

# Diferencia regular y estacional combinadas: (1-B)(1-B^12)y.
serie$diferencia_combinada <- c(rep(NA_real_, 13), diff(diff(serie$mora_pct, lag = 12)))
combinada <- serie[!is.na(serie$diferencia_combinada), ]
grafica_combinada <- ggplot(combinada, aes(fecha, diferencia_combinada)) +
  franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey60") + geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Diferencia regular y estacional combinadas", x = NULL, y = "Cambio (puntos porcentuales)") + theme_minimal(base_size = 13)
ggsave("exploracion/serie_combinada.png", grafica_combinada, width = 11, height = 5.5, dpi = 150)
cajas_combinada <- ggplot(combinada, aes(mes, diferencia_combinada)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_boxplot(fill = "#acd3ee", color = "#1768ac", outlier.color = "#d55e00") +
  labs(title = "Diferencias combinadas por mes", x = "Mes del año", y = "Cambio (puntos porcentuales)") + theme_minimal(base_size = 13)
ggsave("exploracion/cajas_combinada.png", cajas_combinada, width = 11, height = 5.5, dpi = 150)

# ACF con ambas bandas y PACF con banda de ruido blanco, 36 rezagos mensuales.
guardar_correlaciones <- function(x, archivo, titulo) {
  x <- x[!is.na(x)]
  png(archivo, width = 1800, height = 700, res = 150)
  par(mfrow = c(1, 3), mar = c(4, 4, 4, 1), oma = c(0, 0, 2, 0))
  a <- acf(x, lag.max = 36, plot = FALSE)
  plot(a, ci.type = "white", main = "ACF: ruido blanco", xlab = "Rezago (meses)", ylab = "ACF")
  plot(a, ci.type = "ma", main = "ACF: Bartlett (MA)", xlab = "Rezago (meses)", ylab = "ACF")
  p <- pacf(x, lag.max = 36, plot = FALSE)
  plot(p, main = "PACF: ruido blanco", xlab = "Rezago (meses)", ylab = "PACF")
  mtext(titulo, outer = TRUE, cex = 1.1)
  dev.off()
}
guardar_correlaciones(serie$mora_pct, "exploracion/correlaciones_original.png", "Serie original")
guardar_correlaciones(serie$diferencia, "exploracion/correlaciones_primera.png", "Primera diferencia")
guardar_correlaciones(serie$diferencia_estacional, "exploracion/correlaciones_estacional.png", "Diferencia estacional desde la serie original")
guardar_correlaciones(serie$diferencia_combinada, "exploracion/correlaciones_combinada.png", "Diferencias regular y estacional")

grafica <- ggplot(serie, aes(fecha, mora_pct)) +
  franja_intervencion() +
  geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Mora de la cartera de consumo", x = NULL,
       y = "Mora (%)", caption = "Fuente: serie construida por el proyecto") +
  theme_minimal(base_size = 13)
ggsave("exploracion/serie.png", grafica, width = 10, height = 4.5, dpi = 130)

ultimos <- tail(serie[c("fecha", "mora_pct")], 12)
filas <- paste(sprintf("<tr><td>%s</td><td>%.2f %%</td></tr>",
                       ultimos$fecha, ultimos$mora_pct), collapse = "\n")
comun <- serie[complete.cases(serie[c("mora_pct", "diferencia", "diferencia_estacional", "diferencia_combinada")]), ]
varianzas <- c(var(comun$mora_pct), var(comun$diferencia), var(comun$diferencia_estacional), var(comun$diferencia_combinada))
nombres <- c("Original", "Primera diferencia", "Diferencia estacional", "Diferencias regular y estacional")
filas_varianza <- paste(sprintf("<tr><td>%s</td><td>%.4f</td></tr>", nombres, varianzas), collapse = "\n")
print(data.frame(serie = nombres, varianza = varianzas))
# Mismo criterio del informe de referencia: gamma(0) con divisor n y r1 muestral.
gamma0 <- function(x) mean((x - mean(x))^2)
r1 <- function(x) as.numeric(acf(x, lag.max = 1, plot = FALSE)$acf[2])
original <- serie$mora_pct
regular <- na.omit(serie$diferencia)
estacional_x <- na.omit(serie$diferencia_estacional)
combinada_x <- na.omit(serie$diferencia_combinada)
series_diagnostico <- list(original, regular, estacional_x, combinada_x)
identidad <- c(NA_real_, 2 * gamma0(original) * (1 - r1(original)), NA_real_,
               2 * gamma0(estacional_x) * (1 - r1(estacional_x)))
diagnostico <- data.frame(serie = nombres, n = lengths(series_diagnostico),
  varianza = vapply(series_diagnostico, gamma0, numeric(1)),
  r1 = vapply(series_diagnostico, r1, numeric(1)), identidad = identidad,
  media = vapply(series_diagnostico, mean, numeric(1)))
filas_diagnostico <- paste(vapply(seq_len(nrow(diagnostico)), function(i) {
  d <- diagnostico[i, ]
  identidad_texto <- if (is.na(d$identidad)) "—" else sprintf("%.4f", d$identidad)
  sprintf("<tr><td>%s</td><td>%d</td><td>%.4f</td><td>%.4f</td><td>%s</td><td>%.4f</td></tr>",
          d$serie, d$n, d$varianza, d$r1, identidad_texto, d$media)
}, character(1)), collapse = "\n")
print(diagnostico)
tasa <- readr::read_csv("datos/procesados/tibc_mensual.csv", show_col_types = FALSE)
tasa <- tasa[order(tasa$fecha), ]
tasa_periodo <- tasa[tasa$fecha >= min(serie$fecha) & tasa$fecha <= max(serie$fecha), ]
grafica_tasa <- ggplot(tasa_periodo, aes(fecha, tibc_consumo)) +
  franja_intervencion() +
  geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Tasa de interés bancario corriente",
       subtitle = "Consumo y ordinario, mismo período de la serie de mora",
       x = NULL, y = "Tasa (%)",
       caption = "Fuente: Superintendencia Financiera, datos.gov.co (pare-7x5i)") +
  theme_minimal(base_size = 13)
ggsave("exploracion/tasa_interes.png", grafica_tasa, width = 11, height = 5.5, dpi = 150)
# Rachas de valores iguales en meses consecutivos.
racha_id <- cumsum(c(TRUE, diff(tasa$tibc_consumo) != 0 | diff(as.integer(format(tasa$fecha, "%Y")) * 12 + as.integer(format(tasa$fecha, "%m"))) != 1))
duraciones <- as.integer(table(racha_id))
frecuencia <- as.data.frame(table(duraciones), stringsAsFactors = FALSE)
names(frecuencia) <- c("meses", "frecuencia")
frecuencia$meses <- as.integer(as.character(frecuencia$meses))
grafica_duraciones <- ggplot(frecuencia, aes(meses, frecuencia)) +
  geom_col(fill = "#1768ac", width = 0.75) +
  geom_text(aes(label = frecuencia), vjust = -0.4) +
  scale_x_continuous(breaks = frecuencia$meses) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Duración de los períodos con la misma tasa",
       subtitle = "Incluye las rachas incompletas al inicio y al final de la serie",
       x = "Meses consecutivos con el mismo valor", y = "Número de períodos") +
  theme_minimal(base_size = 13)
ggsave("exploracion/frecuencia_duracion_tasa.png", grafica_duraciones, width = 11, height = 5.5, dpi = 150)
filas_frecuencia <- paste(sprintf("<tr><td>%d</td><td>%d</td></tr>", frecuencia$meses, frecuencia$frecuencia), collapse = "\n")
print(frecuencia)
# Correlación de niveles: mora en t frente a tasa observada en t-k.
cruce <- merge(serie[c("fecha", "mora_pct")], tasa[c("fecha", "tibc_consumo")], by = "fecha", all.x = TRUE)
cruce <- cruce[order(cruce$fecha), ]
rezagos <- 0:24
correlaciones <- do.call(rbind, lapply(rezagos, function(k) {
  destino <- seq.int(k + 1, nrow(cruce))
  mora <- cruce$mora_pct[destino]
  tasa_rezagada <- cruce$tibc_consumo[destino - k]
  valido <- complete.cases(mora, tasa_rezagada)
  data.frame(rezago = k, n = sum(valido), correlacion = cor(mora[valido], tasa_rezagada[valido]),
             desde = cruce$fecha[min(destino[valido])], hasta = cruce$fecha[max(destino[valido])])
}))
grafica_correlacion_tasa <- ggplot(correlaciones, aes(rezago, correlacion)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_col(fill = "#1768ac", width = 0.7) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Correlación entre mora y tasa de interés rezagada",
       subtitle = "Niveles originales: mora del mes t frente a la tasa del mes t − k",
       x = "Rezago de la tasa (meses)", y = "Correlación de Pearson") +
  theme_minimal(base_size = 13)
ggsave("exploracion/correlacion_mora_tasa_rezagos.png", grafica_correlacion_tasa, width = 11, height = 5.5, dpi = 150)
filas_correlacion <- paste(sprintf("<tr><td>%d</td><td>%.3f</td><td>%d</td><td>%s a %s</td></tr>",
  correlaciones$rezago, correlaciones$correlacion, correlaciones$n, correlaciones$desde, correlaciones$hasta), collapse = "\n")
print(correlaciones)
# Diferenciar cada serie por separado antes de alinear fechas y rezagos.
mora_cambios <- serie[c("fecha", "diferencia")]
tasa_cambios <- tasa[c("fecha", "tibc_consumo")]
tasa_cambios$cambio_tasa <- c(NA_real_, diff(tasa_cambios$tibc_consumo))
indice_mes <- function(f) as.integer(format(f, "%Y")) * 12L + as.integer(format(f, "%m"))
mes_tasa <- indice_mes(tasa_cambios$fecha)
mes_mora <- indice_mes(mora_cambios$fecha)
matriz_tasa <- sapply(0:24, function(k) tasa_cambios$cambio_tasa[match(mes_mora - k, mes_tasa)])
completa <- !is.na(mora_cambios$diferencia) & complete.cases(matriz_tasa)
cor_cambios <- data.frame(rezago = 0:24,
  correlacion = vapply(1:25, function(j) cor(mora_cambios$diferencia[completa], matriz_tasa[completa, j]), numeric(1)))
fechas_comunes <- mora_cambios$fecha[completa]
# También recalcular los niveles con los mismos meses y tasas históricas.
correlaciones <- do.call(rbind, lapply(0:24, function(k) {
  rate <- tasa$tibc_consumo[match(mes_mora[completa] - k, mes_tasa)]
  data.frame(rezago = k, n = length(fechas_comunes),
    correlacion = cor(serie$mora_pct[completa], rate),
    desde = min(fechas_comunes), hasta = max(fechas_comunes))
}))
grafica_correlacion_tasa <- grafica_correlacion_tasa %+% correlaciones
ggsave("exploracion/correlacion_mora_tasa_rezagos.png", grafica_correlacion_tasa, width = 11, height = 5.5, dpi = 150)
filas_correlacion <- paste(sprintf("<tr><td>%d</td><td>%.3f</td><td>%d</td><td>%s a %s</td></tr>",
  correlaciones$rezago, correlaciones$correlacion, correlaciones$n, correlaciones$desde, correlaciones$hasta), collapse = "\n")
print(correlaciones[c(1, 7, 10, 13, 25), ])
grafica_cor_cambios <- ggplot(cor_cambios, aes(rezago, correlacion)) +
  geom_hline(yintercept = 0, color = "grey55") + geom_col(fill = "#1768ac", width = 0.7) +
  scale_x_continuous(breaks = seq(0, 24, 2)) + scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Correlación entre cambios mensuales de mora y tasa",
       subtitle = sprintf("Mismos %d meses de mora para todos los rezagos", length(fechas_comunes)),
       x = "Rezago del cambio de la tasa (meses)", y = "Correlación de Pearson") + theme_minimal(base_size = 13)
ggsave("exploracion/correlacion_cambios_rezagos.png", grafica_cor_cambios, width = 11, height = 5.5, dpi = 150)
filas_cor_cambios <- paste(sprintf("<tr><td>%d</td><td>%.3f</td><td>%d</td></tr>",
  cor_cambios$rezago, cor_cambios$correlacion, length(fechas_comunes)), collapse = "\n")
print(cor_cambios)
cat("Muestra común de mora:", as.character(min(fechas_comunes)), "a", as.character(max(fechas_comunes)), "n =", length(fechas_comunes), "\n")
# Excluir meses de intervención de la mora sin comprimir el calendario.
intervencion <- serie$fecha >= as.Date("2020-03-01") & serie$fecha <= as.Date("2021-06-01")
# La diferencia de julio de 2021 usa junio, también excluido.
previa_intervencion <- c(FALSE, head(intervencion, -1))
sin_intervencion <- completa & !intervencion & !previa_intervencion
comparacion_intervencion <- do.call(rbind, lapply(0:24, function(k) {
  tasa_nivel <- tasa$tibc_consumo[match(mes_mora[sin_intervencion] - k, mes_tasa)]
  data.frame(rezago = k,
    niveles = cor(serie$mora_pct[sin_intervencion], tasa_nivel),
    diferencias = cor(serie$diferencia[sin_intervencion], matriz_tasa[sin_intervencion, k + 1]))
}))
comparacion_larga <- rbind(
  data.frame(rezago = 0:24, correlacion = comparacion_intervencion$niveles, serie = "Niveles originales"),
  data.frame(rezago = 0:24, correlacion = comparacion_intervencion$diferencias, serie = "Primeras diferencias"))
grafica_sin <- ggplot(comparacion_larga, aes(rezago, correlacion)) +
  geom_hline(yintercept = 0, color = "grey55") + geom_col(fill = "#1768ac", width = 0.7) +
  facet_wrap(~ serie, ncol = 1) + scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Correlación mora y tasa sin el período de intervención",
       subtitle = sprintf("Mismos %d meses de mora en ambos casos y todos los rezagos", sum(sin_intervencion)),
       x = "Rezago de la tasa (meses)", y = "Correlación de Pearson") + theme_minimal(base_size = 13)
ggsave("exploracion/correlaciones_sin_intervencion.png", grafica_sin, width = 11, height = 8, dpi = 150)
filas_sin <- paste(sprintf("<tr><td>%d</td><td>%.3f</td><td>%.3f</td></tr>",
  comparacion_intervencion$rezago, comparacion_intervencion$niveles, comparacion_intervencion$diferencias), collapse = "\n")
print(comparacion_intervencion)
cat("Meses de mora fuera de intervención:", sum(sin_intervencion), "\n")
# Los rezagos utilizan la tasa de la fecha real, incluso si cae fuera del período de mora.
periodos <- list("Antes de la intervención" = completa & serie$fecha < as.Date("2020-03-01"),
                 "Después de la intervención" = completa & serie$fecha >= as.Date("2021-08-01"))
cor_periodos <- do.call(rbind, lapply(names(periodos), function(nombre) {
  seleccion <- periodos[[nombre]]
  do.call(rbind, lapply(0:24, function(k) {
    tasa_nivel <- tasa$tibc_consumo[match(mes_mora[seleccion] - k, mes_tasa)]
    data.frame(periodo = nombre, rezago = k, n = sum(seleccion),
      niveles = cor(serie$mora_pct[seleccion], tasa_nivel),
      diferencias = cor(serie$diferencia[seleccion], matriz_tasa[seleccion, k + 1]))
  }))
}))
periodos_largo <- rbind(
  data.frame(periodo = cor_periodos$periodo, rezago = cor_periodos$rezago, correlacion = cor_periodos$niveles, serie = "Niveles originales"),
  data.frame(periodo = cor_periodos$periodo, rezago = cor_periodos$rezago, correlacion = cor_periodos$diferencias, serie = "Primeras diferencias"))
periodos_largo$periodo <- factor(periodos_largo$periodo, levels = names(periodos))
grafica_periodos <- ggplot(periodos_largo, aes(rezago, correlacion)) +
  geom_hline(yintercept = 0, color = "grey55") + geom_col(fill = "#1768ac", width = 0.7) +
  facet_grid(serie ~ periodo) + scale_x_continuous(breaks = seq(0, 24, 4)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Correlación entre mora y tasa antes y después de la intervención",
       x = "Rezago de la tasa (meses)", y = "Correlación de Pearson") + theme_minimal(base_size = 12)
ggsave("exploracion/correlaciones_antes_despues.png", grafica_periodos, width = 12, height = 8, dpi = 150)
filas_periodos <- paste(sprintf("<tr><td>%s</td><td>%d</td><td>%d</td><td>%.3f</td><td>%.3f</td></tr>",
  cor_periodos$periodo, cor_periodos$rezago, cor_periodos$n, cor_periodos$niveles, cor_periodos$diferencias), collapse = "\n")
print(cor_periodos[cor_periodos$rezago %in% c(0, 6, 9, 12), ])
# Ejemplo didáctico: mismos datos de entrenamiento y estructura ARIMA para aislar regresores.
entrena <- serie$fecha <= as.Date("2023-12-01")
y_modelo <- serie$mora_pct[entrena]
fechas_modelo <- serie$fecha[entrena]
indicadora <- as.numeric(fechas_modelo >= as.Date("2020-03-01") & fechas_modelo <= as.Date("2021-06-01"))
tasa9 <- tasa$tibc_consumo[match(indice_mes(fechas_modelo) - 9, mes_tasa)]
modelos <- list(
  base = arima(y_modelo, order = c(1, 1, 0), method = "ML"),
  intervencion = arima(y_modelo, order = c(1, 1, 0), xreg = cbind(intervencion = indicadora), method = "ML"),
  tasa = arima(y_modelo, order = c(1, 1, 0), xreg = cbind(tasa_9_meses = tasa9), method = "ML"),
  ambos = arima(y_modelo, order = c(1, 1, 0), xreg = cbind(intervencion = indicadora, tasa_9_meses = tasa9), method = "ML")
)
etiquetas_modelos <- c("Base", "Intervención", "Tasa rezagada 9 meses", "Intervención y tasa")
filas_modelos <- paste(vapply(seq_along(modelos), function(i) {
  m <- modelos[[i]]
  sprintf("<tr><td>%s</td><td>%.2f</td><td>%s</td></tr>", etiquetas_modelos[i], AIC(m),
          paste(sprintf("%s = %.4f", names(m$coef), m$coef), collapse = "; "))
}, character(1)), collapse = "\n")
print(lapply(modelos, function(m) c(AIC = AIC(m), m$coef)))
# Regresión descriptiva por mínimos cuadrados; inspeccionar sus residuos sin imponer AR.
modelo_ols <- lm(y_modelo ~ tasa9)
coef_ols <- coef(modelo_ols)
errores_ols <- as.numeric(residuals(modelo_ols))
dif_errores_ols <- diff(errores_ols)
guardar_correlaciones(errores_ols, "exploracion/residuos_ols_acf.png", "Residuos de regresión en niveles")
guardar_correlaciones(dif_errores_ols, "exploracion/residuos_ols_diferencia_acf.png", "Primera diferencia de los mismos residuos")
datos_errores <- rbind(
  data.frame(fecha = fechas_modelo, valor = errores_ols, serie = "Residuos de regresión en niveles"),
  data.frame(fecha = fechas_modelo[-1], valor = dif_errores_ols, serie = "Primera diferencia de esos residuos"))
grafica_errores <- ggplot(datos_errores, aes(fecha, valor)) + franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey55") + geom_line(color = "#1768ac", linewidth = 0.7) +
  facet_wrap(~ serie, ncol = 1, scales = "free_y") +
  labs(title = "Residuos de la regresión y su primera diferencia", x = NULL, y = "Puntos porcentuales") +
  theme_minimal(base_size = 12)
ggsave("exploracion/experimento_errores.png", grafica_errores, width = 11, height = 7, dpi = 150)
cat("OLS intercepto:", coef_ols[1], "pendiente:", coef_ols[2], "r1 residuos:", r1(errores_ols), "r1 diferencias:", r1(dif_errores_ols), "\n")
# Ajustar conjuntamente regresión y AR(1) para el error diferenciado.
modelo_ar1 <- modelos$tasa
innovaciones_ar1 <- as.numeric(residuals(modelo_ar1))[-1]
fechas_innov <- fechas_modelo[-1]
guardar_correlaciones(innovaciones_ar1, "exploracion/innovaciones_ar1_experimento.png", "Innovaciones estimadas del modelo AR(1)")
grafica_innov <- ggplot(data.frame(fecha = fechas_innov, innovacion = innovaciones_ar1), aes(fecha, innovacion)) +
  franja_intervencion() + geom_hline(yintercept = 0, color = "grey55") +
  geom_line(color = "#1768ac", linewidth = 0.7) +
  labs(title = "Innovaciones del modelo con error diferenciado AR(1)", x = NULL, y = "Puntos porcentuales") +
  theme_minimal(base_size = 12)
ggsave("exploracion/innovaciones_ar1_tiempo.png", grafica_innov, width = 11, height = 5, dpi = 150)
lb_ar1 <- Box.test(innovaciones_ar1, lag = 24, type = "Ljung-Box", fitdf = 1)
acf_ar1 <- as.numeric(acf(innovaciones_ar1, lag.max = 24, plot = FALSE)$acf)
cat("INNOVACIONES AR1 Ljung-Box:", unname(lb_ar1$statistic), "p:", lb_ar1$p.value,
    "ACF1:", acf_ar1[2], "ACF12:", acf_ar1[13], "\n")
# Rampas truncadas: se mantienen constantes después de cada fase.
mes_entrena <- indice_mes(fechas_modelo)
rampa_caida <- pmin(pmax(mes_entrena - (2020L * 12L + 2L), 0), 5)
rampa_rebote <- pmin(pmax(mes_entrena - (2020L * 12L + 7L), 0), 6)
modelo_rampas <- arima(y_modelo, order = c(1, 1, 0),
  xreg = cbind(tasa_9_meses = tasa9, caida = rampa_caida, rebote = rampa_rebote), method = "ML")
innov_rampas <- as.numeric(residuals(modelo_rampas))[-1]
acf_rampas <- as.numeric(acf(innov_rampas, lag.max = 24, plot = FALSE)$acf)
lb_rampas <- Box.test(innov_rampas, lag = 24, type = "Ljung-Box", fitdf = 1)
guardar_correlaciones(innov_rampas, "exploracion/innovaciones_rampas_acf.png", "Innovaciones con intervención por rampas")
trayectorias_intervencion <- data.frame(fecha = fechas_modelo,
  caida = coef(modelo_rampas)[["caida"]] * rampa_caida,
  rebote = coef(modelo_rampas)[["rebote"]] * rampa_rebote)
trayectorias_intervencion$total <- trayectorias_intervencion$caida + trayectorias_intervencion$rebote
grafica_intervencion <- ggplot(trayectorias_intervencion, aes(fecha, total)) + franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey55") + geom_line(color = "#1768ac", linewidth = 0.8) +
  labs(title = "Componente de intervención estimado", subtitle = "Suma de rampas de caída y rebote; no es la mora observada",
       x = NULL, y = "Aporte estimado (puntos porcentuales)") + theme_minimal(base_size = 12)
ggsave("exploracion/intervencion_rampas.png", grafica_intervencion, width = 11, height = 5, dpi = 150)
innov_comparacion <- rbind(data.frame(fecha = fechas_innov, valor = innovaciones_ar1, modelo = "Sin intervención"),
  data.frame(fecha = fechas_innov, valor = innov_rampas, modelo = "Con rampas de intervención"))
grafica_innov_comparacion <- ggplot(innov_comparacion, aes(fecha, valor)) + franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey55") + geom_line(color = "#1768ac", linewidth = 0.7) +
  facet_wrap(~ modelo, ncol = 1) + labs(title = "Innovaciones con y sin intervención", x = NULL, y = "Puntos porcentuales") +
  theme_minimal(base_size = 12)
ggsave("exploracion/innovaciones_comparacion_intervencion.png", grafica_innov_comparacion, width = 11, height = 7, dpi = 150)
cat("RAMPAS:", coef(modelo_rampas), "AIC", AIC(modelo_rampas), "ACF1", acf_rampas[2], "ACF12", acf_rampas[13], "LB", unname(lb_rampas$statistic), lb_rampas$p.value, "\n")
# Agregar un MA estacional de período 12, manteniendo la misma regresión y muestra.
modelo_ma_estacional <- arima(y_modelo, order = c(1, 1, 0),
  seasonal = list(order = c(0, 0, 1), period = 12),
  xreg = cbind(tasa_9_meses = tasa9, caida = rampa_caida, rebote = rampa_rebote), method = "ML")
innov_ma_estacional <- as.numeric(residuals(modelo_ma_estacional))[-1]
acf_ma_estacional <- as.numeric(acf(innov_ma_estacional, lag.max = 24, plot = FALSE)$acf)
lb_ma_estacional <- Box.test(innov_ma_estacional, lag = 24, type = "Ljung-Box", fitdf = 2)
guardar_correlaciones(innov_ma_estacional, "exploracion/innovaciones_ma_estacional_acf.png", "Innovaciones con MA estacional")
innov_estacional_df <- rbind(data.frame(fecha = fechas_innov, valor = innov_rampas, modelo = "Sin MA estacional"),
  data.frame(fecha = fechas_innov, valor = innov_ma_estacional, modelo = "Con MA estacional"))
grafica_innov_estacional <- ggplot(innov_estacional_df, aes(fecha, valor)) + franja_intervencion() +
  geom_hline(yintercept = 0, color = "grey55") + geom_line(color = "#1768ac", linewidth = 0.7) +
  facet_wrap(~ modelo, ncol = 1) + labs(title = "Innovaciones: comparación del componente estacional", x = NULL, y = "Puntos porcentuales") + theme_minimal(base_size = 12)
ggsave("exploracion/innovaciones_ma_estacional_tiempo.png", grafica_innov_estacional, width = 11, height = 7, dpi = 150)
cat("MA ESTACIONAL:", coef(modelo_ma_estacional), "AIC", AIC(modelo_ma_estacional),
    "ACF1", acf_ma_estacional[2], "ACF12", acf_ma_estacional[13], "LB", unname(lb_ma_estacional$statistic), lb_ma_estacional$p.value,
    "convergencia", modelo_ma_estacional$code, "\n")
# AICc utiliza n efectivo tras la diferencia y K incluye varianza de innovaciones.
criterios <- do.call(rbind, lapply(list(`Sin MA estacional` = modelo_rampas, `Con MA estacional` = modelo_ma_estacional), function(m) {
  logv <- logLik(m); k <- attr(logv, "df"); n <- attr(logv, "nobs"); a <- AIC(m)
  data.frame(n = n, k = k, log_verosimilitud = as.numeric(logv), AIC = a,
             AICc = a + 2 * k * (k + 1) / (n - k - 1), BIC = BIC(m))
}))
filas_criterios <- paste(sprintf("<tr><td>%s</td><td>%d</td><td>%d</td><td>%.2f</td><td>%.2f</td><td>%.2f</td></tr>", rownames(criterios), criterios$n, criterios$k, criterios$AIC, criterios$AICc, criterios$BIC), collapse = "\n")
print(criterios)
# Ajustes de un paso dentro de muestra: observado menos innovación estimada.
modelos_ajuste <- list("1. Tasa y AR(1)" = modelo_ar1,
                       "2. Tasa, AR(1) y rampas" = modelo_rampas,
                       "3. Tasa, rampas y MA estacional" = modelo_ma_estacional)
datos_ajuste <- do.call(rbind, lapply(names(modelos_ajuste), function(nombre) {
  innov <- as.numeric(residuals(modelos_ajuste[[nombre]]))
  data.frame(fecha = fechas_modelo[-1], observada = y_modelo[-1],
             ajustada = y_modelo[-1] - innov[-1], modelo = nombre)
}))
datos_ajuste$modelo <- factor(datos_ajuste$modelo, levels = names(modelos_ajuste))
grafica_ajuste <- ggplot(datos_ajuste, aes(fecha)) + franja_intervencion() +
  geom_line(aes(y = observada, color = "Mora observada"), linewidth = 0.7) +
  geom_line(aes(y = ajustada, color = "Ajuste de un paso"), linewidth = 0.7, linetype = "dashed") +
  facet_wrap(~ modelo, ncol = 1) +
  scale_color_manual(values = c("Mora observada" = "#1768ac", "Ajuste de un paso" = "#d65f1a")) +
  labs(title = "Mora observada y ajuste de cada modelo", subtitle = "Ajustes dentro de muestra, febrero de 2015 a diciembre de 2023",
       x = NULL, y = "Índice de mora (%)", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave("exploracion/comparacion_ajustes.png", grafica_ajuste, width = 11, height = 10, dpi = 150)
pagina_experimento <- c(
  '<hr style="border:0;border-top:3px solid #1768ac;margin:60px 0">',
  '<h1>Fase de experimentación</h1>',
  '<h2>Experimento 1: residuos de una regresión sin imponer ARIMA</h2>',
  '<p>Regresión por mínimos cuadrados de la mora en niveles frente a la tasa de nueve meses antes, con intercepto. Entrenamiento de enero de 2015 a diciembre de 2023, incluido el período sombreado; sin intervención explícita ni términos estacionales. No se impone una estructura AR a los residuos.</p>',
  sprintf('<p>Mora(t) = %.4f + %.4f × tasa(t−9) + e(t). Los coeficientes son descriptivos; la dependencia y la posible no estacionariedad limitan su inferencia.</p>', coef_ols[1], coef_ols[2]),
  '<p>Luego calculamos &Delta;e(t) = e(t) − e(t−1), con los mismos coeficientes de la regresión original. Por álgebra, &Delta;e(t) = &Delta;mora(t) − &beta; × &Delta;tasa(t−9). No se vuelve a estimar la regresión sobre diferencias.</p>',
  sprintf('<p>ACF en el rezago 1: %.3f para los residuos y %.3f para sus primeras diferencias.</p>', r1(errores_ols), r1(dif_errores_ols)),
  '<img src="experimento_errores.png" alt="Evolución de errores e innovaciones">',
  '<h3>Residuos de la regresión original</h3><img src="residuos_ols_acf.png" alt="ACF y PACF de residuos en niveles">',
  '<h3>Primera diferencia de esos residuos</h3><img src="residuos_ols_diferencia_acf.png" alt="ACF y PACF de residuos diferenciados">',
  '<p>Las bandas son referencias aproximadas para series observadas; no constituyen pruebas exactas después de estimar la regresión. La intervención y las tendencias pueden afectar los correlogramas.</p>',
  '<h2>Experimento 2: AR(1) para el error diferenciado</h2>',
  '<p>Se ajustan conjuntamente la regresión y el AR(1): mora(t) = &beta; tasa(t−9) + &eta;(t), u(t) = &Delta;&eta;(t), u(t) = &phi; u(t−1) + &epsilon;(t). Los coeficientes pueden cambiar respecto a la regresión inicial. No se incluyen intervención ni términos estacionales.</p>',
  sprintf('<p>Estimaciones: &beta; = %.4f; &phi; = %.4f. Las innovaciones se obtienen del ajuste de R; se omite la primera por la inicialización.</p>', coef(modelo_ar1)[["tasa_9_meses"]], coef(modelo_ar1)[["ar1"]]),
  '<img src="innovaciones_ar1_tiempo.png" alt="Innovaciones del modelo en el tiempo">',
  '<img src="innovaciones_ar1_experimento.png" alt="ACF y PACF de las innovaciones del modelo">',
  sprintf('<p>ACF rezago 1 = %.3f; rezago 12 = %.3f. Ljung-Box hasta rezago 24: Q = %.3f, 23 grados de libertad aproximados (descontado un parámetro AR), valor p = %.4f. Bandas y prueba son aproximadas y se interpretan con cautela por el regresor estimado, selección del rezago y período de intervención.</p>', acf_ar1[2], acf_ar1[13], unname(lb_ar1$statistic), lb_ar1$p.value),
  '<h2>Experimento 3: intervención gradual con caída y rebote</h2>',
  '<p>Regresión en niveles: mora(t) = beta × tasa(t−9) + b1 × caída(t) + b2 × rebote(t) + error(t), con primera diferencia del error modelada como AR(1). La rampa de caída crece de marzo a julio de 2020 y permanece en 5; la del rebote crece de agosto de 2020 a enero de 2021 y permanece en 6. Así el efecto total posterior queda constante, no se fuerza a cero. Fechas elegidas exploratoriamente.</p>',
  sprintf('<p>Coeficientes: tasa %.4f; caída %.4f; rebote %.4f; AR(1) %.4f. AIC sin rampas %.2f; con rampas %.2f.</p>', coef(modelo_rampas)[["tasa_9_meses"]], coef(modelo_rampas)[["caida"]], coef(modelo_rampas)[["rebote"]], coef(modelo_rampas)[["ar1"]], AIC(modelo_ar1), AIC(modelo_rampas)),
  '<img src="intervencion_rampas.png" alt="Trayectoria estimada del componente de intervención">',
  '<img src="innovaciones_comparacion_intervencion.png" alt="Innovaciones con y sin rampas">',
  '<h3>ACF y PACF de innovaciones con intervención</h3><img src="innovaciones_rampas_acf.png" alt="Correlogramas de innovaciones con rampas">',
  sprintf('<table><tr><th>Modelo</th><th>ACF 1</th><th>ACF 12</th><th>Ljung-Box Q(24)</th><th>p aproximado</th></tr><tr><td>Sin intervención</td><td>%.3f</td><td>%.3f</td><td>%.3f</td><td>%.4f</td></tr><tr><td>Rampas</td><td>%.3f</td><td>%.3f</td><td>%.3f</td><td>%.4f</td></tr></table>', acf_ar1[2], acf_ar1[13], unname(lb_ar1$statistic), lb_ar1$p.value, acf_rampas[2], acf_rampas[13], unname(lb_rampas$statistic), lb_rampas$p.value),
  '<p>La prueba usa 23 grados de libertad aproximados (24 rezagos menos un parámetro AR); la estimación de regresores y la selección exploratoria limitan la interpretación de sus valores p. La gráfica de intervención es una contribución condicionada al modelo, no un efecto causal demostrado.</p>',
  '<hr style="border:0;border-top:2px solid #1768ac;margin:50px 0"><h2>Experimento 4: componente MA estacional</h2>',
  '<p>Se mantiene la tasa rezagada nueve meses, las dos rampas, una diferencia regular y el AR(1), y se agrega un MA estacional de período 12. No se aplica diferencia estacional. Error: (1 − phi B)(1 − B) eta(t) = (1 + Theta B^12) epsilon(t). La misma muestra se utiliza en ambos modelos.</p>',
  sprintf('<p>Estimaciones: AR(1) %.4f; MA estacional %.4f; tasa %.4f; caída %.4f; rebote %.4f.</p>', coef(modelo_ma_estacional)[["ar1"]], coef(modelo_ma_estacional)[["sma1"]], coef(modelo_ma_estacional)[["tasa_9_meses"]], coef(modelo_ma_estacional)[["caida"]], coef(modelo_ma_estacional)[["rebote"]]),
  sprintf('<table><tr><th>Modelo</th><th>AIC</th><th>ACF 1</th><th>ACF 12</th><th>Q(24)</th><th>p aproximado</th></tr><tr><td>Rampas, sin MA estacional</td><td>%.2f</td><td>%.3f</td><td>%.3f</td><td>%.3f</td><td>%.4f</td></tr><tr><td>Rampas y MA estacional</td><td>%.2f</td><td>%.3f</td><td>%.3f</td><td>%.3f</td><td>%.4f</td></tr></table>', AIC(modelo_rampas), acf_rampas[2], acf_rampas[13], unname(lb_rampas$statistic), lb_rampas$p.value, AIC(modelo_ma_estacional), acf_ma_estacional[2], acf_ma_estacional[13], unname(lb_ma_estacional$statistic), lb_ma_estacional$p.value),
  '<p>Ljung-Box: hasta 24 rezagos, con 23 grados de libertad aproximados para el modelo anterior y 22 para el modelo estacional. Las bandas y valores p son aproximados: se estimaron regresores y se eligieron formas y rezagos de manera exploratoria.</p>',
  '<img src="innovaciones_ma_estacional_tiempo.png" alt="Innovaciones antes y después de agregar el MA estacional">',
  '<h3>ACF y PACF de las innovaciones con MA estacional</h3><img src="innovaciones_ma_estacional_acf.png" alt="Correlogramas de las innovaciones con MA estacional">',
  '<h2>Experimento 5: complejidad y criterios de información</h2>',
  '<p>Se comparan las mismas observaciones, tasa, rampas, diferencia regular y AR(1); el segundo modelo añade un MA estacional. K incluye los coeficientes estimados y la varianza de innovaciones. n es el tamaño efectivo usado por la función de verosimilitud después de diferenciar. AICc agrega una corrección por tamaño de muestra; BIC penaliza de otra forma la complejidad. Menor es mejor dentro de esta comparación.</p>',
  '<table><tr><th>Modelo</th><th>n efectivo</th><th>Parámetros K</th><th>AIC</th><th>AICc</th><th>BIC</th></tr>', filas_criterios, '</table>',
  '<p>Los criterios y diagnósticos de residuos no garantizan ausencia de sobreajuste. Las fechas de intervención y el rezago de tasa fueron elegidos exploratoriamente; aún debe evaluarse con pronósticos fuera de muestra.</p>',
  '<h2>Experimento 6: ajuste de los modelos</h2>',
  '<p>La línea azul es la mora observada y la línea naranja es el ajuste de un paso dentro de la muestra. Para cada mes el ajuste utiliza información previa y parámetros estimados con toda la muestra de entrenamiento. Se omite la primera observación por inicialización. Las fechas de intervención se sombrean en gris.</p>',
  '<img src="comparacion_ajustes.png" alt="Mora observada y ajuste de un paso de los tres modelos">',
  '<p>La cercanía visual dentro de muestra no es una evaluación de pronósticos fuera de muestra.</p>'
)
pagina <- c(
  '<!doctype html><html lang="es"><head><meta charset="utf-8">',
  '<title>Exploración de la mora</title>',
  '<style>body{font:16px system-ui;max-width:1000px;margin:40px auto;padding:0 20px;color:#203047} img{max-width:100%}table{border-collapse:collapse}td,th{padding:7px 25px 7px 0;text-align:left;border-bottom:1px solid #ddd}</style></head><body>',
  '<h1>Exploración de la mora</h1>',
  sprintf('<p>Serie mensual: %s a %s · %s observaciones.</p>', min(serie$fecha), max(serie$fecha), nrow(serie)),
  '<p>En las gráficas temporales, la franja gris indica el período de intervención: marzo de 2020 a junio de 2021.</p>',
  '<h2>1. Serie original</h2><h3>Serie en el tiempo</h3><img src="serie.png" alt="Serie original">',
  '<h3>Cajas y bigotes por mes</h3><img src="cajas_por_mes.png" alt="Cajas por mes">',
  '<h3>ACF y PACF</h3><img src="correlaciones_original.png" alt="Correlaciones de la serie original">',
  '<h2>2. Primera diferencia regular</h2><p>y(t) − y(t−1)</p><h3>Serie en el tiempo</h3><img src="serie_primera_diferencia.png" alt="Primera diferencia">',
  '<h3>Cajas y bigotes por mes</h3><img src="cajas_primera_diferencia.png" alt="Cajas primera diferencia">',
  '<h3>ACF y PACF</h3><img src="correlaciones_primera.png" alt="Correlaciones de la primera diferencia">',
  '<h2>3. Diferencia estacional desde la serie original</h2><p>y(t) − y(t−12)</p><h3>Serie en el tiempo</h3><img src="serie_diferencia_estacional.png" alt="Diferencia estacional">',
  '<h3>Cajas y bigotes por mes</h3><img src="cajas_diferencia_estacional.png" alt="Cajas diferencia estacional">',
  '<h3>ACF y PACF</h3><img src="correlaciones_estacional.png" alt="Correlaciones de la diferencia estacional">',
  '<h2>4. Diferencias regular y estacional combinadas</h2><p>[y(t) − y(t−12)] − [y(t−1) − y(t−13)]</p>',
  '<h3>Serie en el tiempo</h3><img src="serie_combinada.png" alt="Diferencias combinadas">',
  '<h3>Cajas y bigotes por mes</h3><img src="cajas_combinada.png" alt="Cajas diferencias combinadas">',
  '<h3>ACF y PACF</h3><img src="correlaciones_combinada.png" alt="Correlaciones diferencias combinadas">',
  '<p>Bandas aproximadas al 95 %: ruido blanco y, en la ACF, Bartlett bajo un supuesto MA(k−1). Son referencias por rezago y dependen de sus supuestos.</p>',
  '<h2>5. Diagnóstico de sobrediferenciación</h2>',
  '<p>Siguiendo el informe de referencia: varianza con divisor n, autocorrelación r1 y predicción 2 × varianza previa × (1 − r1 previa). La identidad corresponde a una diferencia regular y supone estacionariedad de la serie previa; se muestra como referencia aproximada. Cada transformación usa todas sus observaciones disponibles.</p>',
  '<table><tr><th>Transformación</th><th>n</th><th>Varianza (n)</th><th>r1</th><th>Identidad</th><th>Media</th></tr>', filas_diagnostico, '</table>',
  '<p>En la fila combinada, la serie previa es la diferencia estacional. La cercanía entre identidad y varianza observada no demuestra estacionariedad: las diferencias de muestra y efectos de extremos también influyen.</p>',
  '<h3>Comparación adicional en período común</h3>',
  sprintf('<p>Varianza muestral en el mismo período: %s a %s (%s observaciones). Unidades: puntos porcentuales al cuadrado.</p>', min(comun$fecha), max(comun$fecha), nrow(comun)),
  '<table><tr><th>Serie</th><th>Varianza</th></tr>', filas_varianza,
  '</table><p>La varianza por sí sola no demuestra sobrediferenciación; se interpreta junto con la ACF y el contexto de la serie.</p>',
  '<h2>6. Tasa de interés bancario corriente</h2>',
  sprintf('<p>Modalidad consumo y ordinario. La gráfica muestra el período de la mora: %s a %s (%s observaciones). La tasa era trimestral hasta agosto de 2017 y el proyecto la representa mes a mes según su vigencia.</p>', min(tasa_periodo$fecha), max(tasa_periodo$fecha), nrow(tasa_periodo)),
  '<img src="tasa_interes.png" alt="Serie mensual de tasa de interés bancario corriente de consumo y ordinario">',
  '<h3>Frecuencia de las duraciones sin cambios</h3>',
  '<p>Esta tabla usa toda la tasa disponible desde 2007. Cada período reúne meses consecutivos con exactamente el mismo valor. Los períodos en los extremos pueden estar incompletos. Hasta agosto de 2017, la tasa tenía vigencia trimestral y se representó mensualmente.</p>',
  '<img src="frecuencia_duracion_tasa.png" alt="Frecuencia de duración de períodos con la tasa constante">',
  '<table><tr><th>Duración en meses</th><th>Número de períodos</th></tr>', filas_frecuencia, '</table>',
  '<h2>7. Mora y tasa de interés: correlaciones por rezago</h2>',
  '<p>Para cada rezago k se compara la mora del mes t con la tasa del mes t−k, usando las series en sus niveles originales. Rezago 0 compara el mismo mes. Todos los rezagos usan los mismos meses de mora que el análisis de diferencias; para ello se usan tasas anteriores a 2015 cuando son necesarias. La correlación resume todo el período, no es una correlación móvil.</p>',
  '<img src="correlacion_mora_tasa_rezagos.png" alt="Correlación de la mora con la tasa rezagada entre cero y 24 meses">',
  '<table><tr><th>Rezago (meses)</th><th>Correlación</th><th>Pares</th><th>Período de mora</th></tr>', filas_correlacion, '</table>',
  '<p>Las correlaciones de niveles pueden reflejar tendencias, persistencia y quiebres compartidos; no demuestran causalidad ni indican por sí solas que la tasa mejore el pronóstico.</p>',
  '<h2>8. Correlación entre primeras diferencias</h2>',
  sprintf('<p>Se compara el cambio mensual de la mora en t con el cambio mensual de la tasa en t−k. Para cada rezago se usan los mismos %d meses de mora: %s a %s. Se utilizan tasas anteriores a 2015 cuando el rezago lo requiere.</p>', length(fechas_comunes), min(fechas_comunes), max(fechas_comunes)),
  '<img src="correlacion_cambios_rezagos.png" alt="Correlaciones entre cambios de mora y tasa rezagada">',
  '<table><tr><th>Rezago (meses)</th><th>Correlación</th><th>Pares</th></tr>', filas_cor_cambios, '</table>',
  '<p>Se incluyen los meses de pandemia. Estas correlaciones son exploratorias y no prueban causalidad.</p>',
  '<h2>9. Correlaciones sin período de intervención</h2>',
  sprintf('<p>El informe oficial marca marzo de 2020 a junio de 2021 como período de alivios. Se excluyen esos meses de mora y también julio de 2021, pues su primera diferencia usa junio. Quedan %d meses de mora, iguales en ambas comparaciones y todos los rezagos.</p>', sum(sin_intervencion)),
  '<p>Se calculan primero las diferencias entre meses consecutivos de la serie original y luego se excluyen los meses indicados. Las tasas rezagadas se conservan aunque su fecha caiga en el período de intervención; los rezagos siguen el calendario real.</p>',
  '<img src="correlaciones_sin_intervencion.png" alt="Correlaciones entre mora y tasa sin meses de intervención">',
  '<table><tr><th>Rezago</th><th>Niveles</th><th>Primeras diferencias</th></tr>', filas_sin, '</table>',
  '<p>Es una exploración con observaciones omitidas, no un modelo de intervención. No demuestra causalidad.</p>',
  '<h2>10. Antes y después de la intervención</h2>',
  sprintf('<p>Antes: febrero de 2015 a febrero de 2020 (%d meses de mora). Después: agosto de 2021 a julio de 2026 (%d meses). En cada período se usan los mismos meses de mora para todos los rezagos y ambas transformaciones. Julio de 2021 se excluye porque su primera diferencia depende de junio.</p>', sum(periodos[[1]]), sum(periodos[[2]])),
  '<p>Las tasas rezagadas se toman de sus fechas reales, que pueden estar fuera del período de mora, incluso durante la intervención. Las diferencias se calculan antes de segmentar. Las correlaciones resumen cada período completo; no son correlaciones móviles ni prueban causalidad.</p>',
  '<img src="correlaciones_antes_despues.png" alt="Correlaciones antes y después de la intervención, en niveles y primeras diferencias">',
  '<table><tr><th>Período de mora</th><th>Rezago</th><th>Pares</th><th>Niveles</th><th>Primeras diferencias</th></tr>', filas_periodos, '</table>',
  '<h2>11. Primer experimento: intervención y tasa</h2>',
  '<p>Ejemplo didáctico con datos de enero de 2015 a diciembre de 2023: cuatro modelos de regresión con errores ARIMA(1,1,0), ajustados por máxima verosimilitud. La intervención vale 1 entre marzo de 2020 y junio de 2021. La tasa se incorpora con rezago de nueve meses.</p>',
  '<table><tr><th>Modelo</th><th>AIC</th><th>Coeficientes</th></tr>', filas_modelos, '</table>',
  '<p>La diferencia se aplica al error de regresión: en niveles, mora = intervención + tasa rezagada + error ARIMA. En estos modelos no se han incluido términos estacionales ni se han diagnosticado los residuos; las cifras son exploratorias, no constituyen una selección. El rezago 9 se sugirió con la muestra completa: para evaluar pronósticos debe seleccionarse nuevamente sin usar datos futuros.</p>',
  pagina_experimento, '</body></html>'
)
conexion <- file("exploracion/resultados.html", open = "w", encoding = "UTF-8")
writeLines(pagina, conexion)
close(conexion)
print(ultimos)
cat("\nReporte actualizado: exploracion/resultados.html\n")
