# ACF y PACF

# diferencia: ninguna, regular (lag 1), estacional (lag 12) o ambas
diferenciar <- function(x, diferencia = c("ninguna", "regular", "estacional", "ambas")) {
  diferencia <- match.arg(diferencia)
  switch(diferencia,
         ninguna    = x,
         regular    = diff(x, lag = 1),
         estacional = diff(x, lag = 12),
         ambas      = diff(diff(x, lag = 1), lag = 12))
}

# ACF y PACF con banda de 1,96/sqrt(n)
calcular_acf_pacf <- function(x, rezagos = 36) {
  x <- x[!is.na(x)]
  a <- stats::acf(x, lag.max = rezagos, plot = FALSE)
  p <- stats::pacf(x, lag.max = rezagos, plot = FALSE)
  dplyr::bind_rows(
    tibble::tibble(funcion = "ACF",  rezago = as.numeric(a$lag)[-1], valor = as.numeric(a$acf)[-1]),
    tibble::tibble(funcion = "PACF", rezago = as.numeric(p$lag),     valor = as.numeric(p$acf))
  ) |>
    dplyr::mutate(banda = 1.96 / sqrt(length(x)))
}

# gráfica de ACF y PACF
grafico_acf_pacf <- function(d, nombre, diferencia = "ninguna", rezagos = 36) {
  d <- dplyr::arrange(d, fecha)
  x <- diferenciar(d$mora_pct, diferencia)
  datos <- calcular_acf_pacf(x, rezagos)
  etiqueta <- c(ninguna = "en niveles", regular = "con diferencia regular",
                estacional = "con diferencia estacional",
                ambas = "con diferencia regular y estacional")[[diferencia]]

  ggplot2::ggplot(datos, ggplot2::aes(rezago, valor)) +
    ggplot2::geom_vline(xintercept = c(12, 24, 36), colour = "grey88") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = banda), linetype = "dashed", colour = "#eb6834") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = -banda), linetype = "dashed", colour = "#eb6834") +
    ggplot2::geom_segment(ggplot2::aes(xend = rezago, yend = 0), colour = "#2a78d6", linewidth = 0.7) +
    ggplot2::facet_wrap(~ funcion, ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, rezagos, 6)) +
    ggplot2::labs(title = paste0("ACF y PACF: ", nombre, " (", etiqueta, ")"),
                  subtitle = paste0("Líneas punteadas: banda aproximada de significancia al 95 %. ",
                                    "Líneas verticales: rezagos 12, 24 y 36."),
                  x = "Rezago (meses)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title.position = "plot")
}
