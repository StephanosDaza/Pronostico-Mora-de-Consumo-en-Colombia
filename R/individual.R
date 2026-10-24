# Tablas y gráficas por serie

MESES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# nombre de archivo sin tildes ni espacios
nombre_archivo <- function(x) {
  x <- chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN", x)
  gsub("[^a-z0-9]+", "_", tolower(x))
}

# tabla año x mes
tabla_anio_mes <- function(d) {
  d |>
    dplyr::mutate(anio = lubridate::year(fecha),
                  mes = factor(MESES[lubridate::month(fecha)], levels = MESES)) |>
    dplyr::select(anio, mes, mora_pct) |>
    dplyr::mutate(mora_pct = round(mora_pct, 2)) |>
    tidyr::pivot_wider(names_from = mes, values_from = mora_pct) |>
    dplyr::arrange(anio)
}

# resumen de una serie
resumen_serie <- function(d) {
  d <- dplyr::arrange(d, fecha)
  i_min <- which.min(d$mora_pct)
  i_max <- which.max(d$mora_pct)
  tibble::tibble(
    meses        = nrow(d),
    desde        = min(d$fecha),
    hasta        = max(d$fecha),
    media_pct    = mean(d$mora_pct),
    minimo_pct   = d$mora_pct[i_min],
    fecha_minimo = d$fecha[i_min],
    maximo_pct   = d$mora_pct[i_max],
    fecha_maximo = d$fecha[i_max],
    ultimo_pct   = d$mora_pct[nrow(d)],
    saldo_ultimo_billones = d$saldo[nrow(d)] / 1e12,
    entidades_ultimo      = d$n_entidades[nrow(d)]
  )
}

# perfil estacional: desviación media de cada mes frente a la tendencia, sin 2020-2021
perfil_mensual <- function(d) {
  d <- dplyr::arrange(d, fecha)
  d |>
    dplyr::mutate(desviacion = mora_pct - tendencia_2x12(mora_pct),
                  anio = lubridate::year(fecha),
                  mes = lubridate::month(fecha)) |>
    dplyr::filter(!anio %in% 2020:2021, !is.na(desviacion)) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(desviacion_media_pp = mean(desviacion), anios = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(mes = factor(MESES[mes], levels = MESES))
}

coma <- function(x) format(x, decimal.mark = ",", trim = TRUE)

# gráfica de la serie (puntos en diciembre, franja de alivios)
grafico_serie <- function(d, nombre) {
  diciembres <- dplyr::filter(d, lubridate::month(fecha) == 12)
  ggplot2::ggplot(d, ggplot2::aes(fecha, mora_pct)) +
    ggplot2::annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
                      ymin = -Inf, ymax = Inf, fill = "grey92") +
    ggplot2::geom_line(colour = "#2a78d6", linewidth = 0.8) +
    ggplot2::geom_point(data = diciembres, colour = "#2a78d6", size = 2) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(coma(x), " %")) +
    ggplot2::labs(title = paste("Índice de mora:", nombre),
                  subtitle = "Porcentaje del saldo con más de 30 días de atraso. Puntos: diciembres. Franja gris: alivios (mar-2020 a jun-2021).",
                  x = NULL, y = NULL,
                  caption = "Fuente: Superintendencia Financiera (datos.gov.co, rvii-eis8). Cálculos propios.") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title.position = "plot")
}

# gráfica del perfil estacional
grafico_perfil <- function(perfil, nombre) {
  ggplot2::ggplot(perfil, ggplot2::aes(mes, desviacion_media_pp,
                                       fill = desviacion_media_pp < 0)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#2a78d6", `FALSE` = "#eb6834"), guide = "none") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(coma(x), " pp")) +
    ggplot2::labs(title = paste("Perfil estacional:", nombre),
                  subtitle = "Desviación media de cada mes frente a la tendencia (media móvil 2x12), sin 2020 y 2021.\nAzul: por debajo de la tendencia. Naranja: por encima.",
                  x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(), plot.title.position = "plot")
}

# todo lo de una serie en pantalla, p. ej. ver_serie(series, "Libranza")
ver_serie <- function(series, nombre) {
  d <- dplyr::filter(series, serie == nombre)
  if (nrow(d) == 0) stop("No existe la serie: ", nombre)
  cat("\n==============================\n", nombre, "\n==============================\n")
  print(resumen_serie(d), width = Inf)
  print(tabla_anio_mes(d), n = Inf, width = Inf)
  perfil <- perfil_mensual(d)
  print(perfil, n = 12)
  print(grafico_serie(d, nombre))
  print(grafico_perfil(perfil, nombre))
  invisible(list(datos = d, tabla = tabla_anio_mes(d), perfil = perfil))
}
