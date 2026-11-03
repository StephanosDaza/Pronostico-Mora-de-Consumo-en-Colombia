# Parámetros del trabajo
# Son los únicos valores que se fijan a mano; el resto de fechas se calcula a partir de ellos.
# Para correr con otros valores sin editar este archivo:
#   options(mora.fecha_corte = "2026-01-31", mora.horizonte = 6, mora.n_origenes = 25)

FECHA_CORTE <- as.Date(getOption("mora.fecha_corte", "2026-07-31"))  # último dato usado
HORIZONTE   <- getOption("mora.horizonte", 6)                         # meses a pronosticar
N_ORIGENES  <- getOption("mora.n_origenes", 25)                       # orígenes de la validación
