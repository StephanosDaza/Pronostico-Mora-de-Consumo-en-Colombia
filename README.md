# Pronóstico de la mora en la cartera de consumo

Trabajo final de Series de Tiempo Univariadas (UNAL Medellín, 2026-II).
Esteban Daza Palomino, Juan David Garro Arboleda y Sara Jurado Giraldo.

## Bases de datos

| Base | Fuente | Identificador | Cobertura | Uso |
|---|---|---|---|---|
| Distribución de cartera por producto | Superintendencia Financiera, datos.gov.co | `rvii-eis8` | Mensual, ene-2015 a jul-2026 | Construcción del índice de mora de consumo |
| Tasa de interés bancario corriente | Superintendencia Financiera, datos.gov.co | `pare-7x5i` | Trimestral hasta ago-2017, mensual desde sep-2017 | Covariable (modalidad consumo y ordinario) |
| Calidad de cartera, histórico | Superintendencia Financiera | `icv_historia.xlsx` | Mensual, ene-2011 a dic-2023 | Validación del índice construido |
| Calidad de cartera incluido castigos, histórico | Superintendencia Financiera | `icv_cas_historia.xlsx` | Mensual, ene-2011 a dic-2023 | Efecto de diciembre con y sin castigos |

Las dos primeras se descargan por código desde la API de datos.gov.co. Los dos históricos de
calidad de cartera no tienen API; están en `datos/crudos/` y se descargaron de
<https://www.superfinanciera.gov.co/publicaciones/10082252>.

## Estructura

```
R/                    funciones propias
  descarga.R            descarga de datos.gov.co
  construccion.R        índice de mora de consumo y tasa mensual
  validacion.R          lectura de los históricos oficiales y validación
  caracteristicas.R     tendencia, fuerza estacional y relación con la tasa
  descriptivos.R        tabla por año y mes, perfil estacional y gráficas
  autocorrelacion.R     diferencias, ACF y PACF
  parametros.R          fecha de corte, horizonte y número de orígenes
  particion.R           partición en entrenamiento y evaluación
  transformacion.R      lambda de Box-Cox (Guerrero) y variabilidad por año
  integracion.R         diferencias candidatas y pruebas de raíz unitaria
  identificacion.R      tabla de ACF y PACF y búsqueda automática del SARIMA
  estimacion.R          estimación de los SARIMA, coeficientes, criterios y raíces
analisis/             scripts que se ejecutan, en orden
  01_construir_serie.R  serie, tasa, validación y características
  02_particion.R        paso 1 de la solución: muestra y fecha de corte
  03_transformacion.R   paso 2 de la solución: transformación
  04_integracion.R      paso 3 de la solución: orden de integración
  05_identificacion.R   paso 4 de la solución: identificación y modelos candidatos
  06_estimacion.R       paso 5 de la solución: estimación y selección
analisis_inicial.qmd  documento con el análisis exploratorio y la construcción del modelo (se renderiza a HTML)
datos/crudos/         archivos originales que no se descargan por código
datos/procesados/     salidas de los scripts (no se versionan)
figuras/              figuras de los scripts (no se versionan)
```

## Ejecución

Abrir `proyecto_mora.Rproj` en RStudio y, en la consola:

```r
install.packages(c("dplyr", "tidyr", "purrr", "readr", "readxl", "lubridate", "tibble", "ggplot2", "knitr", "rmarkdown",
                   "tsibble", "fable", "feasts", "fabletools", "urca", "forecast", "uroot"))
source("analisis/01_construir_serie.R", encoding = "UTF-8")
source("analisis/02_particion.R", encoding = "UTF-8")
source("analisis/03_transformacion.R", encoding = "UTF-8")
source("analisis/04_integracion.R", encoding = "UTF-8")
source("analisis/05_identificacion.R", encoding = "UTF-8")
source("analisis/06_estimacion.R", encoding = "UTF-8")
```

Después se abre `analisis_inicial.qmd` y se presiona **Render**. El documento lee los CSV de
`datos/procesados/` y las figuras de `figuras/modelo/`, así que hay que haber corrido antes todos los
scripts. Sale `analisis_inicial.html`, un solo archivo que se puede abrir en cualquier navegador.

El script 01 descarga los datos de la API (`usar_api <- TRUE`). Si la API no responde, se
pueden exportar los CSV desde el portal de datos.gov.co, guardarlos en `datos/crudos/` con los
nombres indicados en el script y cambiar a `usar_api <- FALSE`.

## Parámetros

Los únicos valores que se fijan a mano están en `R/parametros.R`: la fecha de corte (`FECHA_CORTE`),
el horizonte de pronóstico (`HORIZONTE`) y el número de orígenes de la validación (`N_ORIGENES`).
El último mes de entrenamiento se calcula a partir de ellos: fecha de corte menos
`N_ORIGENES + HORIZONTE` meses. Con los valores actuales (julio de 2026, 6 y 25), el
entrenamiento va hasta diciembre de 2023.

## Salidas

- `serie_mora.csv`: fecha, número de entidades, saldo, saldo vigente, saldo vencido e índice de mora (%).
- `tibc_mensual.csv`: tasa de interés bancario corriente de consumo, mes a mes.
- `validacion_icv.csv`: índice construido frente al indicador oficial, con y sin castigos (2015-2023).
- `caracteristicas_serie.csv`: resumen de la serie (rango, volatilidad, fuerza estacional, efecto de diciembre, caída por los alivios y rezago de la tasa).
- `serie_partida.csv`: fecha, índice de mora y muestra (entrenamiento o evaluación).
- `particion.csv`: fechas y meses de entrenamiento y evaluación, con el primer y último origen.
- `transformacion_lambda.csv`: lambda de Guerrero con el entrenamiento y con los datos anteriores a los alivios.
- `transformacion_anual.csv` y `transformacion_correlacion.csv`: media y desviación de cada año y su correlación.
- `integracion_resumen.csv`: varianza y ACF en los rezagos 1 y 12 de cada diferenciación candidata.
- `integracion_pruebas.csv`: KPSS y ADF sobre cada diferenciación.
- `integracion_sugeridas.csv`: diferencias regulares y estacionales que sugieren ndiffs y nsdiffs.
- `identificacion_acf_entrenamiento.csv` y `identificacion_acf_pre_alivios.csv`: ACF y PACF de la serie con diferencia regular.
- `identificacion_automatica.csv`: modelo que propone la búsqueda automática.
- `identificacion_candidatos.csv` e `identificacion_descartados.csv`: modelos candidatos para la estimación y modelos descartados, con su justificación.
- `estimacion_coeficientes.csv`: coeficientes de cada candidato con error estándar, z y valor p.
- `estimacion_comparacion.csv`: AIC, AICc, BIC, varianza, significancia y raíces de cada candidato.
- `estimacion_seleccion.csv`: modelos elegidos por menor AICc y por menor BIC entre los estacionarios e invertibles.

Las figuras de la construcción del modelo quedan en `figuras/modelo/`: partición, media y
variabilidad por año, serie original y transformada, diferenciaciones candidatas y ACF y PACF de la
serie con diferencia regular.
