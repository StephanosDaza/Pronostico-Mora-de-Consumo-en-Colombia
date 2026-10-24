# Pronóstico de la mora en la cartera de consumo

Trabajo final de Series de Tiempo Univariadas (UNAL Medellín, 2026-II).
Esteban Daza Palomino, Juan David Garro Arboleda y Sara Jurado Giraldo.

## Bases de datos

| Base | Fuente | Identificador | Cobertura | Uso |
|---|---|---|---|---|
| Distribución de cartera por producto | Superintendencia Financiera, datos.gov.co | `rvii-eis8` | Mensual, ene-2015 a jul-2026 | Serie principal: índice de mora total y por producto |
| Tasa de interés bancario corriente | Superintendencia Financiera, datos.gov.co | `pare-7x5i` | Trimestral hasta ago-2017, mensual desde sep-2017 | Covariable (modalidad consumo y ordinario) |
| Calidad de cartera, histórico | Superintendencia Financiera | `icv_historia.xlsx` | Mensual, ene-2011 a dic-2023 | Validación del índice construido |
| Calidad de cartera incluido castigos, histórico | Superintendencia Financiera | `icv_cas_historia.xlsx` | Mensual, ene-2011 a dic-2023 | Efecto de diciembre con y sin castigos |

Las dos primeras se descargan por código desde la API de datos.gov.co. Los dos históricos de
calidad de cartera no tienen API; están en `datos/crudos/` y se descargaron de
<https://www.superfinanciera.gov.co/publicaciones/10082252>.

## Estructura

```
R/                 funciones propias
  descarga.R         descarga de datos.gov.co
  construccion.R     índice de mora (total y por producto) y tasa mensual
  validacion.R       lectura de los históricos oficiales y validación
  comparacion.R      métricas para comparar las series
  individual.R       tabla, perfil estacional y gráficas de cada serie
  autocorrelacion.R  ACF y PACF
analisis_inicial.qmd  documento con el análisis inicial (se renderiza a HTML)
analisis/          scripts que se ejecutan, en orden
  01_construir_series.R     series, tasa, validación y comparación
  02_series_individuales.R  tabla y gráficas de cada serie
  03_autocorrelacion.R      ACF y PACF de cada serie
datos/crudos/      archivos originales que no se descargan por código
datos/procesados/  salidas de los scripts (no se versionan)
figuras/           figuras generadas (no se versionan)
```

## Cómo ejecutar

Abrir `proyecto_mora.Rproj` en RStudio y, en la consola:

```r
install.packages(c("dplyr", "tidyr", "purrr", "readr", "readxl", "lubridate", "tibble", "ggplot2", "knitr", "rmarkdown"))
source("analisis/01_construir_series.R", encoding = "UTF-8")
source("analisis/02_series_individuales.R", encoding = "UTF-8")
source("analisis/03_autocorrelacion.R", encoding = "UTF-8")
```

Después se abre `analisis_inicial.qmd` y se presiona **Render**. Sale `analisis_inicial.html`,
un solo archivo que se puede abrir en cualquier navegador. Necesita los CSV que deja el script 01.

El script 01 descarga los datos de la API (`usar_api <- TRUE`). Si la API no responde, se
pueden exportar los CSV desde el portal de datos.gov.co, guardarlos en `datos/crudos/` con los
nombres indicados en el script y cambiar a `usar_api <- FALSE`. La fecha de corte se cambia en
`fecha_corte`.

## Pendiente

- Declarar el entorno con `renv` (`renv::init()`) y fijar las versiones de los paquetes.
