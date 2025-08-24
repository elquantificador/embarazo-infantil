# Embarazo infantil y adolescente en Ecuador: Una radiografía cuantitativa

Este repositorio contiene el código y los archivos necesarios para reproducir el análisis del artículo de El Quantificador sobre las tasas de embarazo infantil y adolescente en Ecuador.

Fuente de los datos: [Registros Estadísticos de Nacidos Vivos y Defunciones Fetales del Instituto Ecuatoriano de Estadísticas y Censos](https://www.ecuadorencifras.gob.ec/nacidos-vivos-y-defunciones-fetales/).

## Requisitos
- R (>= 4.0) (se recomienda usar RStudio).
- Paquetes de R que se instalarán automáticamente al ejecutar los scripts:
  ```r
  install.packages(c("readr", "haven", "tidyr", "dplyr", "readxl",
                     "RColorBrewer", "ggplot2", "rmarkdown"))
  ```
- Opcionalmente LaTeX para compilar el artículo en PDF.

## Cómo reproducir el análisis

1. Clona o descarga este repositorio.
2. Instala los paquetes de R mencionados en los requisitos.
3. Ejecuta `rmarkdown::render("report/report.Rmd")` para generar `report.html`.
   El script `code/analysis.R` cargará todos los archivos de `data/` y generará los gráficos utilizados en el artículo.

Para más información, revisa el perfil de GitHub de la [autora](https://github.com/carolinaespinosa).
