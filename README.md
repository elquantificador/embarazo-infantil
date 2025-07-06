# ¿Cuáles son las tasas de embarazo infantil y adolescente en Ecuador? Una Radiografía Cuantitativa de un Desafío Persistente

Repositorio para el artículo de El Quantificador sobre embarazo infantil y adolescente.

Fuente de los datos: [Registros Estadísticos de Nacidos Vivos y Defunciones Fetales del Instituto Ecuatoriano de Estadísticas y Censos](https://www.ecuadorencifras.gob.ec/nacidos-vivos-y-defunciones-fetales/).

## Reproducir el análisis

1. Instale R (>= 4.0) y los siguientes paquetes:
   ```r
   install.packages(c("readr", "haven", "tidyr", "dplyr", "readxl",
                      "RColorBrewer", "ggplot2", "rmarkdown"))
   ```
2. Ejecute `rmarkdown::render("report.Rmd")` para generar `report.html`.
   El script `code/embarazo_analysis.R` cargará todos los archivos de `data/` y generará los gráficos utilizados en el artículo.

## Estructura

- `data/` contiene los archivos originales suministrados por el INEC.
- `code/embarazo_analysis.R` realiza la limpieza y el análisis descriptivo.
- `report.Rmd` es la versión editada para su publicación en línea.

Para más detalles sobre las fuentes consulte `DATA.md`.
