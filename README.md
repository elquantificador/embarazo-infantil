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


## Licencias y cómo citar

- **Código:** [MIT](LICENSE).
- **Texto, figuras y tablas:** [CC BY-NC 4.0](LICENSE-content).
- **Datos de terceros:** conservan los términos de su fuente
  ([aviso](DATA-NOTICE.md)).
- **Nombre y logotipos:** todos los derechos reservados.

Las cifras y resultados pueden citarse libremente. Menciona a los autores y a
El Quantificador (LIDE) y enlaza la publicación original. Los usos comerciales
de gráficos, tablas o texto requieren autorización previa:
elquantificador@laboratoriolide.org. Ver también `CITATION.cff`.
