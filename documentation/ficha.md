# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Embarazo infantil y adolescente en Ecuador: Una radiografía cuantitativa
- **Descripción del repositorio:** Artículo del Quantificador sobre embarazo infantil y adolescente.
- **Autoría:** Carolina Espinosa Espinoza
- **Fecha de publicación:** 2024-11-19
- **Artículo publicado:** https://elquantificador.org/post/salud/2024-11-19-embarazo-infantil-ecuador/
- **Categoría:** Salud (el sitio lo clasifica también en Valores y comportamiento)
- **Edición:** Septiembre-Diciembre 2024

## Estado

Publicado el 2024-11-19. El título completo del artículo en el sitio es
"¿Cuáles son las tasas de embarazo infantil y adolescente en Ecuador? Una
radiografía cuantitativa de un desafío persistente".

## Pregunta y alcance

El artículo pregunta cuáles son las tasas de embarazo infantil y adolescente en
Ecuador. Calcula tasas de nacidos vivos por cada mil mujeres de 10 a 14 años y
de 15 a 19 años, para el total nacional y por provincia, entre 2013 y 2022. La
unidad de análisis es el nacido vivo registrado según la edad de la madre; el
denominador son las proyecciones poblacionales del INEC, que llegan hasta 2019
y se repiten para 2020, 2021 y 2022.

## Requisitos

- R: no documentada en el repositorio (el README anterior pide R >= 4.0).
- Paquetes de R: dplyr, ggplot2, haven, knitr, RColorBrewer, readr, readxl,
  rmarkdown, tidyr. Los siete con `library()` en `code/embarazo_analysis.R`
  son los de la lista salvo knitr y rmarkdown, que usa `report/report.Rmd`.

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| Registro Estadístico de Nacidos Vivos y Defunciones Fetales (ENV), bases anuales | INEC | 2013 a 2022 | manual; https://www.ecuadorencifras.gob.ec/nacidos-vivos-y-defunciones-fetales/ | `code/embarazo_analysis.R` lee `data/ENV_2013.sav` a `data/ENV_2022.csv` |
| Proyecciones poblacionales, mujeres de 10 a 19 años por provincia | INEC | 2013 a 2019 (2020 a 2022 repiten 2019) | manual; [COMPLETAR: no se encontró el enlace oficial en el repositorio ni en el artículo] | `code/embarazo_analysis.R` lee `data/Proyecciones_poblacionales.xlsx` |

El repositorio no tiene scripts de descarga. Los archivos de `data/` se
descargaron a mano desde el portal del INEC y están incluidos en el
repositorio: 2013 a 2017 en formato `.sav` y 2018 a 2022 en formato `.csv`
(separador `;`).

## Salidas

- `figures/grafico1.png`: tasa de nacidos vivos de madres de 10 a 14 y de 15 a
  19 años por cada mil mujeres, 2013 a 2022.
- `figures/grafico2.png`: tasa de nacidos vivos de madres de 10 a 19 años por provincia en 2022 (las
  ocho provincias con mayor tasa).
- `code/embarazo_analysis.R` muestra los gráficos con `print()` y no los
  guarda; los dos PNG de `figures/` se exportaron aparte.
- `code/validate.R` comprueba que los diez archivos ENV tengan las mismas
  columnas y no escribe archivos.

## Cómo replicar en este repositorio

El repositorio no tiene `MASTER.R`. Los pasos que se deducen de los scripts y
del README anterior son:

1. Abre `embarazo-infantil.Rproj` en RStudio, de modo que la raíz del
   repositorio sea el directorio de trabajo (las rutas son relativas a `data/`).
2. Instala los paquetes de la sección Requisitos.
3. Verifica que `data/` contenga los diez archivos `ENV_2013` a `ENV_2022` y
   `Proyecciones_poblacionales.xlsx`; ya vienen en el repositorio. Opcional:
   ejecuta `code/validate.R`.
4. Ejecuta `code/embarazo_analysis.R`: lee las bases, filtra madres de menos de
   20 años, calcula las tasas con las proyecciones y construye los dos
   gráficos.
5. Para el texto, renderiza `report/report.Rmd`, que incluye
   `figures/grafico1.png` y `figures/grafico2.png`.

Discrepancias del README anterior: menciona `code/analysis.R`, que no existe
(el script es `code/embarazo_analysis.R`), y dice que `report.Rmd` lo invoca,
pero `report.Rmd` solo incluye las imágenes ya exportadas.

## README anterior

Texto del README de este repositorio antes de adoptar el README estándar,
conservado sin cambios de contenido para no perder información.

### Embarazo infantil y adolescente en Ecuador: Una radiografía cuantitativa

Este repositorio contiene el código y los archivos necesarios para reproducir el análisis del artículo de El Quantificador sobre las tasas de embarazo infantil y adolescente en Ecuador.

Fuente de los datos: [Registros Estadísticos de Nacidos Vivos y Defunciones Fetales del Instituto Ecuatoriano de Estadísticas y Censos](https://www.ecuadorencifras.gob.ec/nacidos-vivos-y-defunciones-fetales/).

#### Requisitos
- R (>= 4.0) (se recomienda usar RStudio).
- Paquetes de R que se instalarán automáticamente al ejecutar los scripts:
  ```r
  install.packages(c("readr", "haven", "tidyr", "dplyr", "readxl",
                     "RColorBrewer", "ggplot2", "rmarkdown"))
  ```
- Opcionalmente LaTeX para compilar el artículo en PDF.

#### Cómo reproducir el análisis

1. Clona o descarga este repositorio.
2. Instala los paquetes de R mencionados en los requisitos.
3. Ejecuta `rmarkdown::render("report/report.Rmd")` para generar `report.html`.
   El script `code/analysis.R` cargará todos los archivos de `data/` y generará los gráficos utilizados en el artículo.

Para más información, revisa el perfil de GitHub de la [autora](https://github.com/carolinaespinosa).
