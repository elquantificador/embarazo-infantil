# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Embarazo infantil y adolescente en Ecuador: Una radiografía cuantitativa
- **Descripción del repositorio:** Artículo del Quantificador sobre embarazo infantil y adolescente.
- **Autoría:** [COMPLETAR: nombres, igual que en el sitio]
- **Fecha de publicación:** [COMPLETAR: AAAA-MM-DD]
- **Artículo publicado:** [COMPLETAR: enlace a elquantificador.org]
- **Categoría:** [COMPLETAR: Economía, Política, Salud, Educación o Valores y
  comportamiento]
- **Edición:** [COMPLETAR: por ejemplo, Julio-Diciembre 2026]

## Estado

[COMPLETAR: en preparación, en revisión editorial o publicado. Anota qué falta
para cerrar el artículo.]

## Pregunta y alcance

[COMPLETAR: dos o tres oraciones con la pregunta, el periodo, la cobertura
geográfica y la unidad de análisis.]

## Requisitos

- R [COMPLETAR: versión, por ejemplo 4.5.x].
- Paquetes de R: [COMPLETAR: lista en orden alfabético].

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| [COMPLETAR] | [COMPLETAR] | [COMPLETAR] | [COMPLETAR: descarga automática o manual] | `code/cleaning/00_download_fuente.R` |

Si alguna fuente no permite descarga automática, explica aquí cómo obtenerla y
dónde colocarla en `data/raw/`.

## Salidas

- `outputs/tables/[COMPLETAR].csv`: [COMPLETAR: qué contiene].
- `outputs/graphs/[COMPLETAR].png`: [COMPLETAR: qué muestra].
- `article/images/graficoN.png`: copias numeradas según el orden del texto.

## Cómo replicar en este repositorio

Este repositorio conserva sus instrucciones originales en la sección
"README anterior". Si todavía no tiene `MASTER.R`, sigue esas instrucciones.
Cuando se migre a la estructura estándar, reemplaza esta sección por los pasos
de `MASTER.R`.

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
