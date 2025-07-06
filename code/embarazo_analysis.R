# Articulo El Quantificador Embarazo en niñas y adolescentes en Ecuador
# Autor: Carolina Espinosa

# Preliminares ---------------------------------------------------------------

# Carga de paquetes

library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(ggplot2)

# Carga de base de datos ------------------------------------------------------

read_env <- function(year) {
  path <- file.path("data", sprintf("ENV_%s", year))
  if (year >= 2018) {
    read_delim(paste0(path, ".csv"), delim = ";",
               locale = locale(encoding = "UTF-8"),
               col_types = cols(fecha_mad = col_character(),
                                fecha_insc = col_character()),
               trim_ws = TRUE)
  } else {
    read_sav(paste0(path, ".sav"))
  }
}

years <- 2013:2022
env_list <- lapply(years, read_env)
names(env_list) <- paste0("ENV_", years)
list2env(env_list, envir = globalenv())

#Proyecciones poblacionales mujeres de 10 a 19 años - nivel provincial 2010-2019

#Proyecciones poblacionales mujeres de 10 a 19 años - nivel provincial 2010-2019

Proyecciones_poblacionales <- read_excel("data/Proyecciones_poblacionales.xlsx")

# Limpieza de datos -----------------------------------------------------------

#LIMPIEZA: VARIABLES ANIO_NAC, PROV_NAC, EDAD_MAD

pos_map <- list(
  `2022` = c(9,21,32),
  `2021` = c(9,21,32),
  `2020` = c(9,21,32),
  `2019` = c(9,21,32),
  `2018` = c(10,21,32),
  `2017` = c(9,21,32),
  `2016` = c(9,21,32),
  `2015` = c(9,21,32),
  `2014` = c(15,21,32),
  `2013` = c(16,23,32)
)

for (y in names(pos_map)) {
  df <- get(paste0("ENV_", y))
  assign(paste0("ENV_", y, "_LIMPIA"), df[, pos_map[[y]]])
}

# Desetiquetar la columna prov_nac de 2017
ENV_2017_LIMPIA$prov_nac <- as.character(ENV_2017_LIMPIA$prov_nac)

# Definir un diccionario de mapeo de números a palabras

mapeo <- c("01" = "Azuay",
           "02" = "Bolívar",
           "03" = "Cañar",
           "04" = "Carchi",
           "05" = "Cotopaxi",
           "06" = "Chimborazo",
           "07" = "El Oro",
           "08" = "Esmeraldas",
           "09" = "Guayas",
           "10" = "Imbabura",
           "11" = "Loja",
           "12" = "Los Ríos",
           "13" = "Manabí",
           "14" = "Morona Santiago",
           "15" = "Napo",
           "16" = "Pastaza",
           "17" = "Pichincha",
           "18" = "Tungurahua",
           "19" = "Zamora Chinchipe",
           "20" = "Galápagos",
           "21" = "Sucumbíos",
           "22" = "Orellana",
           "23" = "Santo Domingo de los Tsáchilas",
           "24" = "Santa Elena",
           "88" = "Exterior",
           "90" = "Zonas No Delimitadas")

# Aplicar la transformación usando case_when para los años 2013-2017
for (y in 2013:2017) {
  df_name <- paste0("ENV_", y, "_LIMPIA")
  df <- get(df_name) %>%
    mutate(prov_nac = case_when(
      prov_nac %in% names(mapeo) ~ mapeo[prov_nac],
      TRUE ~ prov_nac
    ))
  assign(df_name, df)
}

# FILTRAR EDAD < 19 2013-2022
for (y in years) {
  df_name <- paste0("ENV_", y, "_LIMPIA_filtr")
  df <- get(paste0("ENV_", y, "_LIMPIA")) %>%
    filter(edad_mad < 20)
  df$edad_mad <- as.character(df$edad_mad)
  df$prov_nac <- as.character(df$prov_nac)
  assign(df_name, df)
}

datos_combinados <- bind_rows(lapply(years, function(y) {
  get(paste0("ENV_", y, "_LIMPIA_filtr"))
}))
  
# Filtrar datos combinados año < 2012
datos_combinados <- datos_combinados %>%
  filter(anio_nac > 2012 )

# Limpieza variables Proyecciones poblacionales mujeres de 10 a 19 años de 2013 a 2019

Proyecciones_poblacionales2013.2019  <- Proyecciones_poblacionales[,c(1,5,6,7,8,9,10,11)]

#Cambio de nombre variable en proyecciones_poblacionales2013.2019 de "Provincia" a "prov_nac"

colnames(Proyecciones_poblacionales2013.2019)[colnames(Proyecciones_poblacionales2013.2019) == "Provincia"] <- "prov_nac"

# Crear tres nuevos años -2020,2021,2022- con la misma proyección que en 2019

Proyecciones_poblacionales2013.2022  <-  
  Proyecciones_poblacionales2013.2019 %>%
  mutate(`2020` = `2019`, `2021` = `2019`, `2022` = `2019`)

PROYECCIONES_long <- Proyecciones_poblacionales2013.2022 %>%
  pivot_longer(cols = -prov_nac, names_to = "anio_nac", values_to = "num_proy")

#TASA DE 10 A 19 AÑOS POR PROVINCIA 2013-2022
embarazos_filtrados.10.19 <- datos_combinados %>%
  filter(edad_mad >= 10 & edad_mad <= 19)

#Agrupa por provincia y año + número de registros
nacimientos_por_provincia10.19 <- embarazos_filtrados.10.19 %>%
  group_by(prov_nac, anio_nac) %>%
  summarize(num_nacimientos = n())

# Creación base de datos con provincia, año, núm nacidos por año y núm proyección madres 10 a 19 años 

tasa_embarazo_prov_10.19 <- merge(nacimientos_por_provincia10.19, PROYECCIONES_long, by.x = c("prov_nac", "anio_nac"), 
                                  by.y = c("prov_nac", "anio_nac"), all.x = TRUE)

tasa_embarazo_prov_10.19$Tasa <- (tasa_embarazo_prov_10.19$num_nacimientos / tasa_embarazo_prov_10.19$num_proy) * 1000 # Por cada 1,000 niñas de 10 a 19 años 

#TASA DE 10 A 14 AÑOS POR PROVINCIA 2013-2022

#Filtro para obtener solo los registros de madres de 10 a 14 años

embarazos_filtrados.10.14 <- datos_combinados %>%
  filter(edad_mad >= 10 & edad_mad <= 14)

#Agrupa por provincia y año + número de registros
nacimientos_por_provincia10.14 <- embarazos_filtrados.10.14 %>%
  group_by(prov_nac, anio_nac) %>%
  summarize(num_nacimientos = n())

# Creación base de datos con provincia, año, núm nacidos por año y núm proyección madres 10 a 14 años 

tasa_embarazo_prov_10.14 <- merge(nacimientos_por_provincia10.14, PROYECCIONES_long, by.x = c("prov_nac", "anio_nac"), by.y = c("prov_nac", "anio_nac"), all.x = TRUE)
tasa_embarazo_prov_10.14$Tasa <- (tasa_embarazo_prov_10.14$num_nacimientos / tasa_embarazo_prov_10.14$num_proy) * 1000 # Por cada 1,000 niñas de 10 a 14 años 

#TASA DE 15 A 19 AÑOS POR PROVINCIA 2013-2022

#Filtro para obtener solo los registros de madres de 15 a 19 años

embarazos_filtrados.15.19 <- datos_combinados %>%
  filter(edad_mad >= 15 & edad_mad <= 19)

#Agrupa por provincia y año + número de registros
nacimientos_por_provincia15.19 <- embarazos_filtrados.15.19 %>%
  group_by(prov_nac, anio_nac) %>%
  summarize(num_nacimientos = n())

# Creación base de datos con provincia, año, núm nacidos por año y núm proyección madres 10 a 14 años 

tasa_embarazo_prov_15.19 <- merge(nacimientos_por_provincia15.19, PROYECCIONES_long, by.x = c("prov_nac", "anio_nac"), by.y = c("prov_nac", "anio_nac"), all.x = TRUE)
tasa_embarazo_prov_15.19$Tasa <- (tasa_embarazo_prov_15.19$num_nacimientos / tasa_embarazo_prov_15.19$num_proy) * 1000 # Por cada 1,000 niñas de 10 a 14 años 

#############################################

#Base de datos proyección poblacional por año 
pr_total_por_anio <- PROYECCIONES_long %>%
  group_by(anio_nac) %>%
  summarize(total_poblacion = sum(num_proy))

#TASA DE EMBARAZO POR AÑO (2013-2022) 10 a 14 años 

#Total de embarazos por año 10 a 14 años 

total_embarazos_por_año.10.14 <- aggregate(. ~ anio_nac, data = embarazos_filtrados.10.14, FUN = length)
total_embarazos_por_año.10.14 <- total_embarazos_por_año.10.14[, -which(names(total_embarazos_por_año.10.14) == "prov_nac")]

# Combinación de dos bases de datos (total_embarazos_por_año.15.14 y pr_total_por_anio) por la columna "Anio nac"
tasatotal10.14 <- merge(total_embarazos_por_año.10.14, pr_total_por_anio, by = "anio_nac", all = TRUE)

# Tasa por cada 1000 niñas 10-14
tasatotal10.14$Tasa_Embarazos <- tasatotal10.14$edad_mad/ tasatotal10.14$total_poblacion * 1000

tasatotal10.14 <- tasatotal10.14[, -which(names(tasatotal10.14) == "edad_mad")]
tasatotal10.14 <- tasatotal10.14[, -which(names(tasatotal10.14) == "total_poblacion")]

#Crear gráfico 

# Convierte la columna "Anio_nac" en un factor
tasatotal10.14$anio_nac <- factor(tasatotal10.14$anio_nac)

# GRÁFICO LINEAL niñas 10 a 14 años 

ggplot(tasatotal10.14, aes(x = anio_nac, y = Tasa_Embarazos)) +
  geom_path(aes(group = 1), color = "#647A8F", size = 1) +  # Especificar group = 1
  geom_point(color = "#647A8F", size = 3) +
  labs(title = "Tasa de Madres de 10 a 14 años a lo largo del tiempo",
       x = "",
       y = "Tasa de Nacidos vivos \n(por cada mil adolescentes habitantes en Ecuador)", caption = "Nota: La tasa representa el número de nacimientos vivos por cada mil niñas habitantes \nen Ecuador. Fuente: Instituto Nacional de Estadísticas y Censos (INEC).") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  # Centrar y ajustar estilo del título
    axis.title.y = element_text(size = 7, margin = margin(r = 20)),  # Ajustar espacio y tamaño del eje y
    plot.caption = element_text(hjust = 0, face = "italic"),  # Añadir nota en cursiva en la parte inferior
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank()   # Eliminar la cuadrícula menor
  ) 

#TASA DE EMBARAZO POR AÑO (2013 A 2022) NIÑAS de 15 a 19 años 
#Total de embarazos por año 15 a 19 años

total_embarazos_por_año.15.19 <- aggregate(. ~ anio_nac, data = embarazos_filtrados.15.19, FUN = length)
total_embarazos_por_año.10.19 <- aggregate(. ~ anio_nac, data = embarazos_filtrados.10.19, FUN = length)
total_embarazos_por_año.15.19 <- total_embarazos_por_año.15.19[, -which(names(total_embarazos_por_año.15.19) == "prov_nac")]

# Combinación de dos bases de datos (total_embarazos_por_año.15.14 y pr_total_por_anio) por la columna "Anio nac"
tasatotal15.19 <- merge(total_embarazos_por_año.15.19, pr_total_por_anio, by = "anio_nac", all = TRUE)

# Tasa por cada 1000 niñas 15-19
tasatotal15.19$Tasa_Embarazos <- tasatotal15.19$edad_mad/ tasatotal15.19$total_poblacion * 1000

tasatotal15.19 <- tasatotal15.19[, -which(names(tasatotal15.19) == "edad_mad")]
tasatotal15.19 <- tasatotal15.19[, -which(names(tasatotal15.19) == "total_poblacion")]

#Crear gráfico 

# Convierte la columna "Anio_nac" en un factor
tasatotal15.19$anio_nac <- factor(tasatotal15.19$anio_nac)

#GRáfico

# Gráfico lineal niñas 15 a 19 años 

ggplot(tasatotal15.19, aes(x = anio_nac, y = Tasa_Embarazos)) +
  geom_path(aes(group = 1), color = "#647A8F", size = 1) +
  geom_point(color = "#647A8F", size = 3) +
  labs(title = "Tasa de Madres de 15 a 19 años a lo largo del tiempo",
       x = "",
       y = "Tasa de Nacidos vivos \n(por cada mil adolescentes habitantes en Ecuador)", caption = "Nota: La tasa representa el número de nacimientos vivos por cada mil adolescentes habitantes \nen Ecuador. Fuente: Instituto Nacional de Estadísticas y Censos (INEC).") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  # Centrar y ajustar estilo del título
    axis.title.y = element_text(size = 7, margin = margin(r = 20)),  # Ajustar espacio y tamaño del eje y
    plot.caption = element_text(hjust = 0, face = "italic"),  # Añadir nota en cursiva en la parte inferior
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank()   # Eliminar la cuadrícula menor
  ) 


#Tasa por año 

# Combinación de dos bases de datos (total_embarazos_por_año.15.14 y pr_total_por_anio) por la columna "Anio nac"
tasatotal10.19 <- merge(total_embarazos_por_año.10.19, pr_total_por_anio, by = "anio_nac", all = TRUE)

# Tasa por cada 1000 niñas 10-19
tasatotal10.19$Tasa_Embarazos <- (tasatotal10.19$edad_mad/ tasatotal10.19$total_poblacion) * 1000

# Convierte la columna "Anio_nac" en un factor
tasatotal10.19$anio_nac <- factor(tasatotal10.19$anio_nac)

#Gráfico lineal dos grupos etarios 

# Combina los conjuntos de datos y agrega una variable para distinguir las tasas
tasatotal10.14$Grupo <- "10-14 años"
tasatotal15.19$Grupo <- "15-19 años"
tasas_combinadas <- dplyr::bind_rows(tasatotal10.14, tasatotal15.19)


Gráfico_1= ggplot(tasas_combinadas, aes(x = anio_nac, y = Tasa_Embarazos, group = Grupo, color = Grupo)) +
  geom_path(size = 1) +
  geom_point(size = 3) +
  labs(title = "Tasa de Madres de 10 a 19 años a lo largo del tiempo",
       x = "",
       y = "Tasa de Nacidos vivos \n(por cada mil niñas y adolescentes habitantes en Ecuador)",
       caption = "Fuente: Instituto Nacional de Estadísticas y Censos (INEC).") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(size = 7, margin = margin(r = 20)),
    plot.caption = element_text(hjust = 0, face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~Grupo, scales = "free_y", ncol = 1)

print(Gráfico_1)

# Datos para el texto --------------------------------------------------------

#Porcentaje de reducción 

# Definir las tasas de 2013 y 2022
tasa_2013 <- 80.19
tasa_2022 <- 48.46

# Calcular la reducción absoluta
reduccion_absoluta <- tasa_2013 - tasa_2022

# Calcular el porcentaje de reducción
porcentaje_reduccion.15.19 <- (reduccion_absoluta / tasa_2013) * 100

# SOLO AÑO 2022 ---------------------------------------------------------------

# Estimación diaria de Embarazo infantil y adolescente niñas de 10 a 14 años en Ecuador 2022

año_2022 <- 2022
edad_min <- 10
edad_max <- 14

niñas10.14_2022 <- subset(datos_combinados, anio_nac == año_2022 & edad_mad >= edad_min & edad_mad <= edad_max)

total_embarazos_niñas10.14_2022 <- nrow(niñas10.14_2022)

Estimación_diaria_2022_niñas10.14_2022 = total_embarazos_niñas10.14_2022 / 365

# Estimación diaria de Embarazo infantil y adolescente niñas de 15 a 19 años en Ecuador 2022

año_2022 <- 2022
edad_min2 <- 15
edad_max2 <- 19

niñas15.19_2022 <- subset(datos_combinados, anio_nac == año_2022 & edad_mad >= edad_min2 & edad_mad <= edad_max2)

total_embarazos_niñas15.19_2022 <- nrow(niñas15.19_2022)

Estimación_diaria_2022_niñas15.19_2022 = total_embarazos_niñas15.19_2022 / 365

# Agrupar prov + año + edad mad 2022 

Por_prov_2022.filtr <- ENV_2022_LIMPIA_filtr %>%
  filter(anio_nac == 2022)

colores <- brewer.pal(5, "Set1")

Por_prov_2022 <- Por_prov_2022.filtr%>%
  group_by(prov_nac, anio_nac) %>%
  summarize(num_nacimientos = n())

# Tasa embarazo infantil por provincia solo del 2022 
tasa_embarazo_2022_prov <- tasa_embarazo_prov_10.19 %>%
  select(prov_nac, anio_nac, Tasa)

tasa_embarazo_2022_prov <- tasa_embarazo_2022_prov %>%
  filter(anio_nac == 2022)

tasa_embarazo_2022_prov <- tasa_embarazo_2022_prov %>%
  arrange(desc(Tasa))

#Crear gráfico de barras 
# Gráfico de barras horizontales - Por provinica 2022

Gráfico_2= ggplot(data = head(tasa_embarazo_2022_prov, 8), aes(x = reorder(prov_nac, desc(-Tasa)), y = Tasa)) +
  geom_col(width = 0.8, fill = "#647A8F", color = "black") +
  geom_text(aes(label = sprintf("%.2f", Tasa)),
            color = "black", size = 3, hjust = -0.1) +
  labs(title = "Tasa de natalidad de Madres Niñas y Adolescentes por Provincia",
       x = "",
       y = "") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),  
    plot.title = element_text(size = 10, hjust = 0, margin = margin(t = unit(1, "lines"))),  # Ajustar el margen superior del título
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula principal
    panel.grid.minor = element_blank(),  # Eliminar la cuadrícula secundaria
    panel.background = element_blank(),  # Eliminar el fondo gris
    plot.background = element_blank()    # Ajustar el margen inferior para dejar espacio para el caption
  ) +
  coord_flip(ylim = c(60, 100)) +
  theme(plot.caption = element_text(hjust = 0, color = "black", face = "italic", margin = margin(t = unit(1, "lines")))) +
  labs(caption = "Nota: La tasa representa el número de nacimientos vivos por cada mil niñas \ny adolescentes habitantes en cada provincia del Ecuador. \nFuente: Instituto Nacional de Estadísticas y Censos (INEC).")

print(Gráfico_2)


