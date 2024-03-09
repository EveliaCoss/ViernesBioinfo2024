######
# Script: Manipulacion y limpieza de los dataframes de superheroes (Parte 2)
# Manipulación de datos usando dplyr
# Autor: Evelia Coss
# Fecha: 15 de marzo 2024
# A partir de los datos corregidos a partir del script "MarvelvsDCComics_limpieza_script.R" visto en la clase 6,
# partiremos del output almacenado en "Info_data.RData" que debe encontrarse en la carpeta "data/".
# Argumentos:
# Los pasos 1 al 8 se encuentran en el script "MarvelvsDCComics_limpieza_script.R". Continuaremos con el Paso 9.
# - Input:  "Info_data.RData"
# - Output: 
#   - marvelDcInfo: dataframe sin duplicados, conformado de 4 columnas ("Name", "Gender", "Race", "Publisher")
#   - infoPowers y  infoStats:  dataframes solo con la modificacion en la columna "Name"
#   Las 3 variables se almacenaron en "Info_data.RData"
#######

# --- Cargar paquetes ----

library(dplyr)     # Manipulación de datos
library(tidyr)     # Manipulación de datos
library(tidyverse) # Manipulación de datos

library(reshape2)  # Transformación de datos
library(cowplot)   # Visualización grafica
library(ggplot2)   # Generar varios graficos en una misma figura

# ---- Importar datos -----
indir = "C:/Users/ecoss/OneDrive - CINVESTAV/Documentos/Posdoc_LIIGH/VieRnesBioinfo/ViernesBioinfo_2024/Presentaciones/data/"
outdir = "C:/Users/ecoss/OneDrive - CINVESTAV/Documentos/Posdoc_LIIGH/VieRnesBioinfo/ViernesBioinfo_2024/Presentaciones/"

# Cargar las variables infoPowers, infoStats, marvelDcInfo
load(file =  paste0(indir, "Info_data.RData"))

# ---- Continuamos con la manipulacion de datos -----

# Paso 9. Unir dataframes con base en una columna en común (Join Datasets)

# Primera union
marvelDcStatsInfo <- left_join(marvelDcInfo, infoStats, by = "Name")
head(marvelDcStatsInfo)[1:5]

# Segunda union
fullMarvelDc <- left_join(marvelDcStatsInfo, infoPowers, by = "Name")
head(fullMarvelDc)[1:5]

# Paso 10. Cambiar formatos en algunas columnas
fullMarvelDc$Name <- as.factor(fullMarvelDc$Name)
fullMarvelDc$Alignment <- as.factor(fullMarvelDc$Alignment)

# Verificar dimensiones
dim(fullMarvelDc)

# Verificamos los formatos con `class()`
class(fullMarvelDc$Name)
class(fullMarvelDc$Gender)
class(fullMarvelDc$Race)
class(fullMarvelDc$Publisher)
class(fullMarvelDc$Alignment)

# Paso 11. Transformar en una sola columna los poderes usando `melt()`

# Convertir en formato largo las todas las demas columnas, excepto las almacenadas en id.

marvelDc <- melt(fullMarvelDc, id = c("Name", "Gender", "Race", "Publisher", "Alignment", "Intelligence.x", 
                                      "Strength", "Speed", "Durability.x", "Power", "Combat", "Total"))

# Verificar los cambios
str(marvelDc)
head(marvelDc, 3)
dim(marvelDc)

# Paso 12. Renombrar la columnas
colnames(marvelDc)[colnames(marvelDc) == "variable"] <- "SuperPower" 
colnames(marvelDc)[colnames(marvelDc) == "Intelligence.x"] <- "Intelligence" 
colnames(marvelDc)[colnames(marvelDc) == "Durability.x"] <- "Durability" 

# Cambiamos el formato de las columnas
marvelDc$Name <- as.factor(marvelDc$Name)
marvelDc$Gender <- as.factor(marvelDc$Gender)
marvelDc$Race <- as.factor(marvelDc$Race)
marvelDc$Publisher <- as.factor(marvelDc$Publisher)
marvelDc$Alignment <- as.factor(marvelDc$Alignment)
marvelDc$SuperPower <- as.factor(marvelDc$SuperPower)

# Verificar el formato de las columnas
class(marvelDc$Name)
class(marvelDc$Gender)
class(marvelDc$Race)
class(marvelDc$Publisher)
class(marvelDc$Alignment)
class(marvelDc$SuperPower)

# Paso 13. Selección de habilidades con TRUE
marvelDc <- marvelDc %>%
  filter(value == "True") %>%
  select(-value) #eliminar columna

head(marvelDc)

# Paso 14. Renombrar el formato de Alignment

# Opcion A - Función `if_else()`
marvelDc_edited <- marvelDc %>% mutate(Group = 
                                         if_else(Alignment == "good", "hero", # Primer if_else
                                                 if_else(Alignment == "bad","villain", "neutral"))) # Segundo if_else



# Opcion B - Funcion `case_when()`
marvelDc_edited <- marvelDc %>% mutate(Group = 
                                         case_when(Alignment == "good" ~ "hero", 
                                                   Alignment == "bad" ~ "villain", 
                                                   TRUE ~ "neutral"))


# Guardar output
save(marvelDc_edited, file = "Presentaciones/data/marvelDc_edited.RData")














