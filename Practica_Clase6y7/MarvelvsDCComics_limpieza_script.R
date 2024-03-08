######
# Script de clase "Intro a R"
# Evelia Coss
# 8 de marzo 2024
#######

# --- Importar datos ----

indir = "C:/Users/ecoss/OneDrive - CINVESTAV/Documentos/Posdoc_LIIGH/VieRnesBioinfo/ViernesBioinfo_2024/Presentaciones/data/"
outdir = "C:/Users/ecoss/OneDrive - CINVESTAV/Documentos/Posdoc_LIIGH/VieRnesBioinfo/ViernesBioinfo_2024/Presentaciones/"

infoCharacters <- read.csv(paste0(indir,"heroesInformation.csv"), na.strings = c("-", "-99")) 
# La opción na.string nos permite sustituir valores - y -99 por NA
infoPowers <- read.csv(paste0(indir,"superHeroPowers.csv"))
infoStats <- read.csv(paste0(indir,"charactersStats.csv"), na.strings = "")

# --- Manipulacion de datos ----

# Renombrar la columna **Name** en todos los dataframe
colnames(infoCharacters)[colnames(infoCharacters) == "name"] <- "Name"
colnames(infoPowers)[colnames(infoPowers) == "hero_names"] <- "Name"

# Seleccionar SOLO los datos de Marvel Comics y DC Comics
unique(infoCharacters$Publisher)

marvelDcInfo <- infoCharacters[(infoCharacters$Publisher == "Marvel Comics" | infoCharacters$Publisher == "DC Comics"), ]
head(marvelDcInfo, 3)

# Verificamos las dimensiones
dim(marvelDcInfo)
length(unique(marvelDcInfo$Name))

# Observar valores duplicados
head(marvelDcInfo[duplicated(marvelDcInfo$Name), ], 3)

length(marvelDcInfo[duplicated(marvelDcInfo$Name), ])

## Revisemos un ejemplo de datos duplicados
# Opcion A
marvelDcInfo[marvelDcInfo$Name == "Batman", ]
# Opcion B
subset(marvelDcInfo, Name == "Batman")

## ----Limpieza de datos -----
## Eliminar duplicados
marvelDcInfo <- marvelDcInfo[!duplicated(marvelDcInfo$Name), ]

# Seleccionar columnas
marvelDcInfo <- marvelDcInfo[, c("Name", "Gender", "Race", "Publisher")]
head(marvelDcInfo, 3)

# Cambiar formatos en algunas columnas
str(marvelDcInfo)

marvelDcInfo$Name <- as.factor(marvelDcInfo$Name)
marvelDcInfo$Gender <- as.factor(marvelDcInfo$Gender)
marvelDcInfo$Race <- as.factor(marvelDcInfo$Race)
marvelDcInfo$Publisher <- as.factor(marvelDcInfo$Publisher)

## ---- Ejercicios ----

# 1) ¿Cuántos personajes hay por cada empresa?
  
### Opcion A
summary(marvelDcInfo$Publisher)

### Opcion B
table(marvelDcInfo$Publisher)
  
# 2) ¿Cuántos personajes son mujeres y hombres hay por cada empresa?
  
### DC Comics
cat("DC Comics, hombres:", nrow(subset(marvelDcInfo, Publisher == "DC Comics" & Gender == "Male")), "\n")
cat("DC Comics, mujeres:", nrow(subset(marvelDcInfo, Publisher == "DC Comics" & Gender == "Female")), "\n") 

## Marvel
cat("Marvel, hombres:", nrow(subset(marvelDcInfo, Publisher == "Marvel Comics" & Gender == "Male")), "\n")
cat("Marvel, mujeres:", nrow(subset(marvelDcInfo, Publisher == "Marvel Comics" & Gender == "Female")), "\n")

# 3) ¿Cuántas razas hay en el dataframe?
  
### Opcion A
  
# Eliminamos los `NA` que tengamos con `!is.na()`, posteriormente, obtenemos los nombres unicos y medimos.

length(unique(marvelDcInfo$Race[!is.na(marvelDcInfo$Race)]))

### Opcion B

# Como esta columna se encuentra en factores, podemos ver el numero de niveles con a funcion `nlevels()`.

nlevels(marvelDcInfo$Race)

# 4) ¿Cuáles son las razas predominantes de cada empresa?
  
# Podemos usar la función [`sort()`](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sort) para ordenar los datos de mayor a menor, usando el argumento `decreasing = TRUE`.

# La raza ***Human o humana*** es la mas predominante en ambas empresas, seguida de la mutante en el caso de Marvel Comics.

head(sort(table(marvelDcInfo$Race), decreasing = TRUE), 10)

## Guardar archivos
# getwd() 
# "C:/Users/ecoss/OneDrive - CINVESTAV/Documentos/Posdoc_LIIGH/VieRnesBioinfo/ViernesBioinfo_2024"

# Guardar una sola variable
save(marvelDcInfo, file = "Presentaciones/data/marvelDcInfo.RData")
# Guardar varias variables
save(infoStats, infoPowers, marvelDcInfo, file = "Presentaciones/data/Info_data.RData")

