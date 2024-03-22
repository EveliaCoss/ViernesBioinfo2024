# Title: Visualizacion de datos ggplot2
# Author: Evelia Coss
# Date:  12/10/2023

###---- Instalacion de paquetes ----
install.packages("dplyr")    # Manipulación de datos
install.packages("tidyr")    # Manipulación de datos
install.packages("tidyverse")# Manipulación de datos
install.packages("reshape2") # Transformación de datos

install.packages("ggplot2")  # Visualización grafica
install.packages("cowplot")  # Generar varios graficos en una misma figura
install.packages("RColorBrewer")  # Generar varios graficos en una misma figura
install.packages("paletteer")  # Generar varios graficos en una misma figura


install.packages("pacman") # Paquete de instalacion de otros paquetes
p_load("dplyr", "tidyr") # los instala de ser necesario y los carga en el ambiente

###---- Cargar paquetes ----
library(dplyr)     # Manipulación de datos
library(tidyr)     # Manipulación de datos
library(tidyverse) # Manipulación de datos
library(reshape2)  # Transformación de datos
library(pacman)
library(cowplot)   # Visualización grafica
library(ggplot2)   # Generar varios graficos en una misma figura
library(RColorBrewer) # Paletas de colores, Info en https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
library(paletteer) # Paletas de colores, Info en https://pmassicotte.github.io/paletteer_gallery/

## Obtener gama de colores
display.brewer.all()

###--- Directorio ----
# getwd() # nota de aqui saque la direccion
indir <- "/Users/sofiasalazar/Desktop/LAB/ViernesBioinfo2024/Clase_ggplot/"

###--- Data ----
infoCharacters <- read.csv(paste0(indir,"data/heroesInformation.csv"), na.strings = c("-", "-99")) # La opcion na.string nos permite sustituir valores - y -99 por NA
infoPowers <- read.csv(paste0(indir,"data/superHeroPowers.csv"))
infoStats <- read.csv(paste0(indir,"data/charactersStats.csv"), na.strings = "")

#Base de datos de los valores de Home ranch (rango de hogar) (log10) y valores de masa corporal (log10) para 462 especies de mamíferos
#Tucker, M. A. et al; Home range-body size patterns: are all mammals equal?
#https://onlinelibrary.wiley.com/journal/14668238?journalRedirectCheck=true
bd<- read.csv(paste0(indir,"data/MMHOME.csv"))


###--- Resumen de la informacion ----
summary(infoCharacters)
summary(infoPowers)
summary(infoStats)
summary(bd)

###--- Data ----
head(infoCharacters)
head(infoPowers)
head(infoStats)

###--- PROCESAMIENTO DE DATOS----
colnames(infoCharacters)[colnames(infoCharacters) == "name"] <- "Name"
colnames(infoPowers)[colnames(infoPowers) == "hero_names"] <- "Name"
unique(infoCharacters$Publisher)
marvelDcInfo <- infoCharacters[(infoCharacters$Publisher == "Marvel Comics" | infoCharacters$Publisher == "DC Comics"), ]

marvelDcInfo <- marvelDcInfo[!duplicated(marvelDcInfo$Name), ]

marvelDcInfo <- marvelDcInfo %>%
  select(Name, Gender, Race, Publisher)

marvelDcStatsInfo <- left_join(marvelDcInfo, infoStats, by = "Name")
head(marvelDcStatsInfo)[1:5]

fullMarvelDc <- left_join(marvelDcStatsInfo, infoPowers, by = "Name")
head(fullMarvelDc)[1:5]

fullMarvelDc$Name <- as.factor(fullMarvelDc$Name)
fullMarvelDc$Gender <- as.factor(fullMarvelDc$Gender)
fullMarvelDc$Race <- as.factor(fullMarvelDc$Race)
fullMarvelDc$Publisher <- as.factor(fullMarvelDc$Publisher)
fullMarvelDc$Alignment <- as.factor(fullMarvelDc$Alignment)

marvelDcGender <- fullMarvelDc %>% filter(!is.na(Gender)) %>%
  group_by(Gender)%>%
  dplyr::count(Publisher) %>%
  select(Gender, Publisher, Count = n)
###--- Graficas basicas ----

# 1. Base del grafico
# ggplot con dos elementos basicos: 
#    - 1) la base de datos y 
#    - 2) los elementos esteticos. 
# ggplot de forma predeterminada muestra las coordenadas y el tema.

ggplot(data = marvelDcGender, mapping = aes(x = Gender, y = Count))

#2. Se agregan las geometrias
# aes(), en este caso las variables (x, y) 
# Tipo de grafica = que queremos representar y agregamos el tipo de geometria para representar los datos.
# aplicar a todo el grafico y a las capas siguientes el mapping (overide posible)
ggplot(data = marvelDcGender) +
    geom_point(mapping = aes(x = Gender, y = Count)) # Grafica de puntos
# aplicar el mapping especifico a una capa (mas usado)
ggplot(data = marvelDcGender, mapping = aes(x = Gender, y = Count)) +
  geom_point()

#3. El grafico anterior se puede ir enriqueciendo al agregar atributos esteticos al mapping 
# o asignando atributos fuera del mapping. Recordar la diferencia entre el mapping y "setting aesthetics".

#a. Agregar elementos aeshetics al mapping

ggplot(marvelDcGender) + 
  geom_point(mapping = aes(x = Gender, y = Count, colour= Publisher)) 

#b. Agregar elementos como setting aesthetics
ggplot(marvelDcGender) + 
    geom_point (aes(x = Gender, y = Count), colour="red")

ggplot(marvelDcGender) + 
  geom_point (aes(x = Gender, y = Count), colour="purple")

ggplot(marvelDcGender) + 
  geom_point (aes(x = Gender, y = Count), colour="blue")

###--- Tipos de geometrias ----

# PALETAS DE COLOR PERSONALIZADAS
dcMarvelPalette <- c("#0476F2", "#EC1E24")
goodBadPalette <- c("#A71D20", "#0DA751", "#818385")


# Para agregar geometrias que representen los datos hay que  utilizar la funcion  geom_ 
# que se usa en ggplot para representa un grafico por medio de objetos geometricos. 
# Y se compone de la palabra geom y luego el nombre de la geometr?a (en ingles).

# ONE VARIABLE continuous (valores)
c <- ggplot(infoCharacters, aes(x = Height))
c
c + geom_area(stat = "bin")
c + geom_density(kernel = "gaussian")
c + geom_dotplot()
c + geom_freqpoly()
c + geom_histogram()

# ONE VARIABLE discrete (cualidades)
d <- ggplot(infoCharacters, aes(x = Gender))
d
d + geom_bar()

#TWO VARIABLES both continuous 
e <- ggplot(data = infoCharacters, aes(x = Height, y = Weight))
e
e + geom_point()
e + geom_point(alpha=0.5)

# alpha es para poner transparencia y permite ver entre mas solido
# mas datos presentes

e + geom_line()
e + geom_count()
e + geom_step()
e + geom_smooth(method = lm) + # metodo lineal, regresion lineal
coord_flip() # para voltear x y y
  
#Etiquetas 
e + geom_label(aes(label= Publisher))
e + geom_text(aes(label= Publisher)) # remplazar puntos por letras

#one discrete, one continuous
f <- ggplot(marvelDcGender, aes(x = Gender, y = Count))
f
f + geom_col()
f + geom_boxplot()
f + geom_violin()

rm(f, e, d, c) #eliminar variables especificas

#both discrete
g <- ggplot(infoCharacters, aes(x = Gender, y = Publisher))
g
g + geom_count()
g + geom_jitter()

#Tres variables
g <- ggplot(infoCharacters, aes(x= Gender, y = Race))
g
g + geom_raster(aes(fill = Publisher))

#Aplica la grafica las capas con diferentes geoms
ggplot(bd, aes(x=Diet, y = Mass))+ 
    geom_boxplot() +
    geom_jitter() # +
  # geom_violin()

# Cuidado en el acomodo de las capas, la posicion de las mismas es importante
ggplot(bd, aes(x=Diet, y = Mass))+ 
  geom_jitter() +
  geom_boxplot()

# puedes seguir agregando capas, cada grafico es una capa
ggplot(bd, aes(x=Diet, y = Mass)) + 
  geom_boxplot() +
  geom_jitter() +
  geom_violin()

#Asignar varias capas y modificar los valores del mapping de la grafica. 
ggplot(bd, aes(x=Home.range, y = Mass))+ 
    geom_jitter(aes(colour=Diet, shape=Diet), size= 2.0, alpha = 0.8)+
    geom_smooth(method = lm, color="black") + # ajustar una linea
    scale_shape_manual(values=c(17, 18, 19)) +
    scale_color_manual(values=c('#CC0033','#E69F00', '#00CCCC'))
  # scale_color_manual(values=c("Carnivore" = '#CC0033', "Omnivore" = '#E69F00', "Herbivore" = '#00CCCC'))

########Etiquetado######################
#Asignar nombres al título, subtitulo, ejes
aa <- ggplot(bd, aes(x=Diet, y = Mass, fill=Diet)) + 
    geom_boxplot() +
    geom_jitter(colour="gray") +
    labs(title = "Rango de hogar y masa corporal", 
         subtitle = "Mamíferos",
         x = "Dieta", y = "Masa (log10)", 
         caption = "Datos: Tucker,(2014), Global Ecol. Biogeogr. 23, 1105–1114",
         fill = "Tipo de dieta")
aa
#########Themes#######
#Los temas son una forma de personalizar los componentes que no son datos de sus gráficos: títulos, etiquetas, fuentes, fondo, cuadrículas y leyendas. Los temas pueden ser utilizados para dar al grafico un aspecto personalizado. 
## Para ver los temas: https://ggplot2.tidyverse.org/reference/ggtheme.html

######Themas completos#############
aa + theme_gray()
aa + theme_light()
aa + theme_dark()
aa + theme_minimal()
aa + theme_void()
aa + theme_test()
aa + theme_classic()

#Manipular theme
?theme

#Asignar estilo a las letras titulo y ejes
#
aa +
    theme(plot.title = element_text(color="red", size=12, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="italic"),
          axis.text = element_text(colour = "#74008D"))

#Asignar estilo al panel
aa +
    theme(plot.background = element_rect(fill = "lightblue3", colour = "NA"),
          panel.background = element_rect(fill = "lightblue", colour = "red"))

#Si quiero usar un tipo de theme y asignar estilo a los títulos 
aa + theme_dark() +
    theme(plot.title = element_text(color="red", size=12, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="italic")
    )

####Escalas scale_()#####
#Una propiedad importante de ggplot2 es el principio de que cada estetica aes()está asociada a una escala.

#La función de ggplot predetermina los valores de las escalas dependiendo de tus datos
ggplot(bd, aes(x=Home.range , y = Mass))+
    geom_point(aes(colour = Diet))

#Dicho de explícitamente quedaría así
ggplot(bd, aes(x=Home.range , y = Mass))+
    geom_point(aes(colour = Diet))+
    scale_x_continuous() +
    scale_y_continuous() +
    scale_colour_discrete()

#Sintaxis para scale()
#1. scale
#2. El nombre de la estética principal(x,y, colour, shape, fill)
#3. El tipo de escala (continuous, discrete, brewer).

#USO GENERAL en la estética del gráfico
#scale_*_continuous() - Asignar valores continuos a valores visuales. 
#scale_*_discrete() - Asigna valores discretos a valores visuales. 
#scale_*_binned() - Asigna valores continuos a contenedores discretos. 
#scale_*_identity() - Usa valores de datos como valores visuales.
#scale_*_manual(values = c()) - Asigna valores discretos a valores visuales elegidos manualmente.

#Cambiemos algunos valores
ggplot(bd, aes(x=Home.range , y = Mass))+
    geom_point(aes(colour = Diet))+
    scale_x_continuous(name = "Rango de hogar (log)") +
    scale_y_continuous(name = "Masa (log)")+
    scale_colour_discrete(name="Dieta")

ggplot(bd, aes(x=Home.range , y = Mass))+
  geom_point(aes(colour = Diet))+
  scale_x_binned(name = "Rango de hogar (log)") +
  scale_y_continuous(name = "Masa (log)")+
  scale_colour_discrete(name="Dieta")

ggplot(bd, aes(x=Home.range , y = Mass))+
  geom_point(aes(colour = Diet))+
  scale_x_binned(name = "Rango de hogar (log)") +
  scale_y_binned(name = "Masa (log)")+
  scale_colour_discrete(name="Dieta")

###Colores####
#Colores predeterminados
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
head(colors(), n = 20)
colors()

#Crear paleta manualmente
Paleta <- c("cadetblue","coral","mediumspringgreen")

p <- ggplot(bd, aes(x=Home.range , y = Mass))+
    geom_point(aes(colour = Diet))+
    scale_colour_manual(values=Paleta)
p

# Colores por posicion
cbPalette <- c(colors()[100], colors()[200] , colors()[40])
b <- ggplot(bd, aes(x=Diet, y = Mass, fill=Diet))+
    geom_bar(stat = "identity") +
    scale_fill_manual(values = cbPalette, 
                      labels=c("label1", "label2", "label3"))
b

#Paleta brewer
display.brewer.all()
ggplot(bd, aes(x=Home.range , y = Mass))+
    geom_point(aes(colour = Diet))+
    scale_color_brewer(palette='Accent') 

#Paleta paletteer
#https://r-charts.com/es/paletas-colores/
#terrain.colors() heat.colors(), topo.colors(), cm.colors(), rainbow().

ggplot(bd, aes(x=Home.range , y = Mass, colour=Mass)) +
    geom_point() +
    scale_colour_gradientn(colours=rainbow(30))

#Crear paleta
p1 <- paletteer_c("ggthemes::Blue-Green Sequential", 30)
ggplot(bd, aes(x=Home.range , y = Mass, colour=Mass))+
    geom_point()+
    scale_colour_gradientn(colours=p1)


p2 <- paletteer_c("grDevices::Inferno", 30)
ggplot(bd, aes(x=Home.range , y = Mass, colour=Mass))+
    geom_point()+
    scale_colour_gradientn(colours=p2)

###sistema de coordenadas####
#Por defecto, los gráficos de ggplot2 tienen coordenadas cartesianas. 
#La función coord_cartesian permite  hacer zoom a los gráficos

range(bd$Home.range) # tener presente el rango va de -4.44 a 5.69
range(bd$Mass) # -2.38  4.52

e <- ggplot(data = bd, aes(x = Home.range, y = Mass))
e
e + geom_point()
#Delimitar x
e + geom_point() +
    coord_cartesian(xlim=c(0,3))
e + geom_point() +
    coord_cartesian(ylim=c(0,2))
e + geom_point() +
    coord_cartesian(xlim=c(0,2.5),
                    ylim=c(0,2.5))

#Los sistemas de coordenadas en ggplot2 se pueden dividir en: (coord_cartesian, coord_fixed, coord_flip) y  lineales (coord_trans, coord_polar, coord_quickmap, coord_map). 

#la unidad a lo largo del eje X será la misma unidad a lo largo del eje Y
e + geom_point()+
    coord_cartesian()

#las unidades difieren según los datos de X y y
e + geom_point()+
    coord_fixed()

#Rotar los ejes
f <- ggplot(bd, aes(x=Diet, y = Mass))
f
f + geom_boxplot()+
    coord_flip()

#La función coord_trans crea sistemas sistemas de coordenadas cartesianas transformadas,afectando a la apariencia de los geoms
#Coordenadas polares
f <- ggplot(bd, aes(x=Diet, y = Mass))
f
f + geom_bar(stat = "identity")

f + geom_bar(stat = "identity")+
    coord_polar()

f + geom_bar(stat = "identity")+
    coord_polar(theta = "y")


#################FACETAS#####################################
#La creación de facetas muestra un subconjunto diferente de los datos. 
#Hay tres tipos de facetas:

#facet_null(): un único gráfico, el predeterminado.

#facet_wrap(): "envuelve" los paneles

#facet_grid(): produce una cuadrícula de paneles definidos por variables que forman las filas y columnas.

#facet_grid()
#Columnas
ggplot(data = bd, aes(x = Home.range, y = Mass)) +
    geom_point() +
    facet_grid(~ Diet)

ggplot(data = bd, aes(x = Home.range, y = Mass)) +
  geom_point() +
  facet_grid(~ Diet, scales = "free")

#Filas
ggplot(data = bd, aes(x = Home.range, y = Mass))+
    geom_point()+
    facet_grid(Environment ~ .)

#Filas y columnas#falta agregar una columna cualitativa
ggplot(data = bd, aes(x = Home.range, y = Mass))+
    geom_point()+
    facet_grid(Environment~Diet)

#facet_wrap(): "envuelve" los paneles
head(bd)
ggplot(data = bd, aes(x = Home.range, y = Mass))+
    geom_point()+
    facet_wrap(Environment ~ Diet)

# Juntar varias graficas en una sola figuras (cowplot)
#Mostrar dos gráficas diferentes
graf1 <- ggplot(bd, aes(x=Diet, y = Mass)) + 
    geom_boxplot() +
    geom_jitter()
graf1

graf2 <- ggplot(bd, aes(x=Diet, y = Mass))+ 
    geom_jitter()
graf2

finalgraf <- plot_grid(graf1, graf2, labels=c("A","B"), ncol = 2, nrow = 1)
finalgraf

finalgraf <- plot_grid(graf1, graf2, labels=c("1A","1B"), ncol = 2, nrow = 1)
finalgraf

## Listo! Solo nos queda guardar nuestro grafico
ggsave(filename = paste0(indir, "ggplot2_final.png"), 
       plot = finalgraf,
       width = 10, height = 7, units = "in", #pulgadas
       bg = "white",
       dpi = 300)

