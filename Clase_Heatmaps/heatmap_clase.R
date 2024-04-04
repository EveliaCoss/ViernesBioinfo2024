library(tidyverse)

# ----Datos----
indir <- "/Users/sofiasalazar/Desktop/LAB/ViernesBioinfo2024/Clase_Heatmaps/"

# datos de expresion (altamente filtrados)
exprMat <- read.csv(paste0(indir, "data/small_counts.csv"), header = T)

# metadatos (no reales)
metadata <- read.csv(paste0(indir, "data/metadata.csv"), header = T)

# ----Crear matriz para heatmap----
hMat <- exprMat %>%
  column_to_rownames("gene") %>%
  as.matrix()

# ----Heatmap base----
library(ComplexHeatmap)

# Lo unico que necesitamos, es una matriz de numeros
dim(hMat)

# el heatmap mas simple
Heatmap(hMat)

# quitemos los nombres de los genes
Heatmap(hMat, show_row_names = F)

# agregamos un nombre al color primario
Heatmap(hMat, show_row_names = F, name = "Z-score")

# Por default, el software hara clustering jerarquico de las columnas y las filas
# si lo quitamos, utilizara el orden de la matriz

Heatmap(hMat, show_row_names = F, name = "Z-score", cluster_rows = F, cluster_columns = F)

# podemos cambiar el orden de la matriz, reordenandola 

new_column_order <- c("SRR12363092", "SRR12363093", "SRR12363102", "SRR12363101" , "SRR12363095",
                      "SRR12363096", "SRR12363098", "SRR12363099")
hMat_ordered <- hMat[,new_column_order]
Heatmap(hMat_ordered, show_row_names = F, name = "Z-score", cluster_rows = F, cluster_columns = F)


# Por default, los colores siempre son azul, blanco y rojo
library(circlize)

# creamos una gama de colores personalizada
breaks <- c(min(hMat), 0, max(hMat))
col_exp <- colorRamp2(breaks, c('darkblue','white', '#736f01'))

Heatmap(hMat_ordered, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp)

# podemos agregar mas breaks

breaks <- c(min(hMat),-1, 0, 1,max(hMat))
col_exp <- colorRamp2(breaks, c('darkblue', 'blue', 'white', 'red', 'darkred'))

Heatmap(hMat_ordered, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp)

# ----Agregar anotaciones----

# empecemos con nuestros metadatos

# Para hacer anotaciones para las columnas, se usa la funcion HeatmapAnnotation

# ----Anotacion de color: valores discretos----
# se necesita una lista nombrada
color_mice <- c("beige" = "#ffecad", "black"= "black", "black" = "black",
                "beige"= "#ffecad","gray"= "darkgray","gray" = "darkgray",
                "black"= "black", "beige"= "#ffecad")

color_anno <- HeatmapAnnotation("Mouse color" = metadata$color, col = list("Mouse color" = color_mice))

# podemos ponerla arriba o abajo
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp, top_annotation = color_anno)

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp, bottom_annotation = color_anno)

# ----Anotacion de meses: valores continuos----

# para valores continuos es mejor crear una gama de colores
col_month <- colorRamp2(c(0, min(metadata$months), max(metadata$months)), 
                         c("white", "#fac3e8", "#a60c72"))
month_anno <- HeatmapAnnotation("Age (months)" = metadata$months, col = list("Age (months)" = col_month))
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp, bottom_annotation = c(color_anno, month_anno) )

# ----Anotacion de peso: valores continuos----

col_weight <- colorRamp2(c(0, min(metadata$weight), max(metadata$weight)), 
                        c("white", "#7bc982", "#013d06"))
weight_anno <- HeatmapAnnotation("Weight (gr)" = metadata$weight, col = list("Weight (gr)" = col_weight))
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp, top_annotation = c(color_anno, month_anno,weight_anno) )

# ----Anotacion de condicion: valores discretos----
# se necesita una lista nombrada
col_condition <- c("control"="#0478b3", "treated" = "#5f0778")
condition_anno <- HeatmapAnnotation("Condition" = metadata$condition, col = list("Condition" = col_condition))

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno, month_anno, weight_anno, condition_anno))

# Intenta crear una anotacion para la variable metadata$treatment_time tanto discreta como continua y comparalo

metadata$treatment_time <- as.character(metadata$treatment_time)
col_time <- c("0" = "#dba04d", "15" = "#71db4d", "30" = "#db4d6c", "240" = "#65ebfc")
time_anno <- HeatmapAnnotation("Time (min)" = metadata$treatment_time, col = list("Time (min)" = col_time))

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno, month_anno, weight_anno,time_anno, condition_anno))

# ----Split----

# Podemos incluir una separacion en el heatmap de acuerdo a una variable de los metadatos

# split en columna
split = data.frame(Condition = metadata$condition)

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno,time_anno, condition_anno),
        column_split = split)

# split en fila

# supongamos que tenemos informacion sobre los genes, creare una tabla con datos sobre los genes
set.seed(1)
row_metadata <- data.frame("gene" = rownames(hMat),
           "type" = c(rep("protein_coding", 34), rep("lncRNA", 16)),
           "logFC" = runif(50, min = -4, max = 4))

row_metadata

split_row = data.frame(Type = row_metadata$type)       
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno,time_anno, condition_anno),
        column_split = split, split = split_row)

# ----Anotaciones de filas----

# Para hacer anotaciones de filas, se usa la funcion RowAnnotation

#-----Anotacion para el logFC: continuo
col_FC <- colorRamp2(c(min(row_metadata$logFC), 0, max(row_metadata$logFC)),
                     c("darkgreen", "white", "darkred"))
FC_anno <- rowAnnotation("log2FC" = row_metadata$logFC, col = list("log2FC" = col_FC))
                     
# lo podemos poner a la izquierda o a la derecha
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno,time_anno, condition_anno),
        column_split = split, split = split_row,
        left_annotation = FC_anno)                  

#-----Anotacion para el tipo de gen: discreto
col_type <- c("protein_coding" = "#b05fa7",
              "lncRNA" = "#f2f249")
type_anno <- rowAnnotation("Gene type" = row_metadata$type, col = list("Gene type" = col_type))

# lo podemos poner a la izquierda o a la derecha
Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno,time_anno, condition_anno),
        column_split = split, split = split_row,
        left_annotation = c(type_anno, FC_anno))

# ----Anotaciones mas complejas-----



                                          