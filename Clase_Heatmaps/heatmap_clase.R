library(tidyverse)

# GUIA SUPREMA: https://jokergoo.github.io/ComplexHeatmap-reference/book/index.html
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
# ----Boxplot----
# Supongamos que tenemos pvalores para los genes y los individuos y los queremos visualizar tambien

set.seed(1)
# generar pvalores falsos, tiene que ser una matriz de la misma dimension que nuestros datos
pvalues_matrix <- matrix(nrow = 50, ncol = 8)
for(i in 1:8) {
  significant_pvalues <- runif(40, min = 0.0001, max = 0.05)
  non_significant_pvalues <- runif(10, min = 0.05, max = 1)
  row_pvalues <- sample(c(significant_pvalues, non_significant_pvalues))
  pvalues_matrix[,i ] <- row_pvalues
}

# para las anotaciones de boxplot se usa la funcion anno_boxplot()
# se pueden hacer tanto para row, column o ambas, en este caso solo tenemos para row (genes)
box_anno <- anno_boxplot(pvalues_matrix, which = c("row"), border = T,
                         gp = gpar(fill = "#CCCCCC"), ylim = NULL,
                         outline=T, pch = 1, size = unit(2, "mm"), axis = T)

draw(box_anno)

# podemos transformar los pvalores para hacerlos mejor visibles
box_anno <- anno_boxplot(-log10(pvalues_matrix), which = c("row"), border = T,
                         gp = gpar(fill = "#CCCCCC"), ylim = NULL,
                         outline=T, pch = 1, size = unit(2, "mm"), axis = T)

draw(box_anno)

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno,time_anno, condition_anno),
        column_split = split, split = split_row,
        left_annotation = c(type_anno, FC_anno),
        right_annotation = rowAnnotation("-log10(p-value)" = box_anno))

# ----Barplot----

# supongamos que tenemos un valor de "temperatura" por muestra que queremos representar

temp <- runif(8, min = 10, max = 35)

# como las muestras estan en las columnas, creamos un HeatmapAnnotation
# y usamos la funcion anno_barplot()

temp_anno <- HeatmapAnnotation("Temperature" = anno_barplot(temp, gp = gpar(fill = "#db5c62")))
draw(temp_anno)

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno, condition_anno, temp_anno),
        column_split = split, split = split_row,
        left_annotation = c(type_anno, FC_anno),
        right_annotation = rowAnnotation("-log10(p-value)" = box_anno))

# podemos crear tambien una anotacion para las filas con rowAnnotation()

gene_bar_anno <- rowAnnotation("Gene size" = anno_barplot(runif(50, max = 2300, min = 78 ),
                               gp = gpar(fill = "#5c78db")))
draw(gene_bar_anno)

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno, condition_anno, temp_anno),
        column_split = split, split = split_row,
        left_annotation = c(type_anno, FC_anno, gene_bar_anno),
        right_annotation = rowAnnotation("-log10(p-value)" = box_anno))

# Podemos modificar la altura del barplot

gene_bar_anno <- rowAnnotation("Gene size" = anno_barplot(runif(50, max = 2300, min = 78 ),
                                                          gp = gpar(fill = "#5c78db"),
                                                          axis_param = list(direction = "reverse"),
                                                          width = unit(3, "cm")))

Heatmap(hMat_ordered, show_column_names = F, show_row_names = F, name = "Z-score", cluster_rows = F, 
        cluster_columns = F, col = col_exp,
        top_annotation = c(color_anno, condition_anno, temp_anno),
        column_split = split, split = split_row,
        left_annotation = c(type_anno, FC_anno, gene_bar_anno),
        right_annotation = rowAnnotation("-log10(p-value)" = box_anno))
# ----Anotaciones dentro del heatmap---

# supongamos que yo quiero dibujar un asterisco dependiento de la significancia de un gen
# y esto a su vez depende de el zscore y de el pvalue
# esto lo podemos lograr haciendo uso del argumento layer_fun()
# ahi podemos definir una funcion que agregue una anotacion dependiendo de una(s)
# condiciones


# los argumentos j, i corresponden a las filas y columnas

# ya que quiero que la anotacion dependa de tanto los zscores (hMat_ordered) y
# los pvalues (pvalues_matrix), entonces se crearan dos "indices" con pindex
Heatmap(hMat_ordered,  show_column_names = F, show_row_names = F,
        name = "Z-score", col = col_exp, cluster_rows = F, 
        cluster_columns = F,
        layer_fun = function(j, i, x, y, width, height, fill) {
  # para definir una posicion en la matriz, necesitamos crear un indice
  # se usa pindex() en lugar de mat[j, i],
  # pues esto nos daria una matriz pequena
  v = pindex(hMat_ordered, i, j)
  vp = pindex(pvalues_matrix, i, j)

  # quiero anotar ** si es muy significativo y * si es poco significativo
  # entonces establezco dos condiciones l y l2, que dependen del valor de zscore 
  # (todos los v) y del de pvalue (todos los vp)
  
  l = ((abs(v) >= 1) & (vp < 0.05) ) # **
  l2 = ((abs(v) >=0.5) & (abs(v) < 1) & (vp < 0.05) ) # *
  
  
  # definimos el texto para poder anotarlo, asi como determinamos la
  # altura y ancho del texto
  gb = textGrob("**")
  gb_w = convertWidth(grobWidth(gb), "mm")
  gb_h = convertHeight(grobHeight(gb), "mm")
  
  gb2 = textGrob("*")
  gb2_w = convertWidth(grobWidth(gb2), "mm")
  gb2_h = convertHeight(grobHeight(gb2), "mm")
  
  # aqui solo muevo un poco la posicion de los simbolos para que queden
  # centrados en las celdas
  # y defino formalmente lo que quiero escribir en las celdas
  grid.text("**", x[l], y[l] - gb_h*0.7 + gb_w*0.4)
  grid.text("*", x[l2], y[l2] - gb2_h*0.5 + gb2_w*0.4)
})

# ----Heatmap final con todos los tipos de anotaciones aprendidas----

heatmap_final <- Heatmap(hMat_ordered,  show_column_names = F, show_row_names = F,
                         name = "Z-score", col = col_exp, cluster_rows = F, 
                         cluster_columns = F, 
                         # separaciones entre columnas y filas
                         column_split = split, split = split_row,
                         # Anotaciones de columna, incluyendo un barplot
                         top_annotation = c(color_anno, month_anno, weight_anno,time_anno, condition_anno, temp_anno),
                         # Anotaciones de fila, incuyendo boxplot y barplot
                         left_annotation = c(type_anno, FC_anno, gene_bar_anno), 
                         right_annotation = rowAnnotation("-log10(p-value)" = box_anno),
                         # Anotacion de texto en celda
                         layer_fun = function(j, i, x, y, width, height, fill) {
                           # para definir una posicion en la matriz, necesitamos crear un indice
                           # se usa pindex() en lugar de mat[j, i],
                           # pues esto nos daria una matriz pequena
                           v = pindex(hMat_ordered, i, j)
                           vp = pindex(pvalues_matrix, i, j)
                           
                           # quiero anotar ** si es muy significativo y * si es poco significativo
                           # entonces establezco dos condiciones l y l2, que dependen del valor de zscore 
                           # (todos los v) y del de pvalue (todos los vp)
                           
                           l = ((abs(v) >= 1) & (vp < 0.05) ) # **
                           l2 = ((abs(v) >=0.5) & (abs(v) < 1) & (vp < 0.05) ) # *
                           
                           
                           # definimos el texto para poder anotarlo, asi como determinamos la
                           # altura y ancho del texto
                           gb = textGrob("**")
                           gb_w = convertWidth(grobWidth(gb), "mm")
                           gb_h = convertHeight(grobHeight(gb), "mm")
                           
                           gb2 = textGrob("*")
                           gb2_w = convertWidth(grobWidth(gb2), "mm")
                           gb2_h = convertHeight(grobHeight(gb2), "mm")
                           
                           # aqui solo muevo un poco la posicion de los simbolos para que queden
                           # centrados en las celdas
                           # y defino formalmente lo que quiero escribir en las celdas
                           grid.text("**", x[l], y[l] - gb_h*0.7 + gb_w*0.4)
                           grid.text("*", x[l2], y[l2] - gb2_h*0.5 + gb2_w*0.4)
                         })

draw(heatmap_final)


# ----Guardar un heatmap----

# la opcion que mas recomiendo es usar la funcion png() o pdf() o incluso svg() y combinarlo con draw()
# en la funcion png podemos jugar con los tamanos

png(filename = paste0(indir, "final_Heatmap.png"), res = 300, height = 30, width = 30, units = "cm")
  draw(heatmap_final)
dev.off()

pdf(file = paste0(indir, "final_Heatmap.pdf"))
draw(heatmap_final)
dev.off()

svg(filename = paste0(indir, "final_Heatmap.svg"), height = 12, width = 12)
draw(heatmap_final)
dev.off()
