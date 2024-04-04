library(DESeq2)
library(tidyverse)
indir <- "/Users/sofiasalazar/Desktop/LAB/ViernesBioinfo2024/Clase_Heatmaps/"
setwd(indir)

counts <- read.csv(paste0(indir, "data/raw_counts.csv"), row.names = "X")
counts <- counts[-c(1:5), ]
counts <- counts[which(rowSums(counts) > 10),]
condition <- c("control", "control", "treated", "treated", "treated", "treated", "treated", "treated")

metadata <- data.frame(sample = colnames(counts), condition)
# agregar info artificial
metadata$color <- c("beige", "black", "black", "beige", "gray", "gray", "black", "beige")
metadata$months <- c(12, 14, 6, 8, 4, 12, 9, 10)
metadata$weight <- runif(8, min = 20, max = 40)
metadata$treatment_time <- c(0, 0, 15, 15, 30, 30, 240, 240) 

norm_counts <- vst(as.matrix(counts))
zscores <- t(scale(t(norm_counts)))

small_counts <- zscores[sample(nrow(zscores), 50), ]

small_counts <- as.data.frame(small_counts) %>%
  rownames_to_column("gene")

write.csv(small_counts, file=paste0(indir, "data/small_counts.csv"),
          row.names = F)
write.csv(metadata, file = paste0(indir, "data/metadata.csv"),
          row.names = F)
