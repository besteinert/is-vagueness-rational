library(gplots)
library(dplyr)
library(data.table)

showDataWhole <- function(path="alpha_marg.csv"){

### Read CSV into R ###
data <- read.csv(file=path, sep=",", row.names=1)
data2d = as.matrix(data)


### Create Heatmap and save pdf###
# interpolate the given colors to create new color palettes with 100 colors.
cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 

name=gsub(".csv", "", path)
pdf(paste('heatmap_',name,".pdf", sep = ""), width=10, height=10)
heatmap.2(data2d,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',col=cols)
dev.off()

}
showDataWhole()
