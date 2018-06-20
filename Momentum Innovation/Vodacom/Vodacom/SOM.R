source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
source("_SOM.R")
bringInRequirements()

OUT_TO_DISK <- TRUE

data <- getData("VOD.csv")
data <- data[, colnames(data) %in% c("PX_CLOSE_1D",
    "LF_MONTHLY_RETURN", "LF_MONTHLY_VOLATILITY",
    "STYLE_VALUE", "STYLE_QUALITY", "STYLE_GROWTH", "STYLE_MOMENTUM")]

gridSize <- SOMGridSizeRecommendation(nrow(data))
som.model <- kohonen::som(as.matrix(data), grid = somgrid(gridSize, gridSize, "hexagonal", toroidal = TRUE), rlen = 100)
som.hc <- cutree(hclust(object.distances(som.model, "codes")), 2)

if (!OUT_TO_DISK) par(mfrow = c(3, 2), mar = c(1, 1, 1, 1))
if (OUT_TO_DISK) png(filename = "images/SOM_dist.neighbours.png", width = 8, height = 8, units = "in", res = 400)
plot(som.model, type = "dist.neighbours", main = "DISTANCE", palette.name = colourPalettes_Blues)
add.cluster.boundaries(som.model, som.hc)
if (OUT_TO_DISK) dev.off()

for (i in 1:ncol(data)) {
    if (OUT_TO_DISK) png(filename = paste("images/SOM_heatmap_", names(data.frame(som.model$data[[1]]))[i], ".png", sep = ""), width = 8, height = 8, units = "in", res = 400)
    plot(som.model, type = "property", property = som.model$codes[[1]][, i], main = names(data.frame(som.model$data[[1]]))[i], palette.name = colourPalettes_Blues)
    add.cluster.boundaries(som.model, som.hc)
    if (OUT_TO_DISK) dev.off()
}