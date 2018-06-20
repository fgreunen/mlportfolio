source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
source("_SOM.R")
bringInRequirements()

OUT_TO_DISK <- TRUE

data <- getData("VOD.csv")
data <- data[order(data$Dates),]

if (OUT_TO_DISK) png(filename = "images/StylesAcrossTime.png", width = 8, height = 8, units = "in", res = 400)
plot(PX_CLOSE_1D ~ Dates, data, type = "l", ylab = "Normalized Range",
     main = "Styles across time")
lines(STYLE_VALUE ~ Dates, data, type = "l", col = "red", lwd = 2)
lines(STYLE_QUALITY ~ Dates, data, type = "l", col = "green", lwd = 2)
lines(STYLE_MOMENTUM ~ Dates, data, type = "l", col = "blue", lwd = 2)
lines(STYLE_GROWTH ~ Dates, data, type = "l", col = "orange", lwd = 2)
legend(min(data$Dates), 1, c("Value", "Quality", "Momentum", "Growth"),
       col = c("red", "green", "blue", "orange"), lty = c(1, 1, 1, 1), lwd = 2, merge = TRUE, bg = "gray90")
if (OUT_TO_DISK) dev.off()