source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
bringInRequirements()

data = bringInDataForTraining("C:/Source/Git/Momentum Innovation/Vodacom/All/4_data/MTN.csv")
data = data[order(data$Dates),]
data$PX_CLOSE_1D = normalize(data$PX_CLOSE_1D)
data$PX_TO_BOOK_RATIO = normalize(data$PX_TO_BOOK_RATIO)
data$EQY_DVD_YLD_12M = normalize(data$EQY_DVD_YLD_12M)
data$EARN_YLD = normalize(data$EARN_YLD)
data$CASH_FLOW_YIELD = normalize(data$CASH_FLOW_YIELD)
data$PX_TO_SALES_RATIO = normalize(data$PX_TO_SALES_RATIO)

plot(PX_CLOSE_1D ~ Dates, data, type = "l", ylab = "Normalized Range",
     main = "Styles across time (VOD)")
lines(PX_TO_BOOK_RATIO ~ Dates, data, type = "l", col = "red", lwd = 2)
lines(EQY_DVD_YLD_12M ~ Dates, data, type = "l", col = "green", lwd = 2)
lines(EARN_YLD ~ Dates, data, type = "l", col = "blue", lwd = 2)
lines(CASH_FLOW_YIELD ~ Dates, data, type = "l", col = "orange", lwd = 2)
lines(PX_TO_SALES_RATIO ~ Dates, data, type = "l", col = "orange", lwd = 2)
legend(min(data$Dates), 1, c("Value", "Quality", "Momentum", "Growth"),
       col = c("red", "green", "blue", "orange"), lty = c(1, 1, 1, 1), lwd = 2, merge = TRUE, bg = "gray90")
