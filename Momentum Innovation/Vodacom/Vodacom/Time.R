source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
source("_SOM.R")
bringInRequirements()

data <- read.csv(file = "VOD.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
data <- data[, colnames(data) %in% c(
    "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D",
    "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO")]
data <- data.frame(sapply(data, as.numeric))

start <- min(which(!is.na(data$PX_CLOSE_1D) == TRUE))
end <- nrow(data)
data <- data[start:end,]
data <- data.frame(sapply(data, na.approx.default, rule = 2))
data <- data.frame(sapply(data, ma, order = 2))
data <- data.frame(sapply(data, na.approx.default, rule = 2))

data$PX_CLOSE_LAG_1D <- as.vector(Lag(data$PX_CLOSE_1D, 1))
data$PX_CLOSE_LAG_2D <- as.vector(Lag(data$PX_CLOSE_1D, 2))
data$PX_CLOSE_LAG_5D <- as.vector(Lag(data$PX_CLOSE_1D, 5))
data$PX_CLOSE_LAG_10D <- as.vector(Lag(data$PX_CLOSE_1D, 10))
data$PX_CLOSE_LAG_30D <- as.vector(Lag(data$PX_CLOSE_1D, 30))

data <- data[31:nrow(data),]
str(data)

window_size <- 66
data$LF_VOLATILITY <- sapply(1:(nrow(data)), function(i) {
    if (i + window_size > nrow(data)) return(NA)

    px_range <- i:(i + window_size)
    px <- data$PX_CLOSE_1D[px_range]
    lagged <- as.vector(Lag(data$PX_CLOSE_1D[px_range], 1))
    px <- px[2:length(px)]
    lagged <- lagged[2:length(lagged)]
    return(sqrt(252 * sum(log(px / lagged) ^ 2) / length(px)))
})

data <- data[1:(nrow(data) - window_size),]

plot(data$LF_VOLATILITY, type = "l")
summary(data)
plot(density(data$LF_VOLATILITY))

data <- data.frame(sapply(data, scale))

x <- 1:nrow(data)
plot(x, data$LF_VOLATILITY, type = "l", ylab = "Normalized Range", main = "Styles across time")
lines(x, data$CASH_FLOW_YIELD, type = "l", col = "red", lwd = 2)
lines(x, data$EARN_YLD, type = "l", col = "red", lwd = 2)
lines(x, data$EQY_DVD_YLD_12M, type = "l", col = "red", lwd = 2)
lines(x, data$PX_TO_BOOK_RATIO, type = "l", col = "red", lwd = 2)
lines(x, data$PX_TO_SALES_RATIO, type = "l", col = "red", lwd = 2)