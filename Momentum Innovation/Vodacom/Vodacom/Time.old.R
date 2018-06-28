source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
source("_SOM.R")
bringInRequirements()

data <- getData("VOD.csv")
data <- data[, colnames(data) %in% c(
    "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D",
    "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO",
    "Dates")]
data <- data[order(data$Dates),]
data$Dates <- as.ts(data$Dates)

data$PX_CLOSE_LAG_1D <- as.vector(Lag(data$PX_CLOSE_1D, 1))
data$PX_CLOSE_LAG_2D <- as.vector(Lag(data$PX_CLOSE_1D, 2))
data$PX_CLOSE_LAG_5D <- as.vector(Lag(data$PX_CLOSE_1D, 5))
data$PX_CLOSE_LAG_10D <- as.vector(Lag(data$PX_CLOSE_1D, 10))
data$PX_CLOSE_LAG_30D <- as.vector(Lag(data$PX_CLOSE_1D, 30))

data <- data[31:nrow(data),]

window_size <- 10
# http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:standard_deviation_volatility
data$LF_VOLATILITY_10D <- sapply(1:(nrow(data)), function(i) {
    if (i + window_size > nrow(data)) return(NA)
    px_range <- i:(i + window_size)
    px_avg <- mean(data$PX_CLOSE_1D[px_range])
    px_dev <- (data$PX_CLOSE_1D[px_range] - px_avg) ^ 2
    px_dev_sum <- sum(px_dev)
    return(sqrt(px_dev_sum / length(px_range)))
})
data <- data[1:(nrow(data) - window_size),]
data <- data[, !colnames(data) %in% c("Dates")]

training_indices <- 1:ceiling(nrow(data) * 0.75)
data.train <- data[training_indices,]
data.test <- data[-training_indices,]

str(data)

model <- mlp(data.train[, -12], size = c(2), data.train$LF_VOLATILITY_10D, maxit = 500, linOut = TRUE,
               learnFunc = "SCG")
predictions <- predict(model, data.test[, -12])
result <- caret::postResample(obs = data.test$LF_VOLATILITY_10D, pred = predictions)
plotRegressionError(predictions, data.test$LF_VOLATILITY_10D)

plot(density(predictions))
plot(density(data.test$LF_VOLATILITY_10D))

percentage_RMSE <- as.vector(result[1]) /  mean(data.test$LF_VOLATILITY_10D)
percentage_RMSE
