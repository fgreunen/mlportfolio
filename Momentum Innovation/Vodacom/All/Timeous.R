# https://towardsdatascience.com/using-open-source-prophet-package-to-make-future-predictions-in-r-ece585b73687
source("_Shared.R")
source("_Functions.R")

bringInRequirements()
data = bringInDataForTraining("4_data/vod.csv")
data = data[,colnames(data) %in% c(
    "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D",
    "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO")]

training_start <- 1
training_end <- 800
test_start <- training_end + 1
test_end <- test_start + 33
training <- data[training_start:training_end,]
test <- data[test_start:test_end,]
tail(training)
head(test)

cast <- forecast(auto.arima(data$PX_TO_BOOK_RATIO), h = nrow(test))
test$PX_TO_BOOK_RATIO <- cast$mean
cast <- forecast(auto.arima(data$EQY_DVD_YLD_12M), h = nrow(test))
test$EQY_DVD_YLD_12M <- cast$mean
cast <- forecast(auto.arima(data$EARN_YLD), h = nrow(test))
test$EARN_YLD <- cast$mean
cast <- forecast(auto.arima(data$CASH_FLOW_YIELD), h = nrow(test))
test$CASH_FLOW_YIELD <- cast$mean
cast <- forecast(auto.arima(data$PX_TO_SALES_RATIO), h = nrow(test))
test$PX_TO_SALES_RATIO <- cast$mean

training_reg = training[, !colnames(training) %in% c("PX_CLOSE_1D")]
test_reg = test[, !colnames(test) %in% c("PX_CLOSE_1D")]

model <- auto.arima(training$PX_CLOSE_1D) #, xreg = training_reg)
cast <- forecast(model, h = nrow(test))#, xreg = test_reg)
acc <- accuracy(cast, test$PX_CLOSE_1D)
rmse <- acc[[4]]
mae <- acc[[6]]
mpe <- acc[[8]]
plot(cast, main = "Quality Style", ylab = "Price ZAC", xlab = "Time (index)")
lines(y = test$PX_CLOSE_1D, x = test_start:test_end, col = "red")
