source("_Shared.R")
source("_Functions.R")
bringInRequirements()

data <- getData("VOD.csv")
data <- data[,!colnames(data) %in% c('Dates')]
par(mfrow = c(1, 1), mar = c(18, 5, 2, 1))
boxplot(data, las = 2, main = "Boxplot - Features", ylab = "Normalized values")

par(mfrow = c(2, 2), mar = c(5, 5, 5, 5))
plot(density(data$PX_TO_BOOK_RATIO), main = "EARN_YLD")
plot(density(data$EQY_DVD_YLD_12M), main = "EQY_DVD_YLD_12M")
plot(density(data$EARN_YLD), main = "EARN_YLD")
plot(density(data$CASH_FLOW_YIELD), main = "CASH_FLOW_YIELD")

plot(density(data$PX_TO_SALES_RATIO), main = "PX_TO_SALES_RATIO")
plot(density(data$PX_CLOSE_1D), main = "PX_CLOSE_1D")
plot(density(data$BASIC_EPS_3YR_AVG_GROWTH), main = "BASIC_EPS_3YR_AVG_GROWTH")
plot(density(data$EBIT_TO_NET_SALES), main = "EBIT_TO_NET_SALES")

plot(density(data$X5_YEAR_AVERAGE_ADJUSTED_ROE), main = "X5_YEAR_AVERAGE_ADJUSTED_ROE")
plot(density(data$RETURN), main = "RETURN")
plot(density(data$STYLE_VALUE), main = "STYLE_VALUE")
plot(density(data$STYLE_QUALITY), main = "STYLE_QUALITY")

plot(density(data$MOMENTUM_252), main = "MOMENTUM_252")
plot(density(data$MOMENTUM_125), main = "MOMENTUM_125")
plot(density(data$STYLE_GROWTH), main = "STYLE_GROWTH")
plot(density(data$STYLE_MOMENTUM), main = "STYLE_MOMENTUM")

plot(density(data$LB_MONTHLY_RETURN), main = "LB_MONTHLY_RETURN")
plot(density(data$LB_MONTHLY_VOLATILITY), main = "LB_MONTHLY_VOLATILITY")
plot(density(data$LF_MONTHLY_RETURN), main = "LF_MONTHLY_RETURN")
plot(density(data$LF_MONTHLY_VOLATILITY), main = "LF_MONTHLY_VOLATILITY")

