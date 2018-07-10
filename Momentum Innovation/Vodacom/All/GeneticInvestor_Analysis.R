source("_Shared.R")
source("_Functions.R")
source("_Mongo.R")
source("_ColourPalettes.R")

bringInRequirements()
dataFiles = getDataFiles()
data = mongo.get("Outputs")
data$T_USEFROM = as.Date(data$T_USEFROM)
data$T_START = as.Date(data$T_START)
data$T_END = as.Date(data$T_END)

#plot(y = dataFiles[[1]]$PX_CLOSE_1D, x = dataFiles[[1]]$Dates, type = "l", main = "MTN", xlab = "Time", ylab = "Price")
#plot(y = dataFiles[[2]]$PX_CLOSE_1D, x = dataFiles[[2]]$Dates, type = "l", main = "OML", xlab = "Time", ylab = "Price")
#plot(y = dataFiles[[3]]$PX_CLOSE_1D, x = dataFiles[[3]]$Dates, type = "l", main = "SLM", xlab = "Time", ylab = "Price")
#plot(y = dataFiles[[4]]$PX_CLOSE_1D, x = dataFiles[[4]]$Dates, type = "l", main = "VOD", xlab = "Time", ylab = "Price")

#mean(data$ACTUAL_TARGET - data$MEAN_TARGET)
#data$ACTUAL_TARGET - data$MEAN_TARGET
smoothingFactor = 22
data$EARN_YLD = smoothFactor(data$EARN_YLD, smoothingFactor)
data$EQY_DVD_YLD_12M = smoothFactor(data$EQY_DVD_YLD_12M, smoothingFactor)
data$PX_TO_SALES_RATIO = smoothFactor(data$PX_TO_SALES_RATIO, smoothingFactor)
data$CASH_FLOW_YIELD = smoothFactor(data$CASH_FLOW_YIELD, smoothingFactor)
data$PX_TO_BOOK_RATIO = smoothFactor(data$PX_TO_BOOK_RATIO, smoothingFactor)

plot(y = smoothFactor(data$ACTUAL_TARGET - data$MEAN_TARGET), x = data$T_START, type = "l", main = "[Actual - Mean] Optimization", xlab = "Time", ylab = "Performance")
abline(h = 0)

#mean(data$EARN_YLD)
#mean(data$EQY_DVD_YLD_12M)
#mean(data$PX_TO_SALES_RATIO)
#mean(data$CASH_FLOW_YIELD)
#mean(data$PX_TO_BOOK_RATIO)

plot(density(data$EARN_YLD), main = "EARN_YLD")
abline(v = mean(data$EARN_YLD), col = "red")
mtext(mean(data$EARN_YLD))

plot(density(data$EQY_DVD_YLD_12M), main = "EQY_DVD_YLD_12M")
abline(v = mean(data$EQY_DVD_YLD_12M), col = "red")
mtext(mean(data$EQY_DVD_YLD_12M))

plot(density(data$PX_TO_SALES_RATIO), main = "PX_TO_SALES_RATIO")
abline(v = mean(data$PX_TO_SALES_RATIO), col = "red")
mtext(mean(data$PX_TO_SALES_RATIO))

plot(density(data$CASH_FLOW_YIELD), main = "CASH_FLOW_YIELD")
abline(v = mean(data$CASH_FLOW_YIELD), col = "red")
mtext(mean(data$CASH_FLOW_YIELD))

plot(density(data$PX_TO_BOOK_RATIO), main = "PX_TO_BOOK_RATIO")
abline(v = mean(data$PX_TO_BOOK_RATIO), col = "red")
mtext(mean(data$PX_TO_BOOK_RATIO))


data.frame("FACTOR" = c(mean(data$EARN_YLD), mean(data$EQY_DVD_YLD_12M), mean(data$PX_TO_SALES_RATIO), mean(data$CASH_FLOW_YIELD), mean(data$PX_TO_BOOK_RATIO)))

#data$MOMENTUM_1_MONTH = smoothFactor(data$MOMENTUM_1_MONTH, smoothingFactor)
#data$MOMENTUM_2_WEEKS = smoothFactor(data$MOMENTUM_2_WEEKS, smoothingFactor)
#data$OPER_INC_TO_NET_SALES = smoothFactor(data$OPER_INC_TO_NET_SALES, smoothingFactor)
#data$SALES_3YR_AVG_GROWTH = smoothFactor(data$SALES_3YR_AVG_GROWTH, smoothingFactor)

data$ID = 1:nrow(data)
melted = melt(data[, colnames(data) %in% c("ID", "EARN_YLD", "EQY_DVD_YLD_12M", "PX_TO_SALES_RATIO", "CASH_FLOW_YIELD", "PX_TO_BOOK_RATIO")],
              id.vars = "ID", value.name = "FACTOR")
p = ggplot(melted, aes(x = ID, y = FACTOR, group = variable, fill = variable)) + geom_area(position = "fill", alpha = 0.85)
p + scale_fill_brewer(palette = "Blues")

par(mfrow = c(3, 2))
plot(data$EARN_YLD, type = "l", ylim = c(0, 1))
plot(data$EQY_DVD_YLD_12M, type = "l")
plot(data$PX_TO_SALES_RATIO, type = "l")
plot(data$CASH_FLOW_YIELD, type = "l")
plot(data$PX_TO_BOOK_RATIO, type = "l")

#plot(data$OPER_INC_TO_NET_SALES, type = "l")
#plot(data$MOMENTUM_1_MONTH, type = "l")
#plot(data$MOMENTUM_2_WEEKS, type = "l")
#plot(data$SALES_3YR_AVG_GROWTH, type = "l")

numberOfStocks = length(dataFiles)
firstDate = first(data$T_USEFROM)
for (i in 1:numberOfStocks) {
    dataFiles[[i]] = dataFiles[[i]][dataFiles[[i]]$Dates >= firstDate,]
}

windowPeriod = 22
amount = 1000000

################ START NAIVE ################
totalValue = amount
numberOfDataPoints = nrow(dataFiles[[1]])
totalValuesNaive = data.frame("PX_CLOSE_1D" = numeric(numberOfDataPoints))
defaultSplit = 1 / numberOfStocks
scores = rep(defaultSplit, numberOfStocks)
shares = numeric(numberOfStocks)
for (i in 1:numberOfStocks) {
    shares[i] = totalValue * scores[i] / dataFiles[[i]]$PX_CLOSE_1D[1]
}
worths = numeric(numberOfStocks)
for (i in 1:numberOfDataPoints) {
    for (j in 1:numberOfStocks) {
        worths[j] = dataFiles[[j]]$PX_CLOSE_1D[i] * shares[j]
    }
    totalValue = sum(worths)
    totalValuesNaive$PX_CLOSE_1D[i] = totalValue

    if (i %% windowPeriod == 1) {
        totalValue = totalValue * 0.999
        for (j in 1:numberOfStocks) {
            shares[j] = totalValue * scores[j] / dataFiles[[j]]$PX_CLOSE_1D[i]
        }
    }
}
################ END NAIVE ################

totalValue = amount
totalValuesMean = data.frame("PX_CLOSE_1D" = numeric(numberOfDataPoints))
bias = rep(1, numberOfStocks)
oldScoreFactor = 0.3
defaultSplit = 1 / numberOfStocks
orderFactor = 0.05
scores = rep(defaultSplit, numberOfStocks)
shares = numeric(numberOfStocks)
for (i in 1:numberOfStocks) {
    shares[i] = totalValue * scores[i] / dataFiles[[i]]$PX_CLOSE_1D[1]
}
oldScores = scores
worths = numeric(numberOfStocks)
for (i in 1:numberOfDataPoints) {
    for (j in 1:numberOfStocks) {
        worths[j] = dataFiles[[j]]$PX_CLOSE_1D[i] * shares[j]
    }
    totalValue = sum(worths)
    totalValuesMean$PX_CLOSE_1D[i] = totalValue

    if (i %% windowPeriod == 1) {
        totalValue = totalValue * 0.999
        values = first(data[data$T_USEFROM > dataFiles[[1]]$Dates[i],])
        prevValues = data[data$T_USEFROM < dataFiles[[1]]$Dates[i],]
        prevValues = prevValues[complete.cases(prevValues),]
        prevValuesLast = nrow(prevValues)
        prevValuesFirst = max(c(prevValuesLast - 220, 0))

        values$EARN_YLD = mean(prevValues$EARN_YLD[prevValuesFirst:prevValuesLast])
        values$EQY_DVD_YLD_12M = mean(prevValues$EQY_DVD_YLD_12M[prevValuesFirst:prevValuesLast])

        #values$PX_TO_SALES_RATIO = mean(prevValues$PX_TO_SALES_RATIO[prevValuesFirst:prevValuesLast])
        #values$CASH_FLOW_YIELD = mean(prevValues$CASH_FLOW_YIELD[prevValuesFirst:prevValuesLast])
        #values$PX_TO_BOOK_RATIO = mean(prevValues$PX_TO_BOOK_RATIO[prevValuesFirst:prevValuesLast])

        #values$OPER_INC_TO_NET_SALES = mean(prevValues$OPER_INC_TO_NET_SALES[prevValuesFirst:prevValuesLast])
        #values$MOMENTUM_1_MONTH = mean(prevValues$MOMENTUM_1_MONTH[prevValuesFirst:prevValuesLast])
        #values$MOMENTUM_2_WEEKS = mean(prevValues$MOMENTUM_2_WEEKS[prevValuesFirst:prevValuesLast])
        #values$SALES_3YR_AVG_GROWTH = mean(prevValues$SALES_3YR_AVG_GROWTH[prevValuesFirst:prevValuesLast])

        if (!is.na(values$EARN_YLD)) {
            for (j in 1:numberOfStocks) {
                scores[j] = (1 - oldScoreFactor) * bias[j] * getScore(dataFiles[[j]][i,], values) + oldScoreFactor * oldScores[j]
            }
            defaultSplit = mean(scores)
            for (j in 1:numberOfStocks) {
                scores[j] = (1 - orderFactor) * scores[j] + orderFactor * defaultSplit
            }
            oldScores = scores
            total = sum(scores)
            scores = scores / total
        }
        for (j in 1:numberOfStocks) {
            shares[j] = totalValue * scores[j] / dataFiles[[j]]$PX_CLOSE_1D[i]
        }
    }
}

allocations = data.frame("MTN" = numeric(numberOfDataPoints),"OML" = numeric(numberOfDataPoints),"SLM" = numeric(numberOfDataPoints),"VOD" = numeric(numberOfDataPoints))
totalValue = amount
totalValues = data.frame("PX_CLOSE_1D" = numeric(numberOfDataPoints))
defaultSplit = 1 / numberOfStocks
scores = rep(defaultSplit, numberOfStocks)
shares = numeric(numberOfStocks)
worths = numeric(numberOfStocks)
for (i in 1:numberOfStocks) {
    shares[i] = totalValue * scores[i] / dataFiles[[i]]$PX_CLOSE_1D[1]
}
oldScores = scores
for (i in 1:numberOfDataPoints) {
    for (j in 1:numberOfStocks) {
        worths[j] = dataFiles[[j]]$PX_CLOSE_1D[i] * shares[j]
    }
    totalValue = sum(worths)
    totalValues$PX_CLOSE_1D[i] = totalValue

    if (i %% windowPeriod == 1) {
        totalValue = totalValue * 0.999
        values = first(data[data$T_USEFROM > dataFiles[[1]]$Dates[i],])

        #print(paste(dataFiles[[1]]$Dates[i], values$T_USEFROM))
        for (j in 1:numberOfStocks) {
            scores[j] = (1 - oldScoreFactor) * bias[j] * getScore(dataFiles[[j]][i,], values) + oldScoreFactor * oldScores[j]
        }
        defaultSplit = mean(scores)
        for (j in 1:numberOfStocks) {
            scores[j] = (1 - orderFactor) * scores[j] + orderFactor * defaultSplit
        }
        oldScores = scores
        total = sum(scores)
        scores = scores / total
        #print(scores)

        allocations[i,]$MTN = scores[1]
        allocations[i,]$OML = scores[2]
        allocations[i,]$SLM = scores[3]
        allocations[i,]$VOD = scores[4]

        for (j in 1:numberOfStocks) {
            shares[j] = totalValue * scores[j] / dataFiles[[j]]$PX_CLOSE_1D[i]
        }
    }
}

par(mfrow = c(1, 1))
plot(totalValues$PX_CLOSE_1D, type = "l",
     main = "Versus",
     ylab = "Value")
lines(totalValuesNaive$PX_CLOSE_1D, type = "l", col = "red")
#lines(totalValuesMean$PX_CLOSE_1D, type = "l", col = "blue")
for (i in 0:(floor(numberOfDataPoints / 252)))
    abline(v = i * 252)

plot(totalValues$PX_CLOSE_1D - totalValuesNaive$PX_CLOSE_1D,
     ylab = "totalValues - totalValuesNaive",
     type = "l", col = "red",
     main = "Naive Allocation Comparison")
#lines(totalValuesMean$PX_CLOSE_1D - totalValuesNaive$PX_CLOSE_1D, type = "l", col = "blue")
abline(h = 0)
for (i in 0:(floor(numberOfDataPoints / 252)))
    abline(v = i * 252)

allocations = allocations[allocations$MTN > 0,]
plot(allocations$MTN, type = "l", main = "MTN Allocation")
abline(h = mean(allocations$MTN))
mtext(mean(allocations$MTN))

plot(allocations$OML, type = "l", main = "OML Allocation")
abline(h = mean(allocations$OML))
mtext(mean(allocations$OML))

plot(allocations$SLM, type = "l", main = "SLM Allocation")
abline(h = mean(allocations$SLM))
mtext(mean(allocations$SLM))

plot(allocations$VOD, type = "l", main = "VOD Allocation")
abline(h = mean(allocations$VOD))
mtext(mean(allocations$VOD))

prices = totalValuesNaive$PX_CLOSE_1D[1:nrow(totalValuesNaive)]
log_returns = diff(log(prices), lag = 1)
naiveVolatility = sd(log_returns)
prices = totalValues$PX_CLOSE_1D[1:nrow(totalValues)]
log_returns = diff(log(prices), lag = 1)
volatility = sd(log_returns)
100 * naiveVolatility / volatility
