source("_Shared.R")
source("_Functions.R")
source("_Mongo.R")
collectionName = "Inputs"
bringInRequirements()

dataFiles = getDataFiles()
mongo.delete(collectionName)
windowSize = 10
numberOfStocks = length(dataFiles)
colNames = names(dataFiles[[1]])[!names(dataFiles[[1]]) %in% c("PX_CLOSE_1D", "Dates")]

for (i in 1:(nrow(dataFiles[[1]]) - windowSize)) {
    start = i
    end = start + windowSize

    data = data.frame(TEMP_ID = numeric(numberOfStocks))
    for (colName in colNames) {
        data[[colName]] = numeric(numberOfStocks)
    }
    data = data[, !colnames(data) %in% c("TEMP_ID")]
    data$TARGET = numeric(numberOfStocks)

    for (j in 1:length(dataFiles)) {
        # Sharpe Ratio:
        forwardPrices = dataFiles[[j]]$PX_CLOSE_1D[start:end]
        log_returns = diff(log(forwardPrices), lag = 1)
        volatility = sd(forwardPrices)
        #data$TARGET[j] = ((last(forwardPrices) / first(forwardPrices)) - 1) / volatility
        data$TARGET[j] = ((mean(forwardPrices) / first(forwardPrices)) - 1) / volatility

        # The rest:
        for (colName in colNames)
            data[[colName]][j] = dataFiles[[j]][[colName]][start]
    }

    useFromDate = format(dataFiles[[1]]$Dates[end + 1], "%Y-%m-%d")
    startDate = format(dataFiles[[1]]$Dates[start], "%Y-%m-%d")
    endDate = format(dataFiles[[1]]$Dates[end], "%Y-%m-%d")
    data = list(VALUES = data, T_START = startDate, T_END = endDate, T_USEFROM = useFromDate)
    mongo.create(collectionName, toJSON(data))
}