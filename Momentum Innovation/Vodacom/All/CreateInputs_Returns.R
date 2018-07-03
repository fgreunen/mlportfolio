source("_Shared.R")

bringInRequirements()
unlink("C:/Working/Inputs_Returns/*") # Delete the old output files.
dir = "4_data/"
files = list.files(dir)

dataFiles = c(replicate(length(files), data.frame()))
for (i in 1:length(dataFiles)) {
    dataFiles[[i]] = bringInData(paste(dir, files[i], sep = "/"))
}

firstDate = as.Date(max(as.vector(sapply(dataFiles, function(x) {
    return(x$Dates[1])
}))))

for (i in 1:length(dataFiles)) {
    dataFiles[[i]] = dataFiles[[i]][dataFiles[[i]]$Dates >= firstDate,]
}

windowSize = 22
for (i in 1:(nrow(dataFiles[[i]]) - windowSize)) {
    start = i
    end = start + windowSize
    numberOfStocks = length(dataFiles)

    data = data.frame(
        "RETURNS" = numeric(numberOfStocks),
        "PX_TO_SALES_RATIO" = numeric(numberOfStocks),
        "CASH_FLOW_YIELD" = numeric(numberOfStocks),
        "EARN_YLD" = numeric(numberOfStocks),
        "EQY_DVD_YLD_12M" = numeric(numberOfStocks),
        "PX_TO_BOOK_RATIO" = numeric(numberOfStocks))

    for (j in 1:length(dataFiles)) {
        prices = dataFiles[[j]]$PX_CLOSE_1D[start:end]
        log_returns = diff(log(prices), lag = 1)
        volatility = sd(log_returns)

        data$RETURNS[j] =
            (dataFiles[[j]]$PX_CLOSE_1D[end] / dataFiles[[j]]$PX_CLOSE_1D[start]) - 1
        data$RETURNS[j] = data$RETURNS[j] / volatility

        #data$RETURNS[j] = -volatility

        #data$RETURNS[j] =
            #(dataFiles[[j]]$PX_CLOSE_1D[end] / dataFiles[[j]]$PX_CLOSE_1D[start]) - 1
        data$PX_TO_SALES_RATIO[j] = dataFiles[[j]]$PX_TO_SALES_RATIO[start]
        data$CASH_FLOW_YIELD[j] = dataFiles[[j]]$CASH_FLOW_YIELD[start]
        data$EARN_YLD[j] = dataFiles[[j]]$EARN_YLD[start]
        data$EQY_DVD_YLD_12M[j] = dataFiles[[j]]$EQY_DVD_YLD_12M[start]
        data$PX_TO_BOOK_RATIO[j] = dataFiles[[j]]$PX_TO_BOOK_RATIO[start]
    }

    filename = paste("C:/Working/Inputs_Returns/output_", dataFiles[[1]]$Dates[start], ".csv", sep = "")
    write.csv(data, file = filename, row.names = FALSE)
}

par(mfrow = c(2, 1))