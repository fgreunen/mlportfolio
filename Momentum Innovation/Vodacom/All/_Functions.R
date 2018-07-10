fillInMissingsBefore = function(x) {
    previousValue = NA
    for (i in 1:length(x)) {
        if (is.na(x[i])) x[i] = previousValue
        previousValue = x[i]
    }
    return(x)
}
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}
smoothFactor = function(x, mavSize = 15) {
    working = x
    temp = working[1:mavSize]
    working = fillInMissingsBefore(working)
    working = mav(working, mavSize)
    working[1:mavSize] = temp
    working = na.approx.default(working, rule = 2)
    working = mav(working, mavSize)
    working[1:mavSize] = temp
    working = na.approx.default(working, rule = 2)
    return(working)
}
scaleFactors = function(dataFiles, colNames) {
    for (colName in colNames) {
        working = numeric(nrow(dataFiles[[1]]))
        for (dataFile in dataFiles) {
            working = working + dataFile[[colName]]
        }
        working = working / length(dataFiles)
        for (i in 1:length(dataFiles)) {
            dataFiles[[i]][[colName]] = dataFiles[[i]][[colName]] / working
        }
    }
    return(dataFiles)
}
getScore = function(data, values) {
    return
    (
    0
    #+ data$PX_TO_BOOK_RATIO * values$PX_TO_BOOK_RATIO
    #+ data$CASH_FLOW_YIELD * values$CASH_FLOW_YIELD
    + data$EARN_YLD * values$EARN_YLD
    + data$EQY_DVD_YLD_12M * values$EQY_DVD_YLD_12M
    #+ data$PX_TO_SALES_RATIO * values$PX_TO_SALES_RATIO
    #+ data$OPER_INC_TO_NET_SALES * values$OPER_INC_TO_NET_SALES
    #+ data$MOMENTUM_1_MONTH * values$MOMENTUM_1_MONTH
    #+ data$MOMENTUM_2_WEEKS * values$MOMENTUM_2_WEEKS
    #+ data$SALES_3YR_AVG_GROWTH * values$SALES_3YR_AVG_GROWTH
    #+ data$MOMENTUM_6_MONTH * values$MOMENTUM_6_MONTH
    )
}
getDataFiles = function() {
    dir = "4_data/"
    files = list.files(dir)
    dataFiles = c(replicate(length(files), data.frame()))
    for (i in 1:length(dataFiles))
        dataFiles[[i]] = bringInDataForTraining(paste(dir, files[i], sep = "/"))

    firstDate = as.Date(max(as.vector(sapply(dataFiles, function(x) {
        return(x$Dates[1])
    }))))

    for (i in 1:length(dataFiles))
        dataFiles[[i]] = dataFiles[[i]][dataFiles[[i]]$Dates >= firstDate,]

    colNames = names(dataFiles[[1]])[!names(dataFiles[[1]]) %in% c("PX_CLOSE_1D", "Dates")]
    return(scaleFactors(dataFiles, colNames))
}

bringInDataForTraining = function(fileName) {
    data = read.csv(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    data = data[, colnames(data) %in% c(
        "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D", "OPER_INC_TO_NET_SALES",
        "SALES_3YR_AVG_GROWTH", "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO", "Dates")]

    data$Dates = as.Date(data$Dates)
    data$PX_TO_BOOK_RATIO = as.numeric(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M = as.numeric(data$EQY_DVD_YLD_12M)
    data$PX_CLOSE_1D = as.numeric(data$PX_CLOSE_1D)
    data$EARN_YLD = as.numeric(data$EARN_YLD)
    data$CASH_FLOW_YIELD = as.numeric(data$CASH_FLOW_YIELD)
    data$PX_TO_SALES_RATIO = as.numeric(data$PX_TO_SALES_RATIO)
    data$OPER_INC_TO_NET_SALES = as.numeric(data$OPER_INC_TO_NET_SALES)
    data$SALES_3YR_AVG_GROWTH = as.numeric(data$SALES_3YR_AVG_GROWTH)

    start = min(which(!is.na(data$PX_CLOSE_1D) == TRUE))
    end = nrow(data)
    data = data[start:end,]

    smoothingFactor = 66
    data$PX_TO_BOOK_RATIO = smoothFactor(data$PX_TO_BOOK_RATIO, smoothingFactor)
    data$EQY_DVD_YLD_12M = smoothFactor(data$EQY_DVD_YLD_12M, smoothingFactor)
    data$EARN_YLD = smoothFactor(data$EARN_YLD, smoothingFactor)
    data$PX_TO_SALES_RATIO = smoothFactor(data$PX_TO_SALES_RATIO, smoothingFactor)
    data$CASH_FLOW_YIELD = smoothFactor(data$CASH_FLOW_YIELD, smoothingFactor)
    data$OPER_INC_TO_NET_SALES = smoothFactor(data$OPER_INC_TO_NET_SALES, smoothingFactor)
    data$SALES_3YR_AVG_GROWTH = smoothFactor(data$SALES_3YR_AVG_GROWTH, smoothingFactor)

    data$PX_TO_BOOK_RATIO[data$PX_TO_BOOK_RATIO < 0] = 0
    data$EQY_DVD_YLD_12M[data$EQY_DVD_YLD_12M < 0] = 0
    data$EARN_YLD[data$EARN_YLD < 0] = 0
    data$PX_TO_SALES_RATIO[data$PX_TO_SALES_RATIO < 0] = 0
    data$CASH_FLOW_YIELD[data$CASH_FLOW_YIELD < 0] = 0
    data$OPER_INC_TO_NET_SALES[data$OPER_INC_TO_NET_SALES < 0] = 0
    data$SALES_3YR_AVG_GROWTH[data$SALES_3YR_AVG_GROWTH < 0] = 0

    windowSize = 132
    data$MOMENTUM_6_MONTH = rep(1, nrow(data))
    for (i in windowSize:nrow(data)) {
        data$MOMENTUM_6_MONTH[i] = data$PX_CLOSE_1D[i] / data$PX_CLOSE_1D[i - windowSize + 1]
    }

    windowSize = 22
    data$MOMENTUM_1_MONTH = rep(1, nrow(data))
    for (i in windowSize:nrow(data)) {
        data$MOMENTUM_1_MONTH[i] = data$PX_CLOSE_1D[i] / data$PX_CLOSE_1D[i - windowSize + 1]
    }
    windowSize = 10
    data$MOMENTUM_2_WEEKS = rep(1, nrow(data))
    for (i in windowSize:nrow(data)) {
        data$MOMENTUM_2_WEEKS[i] = data$PX_CLOSE_1D[i] / data$PX_CLOSE_1D[i - windowSize + 1]
    }

    return(data)
}