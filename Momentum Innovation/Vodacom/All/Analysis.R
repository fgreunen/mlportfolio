clearWorkspace = function(setSeed = TRUE) {
    rm() # Clear the workspace.
    par(mfrow = c(1, 1))
    if (setSeed) set.seed(1231134)
    graphics.off() # Remove old plots
}
installIfNotExists = function(package.name, repo = NULL) {
    if (!require(package.name, character.only = TRUE)) {
        if (is.null(repo)) { install.packages(package.name) }
        else { install.packages(package.name, repo = repo) }
    }
    library(package.name, character.only = TRUE)
}
bringInRequirements = function() {
    clearWorkspace()

    installIfNotExists("ggplot2")
    installIfNotExists("corrplot")
    installIfNotExists("caret")
    installIfNotExists("psych")
    installIfNotExists("mlr")
    installIfNotExists("Amelia")
    installIfNotExists("pscl")
    installIfNotExists("ROCR")
    installIfNotExists("corrplot")
    installIfNotExists("MASS")
    installIfNotExists("gvlma")
    installIfNotExists("tictoc")
    installIfNotExists("forecast")
    installIfNotExists("tseries")
    installIfNotExists("QuantTools")
    installIfNotExists("MASS")
    installIfNotExists("mhsmm")
    installIfNotExists("imputeTS")
    installIfNotExists("car")
    installIfNotExists("mice")
    installIfNotExists("segmented")
    installIfNotExists("zoo")
    installIfNotExists("polynom")
    installIfNotExists("kohonen")
    installIfNotExists("RColorBrewer")
    installIfNotExists("reshape2")
    installIfNotExists("xts")
    installIfNotExists("mlbench")
    installIfNotExists("gridExtra")
    installIfNotExists("kernlab")
    installIfNotExists("RSNNS")
    installIfNotExists("Hmisc")
    installIfNotExists("quantmod")
}
bringInData = function(fileName) {
    data = read.csv(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    data = data[, colnames(data) %in% c(
        "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D",
        "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO", "Dates")]

    data$Dates = as.Date(data$Dates)
    data$PX_TO_BOOK_RATIO = as.numeric(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M = as.numeric(data$EQY_DVD_YLD_12M)
    data$PX_CLOSE_1D = as.numeric(data$PX_CLOSE_1D)
    data$EARN_YLD = as.numeric(data$EARN_YLD)
    data$CASH_FLOW_YIELD = as.numeric(data$CASH_FLOW_YIELD)
    data$PX_TO_SALES_RATIO = as.numeric(data$PX_TO_SALES_RATIO)

    start = min(which(!is.na(data$PX_CLOSE_1D) == TRUE))
    end = nrow(data)
    data = data[start:end,]

    data$PX_TO_BOOK_RATIO = smoothFactor(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M = smoothFactor(data$EQY_DVD_YLD_12M)
    data$EARN_YLD = smoothFactor(data$EARN_YLD)
    data$PX_TO_SALES_RATIO = smoothFactor(data$PX_TO_SALES_RATIO)
    data$CASH_FLOW_YIELD = smoothFactor(data$CASH_FLOW_YIELD)

    data$PX_TO_BOOK_RATIO[data$PX_TO_BOOK_RATIO < 0] = 0
    data$EQY_DVD_YLD_12M[data$EQY_DVD_YLD_12M < 0] = 0
    data$EARN_YLD[data$EARN_YLD < 0] = 0
    data$CASH_FLOW_YIELD[data$CASH_FLOW_YIELD < 0] = 0
    data$PX_TO_SALES_RATIO[data$PX_TO_SALES_RATIO < 0] = 0

    return(data)
}
fillInMissingsBefore = function(x) {
    previousValue = NA
    for (i in 1:length(x)) {
        if (is.na(x[i])) x[i] = previousValue
        previousValue = x[i]
    }
    return(x)
}
smoothFactor = function(x, order = 22) {
    working = x
    working = fillInMissingsBefore(working)
    working = mav(working, order)
    working = na.approx.default(working, rule = 2)
    working = mav(working, order)
    working = na.approx.default(working, rule = 2)
    return(working)
}
getScore = function(data, values) {
    return
    (
    data$PX_TO_BOOK_RATIO[i] * values$S_PX_TO_BOOK_RATIO
    + data$CASH_FLOW_YIELD[i] * values$S_CASH_FLOW_YIELD
    + data$EARN_YLD[i] * values$S_EARN_YLD
    + data$EQY_DVD_YLD_12M[i] * values$S_EQY_DVD_YLD_12M
    + data$PX_TO_SALES_RATIO[i] * values$S_PX_TO_SALES_RATIO
    )
}
mav <- function(x, n) { filter(x, rep(1 / n, n), sides = 1) }
mavback <- function(x, n) {
    a <- mav(x, 1)
    b <- mav(x, (n + 1))
    c <- (1 / n) * ((n + 1) * b - a)
    return(c)
}
bringInRequirements()

vod = bringInData("4_data/VOD.csv")
mtn = bringInData("4_data/MTN.csv")
oml = bringInData("4_data/OML.csv")
slm = bringInData("4_data/SLM.csv")

minLength = min(c(nrow(vod), nrow(mtn), nrow(oml), nrow(slm))) - 1
vod = vod[(nrow(vod) - minLength):nrow(vod),]
mtn = mtn[(nrow(mtn) - minLength):nrow(mtn),]
oml = oml[(nrow(oml) - minLength):nrow(oml),]
slm = slm[(nrow(slm) - minLength):nrow(slm),]

data = read.csv(file = "C:\\Working\\results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
data$Timestamp = as.Date(data$Timestamp)
data$ReturnsBeatMeanReturns = ifelse(data$Returns - data$MeanReturns > 0, 1, 0)
data$TimestampUsable = numeric(nrow(data))
windowPeriod = 22
for (i in 1:nrow(data)) {
    index = i + windowPeriod + 1
    data$TimestampUsable[i] = data$Timestamp[index]
}
data$TimestampUsable = as.Date(data$TimestampUsable)
data = data[which(!is.na(data$TimestampUsable)),]

firstDate = first(data$TimestampUsable)
vod = vod[vod$Dates >= firstDate,]
mtn = mtn[mtn$Dates >= firstDate,]
oml = oml[oml$Dates >= firstDate,]
slm = slm[slm$Dates >= firstDate,]

smoothOrder = 10
data$S_PX_TO_BOOK_RATIO <- smoothFactor(data$S_PX_TO_BOOK_RATIO, smoothOrder)
data$S_EQY_DVD_YLD_12M <- smoothFactor(data$EQY_DVD_YLD_12M, smoothOrder)
data$S_EARN_YLD <- smoothFactor(data$EARN_YLD, smoothOrder)
data$S_CASH_FLOW_YIELD <- smoothFactor(data$CASH_FLOW_YIELD, smoothOrder)
data$S_PX_TO_SALES_RATIO <- smoothFactor(data$PX_TO_SALES_RATIO, smoothOrder)

par(mfrow = c(5, 1))
plot(data$S_PX_TO_BOOK_RATIO, type = "l")
plot(data$S_CASH_FLOW_YIELD, type = "l")
plot(data$S_EARN_YLD, type = "l")
plot(data$S_EQY_DVD_YLD_12M, type = "l")
plot(data$S_PX_TO_SALES_RATIO, type = "l")

amount = 1000000

totalValue = amount
totalValuesNaive = data.frame("PX_CLOSE_1D" = numeric(nrow(vod)))
vodScore = 0.25
mtnScore = 0.25
omlScore = 0.25
slmScore = 0.25
vodShares = totalValue * vodScore / vod$PX_CLOSE_1D[1]
mtnShares = totalValue * mtnScore / mtn$PX_CLOSE_1D[1]
omlShares = totalValue * omlScore / oml$PX_CLOSE_1D[1]
slmShares = totalValue * slmScore / slm$PX_CLOSE_1D[1]
for (i in 1:nrow(vod)) {
    vodWorth = vod$PX_CLOSE_1D[i] * vodShares
    mtnWorth = mtn$PX_CLOSE_1D[i] * mtnShares
    omlWorth = oml$PX_CLOSE_1D[i] * omlShares
    slmWorth = slm$PX_CLOSE_1D[i] * slmShares
    totalValue = vodWorth + mtnWorth + omlWorth + slmWorth
    totalValuesNaive$PX_CLOSE_1D[i] = totalValue

    if (i %% windowPeriod == 1) {
        totalValue = totalValue * 0.999
        vodShares = (vodScore * totalValue) / vod$PX_CLOSE_1D[i]
        mtnShares = (mtnScore * totalValue) / mtn$PX_CLOSE_1D[i]
        omlShares = (omlScore * totalValue) / oml$PX_CLOSE_1D[i]
        slmShares = (slmScore * totalValue) / slm$PX_CLOSE_1D[i]
    }
}

totalValue = amount
totalValues = data.frame("PX_CLOSE_1D" = numeric(nrow(vod)))
allocations = data.frame("VOD" = numeric(nrow(vod)),
                          "MTN" = numeric(nrow(vod)),
                        "OML" = numeric(nrow(vod)),
                        "SLM" = numeric(nrow(vod)),
                        "ISALLOCATED" = numeric(nrow(vod)))
bias = c(1, 1, 1, 1)
oldScoreFactor = 0
defaultSplit = 0.25
orderFactor = 0.25
vodScore = defaultSplit
mtnScore = defaultSplit
omlScore = defaultSplit
slmScore = defaultSplit
vodShares = totalValue * vodScore / vod$PX_CLOSE_1D[1]
mtnShares = totalValue * mtnScore / mtn$PX_CLOSE_1D[1]
omlShares = totalValue * omlScore / oml$PX_CLOSE_1D[1]
slmShares = totalValue * slmScore / slm$PX_CLOSE_1D[1]
oldVodScore = vodScore
oldMtnScore = mtnScore
oldOmlScore = omlScore
oldSlmScore = slmScore
for (i in 1:nrow(vod)) {
    vodWorth = vod$PX_CLOSE_1D[i] * vodShares
    mtnWorth = mtn$PX_CLOSE_1D[i] * mtnShares
    omlWorth = oml$PX_CLOSE_1D[i] * omlShares
    slmWorth = slm$PX_CLOSE_1D[i] * slmShares
    totalValue = vodWorth + mtnWorth + omlWorth + slmWorth
    totalValues$PX_CLOSE_1D[i] = totalValue

    if (i %% windowPeriod == 1) {
        currentDate = vod$Dates[i]
        values = data[data$TimestampUsable <= currentDate,]
        if (nrow(values) > 0) {
            values = last(values)
            if (!is.na(values$CASH_FLOW_YIELD)) {
                totalValue = totalValue * 0.999

                #print(paste(vod$Dates[i], values$TimestampUsable, values$Timestamp))
                #if (values$ReturnsBeatMeanReturns == 1) {
                vodScore = (1 - oldScoreFactor) * bias[1] * getScore(vod, values) + oldScoreFactor * oldVodScore
                mtnScore = (1 - oldScoreFactor) * bias[2] * getScore(mtn, values) + oldScoreFactor * oldMtnScore
                omlScore = (1 - oldScoreFactor) * bias[3] * getScore(oml, values) + oldScoreFactor * oldOmlScore
                slmScore = (1 - oldScoreFactor) * bias[4] * getScore(slm, values) + oldScoreFactor * oldSlmScore
                defaultSplit = mean(c(vodScore, mtnScore, omlScore, slmScore))
                vodScore = (1 - orderFactor) * vodScore + orderFactor * defaultSplit
                mtnScore = (1 - orderFactor) * mtnScore + orderFactor * defaultSplit
                omlScore = (1 - orderFactor) * omlScore + orderFactor * defaultSplit
                slmScore = (1 - orderFactor) * slmScore + orderFactor * defaultSplit

                allocations$ISALLOCATED[i] = 1
                oldVodScore = vodScore
                oldMtnScore = mtnScore
                oldOmlScore = omlScore
                oldSlmScore = slmScore
                total = vodScore + mtnScore + omlScore + slmScore
                vodScore = vodScore / total
                mtnScore = mtnScore / total
                omlScore = omlScore / total
                slmScore = slmScore / total
                #}

                vodShares = (vodScore * totalValue) / vod$PX_CLOSE_1D[i]
                mtnShares = (mtnScore * totalValue) / mtn$PX_CLOSE_1D[i]
                omlShares = (omlScore * totalValue) / oml$PX_CLOSE_1D[i]
                slmShares = (slmScore * totalValue) / slm$PX_CLOSE_1D[i]
            }
        }
    }

    allocations$VOD[i] = vodScore
    allocations$MTN[i] = mtnScore
    allocations$OML[i] = omlScore
    allocations$SLM[i] = slmScore
}

par(mfrow = c(1, 1))
plot(totalValues$PX_CLOSE_1D, type = "l")
lines(totalValuesNaive$PX_CLOSE_1D, type = "l", col = "red")
for (i in 0:(floor(nrow(vod) / 252)))
    abline(v = i * 252)

plot(totalValues$PX_CLOSE_1D - totalValuesNaive$PX_CLOSE_1D, type = "l")
abline(h = 0, col = "red")

allocations = allocations[allocations$ISALLOCATED == 1,]

#par(mfrow = c(4, 1))
#plot(allocations$VOD, type = "l")
#plot(allocations$MTN, type = "l")
#plot(allocations$OML, type = "l")
#plot(allocations$SLM, type = "l")
