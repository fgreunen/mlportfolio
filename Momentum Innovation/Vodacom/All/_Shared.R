clearWorkspace <- function(setSeed = TRUE) {
    rm() # Clear the workspace.
    par(mfrow = c(1, 1))
    if (setSeed) set.seed(1231134)
    graphics.off() # Remove old plots
}

installIfNotExists <- function(package.name, repo = NULL) {
    if (!require(package.name, character.only = TRUE)) {
        if (is.null(repo)) { install.packages(package.name) }
        else { install.packages(package.name, repo = repo) }
        }
    library(package.name, character.only = TRUE)
}
mav <- function(x, n) { filter(x, rep(1 / n, n), sides = 1) }

bringInRequirements <- function() {
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

fillInMissingsBefore <- function(x) {
    previousValue <- NA
    for (i in 1:length(x)) {
        if (is.na(x[i])) x[i] <- previousValue
        previousValue <- x[i]
    }
    return(x)
}
smoothFactor <- function(x) {
    working <- x
    working <- fillInMissingsBefore(working)
    working <- mav(working, 22)
    working <- na.approx.default(working, rule = 2)
    working <- mav(working, 22)
    working <- na.approx.default(working, rule = 2)
    return(working)
}

bringInData <- function(fileName) {
    data <- read.csv(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    data <- data[, colnames(data) %in% c(
        "PX_TO_BOOK_RATIO", "EQY_DVD_YLD_12M", "PX_CLOSE_1D",
        "EARN_YLD", "CASH_FLOW_YIELD", "PX_TO_SALES_RATIO", "Dates")]

    data$Dates <- as.Date(data$Dates)
    data$PX_TO_BOOK_RATIO <- as.numeric(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M <- as.numeric(data$EQY_DVD_YLD_12M)
    data$PX_CLOSE_1D <- as.numeric(data$PX_CLOSE_1D)
    data$EARN_YLD <- as.numeric(data$EARN_YLD)
    data$CASH_FLOW_YIELD <- as.numeric(data$CASH_FLOW_YIELD)
    data$PX_TO_SALES_RATIO <- as.numeric(data$PX_TO_SALES_RATIO)

    start <- min(which(!is.na(data$PX_CLOSE_1D) == TRUE))
    end <- nrow(data)
    data <- data[start:end,]

    data$PX_TO_BOOK_RATIO <- smoothFactor(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M <- smoothFactor(data$EQY_DVD_YLD_12M)
    data$EARN_YLD <- smoothFactor(data$EARN_YLD)
    data$PX_TO_SALES_RATIO <- smoothFactor(data$PX_TO_SALES_RATIO)
    data$CASH_FLOW_YIELD <- smoothFactor(data$CASH_FLOW_YIELD)

    return(data)
}