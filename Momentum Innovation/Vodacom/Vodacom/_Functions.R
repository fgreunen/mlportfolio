getData <- function(fileName) {
    loess.smoothing.span <- 0.4

    data <- read.csv(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    dates <- data$Dates
    data <- data[, !colnames(data) %in% c(
        "Dates",
        "GOVNCE_DISCLOSURE_SCORE", "SOCIAL_DISCLOSURE_SCORE",
        "ENVIRON_DISCLOSURE_SCORE", "ESG_DISCLOSURE_SCORE", "CUR_MKT_CAP")] # Empty
    data <- data.frame(sapply(data, as.numeric))
    start <- min(which(!is.na(data$PX_CLOSE_1D) == TRUE))
    end <- nrow(data)
    data <- data[start:end,]
    dates <- dates[start:end]

    data <- data.frame(sapply(data, remove_outliers))

    if (all(is.na(c(NA, data$EBIT_TO_NET_SALES)))) data$EBIT_TO_NET_SALES <- 0
    if (all(is.na(c(NA, data$EARN_YLD)))) data$EARN_YLD <- 0
    if (all(is.na(c(NA, data$CASH_FLOW_YIELD)))) data$CASH_FLOW_YIELD <- 0
    if (all(is.na(c(NA, data$PX_TO_SALES_RATIO)))) data$PX_TO_SALES_RATIO <- 0
    if (all(is.na(c(NA, data$BASIC_EPS_3YR_AVG_GROWTH)))) data$BASIC_EPS_3YR_AVG_GROWTH <- 0
    if (all(is.na(c(NA, data$PX_TO_BOOK_RATIO)))) data$PX_TO_BOOK_RATIO <- 0

    data <- data.frame(sapply(data, na.approx.default, rule = 2))
    data <- data.frame(sapply(data, ma, order = 2))
    data <- data.frame(sapply(data, na.approx.default, rule = 2))

    data$RETURN <- sapply(1:nrow(data), function(x) {
        if (x == 1) return(0)
        return((data$PX_CLOSE_1D[x] - data$PX_CLOSE_1D[x - 1]) / data$PX_CLOSE_1D[x - 1])
    })

    data$PX_TO_BOOK_RATIO <- normalize(data$PX_TO_BOOK_RATIO)
    data$EQY_DVD_YLD_12M <- normalize(data$EQY_DVD_YLD_12M)
    data$EARN_YLD <- normalize(data$EARN_YLD)
    data$CASH_FLOW_YIELD <- normalize(data$CASH_FLOW_YIELD)
    data$PX_TO_SALES_RATIO <- normalize(data$PX_TO_SALES_RATIO)
    data$OPER_INC_TO_NET_SALES <- normalize(data$OPER_INC_TO_NET_SALES)
    data$EBIT_TO_NET_SALES <- normalize(data$EBIT_TO_NET_SALES)
    data$X5_YEAR_AVERAGE_ADJUSTED_ROE <- normalize(data$X5_YEAR_AVERAGE_ADJUSTED_ROE)
    data$EPS_GROWTH <- normalize(data$X5_YEAR_AVERAGE_ADJUSTED_ROE)
    data$SALES_3YR_AVG_GROWTH <- normalize(data$X5_YEAR_AVERAGE_ADJUSTED_ROE)
    data$BASIC_EPS_3YR_AVG_GROWTH <- normalize(data$BASIC_EPS_3YR_AVG_GROWTH)

    style_value <-
        (data$PX_TO_BOOK_RATIO
        + data$EQY_DVD_YLD_12M
        + data$EARN_YLD
        + data$CASH_FLOW_YIELD
        + data$PX_TO_SALES_RATIO)
    data$STYLE_VALUE <- normalize(na.approx.default(style_value, rule = 2))
    data$STYLE_VALUE <- predict(loess(STYLE_VALUE ~ i,
        data = data.frame("STYLE_VALUE" = data$STYLE_VALUE, i = 1:nrow(data)), span = loess.smoothing.span))

    style_quality <-
        (data$OPER_INC_TO_NET_SALES
        + data$EBIT_TO_NET_SALES
        + data$X5_YEAR_AVERAGE_ADJUSTED_ROE)
    data$STYLE_QUALITY <- normalize(na.approx.default(style_quality, rule = 2))
    data$STYLE_QUALITY <- predict(loess(STYLE_QUALITY ~ i,
        data = data.frame("STYLE_QUALITY" = data$STYLE_QUALITY, i = 1:nrow(data)), span = loess.smoothing.span))

    data$MOMENTUM_252 <- sapply(1:nrow(data), function(x) {
        prev <- x - 252
        if (prev <= 0 || is.na(data$PX_CLOSE_1D[prev])) return(0)
        return((data$PX_CLOSE_1D[x] - data$PX_CLOSE_1D[prev]) / data$PX_CLOSE_1D[prev])
    })
    data$MOMENTUM_252 <- normalize(na.approx.default(data$MOMENTUM_252, rule = 2))

    data$MOMENTUM_125 <- sapply(1:nrow(data), function(x) {
        prev <- x - 125
        if (prev <= 0 || is.na(data$PX_CLOSE_1D[prev])) return(0)
        return((data$PX_CLOSE_1D[x] - data$PX_CLOSE_1D[prev]) / data$PX_CLOSE_1D[prev])
    })
    data$MOMENTUM_125 <- normalize(na.approx.default(data$MOMENTUM_125, rule = 2))

    style_growth <-
        (data$EPS_GROWTH
        + data$SALES_3YR_AVG_GROWTH
        + data$MOMENTUM_252)
    data$STYLE_GROWTH <- normalize(na.approx.default(style_growth, rule = 2))
    data$STYLE_GROWTH <- predict(loess(STYLE_GROWTH ~ i,
        data = data.frame("STYLE_GROWTH" = data$STYLE_GROWTH, i = 1:nrow(data)), span = loess.smoothing.span))

    style_momentum <- (data$MOMENTUM_125 + data$MOMENTUM_252)
    data$STYLE_MOMENTUM <- normalize(na.approx.default(style_momentum, rule = 2))
    data$STYLE_MOMENTUM <- predict(loess(STYLE_MOMENTUM ~ i,
        data = data.frame("STYLE_MOMENTUM" = data$STYLE_MOMENTUM, i = 1:nrow(data)), span = loess.smoothing.span))

    data$RETURN <- normalize(data$RETURN)

    data$LB_MONTHLY_RETURN <- sapply(1:nrow(data), function(x) {
        prev <- x - 22
        if (prev < 0) return(0)
        return((data$PX_CLOSE_1D[x] - data$PX_CLOSE_1D[prev]) / data$PX_CLOSE_1D[prev])
    })
    data$LB_MONTHLY_RETURN <- normalize(na.approx.default(ma(data$LB_MONTHLY_RETURN, 2), rule = 2))
    data$LB_MONTHLY_VOLATILITY <- sapply(1:nrow(data), function(x) {
        prev <- x - 22
        if (prev < 0) return(0)
        return(sd(data$RETURN[prev:x]))
    })
    data$LB_MONTHLY_VOLATILITY <- normalize(na.approx.default(ma(data$LB_MONTHLY_VOLATILITY, 2), rule = 2))

    data$LF_MONTHLY_RETURN <- sapply(1:nrow(data), function(x) {
        nxt <- x + 22
        if (nxt > nrow(data)) return(0)
        return((data$PX_CLOSE_1D[nxt] - data$PX_CLOSE_1D[x]) / data$PX_CLOSE_1D[x])
    })
    data$LF_MONTHLY_RETURN <- normalize(na.approx.default(ma(data$LF_MONTHLY_RETURN, 2), rule = 2))
    data$LF_MONTHLY_VOLATILITY <- sapply(1:nrow(data), function(x) {
        nxt <- x + 22
        if (nxt > nrow(data)) return(0)
        return(sd(data$RETURN[x:nxt]))
    })
    data$LF_MONTHLY_VOLATILITY <- normalize(na.approx.default(ma(data$LF_MONTHLY_VOLATILITY, 2), rule = 2))

    data$PX_CLOSE_1D <- normalize(data$PX_CLOSE_1D)

    data <- removeHighlyCorrelatedFeatures(data)
    data$Dates <- as.Date(dates)
    data <- data[sample(nrow(data)),] # Randomize ordering.

    data <- data[25:(nrow(data) - 25),]
    return(data)
}