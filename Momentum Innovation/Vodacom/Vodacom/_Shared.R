normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
    H <- 5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

installIfNotExists <- function(package.name, repo = NULL) {
    if (!require(package.name, character.only = TRUE)) {
        if (is.null(repo)) { install.packages(package.name) }
        else { install.packages(package.name, repo = repo) }
    }
    library(package.name, character.only = TRUE)
}

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
}

clearWorkspace <- function(setSeed = TRUE) {
    rm() # Clear the workspace.
    par(mfrow = c(1, 1))
    if (setSeed) set.seed(1231134)
    graphics.off() # Remove old plots
}

removeHighlyCorrelatedFeatures <- function(working) {
    tmp <- cor(working)
    tmp[upper.tri(tmp)] <- 0
    diag(tmp) <- 0
    working <- working[, !apply(tmp, 2, function(x) any(x > 0.995))]
    return(working)
}

