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
    installIfNotExists("RMongo")
    installIfNotExists("rjson")
    installIfNotExists("prophet")
}