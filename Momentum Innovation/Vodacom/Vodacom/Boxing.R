source("_Shared.R")
source("_Functions.R")
source("_ColourPalettes.R")
source("_SOM.R")
bringInRequirements()

data <- getData("VOD.csv")
data <- data[order(data$Dates),]

span <- 22
numberOfSpans <- floor(nrow(data) / span)
workedData <- data.frame(
           "START_PRICE" = numeric(numberOfSpans),
           "TARGET" = numeric(numberOfSpans),
           "STYLE_VALUE" = numeric(numberOfSpans),
           "STYLE_QUALITY" = numeric(numberOfSpans),
           "STYLE_MOMENTUM" = numeric(numberOfSpans),
           "STYLE_GROWTH" = numeric(numberOfSpans))
for (i in 1:numberOfSpans) {
    startIndex <- (i - 1) * span + 1
    endIndex <- i * span
    workedData$START_PRICE[i] <- data$PX_CLOSE_1D[startIndex]
    workedData$STYLE_VALUE[i] <- data$STYLE_VALUE[startIndex]
    workedData$STYLE_QUALITY[i] <- data$STYLE_QUALITY[startIndex]
    workedData$STYLE_MOMENTUM[i] <- data$STYLE_MOMENTUM[startIndex]
    workedData$STYLE_GROWTH[i] <- data$STYLE_GROWTH[startIndex]
    workedData$TARGET[i] <- (data$PX_CLOSE_1D[endIndex] - workedData$START_PRICE[i]) / workedData$START_PRICE[i]
}

trainingIndices <- 1:floor(0.8 * nrow(workedData))
testIndices <- (1 + max(trainingIndices)):nrow(workedData)

workedDataTraining <- workedData[trainingIndices,]
workedDataTest <- workedData[testIndices,]

training.control <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 1,
                                 savePred = TRUE, verboseIter = TRUE)
tune.grid <- expand.grid(.layer1 = c(2), .layer2 = c(0), .layer3 = c(0), .decay = c(0.1))
data.fit <- caret::train(TARGET ~ ., data = workedDataTraining, linout = 1, tuneGrid = tune.grid,
                  method = "mlpWeightDecayML", trControl = training.control, maxIter = 1500)
data.predict <- predict(data.fit, workedDataTest)
result <- caret::postResample(obs = workedDataTest$TARGET, pred = data.predict)
plot(data.predict, workedDataTest$TARGET,
     xlim = c(min(workedDataTest$TARGET), max(workedDataTest$TARGET)),
     ylim = c(min(workedDataTest$TARGET), max(workedDataTest$TARGET)))
abline(a = 0, b = 1)
#df_diag <- data.frame(residuals = data.fit$finalModel$residuals,
#fitted = data.fit$finalModel$fitted.values)
#ggplot(data = df_diag, aes(x = fitted, y = residuals)) + geom_point()
print(result)