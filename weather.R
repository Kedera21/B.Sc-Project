setwd("C:/Users/USER/Desktop/Project/Data/Maize data")

Weather = read.csv("Weather.csv")
Weather = Weather[,-1]

library(MLmetrics)
library(caret)
library(tidyverse)

#random seed
set.seed(1787)

#validation method
fitControll = trainControl(method = "repeatedcv", number = 5,
                           repeats = 10, savePredictions = TRUE)

#set random seed
set.seed(1945)

#model used -> linear regression
LR_V <- train(YIELD ~ .,  method = "lm", 
            trControl = fitControll, data = Weather, verbose = FALSE)

print(LR_V)

#mape
mape_LR_V = MAPE(LR_V$pred$pred, LR_V$pred$obs)


#Random Forest
set.seed(1254)

RF_V <- train(YIELD ~ ., data = Weather, method = "rf", 
            trControl = fitControll, verbose = FALSE)
print(RF_V)

#MAPE
mape_RF_V = MAPE(RF_V$pred$pred, RF_V$pred$obs)


#Support Vector Machine
set.seed(546)

SVM_V <- train(YIELD ~ ., data = Weather, verbose = FALSE,
             method = "svmLinear2", trControl = fitControll)

print(SVM_V)

#mape
mape_SVM_V = MAPE(SVM_V$pred$pred, SVM_V$pred$obs)

setwd("C:/Users/USER/Desktop/Project/Data/Maize data/weather")

Weth = read.csv("weather 21.csv")
Weth = Weth[,-1]

preds_we = predict(LR_V, newdata = Weth)
Weth$LR = preds_we

preds_we2 = predict(RF_V, newdata = Weth)
Weth$RF = preds_we2

preds_we3 = predict(SVM_V, newdata = Weth)
Weth$SVM = preds_we3

write.csv(Weth, file = "PREDICTIONS Final.csv")