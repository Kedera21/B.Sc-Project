setwd("C:/Users/USER/Desktop/Project/Data/Maize data")

library(caret)
library(tidyverse)
library(MLmetrics)

Yield = read.csv("Maize Yield April fin.csv")
Yield = Yield[,-1]

str(Yield)
colnames(Yield)

#random seed
set.seed(1988)

#validation method
fitControll = trainControl(method = "repeatedcv", number = 5,
                           repeats = 10, savePredictions = TRUE)

#set random seed
set.seed(1992)

#model used -> linear regression
LR <- train(YIELD ~ .,  method = "lm", 
            trControl = fitControll, data = Yield, verbose = FALSE)

print(LR)

#MAPE
mape_LR = MAPE(LR$pred$pred, LR$pred$obs)

#Random Forest
set.seed(1254)

RF <- train(YIELD ~ ., data = Yield, method = "rf", 
            trControl = fitControll, verbose = FALSE)
print(RF)

#MAPE
mape_RF = MAPE(RF$pred$pred, RF$pred$obs)

#Support Vector Machine
set.seed(546)

SVM <- train(YIELD ~ ., data = Yield, verbose = FALSE,
             method = "svmLinear2", trControl = fitControll)

print(SVM)

#mape
mape_SVM = MAPE(SVM$pred$pred, SVM$pred$obs)

#prediction
Data <- read.csv("2021 weather.csv")
Data = Data[,-1]

preds_LR <- predict(LR, newdata = Data)
Data$LR <- preds_LR

preds_RF <- predict(RF, newdata = Data)
Data$RF <- preds_RF

preds_SVM <- predict(SVM, newdata = Data)
Data$SVM <- preds_SVM

#saving
write.csv(Data, file = 'yields 2021 final.csv')
