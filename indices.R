setwd("C:/Users/USER/Desktop/Project/Data/Maize data/modis")

indices = read.csv("data.csv")
indices = indices[,-1]

library(MLmetrics)
library(caret)
library(tidyverse)

summary(indices)
colnames(indices)

#random seed
set.seed(1787)

#validation method
fitControll = trainControl(method = "repeatedcv", number = 5,
                           repeats = 10, savePredictions = TRUE)

#set random seed
set.seed(2485)

#model used -> linear regression
LR_I <- train(YIELD ~ .,  method = "lm", 
              trControl = fitControll, data = indices, verbose = FALSE)

print(LR_I)

#mape
mape_LR_I = MAPE(LR_I$pred$pred, LR_I$pred$obs)


#Random Forest
set.seed(8954)

RF_I <- train(YIELD ~ ., data = indices, method = "rf", 
              trControl = fitControll, verbose = FALSE)
print(RF_I)

#MAPE
mape_RF_I = MAPE(RF_I$pred$pred, RF_I$pred$obs)


#Support Vector Machine
set.seed(7546)

SVM_I <- train(YIELD ~ ., data = indices, verbose = FALSE,
               method = "svmLinear2", trControl = fitControll)

print(SVM_I)

#mape
mape_SVM_I = MAPE(SVM_I$pred$pred, SVM_I$pred$obs)

setwd("C:/Users/USER/Desktop/Project/Data/Maize data/modis")

mod_ind = read.csv("indices.csv")
mod_ind = mod_ind[,-1]

preds_ind = predict(LR_I, newdata = mod_ind)
mod_ind$LR = preds_ind

preds_ind2 = predict(RF_I, newdata = mod_ind)
mod_ind$RF = preds_ind2

preds_ind3 = predict(SVM_I, newdata = mod_ind)
mod_ind$SVM = preds_ind3

write.csv(mod_ind, file = "PREDICTED.csv")
