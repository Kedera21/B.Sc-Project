setwd("C:/Users/USER/Desktop/Project/Data/Maize data")
#library
library(ggplot2)

#data
Yield = read.csv("Maize Yield April fin.csv")

par(mfrow=c(2,2), mai=c(0.75,0.75,0.1,0.1))

#NDVI
plot(Yield$NDVI, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="NDVI", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$NDVI), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$NDVI, Yield$YIELD), 2)), 
     x = 0.58, y = 4.5, cex=1.5)

#EVI
plot(Yield$EVI, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="EVI", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$EVI), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$EVI, Yield$YIELD), 2)), 
     x = 0.325, y = 4.5, cex=1.5)

#GPP
plot(Yield$GPP, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="GPP", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$GPP), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$GPP, Yield$YIELD), 2)), 
     x = 0.35, y = 4.5, cex=1.5)

#Evapotranspiration
plot(Yield$Evapotranspiration, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="Evapotranspiration", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$Evapotranspiration), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$Evapotranspiration, Yield$YIELD), 2)), 
     x = 23, y = 4.5, cex=1.5)

#Rainfall
plot(Yield$Rainfall, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="Rainfall", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$Rainfall), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$Rainfall, Yield$YIELD), 2)), 
     x = 800, y = 4.5, cex=1.5)

#Temperature
plot(Yield$Temperature, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="Temperature", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$Temperature), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$Temperature, Yield$YIELD), 2)), 
     x = 24, y = 4.5, cex=1.5)

#Wind speed
plot(Yield$Wind.Speed, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="Wind.Speed", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$Wind.Speed), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$Wind.Speed, Yield$YIELD), 2)), 
     x = 11, y = 4.5, cex=1.5)

#Air pressure
plot(Yield$Air.Pressure, Yield$YIELD, pch = 16, col = "blue", 
     ylab= "Yield(ton/ha)", xlab="Air.Pressure", cex = 0.9, cex.axis=1.2, cex.lab=1.2)
abline(lm(Yield$YIELD ~ Yield$Air.Pressure), col = "red", lwd = 3)
text(paste("r:", round(cor(Yield$Air.Pressure, Yield$YIELD), 2)), 
     x = 27, y = 4.5, cex=1.5)