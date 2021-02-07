library(readr)
Corolla <- read_csv("D:/Assignment/Multi linear Regression/ToyotaCorolla.csv")
View(ToyotaCorolla)
Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
