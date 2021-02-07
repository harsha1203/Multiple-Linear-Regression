library(readr)
Computer_Data <- read_csv("D:/Assignment/Multi linear Regression/Computer_Data.csv")
View(Computer_Data)
library(dummies)
attach(Computer_Data)
Computer_Data1=cbind(Computer_Data,dummy(Computer_Data$cd,sep ="_"))
View(Computer_Data1)
str(Computer_Data1)
sum(is.na(Computer_Data))
#from here
multi <- dummy(Computer_Data$multi,sep ="_")
View(multi)
cd <- dummy(Computer_Data$cd,sep = "_")
premium <- dummy(Computer_Data$premium,sep = "_")
Computer_Data1=cbind(Computer_Data,multi,cd,premium)
View(Computer_Data1)
Computer_Data2 <- Computer_Data2[,-1]
Computer_Data2=Computer_Data1[,c(-7:-9,-13,-15,-17)]
View(Computer_Data2)

boxplot(Computer_Data2)
pairs(Computer_Data2)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages('corpcor')
library(corpcor)
cor2pcor(cor(Computer_Data2))

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Computer_Data2, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

names(Computer_Data2)
library(psych)
com<-subset(Computer_Data2, select=c("price","speed","hd","ram","screen","ads"))
pairs.panels(com,col='red')


attach(Computer_Data2)
cor(Computer_Data2) # could not find any multi collinearity
str(Computer_Data2)
model1 <- lm(price~speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no,data = Computer_Data2)
summary(model1) # 77.56
plot(model1)
# p values are significant
#model1 <- lm(price~speed+hd+ram+screen+ads+trend+multi+cd+premium,data = Computer_Data)

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(model1) # Original model

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=2,id.cex=0.7)

#cooks distance
## plotting Influential measures 
install.packages('car')
library(car)
influenceIndexPlot(model1,id.n=3) #1441, 1701
influencePlot(model1.,id.n=3)
influence.measures(model1)
#model2<- lm(price~speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no,data = Computer_Data2[-c(1441, 1701),])
#summary(model2)

model2 <- lm(price ~ ., data = Computer_Data2[-c(1441, 1701),]) #0.78
summary(model2) #0.78
avPlots(model2)

# logarithmic transformation
model3<- lm(price~log(speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no),data = Computer_Data2[-c(1441, 1701),])
summary(model3)
plot(model3) 

# Exponenetial transformation
model4<- lm(log(price)~speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no,data = Computer_Data2[-c(1441, 1701),])
summary(model4) #0.7837
avPlots(model4)
plot(model4)


# polynomial 2D degree
#model4<- lm(log(price)~speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no,data = Computer_Data2[-c(1441, 1701),])
model5 <- lm(price~ speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no + I((speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no)^2),data = Computer_Data2[-c(1441, 1701),])
summary(model5) #78.96
plot(model5)

# polynomial 3D degree
model6 <- lm(price~ speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no + I((speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no)^2) + I((speed+hd+ram+screen+ads+trend+multi_no+cd_no+premium_no)^3),data = Computer_Data2[-c(1441, 1701),])
summary(model6) #79.66
plot(model6)

confint(model6,level=0.95)
pred6<-predict(model6,interval="predict")
pred6
data.frame(Computer_Data2[-c(1441, 1701),],pred6)
model6$residuals
rmse6<-sqrt(mean(model6$residuals^2))
rmse6




























