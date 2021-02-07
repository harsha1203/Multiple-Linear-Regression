library(readr)
ToyotaCorolla <- read_csv("D:/Assignment/Multi linear Regression/ToyotaCorolla.csv")
View(ToyotaCorolla)
ToyotaCorolla1<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(ToyotaCorolla1)
sum(is.na(ToyotaCorolla1))
str(ToyotaCorolla1)
boxplot(ToyotaCorolla1$KM)
summary(ToyotaCorolla1)

# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(ToyotaCorolla1)
plot(ToyotaCorolla1)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(ToyotaCorolla1)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(ToyotaCorolla1))
#colinearity not exist.

# The Linear Model of interest
model.ToyotaCorolla1<- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=ToyotaCorolla1)
summary(model.ToyotaCorolla1)
#cc and doors are insignificant

# Prediction based on only CC 
model.ToyotaCorolla1C<-lm(Price~cc,data = ToyotaCorolla1)
summary(model.ToyotaCorolla1C) # CC became significant
attach(ToyotaCorolla1)
# Prediction based on only doors
model.ToyotaCorolla1D<-lm(Price~Doors,data = ToyotaCorolla1)
summary(model.ToyotaCorolla1D) # doors became significant

# Prediction based on CC and Doors
model.ToyotaCorolla1CD<-lm(Price~cc+Doors)
summary(model.ToyotaCorolla1CD) # Both became Insignificant

# So there exists a collinearity problem b/n CC and Doors
### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(ToyotaCorolla1,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

install.packages("psych")
library(psych)
pairs.panels(ToyotaCorolla1)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.ToyotaCorolla1)
install.packages("car")
library(car)
## plotting Influential measures 
influenceIndexPlot(model.ToyotaCorolla1,id.n=3) # index plots for infuence measures
influencePlot(model.ToyotaCorolla1,id.n=3) # A user friendly representation of the above

# Regression after deleting the 81th observation, which is influential observation
model2<-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data=ToyotaCorolla1[-81,])
summary(model.ToyotaCorolla2)


## Variance Inflation factor to check collinearity b/n variables 
vif(model.ToyotaCorolla1)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.ToyotaCorolla1,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "door" variable
layout(matrix(c(1,2,3,4),2,2))
plot(ToyotaCorolla1)
##preparing new model by excluding doors from input
model.ToyotaCorolla3<-lm(Price~Age_08_04+KM+HP+Quarterly_Tax+Weight,data=ToyotaCorolla1[-81,])
summary(model.ToyotaCorolla3)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
#model is following normal distribution
qqPlot(model.ToyotaCorolla1,id.n = 5)  #close to normal
# QQ plot of studentized residuals helps in identifying outlier 
hist(residuals(finalmodel))

# Exponenetial transformation
model4<-lm(log(Price)~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data=ToyotaCorolla1[-81,])
summary(model4)
plot(model4)
qqPlot(model4)

# logarithmic transformation
model5<-lm(Price~log(Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight),data=ToyotaCorolla1[-81,])
summary(model5)
plot(model5)
qqPlot(model5)

# polynomial 2D degree
model6<-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight + (Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)^2,data=ToyotaCorolla1[-81,])
summary(model6)
plot(model6)
qqPlot(model6)

# polynomial 3D degree
model7<-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight + (Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)^2 + (Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)^3,data=ToyotaCorolla1[-81,])
summary(model7)
plot(model7)
qqPlot(model7)
