library(readr)
Startups <- read_csv("D:/Assignment/Multi linear Regression/50_Startups.csv")
View(Startups)
summary(Startups)
#To Transform the data from Character to Numeric
library(plyr)
Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2"))
class(Startups)
Startups <- as.data.frame(Startups)
class(Startups)
attach(Startups)
plot(Startups)
str(Startups)
Startups$State <- as.numeric(as.character(Startups$State))
str(Startups)
cor(Startups)
pairs(Startups)

summary(Startups)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups))
#colinearity not exist.

plot(MarketingSpend, Profit)# positive relation
plot(Administration, Profit) # looks like no relation
plot(Spend, Profit) # positive relation
plot(State, Profit)
names(Startups)[1]<-"RDSpend"
attach(Startups)

str(Startups)
pairs(Startups)

summary(Startups)
Model1 <- lm(Profit~RDSpend+Administration+MarketingSpend+State,Data=Startups)
summary(Model1)

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
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(Model1) # Original model

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model1,id.n=2,id.cex=0.7)


influence.measures(Model1)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(Model1,id.n=3) # index plots for infuence measures
influencePlot(Model1,id.n=3)

# Regression after deleting the 49th and 50th observation, which is influential observation
Model2 <- lm(Profit~RDSpend+Administration+MarketingSpend+State,Data=Startups[-c(49,50),])
summary(Model2)
plot(Model2)
library(ggplot2)
# we could not find any relation of State with Profit, so we can delete that variable 
Model3 <- lm(Profit~RDSpend+Administration+MarketingSpend,Data=Startups[-c(49,50),])
summary(Model3)

# applying transformation to get better R-squared value
# Logarithmic Transformation
Model4 <- lm(log(Profit)~RDSpend+Administration+MarketingSpend,Data=Startups[-c(49,50),])
summary(Model4)

par(mfrow=c(2,2))
plot(Model4)

pred4<-predict(Model4,interval="predict")
#pred4<-predict(Model4,data.frame(RDSpend=153399,MarketingSpend=407900),interval="predict")
predict(Model4, interval = "confidence")
rmse3 <- sqrt(mean(reg3$residuals^2))
rmse3
# Exponential transformation Transformation
Model5 <- lm(Profit~log(RDSpend+Administration+MarketingSpend),Data=Startups[-c(49,50),])
summary(Model5)

par(mfrow=c(2,2))
plot(Model5)

# polynominal 2D transformation

Model6 <- lm(Profit~(RDSpend+Administration+MarketingSpend)+ I(RDSpend+Administration+MarketingSpend)^2,Data=Startups[-c(49,50),])
summary(Model6)

par(mfrow=c(2,2))
plot(Model6)

# polynominal 3D transformation
Model7 <- lm(Profit~(RDSpend+Administration+MarketingSpend)+ I(RDSpend+Administration+MarketingSpend)^2+ I(RDSpend+Administration+MarketingSpend)^3,Data=Startups[-c(49,50),])
summary(Model7)

par(mfrow=c(2,2))
plot(Model7)








