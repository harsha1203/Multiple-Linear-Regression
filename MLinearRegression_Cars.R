Cars <- read.csv(file.choose())
attach(Cars)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Cars)
#install.packages("Hmisc")
#library(Hmisc)

# 7. Find the correlation between Output (MPG) & inputs (HP, VOL, SP, WT) - SCATTER DIAGRAM
pairs(Cars)

# 8. Correlation coefficient Matrix - Strength & Direction of correlation
cor(Cars)

# The Linear Model of interest
model.car <- lm(MPG~VOL+HP+SP+WT)
summary(model.car)

library(car)
vif(model.car) # variance inflation factor

model2 <- lm(MPG~VOL+HP+SP)
summary(model2)

n=nrow(Cars)
n1=n*0.7
n2=n-n1
train=sample(1:n,n1)
test=Cars[-train,]

pred=predict(model2,newdat=test)
actual=test$MPG
error=actual-pred

test.rmse=sqrt(mean(error**2))
test.rmse

train.rmse = sqrt(mean(model2$residuals**2))
train.rmse


