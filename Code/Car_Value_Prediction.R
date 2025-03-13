library(tidyverse)
library(skimr)
library(readxl)
library(moderndive)
library(ISLR)
library(ggplot2)
library(car)

getwd()

CarData <- read_excel("CarValues.xlsx")

CarData

CarData1 <- subset(CarData, select =-Car ) 
CarData1

head(CarData1)

str(CarData1)

summary(CarData1$Size)

CarData1$Family_Sedan_Dummy <- ifelse(CarData1$Size == 'Family Sedan', 1, 0) #Creating Dummy Variable
CarData1$Upscale_Sedan_Dummy <- ifelse(CarData1$Size == 'Upscale Sedan', 1, 0) #Creating Dummy Variable

names(CarData1) 

head(CarData1)
tail(CarData1)

names(CarData1)[2] = "Price"
names(CarData1)[3] = "CostPerMile"
names(CarData1)[5] = "Predicted_Reliability"
names(CarData1)[4] = "Road_Test_Score"

names(CarData1)[6] = "value"

names(CarData1)[6]

names(CarData1) 

table(CarData1$Family_Sedan_Dummy) #Summary of Dummy
table(CarData1$Upscale_Sedan_Dummy) #Summary of Dummy

names(CarData1) 

head(CarData1)

ncol(CarData1)

----------------------------------------------------------------------------------------------------------------------
fit = lm(formula = value ~ ., data = CarData1[,-1])
summary(fit)

----------------------------------------------------------------------------------------------------------------------

n = nrow(CarData1)
set.seed(123)
train.index <- sample(row.names(CarData1), floor(0.8*n))
test.index <- setdiff(row.names(CarData1), train.index)
train.df <- CarData1[train.index, -1]
test.df <- CarData1[test.index, -1]

names(train.df)
names(test.df)

ncol(train.df)
ncol(test.df)
----------------------------------------------------------------------------------------------------------------------

mod1 = lm(formula = value ~ ., data = train.df)
summary(mod1)

preds.mod1 <- predict(mod1, newdata = test.df)
summary(preds.mod1)
MSE1 <- mean((preds.mod1 - test.df$value)^2)
MSE1
RMSE1 <- sqrt(MSE1)
print(RMSE1)

----------------------------------------------------------------------------------------------------------------------
nrow(test.df)

names(CarData1) 
names(test.df) 
names(train.df)
  
mod2 = lm(formula = value ~ ., data = train.df[,-c(6)])
summary(mod2)



preds.mod2 <- predict(mod2, newdata = test.df[,-c(6)])
print(test.df$value)
MSE2 <- mean((preds.mod2 - test.df$value)^2)
print(MSE2)
RMSE2 <- sqrt(MSE2)
print(RMSE2)


----------------------------------------------------------------------------------------------------------------------

names(CarData1) 
names(train.df)
names(test.df) 

mod3 = lm(formula = value ~ ., data = train.df[,-c(6,7)])
summary(mod3)

preds.mod3 <- predict(mod3, newdata = test.df[,-c(6,7)])
MSE3 <- mean((preds.mod3 - test.df$value)^2)
print(MSE3)
RMSE3 <- sqrt(MSE3)
print(RMSE3)

----------------------------------------------------------------------------------------------------------------------
  
names(CarData1)  

mod4 = lm(formula = value ~ ., data = train.df[,-c(1,6,7)])
summary(mod4)

preds.mod4 <- predict(mod4, newdata = test.df[,-c(1,6,7)])
MSE4 <- mean((preds.mod4 - test.df$value)^2)
print(MSE4)
RMSE4 <- sqrt(MSE4)
print(RMSE4)

print(c(RMSE1, RMSE2, RMSE3, RMSE4))

## Minimum = RMSE4


---------------------------------------------------------------------------------------------------------

full.df = rbind(train.df, test.df)
mod.final <- lm(formula = value ~ ., data = full.df[,-c(1,6,7)])
summary(mod.final)

a = 0.44
b = 70
c = 4

2.04325 * a
0.01138 * b
0.16510 * c

Car_Value121 = 1.2443 - 2.04325 * a + 0.01138 * b + 0.16510 * c

Car_Value121




---------------------------------------------------------------------------------------------------------
names(CarData1) 

hist(CarData1$value)
boxplot(CarData1$value)
log(CarData1$Price)
log(CarData1$`Road-Test Score`)

scatterplotMatrix( ~ Price + CostPerMile + value + Predicted_Reliability + Road_Test_Score,
                  regLine = list(col = 2),
                  col = 1, smooth = list(col.smooth = 4, col.spread = 4),
                  data = CarData1[, -1])

CarData1$Price_New = log(CarData1$Price)
CarData1$value_New = (CarData1$value)^2
CarData1$Predicted_Reliability_New = (CarData1$Predicted_Reliability)^2
CarData1$Road_Test_Score_New = log(CarData1$Road_Test_Score)

plot(CarData1$value , CarData1$Price)
plot(CarData1$value_New , CarData1$Price_New)
plot(CarData1$value_New , CarData1$Predicted_Reliability)
plot(CarData1$value_New , CarData1$Road_Test_Score_New)
plot(CarData1$value_New , CarData1$CostPerMile)

---------------------------------------------------------------------------------------------------------

n = nrow(CarData1)
set.seed(123)
train.index <- sample(row.names(CarData1), floor(0.8*n))
test.index <- setdiff(row.names(CarData1), train.index)

names(CarData1)

train.df <- CarData1[train.index, -1] 
test.df <- CarData1[test.index, -1]

---------------------------------------------------------------------------------------------------------------------

names(train.df)  

mod5 = lm(formula = value_New ~ ., data = train.df[,-c(1,3,4,5)])
summary(mod5)

preds.mod5_log <- predict(mod5, newdata = test.df[,-c(1,3,4,5)])
preds.mod5 = exp(preds.mod5_log)
MSE5 <- mean((preds.mod5 - test.df$value_New)^2)
print(MSE5)
RMSE5 <- sqrt(MSE5)
print(RMSE5)

  
  