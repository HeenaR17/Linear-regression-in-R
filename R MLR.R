library(ggplot2)
library(readr)
library(dplyr)
library(caTools)
library(corrplot)

# dataset fro kaggle
data1 <- read.csv('C:/Users/heena/Downloads/Real estate.csv')

#EDA
head(data1)
tail(data1)
dim(data1)
summary(data)
str(data)

nrow(data1)
ncol(data1)

#datatype of each column
sapply(data1, typeof)

colnames(data1)

#Visualization
ggplot(data1,aes(x = X2.house.age, y = Y.house.price.of.unit.area)) + geom_point() +geom_smooth(method = "lm")
ggplot(data1,aes(x = X3.distance.to.the.nearest.MRT.station, y = Y.house.price.of.unit.area)) + geom_point() +geom_smooth(method = "lm")
ggplot(data1,aes(x = X4.number.of.convenience.stores, y = Y.house.price.of.unit.area)) +geom_smooth(method = "lm")

#renaming columns
data2 <-data1 %>% 
  rename(
   Date=  X1.transaction.date,
   Age_of_house= X2.house.age,
   Distance_to_station= X3.distance.to.the.nearest.MRT.station,
   Convenience_stores= X4.number.of.convenience.stores,
   Latitude= X5.latitude,
   Longitude= X6.longitude,
   House_price= Y.house.price.of.unit.area
  )
colnames(data2)
#Finding missing values
colSums(is.na(data2))
#no null values

#dropping column "No."
data3 = subset(data2, select = -c(No) )
head(data3)

#Splitting into train and test sets
set.seed(123)   
sample = sample.split(data3,SplitRatio = 0.80) 
train =subset(data3,sample ==TRUE)
test=subset(data3, sample==FALSE)

# Multiple Linear Regression
Regression=lm(train$House_price~train$Age_of_house+train$Date+train$Distance_to_station+train$Convenience_stores+train$Latitude+train$Longitude)
print(Regression)
summary(Regression)

coefficients(Regression)

# Null hypothesis :H0 - coefficient is zero (no relation between dependent and independent variable)
# Alternate hypothesis :H1- there is a relation between the two
# p value > 0.05: accept null hypothesis or else reject
# Here we are removing X6 due to high p value(>0.05)
Regression2=lm(train$House_price~train$Age_of_house+train$Date+train$Distance_to_station+train$Convenience_stores+train$Latitude)
summary(Regression2)
print(Regression2)
coef(Regression2)
# residual plots (difference between the observed data of the dependent variable y and the fitted values y)
#Residuals vs Fitted plot gives an indication if there are non-linear patterns
#Residuals should be normally distributed and the Q-Q Plot will show this. If residuals follow close to a straight line on this plot, it is a good indication they are normally distributed
#Scale location plot shows if residuals have equal variance along the regression line
#Leverage plot to identify cases that have high influence in the model
plot(Regression2)

#improving the model by applying log transformation
Regression3=lm(log(train$House_price)~train$Age_of_house+train$Date+train$Distance_to_station+train$Convenience_stores+train$Latitude)
summary(Regression3)
plot(Regression3)
#R squared higher than before. Also points closer to the line in first graph.

data4=subset(data3,select=-c(Longitude))
head(data4)
cor(data4)
#correlation matrix
num_vars <- unlist(lapply(data4, is.numeric))  
dia_nums <- data4[ , num_vars]

dia_corr <- cor(dia_nums)
corrplot(dia_corr, method="color")
corrplot(dia_corr, method="number")

#Date has least correlation with house price. Thus we will remove it and fit the model again.
Regression4=lm(log(train$House_price)~train$DAge_of_house+train$Distance_to_station+train$Convenience_stores+train$Latitude)
summary(Regression4)
plot(Regression4)
#Even if R squared has decreased a little bit, the normality of residuals has gotten better without the data and age columns.


#making predictions for test set
y_pred = predict(Regression4, data.frame(Age_of_house=10,Distance_to_station=100,Convenience_stores=10,Latitude=25))
head(y_pred)


