library(ggplot2)
library(readr)

#dataset from kaggle: https://www.kaggle.com/andonians/random-linear-regression
data <- read.csv('C:/Users/heena/Downloads/trainset.csv')
head(data)
tail(data)
dim(data)
summary(data)
str(data)

#Finding missing values
colSums(is.na(data))
#complete.cases returns vector indicating which cases are complete, i.e., have no missing values.
data = data[complete.cases(data), ]
colSums(is.na(data))


#Plotting box plots to check outliers for x and y values
#'out' shows the values of any data points which lie beyond the extremes 

#Dividing graph into 2
par(mfrow = c(1, 2))
# For x
boxplot(data$x, sub=paste('Outliers', boxplot.stats(data$x)$out))
# For y
boxplot(data$y, sub=paste('Outliers', boxplot.stats(data$y)$out))
#no outliers seen

#seeing correlation between variables (-1 to +1)
cor(data$x, data$y)
#strong correlation seen

#Simple linear regression using lm() function
model <- lm(y~x,data=data)
print(model)
summary(model)

#visualizing results
ggplot() +
  geom_point(aes(x = data$x, y = data$y),
             colour = 'lightgrey') +
  geom_line(aes(x = data$x, y = predict(model, newdata = data)),
            colour = 'black') +
  ggtitle('X and Y linear regression') +
  xlab('X') +
  ylab('Y')

testset <- read.csv('C:/Users/heena/Downloads/testset.csv')
#making predictions for test set
y_pred = predict(model, newdata = testset)
head(y_pred)

#for specific value
a <- data.frame(x=21)
result <- predict(model,a)
print(result)

#visualizing results of test set
ggplot() +
  geom_point(aes(x = testset$x, y = testset$y),
             colour = 'lightgrey') +
  geom_line(aes(x = testset$x, y = predict(model, newdata = testset)),
            colour = 'black') +
  ggtitle('X and Y linear regression-test set') +
  xlab('X') +
  ylab('Y')

#accuracy
cor(testset$y,y_pred) 
#make actuals_predicteds dataframe 
actuals_preds <- data.frame(cbind(actuals=testset$y, predicteds=y_pred)) 
head(actuals_preds)

#mean absolute percentage deviation
err <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
err

# Loading the actual and predicted values into a new dataframe
final <- data.frame (testset$y, y_pred)

# Scatterplot of actual and predicted values
plot(final, xlab = "Actual values", ylab = "Predicted values", pch = 20)
