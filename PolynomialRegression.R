#Import Library
library(caTools)
library(ggplot2)
#import data
data <- read.csv('Polynomial Regression/Position_Salaries.csv')
#delete col 1
data=data[2:3]
#fitting the polynomial regression model to the data set
data$Level2 <- data$Level^2
poly_regressor=lm(formula =Salary ~ .,data =data )
 summary(poly_regressor)
# add level^3 and level^4 for more precision pr<0.05
 data$Level3 <- data$Level^3
 data$Level4 <- data$Level^4
 poly_regressor=lm(formula =Salary ~ .,data =data )
 summary(poly_regressor)
#visualising Polynomial regression model result
 ggplot()+geom_point(aes(x =data$Level,y = data$Salary))+
          geom_line(aes(x = data$Level,y = predict(poly_regressor,newdata =data )),color='Blue')
#predect new data Level=6.5
y_pred=predict(poly_regressor,newdata = data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3,Level4=6.5^4)) 
#visualising the predection
ggplot()+geom_point(aes(x =6.5,y = y_pred))+
  geom_line(aes(x = data$Level,y = predict(poly_regressor,newdata =data )),color='Blue')