#Anjali Nevgi
#Data science and business analytics internship
#GRIP October 2022


#Task 1
#Prediction using supervised ML

Data=read.table('C:\\Users\\anjal\\Desktop\\data.txt',sep=',',header=TRUE)
View(Data)


#Scatter plot
plot(x,y,xlab='Hours studied per day',ylab='Score',main='Scatter plot of Score and Hours studied',pch=20)
abline(LM,col='blue',lty=3)

cor(x,y)

#From the scatter plot and the correlation coeffiecient we can assume that both variables are positively,linearly related

attach(Data) 
names(Data)
y=Scores #Response variable
x=Hours #Dependent variable

#Splitting data into testing and training data in 7:3 ratio

library(caTools)
set.seed(100)
sample=sample.split(Data$Hours, SplitRatio = 0.7)
train=subset(Data, sample == TRUE)
test=subset(Data, sample == FALSE)
dim(train)

#Fitting Simple Linear Regression to the Training set
lm.r= lm(Scores ~ Hours,train)
coef(lm.r)
  
#Predicting the Test set results
ypred = predict(lm.r, newdata = test)

#Creating a data frame of actual vs predicted scores

Df=data.frame(ypred,test[,2])
colnames(Df)=c('Predicted','Actual')
View(Df)
  
library(ggplot2)
  
#Visualising the Training set results
ggplot() + geom_point(aes(x = train$Hours,y = train$Score), colour = 'green') +geom_line(aes(x = train$Hours,y = predict(lm.r, newdata = train)), colour = 'blue') +ggtitle('Score vs Hours studied (Training set)') +xlab('Hours studied') +ylab('Score')
  
#Visualising the Test set results
ggplot() + geom_point(aes(x = test$Hours, y = test$Score),colour = 'red') +geom_line(aes(x = train$Hours,y = predict(lm.r, newdata = train)),colour = 'blue') +ggtitle('Score vs Hours studied (Test set)') +xlab('Hours studied') +ylab('Score')

#Predict the score of a student that studies for 9.5 hours per day


pred=predict(lm.r, data.frame(Hours = 9.5))
pred

p=c('The predicted percentage score of a student that studies for 9.5 hours a day is',pred)
print(p)

#Evaluating the model

par(mfrow=c(2,2))
plot(lm.r)

#The residuals are equally distributed around 0, hence our fitted model is acceptable








