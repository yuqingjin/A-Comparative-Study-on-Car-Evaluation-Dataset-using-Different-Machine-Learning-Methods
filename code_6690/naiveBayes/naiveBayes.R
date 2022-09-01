#ELEN6690 Project
#naiveBayes for classification
rm(list=ls())

# #import data
# data <- read.table("C:/Users/wsy/Desktop/project_6690/data/car.data")

#load data from preprocessing procedure
#source('C:/Users/wsy/Desktop/project_6690/code/preprocessing.R')
source('preprocessing.R')
data <- data_preprocess()

#split training data and test data into 90% 10%
car.train <- data$car.train
ytrain <- data$ytrain

car.test <- data$car.test
ytest <- data$ytest

#split training data and test data into 66% 34%
car1.train <- data$car1.train
ytrain1 <- data$ytrain1

car1.test <- data$car1.test
ytest1 <- data$ytest1

#split training data and test data into 50% 50%
car2.train <- data$car2.train
ytrain2 <- data$ytrain2

car2.test <- data$car2.test
ytest2 <- data$ytest2

#get original data for 10-fold cv
x <- data$x
y <- data$y

#naive Bayes for classification
library(e1071)

nBayes_model <- naiveBayes(ytrain~ ., data = car.train) 
nB_pred <- predict(nBayes_model, car.test,type="class")
cm <- table(nB_pred, ytest) 
nB_accuracy <- (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4])/sum(cm)


nBayes_model1 <- naiveBayes(ytrain1~ ., data = car1.train) 
nB_pred1 <- predict(nBayes_model1, car1.test)
cm1 <- table(nB_pred1, ytest1) #cofusion matrix of 66%-44%
nB_accuracy1 <- (cm1[1,1]+cm1[2,2]+cm1[3,3]+cm1[4,4])/sum(cm1)



nBayes_model2 <- naiveBayes(ytrain2~ ., data = car2.train) 
nB_pred2 <- predict(nBayes_model2, car2.test)
cm2 <- table(nB_pred2, ytest2) #cofusion matrix of 50%-50%
nB_accuracy2 <- (cm2[1,1]+cm2[2,2]+cm2[3,3]+cm2[4,4])/sum(cm2)


#10-fold cross validation
library(klaR)
library(caret)
y <- as.factor(y)
nBayes_model3 <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))
cmk <- table(predict(nBayes_model3$finalModel,x)$class,y)
nB_accuracyk <- (cmk[1,1]+cmk[2,2]+cmk[3,3]+cmk[4,4])/sum(cmk)


#plot
# barplot(cm, beside=TRUE, legend=TRUE,
#         main='prediction distribution of naive Bayes for 90%-10% split',
#         ylim=c(0,150),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(cm1, beside=TRUE, legend=TRUE,
#         main='prediction distribution of naive Bayes for 66%-34% split',
#         ylim=c(0,400),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(cm2, beside=TRUE, legend=TRUE,
#         main='prediction distribution of naive Bayes for 50%-50% split',
#         ylim=c(0,600),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(cmk, beside=TRUE, legend=TRUE,
#         main='prediction distribution of naive Bayes for 10-fold cross-validation',
#         ylim=c(0,1000),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))

#plot confusion matrix
library(yardstick)
library(ggplot2)

#90%-10%
truth_predicted <- data.frame(
  obs = ytest,
  pred = nB_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
labs(title = 'confusion matrix of Naive Bayes for 90%-10% data split')+
xlab('true label') + ylab('prediction')+
scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#66%-34%
truth_predicted1 <- data.frame(
  obs = ytest1,
  pred = nB_pred1
)
cm1 <- conf_mat(truth_predicted1, obs, pred)
autoplot(cm1, type = "heatmap") +
labs(title = 'confusion matrix of Naive Bayes for 66%-34% data split')+
xlab('true label') + ylab('prediction')+
scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#50%-50%
truth_predicted2 <- data.frame(
  obs = ytest2,
  pred = nB_pred2
)
cm2 <- conf_mat(truth_predicted2, obs, pred)
autoplot(cm2, type = "heatmap") +
  labs(title = 'confusion matrix of Naive Bayes for 50%-50% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#10-fold cross validation
truth_predicted3 <- data.frame(
  obs = y,
  pred = predict(nBayes_model3$finalModel,x)$class
)
cm3 <- conf_mat(truth_predicted3, obs, pred)
autoplot(cm3, type = "heatmap") +
  labs(title = 'confusion matrix of Naive Bayes for 10-fold CV')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)



