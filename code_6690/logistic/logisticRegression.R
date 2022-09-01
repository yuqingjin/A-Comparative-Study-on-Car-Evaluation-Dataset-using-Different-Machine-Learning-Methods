#logistic  regression for classfication
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


# #get data matrix
matrix_data <- data$matrix_data
matrix_data1 <- data$matrix_data1
matrix_data2 <- data$matrix_data2


#get original data for 10-fold cv
x <- data$x
y <- data$y
matrix_alldata <- data.frame(x,y)

#logistic regression for multi-class classification
library(nnet)
ytrain <- relevel(as.factor(ytrain),ref='acc') #reference is the mininum label by default
logit.model <- multinom(ytrain~.,data=car.train)
logit_pred <- predict(logit.model,newdata = car.test)
logit_cm <- table(logit_pred, ytest) #cofusion matrix of 90%-10%
logit_accuracy <- (logit_cm[1,1]+logit_cm[2,2]+logit_cm[3,3]+logit_cm[4,4])/sum(logit_cm)



ytrain1 <- relevel(as.factor(ytrain1),ref='acc') #reference is the mininum label by default
logit.model1 <- multinom(ytrain1~.,data=car1.train)
logit_pred1 <- predict(logit.model1,newdata = car1.test) 
logit_cm1 <- table(logit_pred1, ytest1) #cofusion matrix of 90%-10%
logit_accuracy1 <- (logit_cm1[1,1]+logit_cm1[2,2]+logit_cm1[3,3]+logit_cm1[4,4])/sum(logit_cm1)


ytrain2 <- relevel(as.factor(ytrain2),ref='acc') #reference is the mininum label by default
logit.model2 <- multinom(ytrain2~.,data=car2.train)
logit_pred2 <- predict(logit.model2,newdata = car2.test) 
logit_cm2 <- table(logit_pred2, ytest2) #cofusion matrix of 90%-10%
logit_accuracy2 <- (logit_cm2[1,1]+logit_cm2[2,2]+logit_cm2[3,3]+logit_cm2[4,4])/sum(logit_cm2)



#10-fold cross-validation
#svm 10-fold
library(caret)
folds = createFolds(matrix_alldata$y, k = 10)
sum <- 0
for(i in 1:10){
  fold_test <- matrix_alldata[folds[[i]],] 
  fold_train <- matrix_alldata[-folds[[i]],] 
  logit_modelk <- multinom(matrix_alldata$y~.,data=matrix_alldata[,-7])
  fold_predict <- predict(logit_modelk,type='class',newdata=fold_test)
  logit_cmk <- table(fold_predict, fold_test$y) 
  logit_accuracyk <- (logit_cmk[1,1]+logit_cmk[2,2]+logit_cmk[3,3]+logit_cmk[4,4])/sum(logit_cmk)
  sum <- sum + logit_accuracyk
}
mean_accu <- sum/10



# #plot
# barplot(logit_cm, beside=TRUE, legend=TRUE,
#         main='prediction distribution of logistic regression for 90%-10% split',
#         ylim=c(0,150),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# #plot
# barplot(logit_cm1, beside=TRUE, legend=TRUE,
#         main='prediction distribution of logistic regression for 66%-34% split',
#         ylim=c(0,400),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# #plot
# barplot(logit_cm2, beside=TRUE, legend=TRUE,
#         main='prediction distribution of logistic regression for 50%-50% split',
#         ylim=c(0,600),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# #plot
# barplot(logit_cmk, beside=TRUE, legend=TRUE,
#         main='prediction distribution of logistic regression for 10-fold cross validation',
#         ylim=c(0,150),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))


#plot confusion matrix
library(yardstick)
library(ggplot2)
#90%-10%
truth_predicted <- data.frame(
  obs = ytest,
  pred = logit_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  labs(title = 'confusion matrix of Logistic Regression for 90%-10% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#66%-34%
truth_predicted1 <- data.frame(
  obs = ytest1,
  pred = logit_pred1
)
cm1 <- conf_mat(truth_predicted1, obs, pred)
autoplot(cm1, type = "heatmap") +
  labs(title = 'confusion matrix of Logistic Regression for 66%-34% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#50%-50%
truth_predicted2 <- data.frame(
  obs = ytest2,
  pred = logit_pred2
)
cm2 <- conf_mat(truth_predicted2, obs, pred)
autoplot(cm2, type = "heatmap") +
  labs(title = 'confusion matrix of Logistic Regression for 50%-50% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#10-fold cross validation
truth_predicted3 <- data.frame(
  obs = fold_test$y,
  pred = fold_predict
)
cm3 <- conf_mat(truth_predicted3, obs, pred)
autoplot(cm3, type = "heatmap") +
  labs(title = 'confusion matrix of Logistic Regression for 10-fold CV')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)



