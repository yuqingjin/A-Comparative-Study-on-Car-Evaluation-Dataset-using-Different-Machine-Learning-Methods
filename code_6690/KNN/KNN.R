#KNN for classification
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

# #get data matrix
matrix_data <- data$matrix_data
matrix_data1 <- data$matrix_data1
matrix_data2 <- data$matrix_data2


#get original data for 10-fold cv
x <- data$x
y <- data$y
matrix_alldata <- data.frame(x,y)


#KNN
#data is already centered and scaled
library(class)
#cl	is the factor of true classifications of training set

#90%-10% split
knn_pred <- knn(train = car.train, test = car.test,cl = ytrain, k=5)
knn_cm <- table(knn_pred,ytest)
knn_accuracy <- (knn_cm[1,1]+knn_cm[2,2]+knn_cm[3,3]+knn_cm[4,4])/sum(knn_cm)


#66%-34%split
knn_pred1 <- knn(train = car1.train, test = car1.test,cl = ytrain1, k=5)
knn_cm1 <- table(knn_pred1,ytest1)
knn_accuracy1 <- (knn_cm1[1,1]+knn_cm1[2,2]+knn_cm1[3,3]+knn_cm1[4,4])/sum(knn_cm1)


#50%-50%  split
knn_pred2 <- knn(train = car2.train, test = car2.test,cl = ytrain2, k=5)
knn_cm2 <- table(knn_pred2,ytest2)
knn_accuracy2 <- (knn_cm2[1,1]+knn_cm2[2,2]+knn_cm2[3,3]+knn_cm2[4,4])/sum(knn_cm2)



#knn 10-fold
library(caret)
folds = createFolds(matrix_alldata$y, k = 10)
sum <- 0
for(i in 1:10){
  fold_test <- matrix_alldata[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- matrix_alldata[-folds[[i]],] # 剩下的数据作为训练集q
  class <- fold_train$y
  fold_predict <- knn(train = fold_train[,-7], test = fold_test[,-7], cl = class, k=5)
  #fold_predict <- predict(knn_modelk,type='class',newdata=fold_test)
  knn_cmk <- table(fold_predict, fold_test$y) 
  knn_accuracyk <- (knn_cmk[1,1]+knn_cmk[2,2]+knn_cmk[3,3]+knn_cmk[4,4])/sum(knn_cmk)
  sum <- sum + knn_accuracyk
}
mean_accu <- sum/10


# #plot
# #plot confusion matrix
# barplot(knn_cm, beside=TRUE, legend=TRUE,
#         main='prediction distribution of KNN for 90%-10% split',
#         ylim=c(0,140),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(knn_cm1, beside=TRUE, legend=TRUE,
#         main='prediction distribution of KNN for 66%-34% split',
#         ylim=c(0,410),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# barplot(knn_cm2, beside=TRUE, legend=TRUE,
#         main='prediction distribution of KNN for 50%-50% split',
#         ylim=c(0,610),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# barplot(knn_cmk, beside=TRUE, legend=TRUE,
#         main='one of prediction distribution of KNN for 10-fold cross validation',
#         ylim=c(0,130),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))


#plot confusion matrix
library(yardstick)
library(ggplot2)
#90%-10%
truth_predicted <- data.frame(
  obs = ytest,
  pred = knn_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  labs(title = 'confusion matrix of KNN for 90%-10% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#66%-34%
truth_predicted1 <- data.frame(
  obs = ytest1,
  pred = knn_pred1
)
cm1 <- conf_mat(truth_predicted1, obs, pred)
autoplot(cm1, type = "heatmap") +
  labs(title = 'confusion matrix of KNN for 66%-34% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#50%-50%
truth_predicted2 <- data.frame(
  obs = ytest2,
  pred = knn_pred2
)
cm2 <- conf_mat(truth_predicted2, obs, pred)
autoplot(cm2, type = "heatmap") +
  labs(title = 'confusion matrix of KNN for 50%-50% data split')+
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
  labs(title = 'confusion matrix of KNN for 10-fold CV')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)






