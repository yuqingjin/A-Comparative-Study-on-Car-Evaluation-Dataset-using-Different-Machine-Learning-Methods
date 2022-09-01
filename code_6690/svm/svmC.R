#svm for classification
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

#svm 90%-10%
library(e1071)
svm_model<- svm(ytrain~ ., data = car.train, type='C',cost=3)
svm_pred <- predict(svm_model, car.test)
svm_cm <- table(svm_pred, ytest) #cofusion matrix of 90%-10%
svm_accuracy <- (svm_cm[1,1]+svm_cm[2,2]+svm_cm[3,3]+svm_cm[4,4])/sum(svm_cm)



#svm 66%-34%
library(e1071)

svm_model1<- svm(ytrain1~ ., data = car1.train, type='C')
svm_pred1 <- predict(svm_model1, car1.test)
svm_cm1 <- table(svm_pred1, ytest1) #cofusion matrix of 90%-10%
svm_accuracy1 <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)




#svm 50%-50%
svm_model2<- svm(ytrain2~ ., data = car2.train, type='C')
svm_pred2 <- predict(svm_model2, car2.test)
svm_cm2 <- table(svm_pred2, ytest2) #cofusion matrix of 90%-10%
svm_accuracy2 <- (svm_cm2[1,1]+svm_cm2[2,2]+svm_cm2[3,3]+svm_cm2[4,4])/sum(svm_cm2)



#svm 10-fold cv
library(caret)

folds = createFolds(matrix_alldata$y, k = 10)
sum <- 0
for(i in 1:10){
  fold_test <- matrix_alldata[folds[[i]],]
  fold_train <- matrix_alldata[-folds[[i]],] 
  svm_modelk <- svm(fold_train$y~., data = fold_train, type='C')
  fold_predict <- predict(svm_modelk,type='class',newdata=fold_test)
  svm_cmk <- table(fold_predict, fold_test$y) 
  svm_accuracyk <- (svm_cmk[1,1]+svm_cmk[2,2]+svm_cmk[3,3]+svm_cmk[4,4])/sum(svm_cmk)
  sum <- sum + svm_accuracyk
}
mean_accu <- sum/10



#plot confusion matrix
# barplot(svm_cm, beside=TRUE, legend=TRUE,
#         main='prediction distribution of svm for 90%-10% split',
#         ylim=c(0,140),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(svm_cm1, beside=TRUE, legend=TRUE,
#         main='prediction distribution of svm for 66%-34% split',
#         ylim=c(0,400),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# 
# barplot(svm_cm2, beside=TRUE, legend=TRUE,
#         main='prediction distribution of svm for 50%-50% split',
#         ylim=c(0,600),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))
# 
# barplot(svm_cmk, beside=TRUE, legend=TRUE,
#         main='prediction distribution of svm for 10-fold cross validation',
#         ylim=c(0,200),
#         width=0.6,
#         col=c("brown2", "yellow2","yellowgreen","mediumslateblue"))


#plot confusion matrix
library(yardstick)
library(ggplot2)
#90%-10%
truth_predicted <- data.frame(
  obs = ytest,
  pred = svm_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  labs(title = 'confusion matrix of SVM for 90%-10% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#66%-34%
truth_predicted1 <- data.frame(
  obs = ytest1,
  pred = svm_pred1
)
cm1 <- conf_mat(truth_predicted1, obs, pred)
autoplot(cm1, type = "heatmap") +
  labs(title = 'confusion matrix of SVM for 66%-34% data split')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)


#50%-50%
truth_predicted2 <- data.frame(
  obs = ytest2,
  pred = svm_pred2
)
cm2 <- conf_mat(truth_predicted2, obs, pred)
autoplot(cm2, type = "heatmap") +
  labs(title = 'confusion matrix of SVM for 50%-50% data split')+
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
  labs(title = 'confusion matrix of SVM for 10-fold CV')+
  xlab('true label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right",plot.title=element_text(color='black', hjust=0.5),)




#----------plot accuracy of different kernels using hold-out split---------------
svm_model1<- svm(ytrain2~ ., data = car2.train, type='C',kernel='radial')
svm_pred1 <- predict(svm_model1, car2.test)
svm_cm1 <- table(svm_pred1, ytest2) 
svm_accuracy1_r <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(ytrain2~ ., data = car2.train, type='C',kernel='polynomial')
svm_pred1 <- predict(svm_model1, car2.test)
svm_cm1 <- table(svm_pred1, ytest2)
svm_accuracy1_p <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(ytrain2~ ., data = car2.train, type='C',kernel='linear')
svm_pred1 <- predict(svm_model1, car2.test)
svm_cm1 <- table(svm_pred1, ytest2) 
svm_accuracy1_l <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(ytrain2~ ., data = car2.train, type='C',kernel='sigmoid')
svm_pred1 <- predict(svm_model1, car2.test)
svm_cm1 <- table(svm_pred1, ytest2) 
svm_accuracy1_s <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


#plot accuracy using different kernels 
accuracy <- c(svm_accuracy1_r,svm_accuracy1_p,svm_accuracy1_l,svm_accuracy1_s)
kernel  <- c('radial kernel','polynomial kernel','linear kernel','sigmoid kernel')
df <- data.frame(x = kernel, y = accuracy)
ggplot(df,aes(kernel,accuracy))+
  geom_bar(stat="identity",fill="steelblue",width=0.4)+
  labs(title='Accuracy of different kernels for 50%-50% data split')+
  theme(plot.title=element_text(color='black', hjust=0.5),)


#---------------plot accuracy of different kernels using 10-fold CV---------------------
svm_model1<- svm(fold_train$y~., data = fold_train[,-7], type='C',kernel='radial')
svm_pred1 <- predict(svm_model1, fold_test[,-7])
svm_cm1 <- table(svm_pred1, fold_test[,7]) 
svm_accuracy1_r <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(fold_train$y~., data = fold_train[,-7], type='C',kernel='polynomial')
svm_pred1 <- predict(svm_model1, fold_test[,-7])
svm_cm1 <- table(svm_pred1, fold_test[,7])
svm_accuracy1_p <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(fold_train$y~., data = fold_train[,-7], type='C',kernel='linear')
svm_pred1 <- predict(svm_model1, fold_test[,-7])
svm_cm1 <- table(svm_pred1, fold_test[,7]) 
svm_accuracy1_l <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


svm_model1<- svm(fold_train$y~., data = fold_train[,-7], type='C',kernel='sigmoid')
svm_pred1 <- predict(svm_model1,fold_test[,-7])
svm_cm1 <- table(svm_pred1,  fold_test[,7]) 
svm_accuracy1_s <- (svm_cm1[1,1]+svm_cm1[2,2]+svm_cm1[3,3]+svm_cm1[4,4])/sum(svm_cm1)


#plot accuracy using different kernels
accuracy <- c(svm_accuracy1_r,svm_accuracy1_p,svm_accuracy1_l,svm_accuracy1_s)
kernel  <- c('radial kernel','polynomial kernel','linear kernel','sigmoid kernel')
df <- data.frame(x = kernel, y = accuracy)
ggplot(df,aes(kernel,accuracy))+
  geom_bar(stat="identity",fill="steelblue",width=0.4)+
  labs(title='Accuracy of different kernels using 10-fold CV')+
  theme(plot.title=element_text(color='black', hjust=0.5),)


