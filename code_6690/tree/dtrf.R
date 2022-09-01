#ELEN6690 project
rm(list=ls())

#import data
data <- read.table("E:/6690/car.data")

#split data into 7 variables
library(stringr)
data1 <- str_split_fixed(data$V1, ",", 7)
colnames(data1) <- c("buying","maint","doors","persons","lug_boot","safety","class")


#data preprocessing

#convert variables into numeric attribute
temp1 <- sub("vhigh","4",data1[,1]) 
temp1 <- sub("high","3",temp1)
temp1 <- sub("med","2",temp1)
temp1 <- sub('low','1',temp1)

temp2 <- sub("vhigh","4",data1[,2])
temp2 <- sub("high","3",temp2)
temp2 <- sub("med","2",temp2)
temp2 <- sub('low','1',temp2)

#???
temp3 <- sub("5more","5",data1[,3])
temp4 <- sub("more","5",data1[,4])

temp5 <- sub("small","3",data1[,5])
temp5 <- sub("med","2",temp5)
temp5 <- sub('big','1',temp5)

temp6 <- sub("high","3",data1[,6])
temp6 <- sub("med","2",temp6)
temp6 <- sub('low','1',temp6)

#convert character into numeric
temp1 <- as.numeric(temp1)
temp2 <- as.numeric(temp2)
temp3 <- as.numeric(temp3)
temp4 <- as.numeric(temp4)
temp5 <- as.numeric(temp5)
temp6 <- as.numeric(temp6)
x <- cbind(temp1,temp2,temp3,temp4,temp5,temp6)
colnames(x) <- c("buying","maint","doors","persons","lug_boot","safety")

#output
y <- data1[,7]
y<-as.factor(y)
#data standarizatio and centralization
x_scale <- scale(x,center=T,scale=T)

#min-max normalization
x_min<-apply(x_scale, 2, min)
center <- sweep(x, 2, x_min,'-')
x_max<-apply(x, 2, max)
R <- x_max - x_min
x1<- sweep(center, 2, R, "/")
x1 <- data.frame(x1)


#matrix input and output
matrix_data <- data.frame(x1,y)




#data set split
set.seed(11)

#train 90%, test 10%
train.size <- nrow(x1) * 0.9
train <-sample(1:nrow(x1), train.size)
test <- (-train)
car.train <- x1[train, ]
car.test <- x1[test, ]
ytrain <- y[train]
ytest <- y[test]
#matrix input and output
matrix_data <- data.frame(car.train,ytrain)
yt=table(ytest)
yt
#Pie Chart showing the Class Distribution 
acc=yt[1]
acc
good=yt[2]
good
unacc=yt[3]
vgood=yt[4]
library(plotrix)
pie3D(x=c(acc,good,unacc,vgood),radius=1,explode = 0.05,labels = c("Acceptable","Good","Unacceptable","Very Good"),main="Condition testing set 90-10%",start=67)



#train 66%, test 34%
train1.size <- nrow(x1) * 0.66
train1 <-sample(1:nrow(x1), train1.size)
test1 <- (-train1)
car1.train <- x1[train1, ]
car1.test <- x1[test1, ]
ytrain1 <- y[train1]
ytest1 <- y[test1]
#matrix input and output
matrix_data1 <- data.frame(car1.train,ytrain1)

yt=table(ytest1)
yt
acc=yt[1]
acc
good=yt[2]
good
unacc=yt[3]
vgood=yt[4]
pie3D(x=c(acc,good,unacc,vgood),radius=1,explode = 0.05,labels = c("Acceptable","Good","Unacceptable","Very Good"),main="Condition testing set 66-34%",start=67)




#train 50%, test 50%
train2.size <- nrow(x1) /2
train2 <-sample(1:nrow(x1), train2.size)
test2 <- (-train2)
car2.train <- x1[train2, ]
car2.test <- x1[test2, ]
ytrain2 <- y[train2]
ytest2 <- y[test2]
#matrix input and output
matrix_data2 <- data.frame(car2.train,ytrain2)

yt=table(ytest2)
yt
acc=yt[1]
acc
good=yt[2]
good
unacc=yt[3]
vgood=yt[4]
pie3D(x=c(acc,good,unacc,vgood),radius=1,explode = 0.05,labels = c("Acceptable","Good","Unacceptable","Very Good"),main="Condition testing set 50-50%",start=67)



# #10 fold cross validation 
# # Define training control
# set.seed(123)
# library(caret)
# #library(tidyverse)
# # K=10, repeats 10 times
# train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
# # Train the model
# model <- train(y~., data = matrix_data, method = "lm",trControl = train.control)
# # Summarize the results
# #print(model)

#decision tree
library(caret)
library(rpart.plot)
library(rpart)
library(e1071)
# 90%
set.seed(123)
fit<- rpart(ytrain~., data=car.train, method='class')
rpart.plot(fit, extra=8)
y_pred<- predict(fit, car.test, type='class')
table_mat<- table(ytest, y_pred)
table_mat
accuracy <-sum(diag(table_mat))/sum(table_mat)
accuracy

timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)



#plot confusion matrix
library(yardstick)
library(ggplot2)
truth_predicted <- data.frame(
  obs = ytest,
  pred = y_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")





#barplot(table_mat,width=0.6, xlim=c(-1,12),ylim=c(0,150),beside=TRUE,legend=TRUE, main='prediction of decision tree for 90%-10% split',col=c("red","blue","yellow","green"))





#K fold
folds=createFolds(matrix_data$y,k=10)
sum<- 0
for(i in 1:10){
  fold_test<-matrix_data[folds[[i]],]
  fold_train<-matrix_data[-folds[[i]],]
  fit3<-rpart(fold_train$y~.,data=fold_train,method='class')
  fold_predict<-predict(fit3,type='class',newdata=fold_test)
  table_mat3<-table(fold_predict,fold_test$y)
  accuracy3<- sum(diag(table_mat3))/sum(table_mat3)
  sum<-sum+accuracy3
}
accuracy_sum<- sum/10
accuracy_sum
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)


table_mat3
rpart.plot(fit3, extra=4)
#plot confusion matrix
truth_predicted <- data.frame(
  obs = fold_test,
  pred = fold_predict
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(table_mat3, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")



#barplot(table_mat3,width=0.6, xlim=c(-1,12),ylim=c(0,150),beside=TRUE,legend=TRUE, main='prediction of decision tree for 10-folds cross validation',col=c("red","blue","yellow","green"))




#66%
fit1<- rpart(ytrain1~., data=car1.train, method='class')
rpart.plot(fit1, extra=8)
y_pred1<- predict(fit1, car1.test, type='class')
table_mat1<- table(ytest1, y_pred1)
table_mat1
accuracy1 <-sum(diag(table_mat1))/sum(table_mat1)
accuracy1
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)

#plot confusion matrix
truth_predicted <- data.frame(
  obs = ytest1,
  pred = y_pred1
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")

#barplot(table_mat1,width=0.6, xlim=c(-1,12),ylim=c(0,500),beside=TRUE,legend=TRUE, main='prediction of decision tree for 66%-33%',col=c("red","blue","yellow","green"))



# 50%
fit2<- rpart(ytrain2~., data=car2.train, method='class')
rpart.plot(fit2, extra=8)
y_pred2<- predict(fit, car2.test, type='class')
table_mat2<- table(ytest2,y_pred2)
table_mat2
accuracy <-sum(diag(table_mat2))/sum(table_mat2)
accuracy

timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)

truth_predicted <- data.frame(
  obs = ytest2,
  pred = y_pred2
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")


#barplot(table_mat1,width=0.6, xlim=c(-1,12),ylim=c(0,500),beside=TRUE,legend=TRUE, main='prediction of decision tree for 50%-50%',col=c("red","blue","yellow","green"))

df <- data.frame(Decisiontree=c("90-10%", "66-34%", "50-50%","10 folds"),
                 accuracy=c(91.9, 92.9, 94.4,100))
ggplot(df,aes(Decisiontree,accuracy))+geom_bar(stat="identity",fill="steelblue",width=0.5)+labs(title='accuracy using decision tree')






#random forest
#90%
library(randomForest)
set.seed(222)
rf <- randomForest(as.factor(ytrain)~., data=car.train, ntree=500,mtry=3,importance=TRUE)
rf_pred<- predict(rf, car.test, type='class')
mean(rf_pred==ytest)
table(rf_pred, ytest)
ran=table(rf_pred, ytest)
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
#plot confusion matrix



truth_predicted <- data.frame(
  obs = ytest,
  pred = rf_pred
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")



#barplot(ran,width=0.6, xlim=c(-1,13),ylim=c(0,150),beside=TRUE,legend=TRUE, main='prediction of random forest for 90%-10% split',col=c("red","blue","yellow","green"))
plot(randomforest)
summary(ran)

#66%
rf1 <- randomForest(as.factor(ytrain1)~., data=car1.train, ntree=500,mtry=3,importance=TRUE)
rf_pred1<- predict(rf1, car1.test, type='class')
mean(rf_pred1==ytest1)
table(rf_pred1, ytest1)
res1 <- table(rf_pred1,ytest1)
print(res1)
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)

truth_predicted <- data.frame(
  obs = ytest1,
  pred = rf_pred1
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")


#barplot(res1,width=0.6, xlim=c(-1,13),ylim=c(0,500),beside=TRUE,legend=TRUE, main='prediction of random forest for 66%-34% split',col=c("red","blue","yellow","green"))

# 50%
rf2 <- randomForest(as.factor(ytrain2)~., data=car2.train, ntree=500,mtry=3,importance=TRUE)
rf_pred2<- predict(rf2, car2.test, type='class')
r2=mean(rf_pred2==ytest2)
r2
table(rf_pred2, ytest2)
res2 <- table(rf_pred2,ytest2)
print(res2)
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)


truth_predicted <- data.frame(
  obs = ytest2,
  pred = rf_pred2
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")


#barplot(res2,width=0.6, xlim=c(-1,13),ylim=c(0,600),beside=TRUE,legend=TRUE, main='prediction of random forest for 50%-50% split',col=c("red","blue","yellow","green"))


#K fold
folds1=createFolds(matrix_data$y,k=10)
sum1<- 0
for(i in 1:10){
  fold1_test<-matrix_data[folds1[[i]],]
  fold1_train<-matrix_data[-folds1[[i]],]
  rf3<-randomForest(fold1_train$y~.,data=fold1_train, ntree=500, mtry=3, importance=TRUE)
  fold1_predict<-predict(rf3,type='class',newdata=fold1_test)
  table_mat4<-table(fold1_predict,fold1_test$y)
  accuracy4<- mean(fold1_predict==fold1_test$y)
  sum1<-sum1+accuracy4
}
accuracy_sum1<- sum1/10
accuracy_sum1
timestart<-Sys.time()
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)

table_mat4
barplot(table_mat4,width=0.6, xlim=c(-1,13),ylim=c(0,150),beside=TRUE,legend=TRUE, main='prediction of random forest for 10 folds cross validation',col=c("red","blue","yellow","green"))

da <- data.frame(randomforest=c("90-10%", "66-34%", "50-50%","10 folds"),
                 accuracy=c(97.1, 97.6, 95.3,100))
ggplot(da,aes(randomforest,accuracy))+geom_bar(stat="identity",fill="steelblue",width=0.5)+labs(title='accuracy using randomforest')
library(gplots)
yw<-data.matrix(x1)
heatmap(yw,main="CAR Evaluation Data",margins=c(10,12),cexRow = 1,Rowv = NA, Colv = NA)

ggplot(x1,aes(x=class,y=lug_boot))+geom_histogram(stat="count")+labs(title="Class Vs Luggage boot",subtitle="Histogram",y="Frequency of Luggage boot",x="Class")








