#ELEN6690 project
rm(list=ls()) #remove objects from memory

#import data
data <- read.table("/Users/macbook/Desktop/6690project/data/car.data")

#split data into 7 variables
library(stringr)
data1 <- str_split_fixed(data$V1, ",", 7) #split string and return the matrix form
colnames(data1) <- c("buying","maint","doors","persons","lug_boot","safety","class")
y <-data1[,7]

#data preprocessing

#convert variables into numeric attribute
temp1 <- sub("vhigh","4",data1[,1]) #substitute vhigh to 4
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

temp7 <- sub("unacc","0",y)
temp7 <- sub("acc","1",temp7)
temp7 <- sub("vgood","3",temp7)
temp7 <- sub("good","2",temp7)

#convert character into numeric
temp1 <- as.numeric(temp1)
temp2 <- as.numeric(temp2)
temp3 <- as.numeric(temp3)
temp4 <- as.numeric(temp4)
temp5 <- as.numeric(temp5)
temp6 <- as.numeric(temp6)
temp7 <- as.numeric(temp7)

x <- cbind(temp1,temp2,temp3,temp4,temp5,temp6)
colnames(x) <- c("buying","maint","doors","persons","lug_boot","safety")

#min-max normalization
x_min<-apply(x, 2, min)
center <- sweep(x, 2, x_min,'-')
x_max<-apply(x, 2, max)
R <- x_max - x_min   
x1<- sweep(center, 2, R, "/")       
x1 <- data.frame(x1)
matrix_data0 <- data.frame(x1,temp7)

#train 90%, test 10%
train.size <- nrow(x1) * 0.9
train <-sample(1:nrow(x1), train.size)
test <- -train
car.train <- x1[train, ]
car.test <- x1[test, ]
ytrain <- temp7[train]
ytest <- temp7[test]
#matrix input and output
matrix_data <- data.frame(car.train,ytrain)

#train 66%, test 34%
train1.size <- nrow(x1) * 0.66
train1 <-sample(1:nrow(x1), train1.size)
test1 <- -train1
car1.train <- x1[train1, ]
car1.test <- x1[test1, ]
ytrain1 <- temp7[train1]
ytest1 <- temp7[test1]
#matrix input and output
matrix_data1 <- data.frame(car1.train,ytrain1)

#train 50%, test 50%
train2.size <- nrow(x1) /2
train2 <-sample(1:nrow(x1), train2.size)
test2 <- -train2
car2.train <- x1[train2, ]
car2.test <- x1[test2, ]
ytrain2 <- temp7[train2]
ytest2 <- temp7[test2]
#matrix input and output
matrix_data2 <- data.frame(car2.train,ytrain2)


# Method: Multilayer Perceptron
library(keras)
#install_keras()
#install.packages("tensorflow")
library(tensorflow)
#install_tensorflow()

#90%-10%train-test datasplit
#converting the target variable to once hot encoded vectors using keras inbuilt function
train_y<-to_categorical(ytrain,4)
test_y<-to_categorical(ytest,4)

#defining a keras sequential model
model <- keras_model_sequential()
car_train=as.matrix(car.train)
car_test=as.matrix(car.test)
#defining the model with 1 input layer[6 neurons], 1 hidden layer[30 neurons] with dropout rate 0.4 and 1 output layer[1 neurons]
model %>%
  layer_dense(units =6,input_shape=dim(car_train)[2]) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =60) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =4) %>%
  layer_activation(activation = 'softmax')
#compiling the defined model with metric = accuracy and optimiser as adam.
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'Adam',
  metrics = c('accuracy')
)
#summary(model)
#fitting the model on the training dataset
history <- model %>% fit(car_train, train_y, epochs = 100, batch_size = 5)

#plot confusion matrix
pred <- model %>% predict(car_test, batch_size = 5)
colnames(pred) <- c('unacc','acc','good','vgood')
colnames(test_y) <- c('unacc','acc','good','vgood')
#names(pred)[1:4][max.col(pred[, 1:4])]
pred_new = colnames(pred)[apply(pred,1,which.max)]
test_y_new = colnames(test_y)[apply(test_y,1,which.max)]

library(yardstick)
library(ggplot2)
truth_predicted <- data.frame(
  obs = test_y_new,
  pred = pred_new
)
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(car_test, test_y, batch_size = 5)




#66%-34%train-test datasplit
#converting the target variable to once hot encoded vectors using keras inbuilt function
train1_y<-to_categorical(ytrain1,4)
test1_y<-to_categorical(ytest1,4)

#defining a keras sequential model
model1 <- keras_model_sequential()
car1_train=as.matrix(car1.train)
car1_test=as.matrix(car1.test)
#defining the model with 1 input layer[6 neurons], 1 hidden layer[30 neurons] with dropout rate 0.4 and 1 output layer[1 neurons]
model1 %>%
  layer_dense(units =6,input_shape=dim(car1_train)[2]) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =60) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =4) %>%
  layer_activation(activation = 'softmax')
#compiling the defined model with metric = accuracy and optimiser as adam.
model1 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'Adam',
  metrics = c('accuracy')
)
summary(model1)
#fitting the model on the training dataset
model1 %>% fit(car1_train, train1_y, epochs = 100, batch_size = 5)

#plot confusion matrix
pred1 <- model1 %>% predict(car1_test, batch_size = 5)
colnames(pred1) <- c('unacc','acc','good','vgood')
colnames(test1_y) <- c('unacc','acc','good','vgood')
pred1_new = colnames(pred1)[apply(pred1,1,which.max)]
test1_y_new = colnames(test1_y)[apply(test1_y,1,which.max)]

library(yardstick)
library(ggplot2)
truth_predicted1 <- data.frame(
  obs = test1_y_new,
  pred = pred1_new
)
cm1 <- conf_mat(truth_predicted1, obs, pred)
autoplot(cm1, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")

#Evaluating model on the cross validation dataset
loss_and_metrics <- model1 %>% evaluate(car1_test, test1_y, batch_size = 5)




#50%-50%train-test datasplit
#converting the target variable to once hot encoded vectors using keras inbuilt function
train2_y<-to_categorical(ytrain2,4)
test2_y<-to_categorical(ytest2,4)

#defining a keras sequential model
model2 <- keras_model_sequential()
car2_train=as.matrix(car2.train)
car2_test=as.matrix(car2.test)
#defining the model with 1 input layer[6 neurons], 1 hidden layer[30 neurons] with dropout rate 0.4 and 1 output layer[1 neurons]
model2 %>%
  layer_dense(units =6,input_shape=dim(car2_train)[2]) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =60) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =4) %>%
  layer_activation(activation = 'softmax')
#compiling the defined model with metric = accuracy and optimiser as adam.
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'Adam',
  metrics = c('accuracy')
)
summary(model2)
#fitting the model on the training dataset
model2 %>% fit(car2_train, train2_y, epochs = 100, batch_size = 5)

#plot confusion matrix
pred2 <- model %>% predict(car2_test, batch_size = 5)
colnames(pred2) <- c('unacc','acc','good','vgood')
colnames(test2_y) <- c('unacc','acc','good','vgood')
#names(pred)[1:4][max.col(pred[, 1:4])]
pred2_new = colnames(pred2)[apply(pred2,1,which.max)]
test2_y_new = colnames(test2_y)[apply(test2_y,1,which.max)]

library(yardstick)
library(ggplot2)
truth_predicted2 <- data.frame(
  obs = test2_y_new,
  pred = pred2_new
)
cm2 <- conf_mat(truth_predicted2, obs, pred)
autoplot(cm2, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")

#Evaluating model on the cross validation dataset
loss_and_metrics <- model2 %>% evaluate(car2_test, test2_y, batch_size = 5)



#10 fold cross validation with ANN
library(keras)
library(tensorflow)
library(caret)

model <- keras_model_sequential()
#defining the model with 1 input layer[6 neurons], 1 hidden layer[30 neurons] with dropout rate 0.4 and 1 output layer[1 neurons]
model %>%
  layer_dense(units =6,input_shape=6) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =60) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =30) %>%
  layer_activation(activation = 'relu') %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units =4) %>%
  layer_activation(activation = 'softmax')
#compiling the defined model with metric = accuracy and optimiser as adam.
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'Adam',
  metrics = c('accuracy')
)
# Define training control
folds = createFolds(matrix_data0$temp7,k=10)
sum<- 0
for(i in 1:1) {
  fold_test<-matrix_data0[folds[[i]],]
  x_test <- fold_test[c(1:6)]
  x_test=as.matrix(x_test)
  y_test <-fold_test[,7]
  fold_train<-matrix_data0[-folds[[i]],]
  x_train <- fold_train[c(1:6)]
  x_train=as.matrix(x_train)
  y_train <-fold_train[,7]
  y_train<-to_categorical(y_train,4)
  model %>% fit(x_train, y_train, epochs = 100, batch_size = 5)
  fold_predict<-model%>%predict(x_test)
  fold_predict <- as.data.frame.matrix(fold_predict)
  temp=colnames(fold_predict)[apply(fold_predict,1,which.max)]
  y_predict <- sub("V1","0",temp)
  y_predict <- sub("V2","1",y_predict)
  y_predict <- sub("V3","2",y_predict)
  y_predict <- sub('V4','3',y_predict)
  y_predict <- as.numeric(y_predict)
  table_mat3<-table(y_predict, y_test)
  accuracy3<-sum(diag(table_mat3))/sum(table_mat3)
  sum<-sum+accuracy3
}
accuracy_sum <-sum
accuracy_sum
table_mat3


#10-fold cross validation confusion matrix
library(yardstick)
library(ggplot2)

y_test<- as.factor(y_test)
y_predict<- as.factor(y_predict)

truth_predicted3 <- data.frame(
  obs = y_test,
  pred = y_predict
)
cm3 <- conf_mat(truth_predicted3, obs, pred)
autoplot(cm3, type = "heatmap") + xlab('trub label') + ylab('prediction')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+labs(fill="frequency")