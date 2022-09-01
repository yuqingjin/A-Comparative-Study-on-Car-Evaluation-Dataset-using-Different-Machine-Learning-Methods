#preproccessing
#naive Bayes
#rm(list=ls())

data_preprocess <- function(){

#import data
data <- read.table("C:/Users/wsy/Desktop/project_6690/data/car.data")

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

temp5 <- sub("small","1",data1[,5])
temp5 <- sub("med","2",temp5)
temp5 <- sub('big','3',temp5)

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
# class <- data1[,7]
# #convert y into number
# temp7 <- sub("unacc","0",class)
# temp7 <- sub("acc","1",temp7)
# temp7 <- sub("vgood","3",temp7)
# temp7 <- sub("good","2",temp7)
# temp7 <- as.numeric(temp7)
# y <- temp7

y <- data1[,7]
y <- as.factor(y)


#data standarization and centralization
x_scale <- scale(x,center=T,scale=T)

#min-max normalization
x_min<-apply(x_scale, 2, min)
center <- sweep(x, 2, x_min,'-')
x_max<-apply(x, 2, max)
R <- x_max - x_min
x1<- sweep(center, 2, R, "/")
x1 <- data.frame(x1)


#data set split
set.seed(13)#13

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

pre_result <- list(car.train=car.train,ytrain=ytrain,
                   car.test=car.test,ytest=ytest,
                   car1.train=car1.train,ytrain1=ytrain1,
                   car1.test=car1.test,ytest1=ytest1,
                   car2.train=car2.train,ytrain2=ytrain2,
                   car2.test=car2.test,ytest2=ytest2,
                   matrix_data=matrix_data,
                   matrix_data1=matrix_data1,
                   matrix_data2=matrix_data2,
                   x=x1,y=y
                   )
return (pre_result)

}