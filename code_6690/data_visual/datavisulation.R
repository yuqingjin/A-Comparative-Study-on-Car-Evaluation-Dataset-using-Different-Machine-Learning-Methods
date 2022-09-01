#Importing the data
ds=read.table("E:/6690/car.data")
#Naming the columns 
library(stringr)
ds <- str_split_fixed(ds$V1, ",", 7)
colnames(ds)=c("buying","maint","doors","persons","lug_boot","safety","class")
set.seed(777)
#Summary
library(dplyr)
#dimensions of the dataset
dim(ds)
#simple glimpse of the dataset
glimpse(ds)
#summary of each column
summary(ds)
#Visualisation
library(ggplot2)
ds<-data.frame(ds)
ggplot(ds,aes(x=class,fill=lug_boot))+geom_histogram(stat="count")+labs(title="Class Vs Luggage boot",y="Frequency of Luggage boot",x="Class")

ggplot(ds,aes(x=class,fill=doors))+geom_histogram(stat="count")+labs(title="Class Vs Doors",y="Frequency of Doors",x="Class")

ggplot(data = ds,aes(fill=as.factor(maint),x=class))+geom_density(alpha=0.3)+facet_wrap(~class)
library(caTools)
shuffle_index=sample(1:nrow(ds))
ds=ds[shuffle_index,]
split=sample.split(ds$class,SplitRatio = 0.8)
head(split)
#Splitting the dataset into testing and training parts
training_set=subset(ds,split==TRUE)
testing_set=subset(ds,split==FALSE)

