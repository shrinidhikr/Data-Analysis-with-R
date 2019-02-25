library(caret)
library(kknn)
library(ggvis)

f = file.choose()
data = read.csv(f)

data$Age = ifelse(is.na(data$Age),ave(data$Age, FUN = function(x) mean(x, na.rm = TRUE)), data$Age)

#data$Embarked = factor(data$Embarked, levels = c("S","C","Q"), labels = c(1,2,3))
#data$Embarked = ifelse(is.na(data$Embarked),mode(data$Embarked), data$Embarked)

train = data[c(3,5,6,7,8,10)]
train$Sex = factor(train$Sex, levels = c("male","female"), labels = c(1,0))

k1<-knn3(Survived~.,data)
k1

f1 = file.choose()
data1 = read.csv(f1)

data1$Age = ifelse(is.na(data1$Age),ave(data1$Age, FUN = function(x) mean(x, na.rm = TRUE)), data1$Age)
data1$Fare = ifelse(is.na(data1$Fare),ave(data1$Fare, FUN = function(x) mean(x, na.rm = TRUE)), data1$Fare)
#data1$Embarked = factor(data1$Embarked, levels = c("S","C","Q"), labels = c(1,2,3))
#data1$Embarked = ifelse(is.na(data1$Embarked),mode(data1$Embarked), data1$Embarked)

test = data1[c(2,4,5,6,7,9)]
test$Sex = factor(test$Sex, levels = c("male","female"), labels = c(1,0))

c1<-factor(data$Survived)
c1
knn3Train(train,test,c1,k=10,prob=TRUE)

