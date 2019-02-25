#grades dataset
library(caret)
library(kknn)
library(ggvis)
f=file.choose()
f1=read.csv(f)
k1<-knn3(grade~.,f1)
k1
train<-rbind(f1[1:3,2:3],f1[4:6,2:3])
train
test<-f1[1:3,2:3]
test
c1<-factor(c(rep("A",3),rep("C",3)))
c1
knn3Train(train,test,c1,k=5,prob=TRUE)



#titanic datset
library(caret)
library(kknn)
library(ggvis)

f = file.choose()
f1 = read.csv(f)
f1 = na.omit(f1)
View(f1)

data = f1[-c(4,9,11,12)]
levels(data$Sex) = c(1,0)
View(data)

k1<-knn3(Survived~.,data)
k1
train<-data[-1]
View(train)

train1=train[-1]
View(train1)

fc = file.choose()
test<- read.csv(fc)
test = na.omit(test)
View(test)
nrow(test)
c1<-factor(train[,1])
c1
knn3Train(data.frame(train1),data.frame(test),c1,k=3,prob=TRUE)

