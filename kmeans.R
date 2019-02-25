data_iris=iris
index = sample(1:nrow(data_iris),size=0.5*nrow(data_iris))

trainc=data_iris[index,]
testc=data_iris[-index,]

data_iris$Species=NULL

train=data_iris[index,]
test=data_iris[-index,]

train
test

result<-kmeans(train,3)

result$cluster
result$centers

result

table(trainc$Species,result$cluster)

table(testc$Species,result$cluster)

plot(train[c("Petal.Length","Petal.Width")],col=result$cluster)
