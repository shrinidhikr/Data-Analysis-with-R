# Million songs dataset
# Classification based on genre of the songs using K-nearest neighbour
library(caret)
library(kknn)
library(ggvis)
library(ggplot2)

f = file.choose()
f1 = read.csv(f)

# choose to ignore missing data
# f1 = na.omit(f1)

data = f1[-c(2,3,4)]

# Choosing to filling avg values instead of 'NaN'
for(i in 2:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Remove duration of songs
data = data[-7]

# Required dataset with all necessary attributes useful for clustering model
data = data[c(1,2,3,4,5,6)]

# Training the model based on Genre
train = data[sample(nrow(data),30000),]
k1<-knn3(genre~.,train)
k1

# Taking the factor/levels in the genre column
c1 = factor(train$genre)
trainc = train
train$genre=NULL

# Apply feature scaling
#train[,2] = scale(train[,2])

# Test set
test = data[sample(nrow(data),500),]
testc = test
actual = testc$genre
test$genre=NULL

# Model result and Summary 
result = knn3Train(train,test,c1,k=12,prob=TRUE)
result
summary(result)

#table for actual vs predicted... needs to have same length
cm = as.matrix(table(Actual = actual , Predicted = as.vector(result)))
cm
predicted = as.factor(result)
plot(testc$genre,predicted)
ggplot(data=testc,aes(x=predicted,fill=factor(actual)))+geom_bar(position = "fill")

