# Million songs dataset
# Clustering based on Tempo and Loudness of the songs using k-means
library(ggplot2)
f = file.choose()
f1 = read.csv(f)
set.seed(20)

# Choose to ignore missing data
# f1 = na.omit(f1)
data = f1[-c(2,3,4)]

# Choosing to filling avg values instead of 'NaN'
for(i in 2:ncol(data))
{
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Remove duration of songs, since it does not contribute to clustering 
data = data[-7]

# Taking samples of data
#index = sample(1:nrow(data),size=0.03*nrow(data))

# Required dataset with all necessary attributes useful for clustering model
data1 = data[c(1,2,3,4,5,6)]
summary(data1)

train = data1[sample(nrow(data1),300),1:6]
actual = train$genre
train$genre=NULL

# Apply feature scaling
#train = scale(train)
#View(train)

# Result and Summary of kmeans clustering
# Initially considering k value to be equal to number of genre in dataset, to see any realation 
km = length(unique(data$genre))
result = kmeans(train,km)
result
str(result)
summary(result)

# Regular plot of clusters
table(actual,result$cluster)
plot(train[c("loudness","tempo")],col=result$cluster)

# Using ggplot to represent clusters with it's central mean locations and the data-points grouped around them
x <- tapply(train$loudness,result$cluster,mean)
y <- tapply(train$tempo,result$cluster,mean)
kcenters <- data.frame(x,y)
ggplot(train,aes(loudness,tempo)) + geom_point(col=result$cluster,size=4)  + geom_point(data=kcenters,aes(x,y),pch=8,size=10,colour=1:10)

# Optimal K value for the model based on elbow method
kmean_withinss <- function(k) {
  outcome <- kmeans(train, k)
  return (outcome$tot.withinss)
}
# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(1, 20, by = 1))

# Using the k value from the elbow-method, again verify the model
clusterk = kmeans(train,5)
clusterk
str(clusterk)
summary(clusterk)
table(actual,clusterk$cluster)
# Using ggplot to represent clusters with it's central mean locations and the data-points grouped around them
x1 <- tapply(train$loudness,clusterk$cluster,mean)
y1 <- tapply(train$tempo,clusterk$cluster,mean)
fkcenters <- data.frame(x1,y1)
ggplot(train,aes(loudness,tempo)) + geom_point(col=clusterk$cluster,size=4)  + geom_point(data=fkcenters,aes(x1,y1),pch=8,size=10,colour=1:5)


# Million songs dataset
# Classification based on genre of the songs using K-nearest neighbour
library(caret)
library(kknn)
library(ggvis)
library(ggplot2)

# choose to ignore missing data
# f1 = na.omit(f1)

dataknn = f1[-c(2,3,4)]

# Choosing to filling avg values instead of 'NaN'
for(i in 2:ncol(dataknn)){
  dataknn[is.na(dataknn[,i]), i] <- mean(dataknn[,i], na.rm = TRUE)
}

# Remove duration of songs
dataknn = dataknn[-7]

# Required dataset with all necessary attributes useful for clustering model
dataknn = dataknn[c(1,2,3,4,5,6)]

# Training the model based on Genre
trainknn = dataknn[sample(nrow(dataknn),30000),]
k1<-knn3(genre~.,trainknn)
k1

# Taking the factor/levels in the genre column
c1 = factor(trainknn$genre)
trainknnc = trainknn
trainknn$genre=NULL

# Apply feature scaling
#trainknn[,2] = scale(trainknn[,2])

# Test set
testknn = dataknn[sample(nrow(dataknn),500),]
testknnc = testknn
actual = testknnc$genre
testknn$genre=NULL

# Model result and Summary 
resultknn = knn3Train(trainknn,testknn,c1,k=12,prob=TRUE)
resultknn

#confusion matrix for actual vs predicted... needs to have same length
cm = as.matrix(table(Actual = actual , Predicted = as.vector(resultknn)))
cm
accuracy = sum(diag(cm))/length(actual)
accuracy
predicted = as.factor(resultknn)
plot(testknnc$genre,predicted)
ggplot(data=testknnc,aes(x=predicted,fill=factor(actual)))+geom_bar(position = "fill")

