data<-iris
View(data)
plot(data$Sepal.Length ~ data$Petal.Length, 
     main = "Iris" ,ylab = "Sepal" ,xlab = "Petal", col = data$Species , pch=5)
hist(data$Sepal.Length,col="red",xlab = "Sepal")
boxplot(data$Sepal.Length~data$Species)

library(ggplot2)
f=file.choose()
f1=read.csv(f)
f1=f1[-c(1,2)]
View(head(f1))
ggplot(data=f1,aes(x=price))+geom_histogram(bins = 30, fill ="#800fff", col="black")
ggplot(data=f1,aes(x=price,fill=factor(air_cond)))+geom_histogram(bins = 30,
                                                                  position = "fill")
ggplot(data=f1,aes(x=waterfront,fill=factor(air_cond)))+geom_bar(position = "fill")
ggplot(data=f1,aes(x=waterfront,fill=factor(sewer)))+geom_bar(position = "fill")
