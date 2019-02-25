#weight-height dataset
f=file.choose()
h.data=read.csv(f)
h.data1=h.data[sample(nrow(h.data),1000),] #display data

plot(h.data1$Weight,h.data1$Height, ann = "FALSE", xaxt='n',yaxt='n',col="green")
par(new=TRUE)
#plot the data

h.regression<-lm(Height~Weight,data=h.data1)

#all kind of data is found in Summary function
summary(h.regression)

#add the regression line
abline(h.regression,col="blue")
par(new="TRUE")

#testing set
h.data2=h.data[sample(nrow(h.data),50),]
h.data2
p=vector()
length(p)=nrow(h.data2)
p=predict.lm(h.regression,as.data.frame(h.data2))
p
plot(p,h.data2$Weight,xlab = "Weights" , ylab = "Heights", main = "Predicted Heights based on Weights",col = "dark red")


