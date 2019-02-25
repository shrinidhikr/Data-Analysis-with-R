mouse.data<-data.frame(weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3))
mouse.data #dispplay data

plot(mouse.data$weight,mouse.data$size,xlab = "Weight",ylab = "Size") 
#plot the data

#draw a linear model 
#y-Values(Mouse Size) = y-intercept+slop * x-values(Mouse Weight)
#Size=y-intercept+Slope*Weight
mouse.regression<-lm(size~weight,data=mouse.data)

#all kind of data is found in Summary function
summary(mouse.regression)

#add the regression line
abline(mouse.regression,col="blue")
