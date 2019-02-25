mouse.tdata<-data.frame(weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),
                        size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3),
                        tail=c(0.7,1.3,0.7,2.0,3.6,3.0,2.9,3.9,4.0))
mouse.tdata #dispplay data

plot(mouse.tdata) 
#plot the data

#draw a multiple model 
#y-Values(Mouse Size) = y-intercept + slope1 * x-values(Mouse Weight) + slope2 * z-values(Mouse tail)
#Size=y-intercept+Slope*Weight+Slope*tail
mouse.mregression<-lm(size~weight+tail,data=mouse.tdata)

#all kind of data is found in Summary function
#check adjusted R square value for multiple
summary(mouse.mregression)
