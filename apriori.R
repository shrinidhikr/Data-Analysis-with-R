library(arules)
f=file.choose()
f1=read.csv(f)
data<-f1[2:6] #column 1 excluded since not required
data
for (i in 1:5) #columns iterator
{
  data[,i]<-discretize(data[,i],breaks=2) #discretize gives range values
}
data
rules<-apriori(data,parameter=list(supp=0.5,conf=0.9,target="rules"))

summary(rules)
inspect(rules)

