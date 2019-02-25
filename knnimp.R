f=file.choose()
f1=read.csv(f)
data=f1[,1:2]
data
a=as.matrix(data)
a

#choosing from csv file
x=a[nrow(data),1]
y=a[nrow(data),2]
x
y

#choose from input
#age=readline(prompt="Enter age")
#age=as.integer(age)
#loan=readline(prompt="Enter loan amt")
#loan=as.integer(loan)
#age
#loan

dist=vector()
length(dist)=nrow(data)-1
for(i in 1:nrow(data)-1)
{
  dist[i]=sqrt(((x-a[i,1])^2)+((y-a[i,2])^2))
}

dist
min(dist)
neighbour=which.min(dist)
neighbour
print(paste("The customer with age :",x,"who applied for loan amount:",y,"may pay the loan(y/n) recommended by our system using nearest neighbour method is", a[neighbour,2]))

