f=file.choose()
f1=read.csv(f)
data<-f1[2:6]
data

#support of each items purchased as single item
support1_item=vector() #declaring an array
support1_item #empty array
for(i in 1:ncol(data))
{
  support1_item[i]=sum(data[i])/nrow(data)
  if(support1_item[i]>0.5) #golden rule
    print(paste("Support of ",colnames(data[i]),support1_item[i]))
}

#support of 2 items influencing purchase
#onion->potato
#confidence(oUp)=supp(oUp)/supp(o)

for(k in 2:ncol(data))
{
 op=0
 for(j in 1:nrow(data))
 { 
   if(data[j,1]==1&&data[j,k]==1)
   {
     op=op+1
     print(paste(j," ",data[j,1]," ",data[j,k]))
   }
 
 
 }
 
 print(op)
 print(paste(colnames(data[1]),"->",colnames(data[k])))
 
 if(op>=3)
 {
   s=op/(sum(data[1]))
  print(s)
 }
 
}

#onion and potato influncing burger

opb=0

for(i in 1:nrow(data))
{
  if(data[i,1]==1&&data[i,2]==1&&data[i,3]==1)
  {
    opb=opb+1
    print(paste(i," ",data[i,1]," ",data[i,2]," ",data[i,3])) 
  }
}

print(opb)

if(opb>=2)
{
  s=opb/op
  print(s)
}

