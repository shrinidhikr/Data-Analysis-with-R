f=file.choose()
f1=read.csv(f)
data<-f1
unique_weather=unique(data[1]) #unique function -> unique entries in the column
unique_play=unique(data[2])
unique_weather
unique_play
likelihood=matrix(0,nrow=nrow(unique_weather),ncol=nrow(unique_play))
#intialising the likelihood matrix to zero
likelihood
for(i in 1:nrow(data))
{
  for(j in 1:nrow(unique_weather))
  {
    if(data[i,1]==unique_weather[j,1])
    {
      for(k in 1:nrow(unique_play))
      {
        if(data[i,2]==unique_play[k,1])
        {
          likelihood[j,k]=likelihood[j,k]+1
        }
      }
    }
  }
}
likelihood
probr=vector()
for(i in 1:nrow(likelihood))
{
  sumrow=0
  for(j in 1:ncol(likelihood))
  {
    sumrow=sumrow+likelihood[i,j]
  }
  probr[i]=sumrow/nrow(data)
}
probr
probc=vector()
for(i in 1:ncol(likelihood))
{
  sumcol=0
  for(j in 1:nrow(likelihood))
  {
    sumcol=sumcol+likelihood[j,i]
  }
  probc[i]=sumcol/nrow(data)
}
probc

