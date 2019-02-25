library(igraph)
g<-graph(c(1,2,1,3,1,4,2,3,3,1,4,2,4,1),directed=TRUE)
g
vcount(g)
ecount(g)
pr_init=vector()
for(i in 1:vcount(g))
{
  pr_init[i]=1/vcount(g)
}
pr_init
out<-degree(g,v=V(g),mode="out")
out
pr_calc=vector()
for(i in 1:vcount(g))
{
  pr_calc[i]=0
}
for(i in 1:vcount(g))
{
  for(j in 1:vcount(g))
  {
    if(i!=j)
    {
      pr_calc[i]=pr_calc[i]+(pr_init[j]/out[j])
    }
  }
}
pr_calc
d=0.85
for(i in 1:vcount(g))
{
  pr_calc[i]=((1-d)/vcount(g))+(d*(pr_calc[i]))
}
pr_calc
