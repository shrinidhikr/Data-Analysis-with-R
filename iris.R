library(e1071)
data(iris)
df=as.data.frame(iris)
Naive_Bayes_Model=naiveBayes(Species ~., df) 
#check every row of survived column ~.
Naive_Bayes_Model
NB_predictions=predict(Naive_Bayes_Model, df)
#generating probablities
NB_predictions
table(NB_predictions,df$Species) #compares generated values with dataframe values


