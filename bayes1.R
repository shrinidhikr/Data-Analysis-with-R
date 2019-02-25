library(e1071)
data(Titanic)
Titanic_df=as.data.frame(Titanic)
Naive_Bayes_Model=naiveBayes(Survived ~., Titanic_df) 
#check every row of survived column ~.
Naive_Bayes_Model
NB_predictions=predict(Naive_Bayes_Model,Titanic_df)
#generating probablities
NB_predictions
table(NB_predictions,Titanic_df$Survived) 
#compares generated values with dataframe values

