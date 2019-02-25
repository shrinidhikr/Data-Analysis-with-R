# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('kc_house_data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$price, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set = training_set[-c(1,2)]
test_set = test_set[-c(1,2)]

# Feature Scaling
# training_set = as.data.frame(scale(training_set))
# test_set = as.data.frame(scale(test_set))

# Plot data 
# plot(training_set$land_value,training_set$living_area)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = price ~.,
               data = as.data.frame(training_set))
summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = as.data.frame(test_set))
#abline(regressor)


# Backward feature extraction
regressor1 = lm(formula = price ~ land_value + age + waterfront + lot_size + construction + air_cond + living_area + bathrooms,
               data = as.data.frame(training_set))
summary(regressor1)

# Predicting the Test set results
y_pred1 = predict(regressor1, newdata = as.data.frame(test_set))


regressor2 = lm(formula = price ~ land_value + waterfront + construction + air_cond + living_area + bathrooms,
                data = as.data.frame(dataset))
summary(regressor2)

# Predicting the Test set results
y_pred2 = predict(regressor2, newdata = as.data.frame(test_set))


regressor3 = lm(formula = price ~ land_value + living_area ,
                data = as.data.frame(dataset))
summary(regressor3)

# Predicting the Test set results
y_pred3 = predict(regressor3, newdata = as.data.frame(test_set))
