library(ISLR)
head(iris)
library(e1071)
help("svm")

####
## SUPPORT VECTOR MODEL
model <- svm(Species ~ ., data = iris)
summary(model)
### Dont't predict model on your dataset make sure you train test split the data first. 
pred.values <- predict(model, iris[1:4])
table(pred.values, iris[,5])
# Parameters
# Cost: allows the support vector to have a soft margin, which allows some example to be ignored or place on the opposite side of that margin. Controls the influece of each individual support vector. Trades error penelty for stability
# Gamma, Non-linear kernal function, radial is non-linear large gamma leads to high bias and low variance models.  

# How do we get the computer to choose parameters for us

tune.results <- tune(svm, train.x = iris[1:4], train.y = iris[,5], kernel = 'radial', ranges = list(cost = c(0.5, 1, 1.5), gamma = c(0.1, 0.5, 0.7)))
# train.x = features you want to train on
#train.y = labels for the features
summary(tune.results)
# Cost = 1.5
# Gamma  = 0.1
tuned.svm <-  svm(Species ~ ., data = iris, kernel = 'radial', cost = 1.5, gamma = 0.1)
summary(tuned.svm)

tuned.predicted <- predict(tuned.svm, iris[1:4])
table(tuned.predicted, iris[,5])
