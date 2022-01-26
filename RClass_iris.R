data_iris <- iris
str(data_iris)
summary(data_iris)

#install.packages("caTools")
library(caTools)
set.seed(123)
data_split <- sample.split(data_iris$Species,SplitRatio = 0.7)
training_set <- subset(data_iris,data_split==TRUE)
test_set <- subset(data_iris,data_split==FALSE)
nrow(training_set) #105 from 150 (70%)
nrow(test_set) #45 from 150 (30%)
training_set[,1:4] = scale(training_set[,1:4])
training_set[,1:4]
test_set[,1:4] = scale(test_set[,1:4])
test_set[,1:4]

#install.packages("e1071")
library(e1071)
mymodel <- svm(Species~., data = iris)
mymodel

classifier1 = svm(formula = Species~., data = training_set, type = 'C-classification', kernel = 'radial')
classifier1
classifier2 = svm(formula = Species~ Petal.Width + Petal.Length, data = training_set, type = 'C-classification', kernel = 'radial')
classifier2

test_pred1 = predict(classifier1, type = 'response', newdata = test_set[-5])
head(test_pred1)
test_pred2 = predict(classifier2, type = 'response', newdata = test_set[-5])
head(test_pred2)

cm1 = table(test_set[,5], test_pred1)
cm1
cm2 = table(test_set[,5], test_pred2)
cm2

ACC <- sum( diag (cm1) ) / nrow ( test_set )
ACC

