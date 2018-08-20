

require(dplyr)
require(caTools)
require(randomForest)

#read data in
data =  read.csv("insurance_claims.csv")

#Question 1: make all variables factors
str(data)
data$previous_claim = as.factor(data$previous_claim)

#Question 2: what is the proprtion of claims?
data %>%
  group_by(claim) %>% 
  summarise(number = n())

#Quesiton 3: split the data into training and testing sets
set.seed(123)
split = sample.split(data$claim, SplitRatio = 0.8)
training_data = data[split,]
testing_data = data[!split,]

#Question 4: build classifier using the training data set
classifier = randomForest(claim ~ bmi + gender + age_bracket + previous_claim,
                          data = training_data,
                          ntree = 100)

#Question 5: predict the results for the trianing data set
training_predict = predict(classifier, newdata = training_data)

#Question 6: calculate the confusion matrix for the training data set
training_confusion = table(training_predict, training_data$claim)

#Question 7: calculate the accuracy on the training data set
sum(diag(training_confusion))/ sum(training_confusion)

#Question 8: predict the results for the testing data set
testing_predict = predict(classifier, newdata = testing_data)

#Question 9: calculate the confusion matric for the testing data set
testing_confusion = table(testing_predict, testing_data$claim)

#Question 10: calculate the accuracy for the testing data set
sum(diag(testing_confusion))/ sum(testing_confusion)
