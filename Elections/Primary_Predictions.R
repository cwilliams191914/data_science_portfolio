# NEED TO MAKE SURE I SPLIT TRAINING AND TESTING SET TO HAVE SAME PROPORTION OF WINS FOR EACH CANDIDATE
# AND SAME PROPORTION OF EACH STATE

#LOADS OF MISSING DATA FROM MERGE

require(dplyr)
require(caTools)

#read data in
primary_data = read.csv('primary_winner.csv')
county_data = read.csv('county_data.csv')

#Question 1:  merge the primary data and the county data using an inner join
data = primary_data %>% 
  inner_join(county_data, by = 'fips')

#Question 2: count number of counties won by candidate
primary_data %>% 
  group_by(winner) %>% 
  summarise(number_wins = n())

#Question 3: split into training and testing sets with 80% of data in the training data set
#Hint 1: make sure to split on column winner to ensure the same proportion of winners in training and testing sets
set.seed(123)
split = sample.split(data$winner, SplitRatio = 0.8)
training_data = data[split,]
testing_data = data[!split,]

#check the proportions held
training_data %>% 
  group_by(winner) %>% 
  summarise(percent_won = n()/nrow(training_data))
testing_data %>% 
  group_by(winner) %>% 
  summarise(percent_won = n()/nrow(testing_data))

#Question 4:  what are the levels of the factor winner?
levels(data$winner)

#Question 5: build a logistic regression model to predict winners
logistic_model = glm(winner ~ age_over_65 +
                       female_perc +
                       foreign_born_perc +
                       bachelors_perc +
                       household_income,
                     data = data,
                     family = binomial)

#Question 6:  look at the summary of the model, refit the model dropping the least significant variable
summary(logistic_model)
logistic_model = glm(winner ~ age_over_65 +
                       female_perc +
                       foreign_born_perc +
                       bachelors_perc,
                     data = data,
                     family = binomial)

#Question 7:  predict the probability for the training data set
training_data$winner_prediction_perc = predict(logistic_model, training_data, type = 'response')

#Question 8: try 0.4, 0.5, 0.6 and 0.7 as the thresholds then use the one with the best
#prediction rate
for(i in c(0.4, 0.5, 0.6, 0.7)){
  training_data = training_data %>% 
    mutate(winner_prediction = ifelse(winner_prediction_perc > i, 'Hillary Clinton', 'Bernie Sanders'))
  print(sum(as.character(training_data$winner) == training_data$winner_prediction)/
          nrow(training_data))
}

#Question 9: calculate the consufion matrix for the training set using the best threshold
training_data = training_data %>% 
  mutate(winner_prediction = ifelse(winner_prediction_perc > 0.6, 'Hillary Clinton', 'Bernie Sanders'))
#Confusion matrix
table(training_data$winner, training_data$winner_prediction)

#Question 10:  predict the winner for the testing set
testing_data$winner_prediction_perc = predict(logistic_model, testing_data, type = 'response')
testing_data = testing_data %>% 
  mutate(winner_prediction = ifelse(winner_prediction_perc > 0.6, 'Hillary Clinton', 'Bernie Sanders'))

#Question 11: calcualte the Confusion matrix and the correct prediction percentage
#Confusion matrix
test_confusion = table(testing_data$winner, testing_data$winner_prediction)
sum(diag(test_confusion))/ sum(test_confusion)
