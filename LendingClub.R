#library imports
library(stringr)
library(dplyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(ggplot2)

#Import data
data = read.csv("/Users/Jake/Projects/LendingClub/LoanStats3a.csv", header=TRUE, stringsAsFactors=FALSE, skip=1)


head(data)
dim(data)

#Drop the description column
data = subset(data, select = -desc)
summary(data)

#We have 42538 observations. A number of columns are primarily NA, so we select for the columns of interest.
#Select all columns from ID to last_credit_pull_d

data = select(data, id:last_credit_pull_d)

#Predicting risky loans is a binary prediction problem. A simple strategy for determining which loans are bad and which are good
#is as follows:

bad = c("Late (31-120 days)", "Default", "Charged Off")

#create a binary indicator of whether a loan is considered bad or good.
data$bad_loans = ifelse(data$loan_status %in% bad, 1, ifelse(data$loan_status=="", NA, 0))
data$bad_loans = factor(data$bad_loans)

#This results in 5634 bad loans from a total of 42538. We can use this column to investigates potential explanatory variables.
#We use, for example, loan_amnt, term, int_rate, grade and fico

ggplot(data = data, aes(x = loan_amnt, color = bad_loans)) + geom_histogram() + 
  ggtitle ("Loan amount and relationship with bad loans")

ggplot(data = data, aes(x = loan_amnt, color = bad_loans)) + geom_freqpoly() + 
  ggtitle("Loan amount and relationship with bad loans")

ggplot(data = data, aes(x = term,  fill =  bad_loans)) + geom_bar() + 
  ggtitle("Loan amount and relationship with term")

ggplot(data = data, aes(x = int_rate, fill = bad_loans)) + geom_bar() + 
  ggtitle("Loan amount and relationship with interest rate") #This chart can be improved with binning into categories.

ggplot(data = data, aes(x = grade, fill = bad_loans)) + geom_bar() + 
  ggtitle("Loan amount and relationship with loan grade") #Potentially the most interesting chart of the ones presented.

#Finally, we create a simple random Forest model to see if we can accurately predict bad loans.

data$renter = ifelse(data$home_ownership == 'RENT', 1, 0)

data_rf = select(data, loan_amnt, term, grade, renter, annual_inc, bad_loans)

summary(data_rf)
data_rf = na.omit(data_rf)

data_rf$grade = factor(data_rf$grade)
data_rf$term = factor(data_rf$term)
data_rf = na.omit(data_rf)

library(caret)
split = createDataPartition(data_rf$bad_loans, p=0.75, list = FALSE)
training = data_rf[split,]
test = data_rf[-split,]

#train model on training set

rf = randomForest(as.factor(bad_loans) ~ loan_amnt + term + grade + renter + annual_inc,
                  type="classification", data=training, importance=TRUE, na.action=na.omit)


data_rf_predict = factor(predict(rf, newdata = test))

#We can see that our model is not yet very useful:
table(data_rf_predict, test$bad_loans)

#In fact, the addition of a fico score makes the problem very solvable. Unfortunately, lending club has scrubbed fico data from 
#its public datasets as the addition of fico data causes privacy issues.
