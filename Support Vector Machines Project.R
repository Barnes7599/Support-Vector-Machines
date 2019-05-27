library(tidyverse)
library(ggthemes)
library(caTools)

loans <- read.csv("loan_data.csv")

head(loans)
str(loans)
summary(loans)
loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)
loans$credit.policy <- as.factor(loans$credit.policy)

str(loans)
mean(loans$fico)

purpose_of_loans <- loans %>% 
    group_by(purpose) %>% 
    summarise(count = n())

purpose_of_loans <- as.data.frame(purpose_of_loans)
purpose_of_loans

ggplot(loans, aes(fico)) +
    geom_histogram(aes(fill = not.fully.paid), color = 'black', bins = 20) +
    geom_vline(xintercept = 710, color = 'black', linetype = 'dashed') +
    annotate("text", label = 'Mean = 710', x = 750, y = 1100, color = 'black') +
    theme_calc()

ggplot(purpose_of_loans, aes(x = purpose, y = count)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -.5) +
    theme_calc()

ggplot(loans, aes(purpose)) +
    geom_bar(aes(fill = not.fully.paid), position = 'dodge') +
    labs(y = "", x = "", title = 'Number of Loans by Purpose') +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(loans, aes(int.rate, fico)) + 
    geom_point(aes(color = not.fully.paid), size = 1.5, alpha = .5)

## TRAIN TEST SPLIT ##
set.seed(101)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, sample == TRUE)
test <- subset(loans, sample == FALSE)
library(e1071)

model <- svm(loans$not.fully.paid ~ ., data = loans)
summary(model)
str(test)
predicted.values <- predict(model, test[1:13])
head(predicted.values)
table(predicted.values, test$not.fully.paid)


tune.results <- tune(svm, train.x = not.fully.paid ~ ., data = train, kernel = 'radial', ranges = list(cost = c(1,10), gamma = c(0.1,1)))

summary(tune.results)

model <- svm(not.fully.paid ~ ., data = train, cost = 10, gamma = 0.1)

predicted.values <- predict(model, test[1:13])
table(predicted.values, test$not.fully.paid)
