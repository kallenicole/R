# Author: Kalle Georgiev
# Date: 12/17/2021
# Purpose: Case III, National City Customer Propensity Model, CSCIE-96

# Import libraries
library(vtreat)
library(dplyr)
library(MLmetrics)
library(ggplot2)
library(ggthemes)
library(caret)
library(e1071)
library(rpart.plot)
library(lubridate)
library(stringr)
library(randomForest)
library(readr)
library(mlbench)
library(rcompanion)

# Set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/III National City Bank/training")

# Options
options(scipen=999)

# Function to load and join datasets
load_and_join_data <- function() {
  current_data <- read.csv('CurrentCustomerMktgResults.csv')
  vehicle_data <- read.csv('householdVehicleData.csv')
  credit_data <- read.csv('householdCreditData.csv')
  axiom_data <- read.csv('householdAxiomData.csv')
  
  join_data <- list(current_data, vehicle_data, credit_data, axiom_data) %>%
    Reduce(function(d1, d2) left_join(d1, d2, by = 'HHuniqueID'), .)
  
  join_data$Y_AcceptedOffer <- as.factor(join_data$Y_AcceptedOffer)
  
  # Remove unnecessary columns
  cols_to_remove <- c('EstRace', 'headOfhouseholdGender', 'PetsPurchases')
  join_data <- join_data %>% select(-all_of(cols_to_remove))
  
  # Process call times and lengths
  join_data <- join_data %>%
    mutate(CallStart = strptime(CallStart, format = "%H:%M:%OS", tz = "EST"),
           CallEnd = strptime(CallEnd, format = "%H:%M:%OS", tz = "EST"),
           CallLength = as.integer(str_remove_all(format(round(difftime(CallEnd, CallStart, units = "mins"), 2), nsmall = 2), "mins"))) %>%
    select(-CallStart, -CallEnd)
  
  # Convert columns to appropriate types
  join_data$annualDonations <- as.numeric(gsub('\\$|,', '', join_data$annualDonations))
  join_data$DigitalHabits_5_AlwaysOn <- as.ordered(join_data$DigitalHabits_5_AlwaysOn)
  
  return(join_data)
}

# Function to prepare training and test datasets
prepare_datasets <- function(join_data) {
  splitPercent <- round(nrow(join_data) * 0.9)
  set.seed(1234)
  idx <- sample(seq_len(nrow(join_data)), size = splitPercent)
  
  train_data <- join_data[idx, ]
  test_data <- join_data[-idx, ]
  
  return(list(train = train_data, test = test_data))
}

# Function to explore data
explore_data <- function(data) {
  summary(data)
  names(data)
  head(data)
  tail(data)
  lapply(names(data), function(col) table(data[[col]]))
}

# Function to visualize data using ggplot2
visualize_data <- function(data) {
  p1 <- ggplot(data, aes(x = Marital)) + geom_bar() + theme_economist()
  p2 <- ggplot(data, aes(x = AffluencePurchases)) + geom_bar() + theme_economist()
  p3 <- ggplot(data, aes(x = Y_AcceptedOffer, y = CallLength)) + geom_boxplot() + theme_economist()
  p4 <- ggplot(data, aes(x = Y_AcceptedOffer, y = CallLength, scale = "area")) + geom_violin() + theme_economist()
  p5 <- ggplot(data, aes(x = as.factor(CarLoan))) + geom_bar() + theme_economist()
  p6 <- ggplot(data, aes(x = as.factor(DefaultOnRecord))) + geom_bar() + theme_economist()
  
  gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
}

# Function to apply vtreat and prepare datasets
apply_vtreat <- function(train_data, test_data, informativeFeatures, targetVariable, successClass) {
  plan <- designTreatmentsC(train_data, informativeFeatures, targetVariable, successClass)
  treated_train <- prepare(plan, train_data, pruneSig = 0.05)
  treated_validation <- prepare(plan, test_data, pruneSig = 0.05)
  
  return(list(train = treated_train, validation = treated_validation))
}

# Function to train models
train_models <- function(treated_train) {
  control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
  metric <- "Accuracy"
  
  models <- list()
  
  # Random Forest
  mtry <- sqrt(ncol(treated_train))
  tunegrid <- expand.grid(.mtry = mtry)
  set.seed(1234)
  models$rf <- train(as.factor(Y_AcceptedOffer) ~ ., data = treated_train, method = 'rf', metric = metric, tuneGrid = tunegrid, trControl = control)
  
  # KNN
  set.seed(1234)
  models$knn <- train(Y_AcceptedOffer ~ ., data = treated_train, method = "knn", preProcess = c("center", "scale"), tuneLength = 13, trControl = control)
  
  # Decision Tree
  set.seed(1234)
  models$rpart <- train(Y_AcceptedOffer ~ ., data = treated_train, method = "rpart", tuneGrid = data.frame(cp = c(0.1, 0.01, 0.05, 0.07)), control = rpart.control(minsplit = 1, minbucket = 2), trControl = control)
  
  return(models)
}

# Function to evaluate models
evaluate_models <- function(models, treated_train) {
  results <- resamples(models)
  summary(results)
  
  scales <- list(x = list(relation = "free"), y = list(relation = "free"))
  dotplot(results, scales = scales)
  bwplot(results, scales = scales)
}

# Function to apply model to validation set
apply_model_to_validation <- function(models, treated_validation) {
  results <- list()
  
  # Random Forest
  set.seed(1234)
  results$rf <- confusionMatrix(predict(models$rf, treated_validation), as.factor(treated_validation$Y_AcceptedOffer))
  
  # KNN
  set.seed(1234)
  results$knn <- confusionMatrix(predict(models$knn, treated_validation), as.factor(treated_validation$Y_AcceptedOffer))
  
  # Decision Tree
  set.seed(1234)
  results$rpart <- confusionMatrix(predict(models$rpart, treated_validation), as.factor(treated_validation$Y_AcceptedOffer))
  
  return(results)
}

# Function to identify top 100 prospects
identify_top_prospects <- function(model, plan, prospect_data) {
  treated_prospects <- prepare(plan, prospect_data, pruneSig = 0.05)
  prospect_probs <- predict(model, treated_prospects, type = 'prob')
  
  prospect_prob_results <- data.frame(HHuniqueID = prospect_data$HHuniqueID, successProb = prospect_probs[, 1])
  top_onehundred <- prospect_prob_results %>% arrange(desc(successProb)) %>% head(100)
  
  mean_success_prob <- mean(top_onehundred$successProb)
  write.csv(top_onehundred, file = "top_prospects.csv")
  
  return(top_onehundred)
}

# Main script
join_data <- load_and_join_data()
datasets <- prepare_datasets(join_data)
explore_data(datasets$train)
visualize_data(datasets$train)

informativeFeatures <- c('RecentBalance', 'NoOfContacts', 'DaysPassed', 'HHInsurance', 'Communication')
targetVariable <- 'Y_AcceptedOffer'
successClass <- 'Accepted'
treated_data <- apply_vtreat(datasets$train, datasets$test, informativeFeatures, targetVariable, successClass)

models <- train_models(treated_data$train)
evaluate_models(models, treated_data$train)
validation_results <- apply_model_to_validation(models, treated_data$validation)

prospects <- read.csv('~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/III National City Bank/ProspectiveCustomers.csv')
top_prospects <- identify_top_prospects(models$rf, treated_data$plan, prospects)

# End - this was fun!
