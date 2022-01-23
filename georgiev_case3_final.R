#' Author: Kalle Georgiev
#' Date: 12/17/2021
#' Purpose: Case III, National City Customer Propensity Model, CSCIE-96
#' "It's not the note you play that's the wrong note. It's the note you play afterwards
#' that makes it right or wrong." -Miles Davis
#' 
#' 1) Try 3 different model algorithms and evaluate which one is best for this business case: 
#' (Random Forest, KNN, Decision Tree)
#' 
#' 2) Identify the top 100 prospects by passing the prospect list into the best model.

# import libraries
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

#set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/III National City Bank/training")

# options
options(scipen=999)

############################### SAMPLE #########################################

# Read in raw customer data
current_data   <- read.csv('CurrentCustomerMktgResults.csv')
vehicle_data <- read.csv('householdVehicleData.csv') 
credit_data <- read.csv('householdCreditData.csv')
axiom_data <-read.csv('householdAxiomData.csv')

# Perform a join to add in other data sets
join_data <- left_join(current_data, vehicle_data, by = c('HHuniqueID'))
join_data <- left_join(join_data, credit_data, by = c('HHuniqueID'))
join_data <- left_join(join_data, axiom_data, by = c('HHuniqueID'))

# Load prospects data
prospects <- read.csv('~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/III National City Bank/ProspectiveCustomers.csv')

# Perform a join for additional prospect data
prospect_data <- left_join(prospects, vehicle_data, by = c('HHuniqueID'))
prospect_data <- left_join(prospect_data, credit_data, by = c('HHuniqueID'))
prospect_data <- left_join(prospect_data, axiom_data, by = c('HHuniqueID'))

# This is a classification problem so ensure R knows Y isn't 0/1 as integers
join_data$Y_AcceptedOffer <- as.factor(join_data$Y_AcceptedOffer)
summary(join_data$Y_AcceptedOffer)

#Remove race/gender/pets data because they aren't appropriate or necessary for this customer propensity model
join_data$EstRace <- NULL
prospect_data$EstRace <- NULL
join_data$headOfhouseholdGender <- NULL
prospect_data$headOfhouseholdGender <- NULL
join_data$PetsPurchases <- NULL
prospect_data$PetsPurchases <- NULL

#Change call start and end times to "time" because it is a CHR
join_data$CallStart <- strptime(join_data$CallStart, format = "%H:%M:%OS", tz = "EST")
join_data$CallEnd <- strptime(join_data$CallEnd, format = "%H:%M:%OS", tz = "EST")

#Get the length of the calls
join_data$CallLength <- format(round(difftime(join_data$CallEnd, join_data$CallStart, units = "mins"), 2), nsmall = 2)

#Remove "mins" from call length column
join_data <- join_data %>% mutate(CallLength = str_remove_all(CallLength, "mins"))

#Change call length column to an integer
join_data$CallLength <- as.integer(join_data$CallLength)

#Remove the call start and call end because they don't play nicely with vtreat
#and they don't have anything to do with the prospect data
join_data$CallStart <- NULL
join_data$CallEnd <- NULL

#Change annual donations from character to numeric
join_data$annualDonations <- as.numeric(gsub('\\$|,', '', join_data$annualDonations))

#Change Digital Habits to ordered levels
join_data$DigitalHabits_5_AlwaysOn <- as.ordered(join_data$DigitalHabits_5_AlwaysOn)
min(join_data$DigitalHabits_5_AlwaysOn)

## Partition schema
splitPercent <- round(nrow(join_data) %*% .9) #90/10 split to avoid overfitting
totalRecords <- 1:nrow(join_data)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

train_data <- join_data[idx,]
test_data  <- join_data[-idx,]

############################### EXPLORE ########################################

#I am exploring the current customer data to possibly gain some insights into 
#the type of customers that lean towards accepting our offers. 
summary(train_data)
names(train_data)
head(train_data)
tail(train_data)
names(train_data)
table(train_data$Job)
table(train_data$Education)
table(train_data$Communication)
table(train_data$DefaultOnRecord)
table(train_data$carMake)
table(train_data$DigitalHabits_5_AlwaysOn)
table(train_data$past_Outcome) #there is a lot of missing info here
table(prospect_data$past_Outcome) #also lots of missing info here
table(train_data$annualDonations)
table(train_data$DaysPassed)
table(train_data$CarLoan)

#### Acceptance vs. other factors ####

#what are the proportions of accepted offers among education levels?
#a higher proportion of people in the tertiary group accepted the offer than the other ed levels
education_offer <- table(train_data$Education, train_data$Y_AcceptedOffer)
prop.table(education_offer, 1) #rows (education levels) add up to 1
prop.table(education_offer, 2) #proportion in each ed level of accepted/not accepted

#what are the proportions of accepted offers among employment types?
#Blue-collar had the lowest acceptance (28%) while students had the highest (69%)
job_offer <- table(train_data$Job, train_data$Y_AcceptedOffer)
prop.table(job_offer, 1) #rows (job types) add up to 1

#does your phone type affect offer acceptance? Answer = no, about the same.
#and it looks like most people use a cell phone in this data set 
comm_offer <- table(train_data$Communication, train_data$Y_AcceptedOffer)
prop.table(comm_offer, 1) #rows (phone type) add up to 1
prop.table(comm_offer, 2) #columns add up to 1

#how do affluence purchases relate to offer acceptance?
affluence_offer <- table(train_data$AffluencePurchases, train_data$Y_AcceptedOffer)
prop.table(affluence_offer, 1) #rows (affluence purchases) add up to 1
prop.table(affluence_offer, 2) #columns add up to 1

#how do digital habits relate to offer acceptance?
digital_offer <- table(train_data$DigitalHabits_5_AlwaysOn, train_data$Y_AcceptedOffer)
prop.table(digital_offer, 1) #rows (digital habits) add up to 1
prop.table(digital_offer, 2) #columns add up to 1

#### GGPLOT PLAYGROUND ####

#how many divorced/married/single customers?
ggplot(train_data, aes(x= Marital)) + geom_bar() + theme_economist()

#how many customers have made luxury purchases?
ggplot(train_data, aes(x= AffluencePurchases)) + 
  geom_bar() + theme_economist()

#what is the relationship between call length and accepting offer?
#the longer the call, the higher the chance the offer will be accepted
ggplot(data = train_data, mapping = aes(x = Y_AcceptedOffer, y = CallLength)) +
  geom_boxplot() + theme_economist()

#violin plot of call length/accepted offer
ggplot(data = train_data, mapping = aes(x = Y_AcceptedOffer, y = CallLength, scale="area")) +
  geom_violin() + theme_economist()

#how many customers have a car loan? Most don't.
train_data$CarLoan <- as.factor(train_data$CarLoan)
ggplot(train_data, aes(x= CarLoan)) + 
  geom_bar() + theme_economist()

#how many customers have a default on record? Most don't.
train_data$DefaultOnRecord <- as.factor(train_data$DefaultOnRecord)
ggplot(train_data, aes(x= DefaultOnRecord)) + 
  geom_bar() + theme_economist()


############################### MODIFY #########################################

#move target variable to last column
join_data <- join_data %>% relocate(Y_AcceptedOffer, .after = last_col())

#remove call length as it doesn't apply to prospects
join_data$CallLength <- NULL
train_data$CallLength <- NULL
test_data$CallLength <- NULL

informativeFeatures <- c('RecentBalance', 'NoOfContacts', 
                         'DaysPassed', 'HHInsurance', 'Communication') 

targetVariable      <- names(join_data)[24]
successClass        <- 'Accepted'

plan <- designTreatmentsC(train_data, 
                          informativeFeatures, 
                          targetVariable, 
                          successClass)

#Apply the rules to the set
treated_train <- prepare(plan, train_data, pruneSig = 0.05)
treated_validation <- prepare(plan, test_data, pruneSig = 0.05)

############################### MODEL ##########################################
#Good resource: https://machinelearningmastery.com/compare-the-performance-of-machine-learning-algorithms-in-r/

# TRAINING THREE MODELS
##10 folds repeated 3 times 
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)


#### RANDOM FOREST ####
#Metric compare model is Accuracy
metric <- "Accuracy"

#Take the square root of the number of columns for mtry
mtry <- sqrt(ncol(treated_train))

tunegrid <- expand.grid(.mtry=mtry)

set.seed(1234)
fit.rf <- train(as.factor(Y_AcceptedOffer) ~., 
                   data = treated_train, 
                   method = 'rf', 
                   metric = 'Accuracy', 
                   tuneGrid = tunegrid, 
                   trControl = control)
fit.rf

#predict
pred_probs_rf   <- predict(fit.rf, treated_train, type = c("prob"))
pred_classes_rf <- predict(fit.rf,  treated_train)


#confusion matrix
caret::confusionMatrix(pred_classes_rf, as.factor(treated_train$Y_AcceptedOffer))
#True positive rate: 42.9%.....False positive rate: 11%

#variable importance
varImp(fit.rf)
plot(varImp(fit.rf), top = 20)

#accuracy (70.4%)
Accuracy(pred_classes_rf, as.factor(treated_train$Y_AcceptedOffer))
#RMSE
rmse(pred_classes_rf, as.factor(treated_train$Y_AcceptedOffer))

# Organize w/Actual
results_rf <- data.frame(actual  = treated_train$Y_AcceptedOffer,
                      classes = pred_classes_rf,
                      probs   = pred_probs_rf)
head(results_rf)


# ROC
ROCobj <- roc(as.integer(results_rf$classes), as.integer(results_rf$actual))
plot(ROCobj)




#### KNN ####
set.seed(1234)
fit.knn <- train(Y_AcceptedOffer~., data=treated_train, method="knn", preProcess = c("center","scale"), tuneLength = 13, trControl=control)
fit.knn

#plot
plot(fit.knn)

#predict
pred_probs_knn <- predict(fit.knn, treated_train, type = c("prob"))
pred_classes_knn <- predict(fit.knn, treated_train)

#confusion matrix
caret::confusionMatrix(pred_classes_knn, as.factor(treated_train$Y_AcceptedOffer))
#True positive rate: 50.8%.....False positive rate: 16%

#accuracy 1) (70.6%)
Accuracy(pred_classes_knn, as.factor(treated_train$Y_AcceptedOffer))
#RMSE
rmse(pred_classes_knn, as.factor(treated_train$Y_AcceptedOffer))




#### DECISION TREE ####
set.seed(1234)
fit.rpart <- train(Y_AcceptedOffer~., data=treated_train, method="rpart", 
                   #Define a range for the CP to test
                   tuneGrid = data.frame(cp = c(0.1, 0.01, 0.05, 0.07)), 
                   #ie don't split if there are less than 1 record left 
                   #and only do a split if there are at least 2+ records
                   control = rpart.control(minsplit = 1, minbucket = 2),
                   trControl=control)

#predict
pred_probs_rpart <- predict(fit.rpart, treated_train, type = c("prob"))
pred_classes_rpart <- predict(fit.rpart, treated_train)

#confusion matrix
caret::confusionMatrix(pred_classes_rpart, as.factor(treated_train$Y_AcceptedOffer))
#True positive rate: 52.7%....False positive rate: 20.9%

# Plot a pruned tree
prp(fit.rpart$finalModel, extra = 1)

#accuracy (68.5%)
Accuracy(pred_classes_rpart, as.factor(treated_train$Y_AcceptedOffer))
#RMSE
rmse(pred_classes_rpart, as.factor(treated_train$Y_AcceptedOffer))

# collect resamples
results <- resamples(list(RPART=fit.rpart, KNN=fit.knn, RF=fit.rf))

#results summary
summary(results)

# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)

# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


#### APPLY TO VALIDATION SET ####

# RANDOM FOREST
set.seed(1234)
test.rf <- predict(fit.rf,treated_validation) 
confusionMatrix(test.rf,as.factor(treated_validation$Y_AcceptedOffer)) #(66.5%)

# KNN
set.seed(1234)
test.knn <- predict(fit.knn,treated_validation) 
confusionMatrix(test.knn,as.factor(treated_validation$Y_AcceptedOffer)) #(64.7%)

# DECISION TREE
set.seed(1234)
test.rpart <- predict(fit.rpart,treated_validation)
confusionMatrix(test.rpart,as.factor(treated_validation$Y_AcceptedOffer)) #(67.2%)


############################### ASSESS #########################################


#### PROSPECTIVE CUSTOMERS ####

#move target variable to last column
prospect_data <- prospect_data %>% relocate(Y_AcceptedOffer, .after = last_col())

#### VTREAT ####

#Apply the rules to the set
treated_prospects <- prepare(plan, prospect_data, pruneSig = 0.05)

# 4. Make predictions
prospect_preds <- predict(fit.rf, treated_prospects, type = 'raw') #change to whichever model you want to run
prospect_probs <- predict(fit.rf, treated_prospects, type= 'prob') #change to whichever model you want to run
head(prospect_probs)

# 5. Join probabilities back to ID
prospect_prob_results <- cbind(prospects$HHuniqueID, prospect_probs[, 1])
head(prospect_prob_results)

# 6. Identify the top 100 "success" class probabilities from prospectsResults
top_onehundred <- data.frame(prospect_prob_results[order(prospect_prob_results[, 2], 
                                              decreasing = TRUE),][1:100,])
head(top_onehundred)

names(top_onehundred)[1] <- 'HHuniqueID'
names(top_onehundred)[2] <- 'successProb'

#get the average probability of success of top 100 prospects
top_onehundred$successProb <- as.numeric(top_onehundred$successProb)
mean(top_onehundred$successProb)

#Write the scored set to a CSV file
write.csv(top_onehundred, file = "top_prospects.csv")

#### EDA on TOP 100 prospects ####

#join top prospects back to informative data based on ID
prospects_top <- left_join(top_onehundred, prospect_data, by = c('HHuniqueID'))

summary(prospects_top)
head(prospects_top)
table(prospects_top$Job)
table(prospects_top$Education)
table(prospects_top$NoOfContacts)
table(prospects_top$DefaultOnRecord)
table(prospects_top$CarLoan)
table(prospects_top$carMake)
table(prospects_top$HHInsurance)
table(prospects_top$PrevAttempts)
table(prospects_top$Communication)
table(prospects_top$DigitalHabits_5_AlwaysOn)
table(prospects_top$carYr)

#how many prospects have made luxury purchases?
ggplot(prospects_top, aes(x= AffluencePurchases)) + 
  geom_bar() + theme_economist()

#how many prospects have a car loan? Most of them do not have a car loan.
prospects_top$CarLoan <- as.factor(prospects_top$CarLoan)
ggplot(prospects_top, aes(x= CarLoan)) + 
  geom_bar() + theme_economist()

#how many prospects have a default on record? Zero.
prospects_top$DefaultOnRecord <- as.factor(prospects_top$DefaultOnRecord)
ggplot(prospects_top, aes(x= DefaultOnRecord)) + 
  geom_bar() + theme_economist()

#education vs job
ggplot(prospects_top, 
       aes(x = Education, fill = Job)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#Average age of prospects
mean(prospects_top$Age) #45.7 yrs old

#how many attempts to contact/sell? They are on the lower end. 
ggplot(data = prospects_top, mapping = aes(x = PrevAttempts)) +
  geom_histogram(binwidth = 0.1)


#End - this was fun!