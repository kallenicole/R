#Author: Kalle Georgiev
#CSCIE-96 Section 2, Nov 4, 2023
#Dataset: https://www.kaggle.com/datasets/laotse/credit-risk-dataset/data

#set working directory, housekeeping
setwd("~/Documents/CSCIE-96TA")
options(stringsAsFactors = FALSE)
options(scipen=999)
set.seed(9876)

#bring in libraries
library(dplyr) #data manipulation, uses the pipe symbol: %>%
library(ggplot2) #visualizations
library(stringr) #string manipulation, pattern matching, text extraction
library(lubridate) #everything dates, which is normally a mess
library(caret) #algorithms
library(vtreat) #variable treatment before modeling
library(MLmetrics) #evaluation metrics
library(randomForest) #rf algorithm
library(pROC) #Receiver Operating Characteristic (ROC) curves
library(rpart.plot) #plots decision trees
library(Metrics) #precision, recall, f1


################################ SAMPLE ########################################
#bring in data
credit_risk <- read.csv('credit_risk_dataset.csv')

#change outcome to a factor (did they default or not)
credit_risk$loan_status <- ifelse(credit_risk$loan_status == 1, "default", "non-default")
credit_risk$loan_status <- factor(credit_risk$loan_status, levels = c("non-default", "default"))

#factoring
credit_risk$loan_grade <- factor(credit_risk$loan_grade)
credit_risk$person_home_ownership <- factor(credit_risk$person_home_ownership, 
                                            levels = c("OWN", "MORTGAGE", "RENT", "OTHER"))

#initial cleanup (get rid of NAs and outliers)
#NA's?
colSums(is.na(credit_risk))
summary(credit_risk)

# I have decided to select only rows without any missing values: complete cases
credit_risk <- credit_risk[complete.cases(credit_risk), ]

#remove obvious outliers (data entry errors, impossibilities, etc)
credit_risk <- credit_risk[credit_risk$person_income < 300000,] #max $6mm
credit_risk <- credit_risk[credit_risk$person_age < 100,]
credit_risk <- credit_risk[credit_risk$person_emp_length < 40,] #max 123 years

# partition schema
# 10% for vtreat & split 80/10/10 for the remaining
vtreat_portion   <- sample(1:nrow(credit_risk), nrow(credit_risk)*.1) 
vtreat_rows      <- credit_risk[vtreat_portion,] #10%
remaining        <- credit_risk[-vtreat_portion,] #90%
training_index   <- sample(1:nrow(remaining), nrow(remaining)*.8) #80%/10%/10% split
train_data       <- remaining[training_index,] #80% of the 90%
remaining2       <- remaining[-training_index,] #the leftovers from the 80%, should be 20%
validation_index <- sample(1:nrow(remaining2), nrow(remaining2)*.5)
validation_data  <- remaining2[validation_index,] #half of the 20% = 10%
test_data        <- remaining2[-validation_index,] #other half of the 20% = 10%

#can remove these
remove(remaining)
remove(remaining2)

############################# EXPLORE ########################################
#age?
ggplot(train_data, aes(x=person_age)) +
  geom_histogram(binwidth=1, fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Histogram of Person's Age", x="Age", y="Count")

#income?
ggplot(train_data, aes(x=person_income)) +
  geom_histogram(binwidth=25000, fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Histogram of Person's Income", x="Income", y="Count")


#loan intent
ggplot(train_data, aes(x=loan_intent)) +
  geom_bar(fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Count of Loan Intents", x="Loan Intent", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#loan grade - notice the order because I specificied the factor levels
ggplot(train_data, aes(x=loan_grade)) +
  geom_bar(fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Count of Loan Grades", x="Loan Grade", y="Count")

#home ownership
ggplot(train_data, aes(x=person_home_ownership)) +
  geom_bar(fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Count of Home Ownership", x="Home Ownership", y="Count")

#loan intent by amt
ggplot(train_data, aes(x=loan_intent, y=loan_amnt)) +
  geom_boxplot(fill="skyblue", color="black", outlier.alpha = 0.2) +
  theme_minimal() +
  labs(title="Boxplot of Loan Amount by Loan Intent", x="Loan Intent", y="Loan Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#age & income vs default
ggplot(train_data, aes(x=person_age, y=person_income, color=loan_status)) +
  geom_jitter(alpha = 0.3, width = 0.3, height = 0.3, size = 0.6) +
  theme_minimal() +
  labs(title="Scatter Plot of Age vs Income", x="Age", y="Income") +
  scale_color_manual(values=c("skyblue", "darkred"), 
  labels=c("No Default", "Default"), name="Loan Status")

#density plot
ggplot(train_data, aes(x=person_age, y=person_income, color=loan_status)) +
  geom_density_2d(alpha = 0.5) +
  facet_wrap(~ loan_status) +
  theme_minimal() +
  labs(title="Density Plot of Age vs Income", x="Age", y="Income") 

#heatmap
ggplot(train_data, aes(x=person_age, y=person_income)) +
  geom_bin2d(aes(fill=..count..), bins=30) +   # You can adjust bins as needed
  theme_minimal() +
  labs(title="Heatmap of Age vs Income", x="Age", y="Income") +
  scale_fill_viridis_c(trans="log", guide="colorbar", name="Number of People")   # Using a color scale from the viridis package for better visuals


#credit history length
ggplot(train_data, aes(x=cb_person_cred_hist_length)) +
  geom_histogram(binwidth=5, fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Histogram of Credit History Length", x="Years", y="Count")

#defaults on file?
ggplot(train_data, aes(x=cb_person_default_on_file, fill=loan_intent)) +
  geom_bar(color="black") +
  theme_minimal() +
  ylim(0, 20000) +
  labs(title="Count of Defaults", x="Default On File Y/N", y="Count")

#defaults/non-defaults from applicants with a default on file?
ggplot(train_data, aes(x=cb_person_default_on_file, fill = loan_status)) +
  geom_bar(color="black") +
  theme_minimal() +
  labs(title="Count of Defaults from Applicants with a Default on File", x="Default Y/N", y="Count")

table(train_data$loan_status) #my data is imbalanced

###################################### MODIFY ####################################
#feature engineering
#remove outliers - see above before graphing (iterative process)
#vtreat

############### data enrichment

#debt to income ratio (-ish)
train_data$debt_to_income <- train_data$loan_amnt / train_data$person_income
validation_data$debt_to_income <- validation_data$loan_amnt / validation_data$person_income
test_data$debt_to_income <- test_data$loan_amnt / test_data$person_income
vtreat_rows$debt_to_income <- vtreat_rows$loan_amnt / vtreat_rows$person_income


############### vtreat
informativeFeatures <- c(#'cb_person_default_on_file', 
                         #'cb_person_cred_hist_length',
                         'loan_amnt',
                         #'loan_risk_category', 
                         'debt_to_income', 
                         'person_home_ownership', 
                         'loan_grade',
                         'loan_int_rate') 

targetVariable      <- names(vtreat_rows)[9]
successClass        <- 'non-default'

plan <- designTreatmentsC(vtreat_rows, 
                          informativeFeatures, 
                          targetVariable, 
                          successClass)

#apply the vtreat rules to the sets
treated_train <- prepare(plan, train_data)
treated_validation <- prepare(plan, validation_data)
treated_test <- vtreat::prepare(plan, test_data)

#--------------
#By setting pruneSig = 0.05 in the prepare() function, you're specifying that any variable with a 
#significance level (analogous to a p-value) greater than 0.05 should be 
#considered "non-significant" and thus pruned from the dataset. In classical 
#hypothesis testing, a p-value of less than 0.05 is often considered evidence 
#that there is a statistically significant relationship
#--------------

######################################## MODEL ##################################

#random forest
#decision tree
#glm

######### RANDOM FOREST 

# training scheme: cross validation - 10 folds (randomly selecting 10 subsets of the data)
control <- trainControl(method="cv", number=10, verboseIter = TRUE)

#train model
fit_rf <- train(loan_status ~., 
                data=treated_train, 
                method="rf", 
                ntree = 200, #how many trees
                trControl=control, 
                tuneGrid = data.frame(mtry = 2))
#--------------
#mtry parameter: refers to the number of variables randomly sampled at each split when building a tree.
#The value of mtry can significantly influence the performance of a RandomForest. 
#If mtry is too low, trees might be too constrained and miss important interactions. 
#If mtry is too high, the randomness might not be sufficient, leading to overfitting.
#--------------

#variable importance
varImp(fit_rf)
plot(varImp(fit_rf), top = 7)

#gives you the estimated probabilities for each class
pred_probs_rf   <- predict(fit_rf, treated_train, type = "prob")
head(pred_probs_rf)

#gives you the actual predicted class based on the highest probability
pred_classes_rf <- predict(fit_rf,  treated_train)
head(pred_classes_rf)

#confusion matrix
conf_matrix <- confusionMatrix(pred_classes_rf, as.factor(treated_train$loan_status))
conf_matrix

#-------------------
#Kappa: This measures how much better the model is than just random guessing. 
#A score of 1 is perfect, and 0 is no better than random. 
#--------------------

#find F1 score - F1 is like a combination of these two percentages. 
#It tells us how balanced our model is in finding the right answers and avoiding mistakes.
f1_score <- conf_matrix$byClass["F1"]
f1_score


#----------------------
#Something you can do if you want to make you model give more weight to those that will default? 
non_default_weight <- 1
default_weight <- 2 # Increase this value to prioritize classifying defaults correctly

obs_weights <- ifelse(treated_train$loan_status == "non-default", 
                      non_default_weight, default_weight)

#specify the weights in training your model
rf_model <- train(as.factor(loan_status) ~ .,
                   data = treated_train,
                   method = "rf",
                   verbose = FALSE,
                   ntree = 200,
                   weights = obs_weights,
                   trControl = control,
                   tuneGrid = data.frame(mtry = 2))
#----------------------



########## LOGISTIC REGRESSION

#train model
fit_glm <- glm(loan_status ~., treated_train, family = 'binomial')
summary(fit_glm)


#get predictions
train_preds_glm <- predict(fit_glm,  treated_train, type='response')
head(train_preds_glm)

#classify 
cutoff      <- 0.50
train_classes_glm <- ifelse(train_preds_glm >= cutoff, 'default', 'non-default') 

#organize with actual
train_results_glm <- data.frame(actual  = treated_train$loan_status,
                                probability = train_preds_glm,
                                classes = train_classes_glm)
head(train_results_glm)
tail(train_results_glm)

# Get a confusion matrix
conf_mat_glm <- ConfusionMatrix(train_results_glm$classes, train_results_glm$actual)
conf_mat_glm
Accuracy(train_results_glm$classes, train_results_glm$actual) 




########### TUNED DECISION TREE

# Create a custom control object for tuning
tune_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # Number of folds
  search = "grid",        # Grid search - finding the best combination of hyperparameters--it exhaustively evaluates various hyperparameter settings to fine tune.   . 
  verboseIter = TRUE,     # Display progress
  returnData = FALSE,     # Don't return data for each resampling iteration
  returnResamp = "final"  # Return results for the final model
)

# Tune the decision tree model
tuned_dt <- train(
  loan_status ~ .,
  data = treated_train,
  method = "rpart",
  trControl = tune_control,
  tuneGrid = data.frame(cp = c(0.01, 0.05, 0.1))
)

# Print the best hyperparameters
print(tuned_dt)

#----------------
#A cp (complexity parameter) value of 0.01 means that the algorithm will consider pruning branches 
#from the tree if doing so results in a small improvement in the model's performance, where "small" is defined 
#as an improvement smaller than 0.01 (1%). In other words, a smaller cp value will result in 
#more aggressive pruning, while a larger cp value will result in less aggressive pruning.
#----------------

# Plot the pruned tree
prp(tuned_dt$finalModel, extra = 1, fallen.leaves = TRUE, cex = 0.8)

# Make predictions on the training set
pred_classes_dt <- predict(tuned_dt, treated_train)

# Get the confusion matrix
confusionMatrix(pred_classes_dt, as.factor(treated_train$loan_status))

###### using rpart.plot...better visual
#http://www.milbo.org/rpart-plot/prp.pdf
dt2 <- rpart(loan_status~., data = treated_train, cp = .01)
rpart.plot(dt2, extra=106) #percentages are percentages of the data, decimals are prob of default 


################################# ASSESS #######################################

##### APPLY RANDOM FOREST TO VALIDATION SET #######
pred_probs_validation   <- predict(fit_rf, treated_validation, type = "prob")
head(pred_probs_validation)

#gives you the actual predicted class based on the highest probability
pred_classes_validation <- predict(fit_rf,  treated_validation)
head(pred_classes_validation)

#confusion matrix
conf_matrix <- confusionMatrix(pred_classes_validation, treated_validation$loan_status)
conf_matrix

#Some manual calculations of precision, recall, and F1 (always dangerous)
# Calculate TP, FP, FN
TP <- sum(pred_classes_validation == "default" & treated_validation$loan_status == "default")
FP <- sum(pred_classes_validation == "default" & treated_validation$loan_status == "non-default")
FN <- sum(pred_classes_validation == "non-default" & treated_validation$loan_status == "default")

# Calculate precision and recall
precision_rf <- TP / (TP + FP)
precision_rf #Out of all the instances predicted as positive (defaulted), how many were truly positive?
recall_rf <- TP / (TP + FN)
recall_rf #Out of all the actual positive instances in the dataset (defaulted), how many did the model correctly identify as positive?

#---------------
#Business case:
#I can make the argument that missing some applicants who would have defaulted is a risk, 
#but it's often considered a manageable risk compared to making inaccurate predictions that could 
#harm applicants' financial well-being or the lender's business. This is more of a business decision.
#Let's say I aim to maximize precision to minimize the cost and negative consequences associated with 
#false positive predictions, even if it means potentially missing some applicants who would default. 
#The goal is to make accurate and responsible lending decisions while mitigating the 
#risk of false accusations.
#---------------

# Calculate F1 score
f1_score_manual_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
f1_score_manual_rf

#--------------
# there are other ways to calculate precision and recall, but they require 1's and 0's
Metrics::recall(validation_results_glm$classes, validation_results_glm$actual)
Metrics::precision(validation_results_glm$classes, validation_results_glm$actual)
# to calculate f1_score
Metrics::f1(validation_results_glm$classes, validation_results_glm$actual)
#---------------

##### APPLY GLM TO VALIDATION SET #####

#predict on validation data
validation_preds_glm <- predict(fit_glm, treated_validation, type = 'response')
head(validation_preds_glm)

# Create class predictions based on the threshold
validation_classes_glm <- ifelse(validation_preds_glm >= cutoff, 'default', 'non-default')
head(validation_classes_glm)

# Organize with actual
validation_results_glm <- data.frame(
  actual = treated_validation$loan_status,
  probability = validation_preds_glm,
  classes = validation_classes_glm
)

#need to make the same class with the same levels
class(treated_validation$loan_status)
class(validation_results_glm$classes)
validation_results_glm$classes <- factor(validation_results_glm$classes, levels = c("non-default", "default"))


# Get a confusion matrix for the validation set
conf_mat_validation_glm <- confusionMatrix(validation_results_glm$classes, validation_results_glm$actual)
conf_mat_validation_glm

#accuracy of validation set
Accuracy(validation_results_glm$classes, validation_results_glm$actual) 

#Some manual calculations of precision, recall, and F1
# Calculate TP, FP, FN
TP <- sum(validation_results_glm$classes == "default" & validation_results_glm$actual == "default")
FP <- sum(validation_results_glm$classes == "default" & validation_results_glm$actual == "non-default")
FN <- sum(validation_results_glm$classes == "non-default" & validation_results_glm$actual == "default")

# Calculate precision and recall
precision_glm <- TP / (TP + FP)
precision_glm #"Out of all the instances predicted as positive, how many were truly positive?"
recall_glm <- TP / (TP + FN)
recall_glm #Out of all the actual positive instances in the dataset, how many did the model correctly identify as positive?

# Calculate F1 score
f1_score_manual_glm <- 2 * (precision_glm * recall_glm) / (precision_glm + recall_glm)
f1_score_manual_glm



##### APPLY DECISION TREE TO VALIDATION SET #####

pred_classes_validation_dt <- predict(tuned_dt, newdata = treated_validation)

# Get the confusion matrix for the validation set
confusionMatrix(pred_classes_validation_dt, as.factor(treated_validation$loan_status))

TP <- sum(pred_classes_validation_dt == "default" & treated_validation$loan_status == "default")
FP <- sum(pred_classes_validation_dt == "default" & treated_validation$loan_status == "non-default")
FN <- sum(pred_classes_validation_dt == "non-default" & treated_validation$loan_status == "default")

# Calculate precision and recall
precision_dt <- TP / (TP + FP)
precision_dt #Out of all the instances predicted as positive (defaulted), how many were truly positive?
recall_dt <- TP / (TP + FN)
recall_dt #Out of all the actual positive instances in the dataset (defaulted), how many did the model correctly identify as positive?

# Calculate F1 score
f1_score_manual_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)
f1_score_manual_dt

########## COMPARE MODELS

PRF_models <- data.frame(algorithm = c("Random Forest", "Logistic Regression", "Decision Tree"), 
                            precision = c(precision_rf, precision_glm, precision_dt), 
                            recall = c(recall_rf, recall_glm, recall_dt), 
                            F1 = c(f1_score_manual_rf, f1_score_manual_glm, f1_score_manual_dt))
PRF_models


# Compare the randomn forest with decision tree
results <- resamples(list(DT = tuned_dt, RF = fit_rf))
summary(results)

# Visualize model comparisons
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales) 
dotplot(results, scales = scales)

# Visualize model comparisons with a legend
densityplot(results, scales = scales, pch = "|", auto.key = list(title = "Model", columns = 1))

#------------
#the Random Forest model, which exhibits a higher range of accuracies and Kappa values, 
#may have a better chance of generalizing to unseen data compared to the 
#Decision Tree model that is more concentrated around one accuracy range. 
#------------


######## APPLY CHOSEN MODEL TO THE TEST SET (FINAL EXAM)
#Let's say RF is the winner?

# Apply RF to the test set
pred_probs_test <- predict(fit_rf, treated_test, type = "prob")
head(pred_probs_test)

# Obtain the predicted classes based on the highest probability
pred_classes_test <- predict(fit_rf, treated_test)
head(pred_classes_test)

# Confusion matrix for the test set
conf_matrix_test <- confusionMatrix(pred_classes_test, treated_test$loan_status)
conf_matrix_test


#https://machinelearningmastery.com/compare-the-performance-of-machine-learning-algorithms-in-r/
