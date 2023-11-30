#Author: Kalle Georgiev
#CSCIE-96 Section 2, Nov 4, 2023
#Dataset: https://www.kaggle.com/datasets/uciml/red-wine-quality-cortez-et-al-2009

# set working directory, housekeeping
setwd("~/Documents/CSCIE-96TA")
options(stringsAsFactors = FALSE)
options(scipen=999)
set.seed(6543)


# bring in libraries
library(ggplot2) #visualizations
library(caret) #training models
library(vtreat) #variable treatment before modeling
library(reshape2) #reshaping data frames
library(GGally) #plotting variable intersections
library(gridExtra) #plotting predictions
library(MLmetrics) #evaluation metrics
library(ModelMetrics) #model KPIs


################################## SAMPLE ######################################
wine <- read.csv('winequality-red.csv')
  
summary(wine)
sum(is.na(wine))

# Assuming your data frame is named 'your_data_frame'
names(wine) <- gsub("\\.", "_", names(wine))


# partition schema
# 10% for vtreat & split 80/20 for the remaining
vtreat_portion   <- sample(1:nrow(wine), nrow(wine)*.1) 
vtreat_rows      <- wine[vtreat_portion,] #10%
remaining        <- wine[-vtreat_portion,] #90%
training_index   <- sample(1:nrow(remaining), nrow(remaining)*.8) #80%/20% split
train_data       <- remaining[training_index,] #80% 
validation_data  <- remaining[-training_index,] #20%

# can remove
remove(remaining)


################################## EXPLORE #####################################

# wine quality distribution
ggplot(train_data, aes(x=quality)) + 
  geom_histogram(binwidth=1, fill="skyblue", color="black") +
  labs(title="Distribution of Wine Quality",
       x="Wine Quality",
       y="Count")

# boxplots for wine quality
ggplot(train_data, aes(x=factor(quality), y=fixed.acidity)) + 
  geom_boxplot(fill = "skyblue", color = "black", outlier.alpha = 0.3) +
  labs(title="Fixed Acidity vs Wine Quality",
       x="Wine Quality",
       y="Fixed Acidity")

# heatmap of variables
correlation <- cor(train_data)
melted_correlation <- melt(correlation)
ggplot(data = melted_correlation, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="skyblue", high="darkred", mid="white", midpoint=0) +
  theme_minimal() +
  labs(title="Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# violin plot - alcohol
ggplot(train_data, aes(x=factor(quality), y=alcohol)) + 
  geom_violin(color="skyblue") +
  labs(title="Alcohol vs Wine Quality",
       x="Alcohol",
       y="Wine Quality")

# violin plot - pH
ggplot(train_data, aes(x=factor(quality), y=pH)) + 
  geom_violin(color="skyblue") +
  labs(title="pH vs Wine Quality",
       x="pH",
       y="Wine Quality")

# violin plot - acidity
ggplot(train_data, aes(x=factor(quality), y=fixed.acidity)) + 
  geom_violin(color="skyblue") +
  labs(title="Acidity vs Wine Quality",
       x="Acidity",
       y="Wine Quality")


### plot using GGally
# change target variable to a factor for this plot
train_data$quality <- as.factor(train_data$quality)

# Since I've used colour = quality in your ggpairs call, 
# the plots will use color to represent different levels of wine quality. 
# This helps in identifying patterns or trends specific to certain quality levels.
ggpairs(train_data, columns = 8:12, ggplot2::aes(colour = quality))


################################### MODIFY #####################################

# Change quality back to int
train_data$quality <- as.numeric(train_data$quality)

# obviously you would do more modification here

############### vtreat
informativeFeatures <- names(vtreat_rows)[1:11] 
targetVariable      <- 'quality'

# esign treatments N for numeric
plan <- designTreatmentsN(vtreat_rows, 
                          informativeFeatures, 
                          targetVariable)

# apply the vtreat rules to the sets
treated_train <- prepare(plan, train_data)
treated_validation <- prepare(plan, validation_data)


################################### MODEL ###################################### 

#Use caret package to train 3 models
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

##### GLM
glm_fit <- train(quality ~ ., data = train_data, method = "glm", 
                trControl = fitControl)

###### Random Forest
rf_fit <- train(quality ~ ., data = train_data, method = "rf", 
               trControl = fitControl)

###### Gradient Boosting Machine: In technical terms, GBM is a method where multiple simple  
#models come together to create a strong and accurate prediction model. 
#Each model learns from the mistakes of the previous one, making the overall predictions more 
#precise with each step. This method is powerful for solving complex problems where a single 
#model might not be effective enough.
gbm_fit <- train(quality ~ ., data = train_data, method = "gbm", 
                trControl = fitControl, verbose = FALSE)


# call summary on glm to see p-values 
# (now i could reduce the amt of informative features based on p-values by re-doing vtreat)
summary(glm_fit)

# all KPIs are calculated under the hood
# you can get to the RMSE, r-squared, etc for each model by getting to the model results: 
rf_fit$results
glm_fit$results
gbm_fit$results

################################### ASSESS #####################################

# compare models using caret function resamples()
results_train <- resamples(list(GLM = glm_fit, RF = rf_fit, GBM = gbm_fit))
summary(results_train)

# visualize trained models
bwplot(results_train)



#-------------------------------------------------------------------------------
#R-squared as a score that tells you how well your model captures the patterns 
#or relationships in your data. It's like a grading system where a score of 0 
#means your model didn't capture any of the patterns, and a score of 1 means it 
#captured them perfectly. The closer the score is to 1, the better your model 
#is at predicting or explaining the outcomes based on the information it was given. 
#In simple terms, it's a way of measuring how good a job your model is doing.
#-------------------------------------------------------------------------------

##### Apply trained models to validation set

# predict using the GLM model
glm_predictions <- predict(glm_fit, newdata = treated_validation)

# predict using RF model
rf_predictions <- predict(rf_fit, newdata = treated_validation)

# predict using GBM model
gbm_predictions <- predict(gbm_fit, newdata = treated_validation)



# combine predictions into one data frame
combined_predictions <- data.frame(
  Actual = treated_validation$quality,
  GLM = glm_predictions,
  RF = rf_predictions,
  GBM = gbm_predictions
)

combined_predictions

# melt the data frame to long format for faceting
long_predictions <- reshape2::melt(combined_predictions, id.vars = "Actual")

# plot predictions vs actuals using ggplot2 with faceting
ggplot(long_predictions, aes(x = Actual, y = value)) + 
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  xlab("Actual") + ylab("Predicted") +
  ggtitle("Model Predictions Comparison")


# you can save this in a variable if you want: actual target variable values
actual <- treated_validation$quality


# Calculate metrics for each model
# Example: Calculating RMSE (Model Metrics)
rmse_glm <- ModelMetrics::rmse(actual, glm_predictions)
rmse_rf <- ModelMetrics::rmse(actual, rf_predictions)
rmse_gbm <- ModelMetrics::rmse(actual, gbm_predictions)

# Combine metrics into a data frame for comparison
validation_RMSE_results <- data.frame(
  Model = c("GLM", "RF", "GBM"),
  RMSE = c(rmse_glm, rmse_rf, rmse_gbm)
)

# get model RMSEs
validation_RMSE_results


# Calculate MAPE for each model (MLmetrics) 
# always try to use a function from a library instead of writing your own equation to reduce errors
mape_glm <- MLmetrics::MAPE(actual, glm_predictions)
mape_rf <- MLmetrics::MAPE(actual, rf_predictions)
mape_gbm <- MLmetrics::MAPE(actual, gbm_predictions)


# Combine metrics into a data frame for comparison
validation_MAPE_results <- data.frame(
  Model = c("GLM", "RF", "GBM"),
  RMSE = c(mape_glm, mape_rf, mape_gbm)
)

# get model MAPEs
validation_MAPE_results

# Random forest has highest R squared and lowest MAPE and RMSE (errors)

# Is there an issue with how I have approached this modeling problem? (Wine ratings)

