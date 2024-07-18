#' Author: Kalle Georgiev
#' Date: 11/22/2021
#' Purpose: Case II, CSCIE-96
#' "You can lie with data, but you can't tell the truth without it." -I forgot where
#' I read this, but it is a great quote. :)

#set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/II Gov Contractor")

library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(stringr)

# ops
options(stringsAsFactors = F)

# Functions
# Custom cleaning function
clean_text <- function(xVec) {
  xVec <- tolower(xVec)
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>|\\.\\.\\.", "", xVec)
  return(xVec)
}

#################################SAMPLE########################################
# Read in Data
tweets <- read.csv('student_tm_case_training_data.csv', encoding = "UTF-8")
tweets_score <- read.csv('student_tm_case_score_data.csv', encoding = "UTF-8")

# Rename columns
names(tweets)[1:2] <- c('doc_id', 'text')
names(tweets_score)[1:2] <- c('doc_id', 'text')

# Partition schema
set.seed(1234)
splitPercent <- round(nrow(tweets) * 0.8)
idx <- sample(seq_len(nrow(tweets)), splitPercent)

tweets_train <- tweets[idx, ]
tweets_test  <- tweets[-idx, ]

################ EXPLORE - SEE georgiev_TM_case_exploration.R ##################
head(tweets_train)
summary(tweets_train)
table(tweets_train$label) # 28.6% of the training set are labeled political

############################### MODIFY #########################################

# Clean the train, test, and score tweets text
tweets_train$text <- clean_text(tweets_train$text)
tweets_test$text <- clean_text(tweets_test$text)
tweets_score$text <- clean_text(tweets_score$text)

# Initial iterator to make vocabulary
iterMaker <- itoken(tweets_train$text, progressbar = TRUE)
textVocab <- create_vocabulary(iterMaker, stopwords = stopwords('SMART'))

# Prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
tweetsDTM <- create_dtm(iterMaker, vectorizer)

############################### MODEL ########################################

# Train text only model
textFit <- cv.glmnet(tweetsDTM,
                     y = as.factor(tweets_train$label),
                     alpha = 0.9,
                     family = 'binomial',
                     type.measure = 'auc',
                     nfolds = 7,
                     intercept = FALSE)

# Examine
bestTerms <- as.data.frame(as.matrix(coefficients(textFit)))
bestTerms <- subset(bestTerms, s1 != 0)
bestTerms <- data.frame(tokens = rownames(bestTerms), coeff = bestTerms$s1)

rownames(bestTerms) <- NULL
head(bestTerms[order(bestTerms$coeff, decreasing = TRUE), ])

# Training predictions and confusion matrix 
trainingPreds <- predict(textFit, tweetsDTM, type = 'class')

# Confusion matrix for training
confusionMatrix(as.factor(trainingPreds), as.factor(tweets_train$label))

###### Apply model to tweets TEST set
testIT <- itoken(tweets_test$text, tokenizer = word_tokenizer)
tweets_testDTM <- create_dtm(testIT, vectorizer)
testPreds <- predict(textFit, tweets_testDTM, type = 'class')

# Confusion matrix for testing
confusionMatrix(as.factor(testPreds), as.factor(tweets_test$label))

###### Apply model to tweets SCORE set
scoreIT <- itoken(tweets_score$text, tokenizer = word_tokenizer)
tweets_scoreDTM <- create_dtm(scoreIT, vectorizer)
scorePreds <- predict(textFit, tweets_scoreDTM, type = 'class')

# Make a new prediction data frame with prediction as the testPreds values as the "label"
tweets_predicted <- cbind(tweets_score, prediction = scorePreds)
head(tweets_predicted)

# Write the scored set to a CSV file
write.csv(tweets_predicted, file = "scored_data.csv")

# How many of these tweets have been classified as political and non? 21% political
table(tweets_predicted$prediction)
# How many of the training tweets were classified as political? 28.6% 
table(tweets_train$label)
# How many of the test tweets were classified as political? 28.5%
table(tweets_test$label)

# End - this was fun!
