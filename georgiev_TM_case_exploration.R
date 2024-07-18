# Author: Kalle Georgiev
# Date: 11/22/2021
# Purpose: Case II, CSCIE-96
# "Make it nice or make it twice." -Daniel Humm

# Set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/II Gov Contractor")

# Import libraries
library(vtreat)
library(dplyr)
library(MLmetrics)
library(ggplot2)
library(ggthemes)
library(ggdendro)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(qdap)
library(qdapRegex)
library(hexbin)
library(radiant.data)
library(DataExplorer)
library(stringr)
library(twitteR)
library(rtweet)
library(readr)
library(DataCombine)
library(stringi)
library(tidytext)
library(textdata)
library(radarchart)
library(mgsub)
# library(emo)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL', 'C')

# Custom cleaning function
clean_text <- function(xVec) {
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

try_tolower <- function(x) {
  y <- NA
  try_error <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y <- tolower(x)
  return(y)
}

# Cleaning function
clean_corpus <- function(corpus, custom_stopwords) {
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(try_tolower))
  corpus <- tm_map(corpus, removeWords, custom_stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

# Bigram token maker
bigram_tokens <- function(x) {
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

# Create custom stop words
custom_stopwords <- c(stopwords('english'), 'rt', 'lol', 'smh', 'just', 'can', 'us', "i'm", "it's", 'amp', 'httpstco')

# Function to load and preprocess tweets
load_and_preprocess_tweets <- function(file_path, split_ratio = 0.8) {
  tweets <- read.csv(file_path)
  split_percent <- round(nrow(tweets) * split_ratio)
  set.seed(1234)
  idx <- sample(1:nrow(tweets), split_percent)
  
  tweets_train <- tweets[idx,]
  tweets_test <- tweets[-idx,]
  
  names(tweets_train)[1] <- 'doc_id'
  names(tweets_train)[2] <- 'text'
  
  emoji_tweets <- tweets_train
  
  tweets_train$text <- str_replace(tweets_train$text, "\\.\\.\\.", "")
  tweets_train$text <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>", "", tweets_train$text)
  tweets_train$text <- gsub("<U\\+\\w+>", "", tweets_train$text)
  
  list(tweets_train = tweets_train, tweets_test = tweets_test, emoji_tweets = emoji_tweets)
}

# Function to create and clean corpus
create_clean_corpus <- function(text_data, custom_stopwords) {
  vcorpus <- VCorpus(VectorSource(text_data))
  clean_corpus(vcorpus, custom_stopwords)
}

# Function to generate DTM and TDM
generate_dtm_tdm <- function(corpus) {
  list(
    dtm = DocumentTermMatrix(corpus),
    tdm = TermDocumentMatrix(corpus)
  )
}

# Load and preprocess tweets
tweet_data <- load_and_preprocess_tweets('student_tm_case_training_data.csv')
tweets_train <- tweet_data$tweets_train
tweets_test <- tweet_data$tweets_test
emoji_tweets <- tweet_data$emoji_tweets

# Make a volatile corpus
txt_corpus <- VCorpus(DataframeSource(tweets_train))

# Preprocess the corpus
txt_corpus <- clean_corpus(txt_corpus, custom_stopwords)

# Compare a single tweet before and after cleaning. looks pretty clean.
tweets_train$text[6]
content(txt_corpus[[6]])

# Generate DTM and TDM
dtm_tdm <- generate_dtm_tdm(txt_corpus)
txt_dtm <- dtm_tdm$dtm
txt_tdm <- dtm_tdm$tdm

txt_dtmM <- as.matrix(txt_dtm)
txt_tdmM <- as.matrix(txt_tdm)

# Vector source corpus
vtext_corpus <- create_clean_corpus(tweets_train$text, custom_stopwords)

# DTM from Vector source
vtweet_TDM <- TermDocumentMatrix(vtext_corpus)

# Explore the data
explore_data <- function(tweets_train) {
  list(
    names = names(tweets_train),
    summary = summary(tweets_train),
    head = head(tweets_train),
    tail = tail(tweets_train)
  )
}

# Exploring data
explored_data <- explore_data(tweets_train)
explored_data$names
explored_data$summary
explored_data$head
explored_data$tail

# Logical T/F vector that a string appears at least ONCE
keywords_search <- function(keywords, text_data) {
  sapply(keywords, function(word) grepl(word, text_data, ignore.case = TRUE))
}

keywords <- c("protest", "covid", "trump|antifa")
keyword_appearances <- keywords_search(keywords, tweets_train$text)

# Calculate the % of times words show up among all tweets
calculate_percentage <- function(logical_vector, total_rows) {
  sum(logical_vector) / total_rows
}

percentages <- sapply(keyword_appearances, calculate_percentage, nrow(tweets_train))
names(percentages) <- keywords
percentages

# Sentiment analysis
sentiment_analysis <- function(tweets_train, label) {
  data <- filter(tweets_train, label == label)
  data$text <- clean_text(data$text)
  polarity(data[2])
}

# Polarity of political vs non-political tweets
political_polarity <- sentiment_analysis(tweets_train, 1)
non_political_polarity <- sentiment_analysis(tweets_train, 0)

# NRC Sentiment and Radar Chart
get_nrc_sentiment <- function(text_data, custom_stopwords) {
  nrc <- textdata::lexicon_nrc()
  
  sent_corpus <- create_clean_corpus(text_data, custom_stopwords)
  sent_DTM <- DocumentTermMatrix(sent_corpus)
  tidyCorp <- tidy(sent_DTM)
  
  nrc_sent <- inner_join(tidyCorp, nrc, by = c('term' = 'word'))
  nrc_sent <- nrc_sent[!grepl('positive|negative', nrc_sent$sentiment),]
  
  nrc_sent_radar <- as.data.frame.matrix(table(nrc_sent$sentiment, nrc_sent$document))
  colnames(nrc_sent_radar) <- c("political", "non-political")
  
  nrc_sent_radar <- prop.table(as.matrix(nrc_sent_radar), 2)
  
  data.frame(labels = rownames(nrc_sent_radar), nrc_sent_radar)
}

# Prepare text for NRC sentiment analysis
pol_tweets <- tweets_train %>%
  group_by(label) %>%
  summarise(across(everything(), str_c, collapse = "")) %>%
  select(-doc_id)

tweets_political <- filter(pol_tweets, label == 1)
tweets_non_political <- filter(pol_tweets, label == 0)
sent_text <- c(tweets_political$text, tweets_non_political$text)

# Generate radar chart data
nrc_sent_radar <- get_nrc_sentiment(sent_text, custom_stopwords)

# The Radar Chart
chartJSRadar(scores = nrc_sent_radar, labelSize = 10, showLegend = TRUE)

#### WORDCLOUD ####

# Vector Corpus
corpora <- list(political_c = VCorpus(VectorSource(political$text)),
                non_political_c = VCorpus(VectorSource(non_political$text)))

# Cleaning
corpora <- lapply(corpora, clean_corpus, customStopwords = customStopwords)

# Extract and combine text
corpora <- lapply(corpora, function(corpus) paste(sapply(corpus, content), collapse = " "))

# Combine all subject matter tweets into a single document encompassing all tweets
all_tweets <- unlist(corpora)
tweet_corpus <- VCorpus(VectorSource(all_tweets))

# Make TDM
tweetTDM <- TermDocumentMatrix(tweet_corpus)
tweetTDMm <- as.matrix(tweetTDM)

# Label the new TDM
colnames(tweetTDMm) <- c("Political", "NonPolitical")

# Make commonality cloud
commonality.cloud(tweetTDMm, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Spectral"), scale = c(3.5, 0.5))

# Make comparison cloud
comparison.cloud(tweetTDMm, max.words = 75, random.order = FALSE, title.size = 1.5, colors = brewer.pal(ncol(tweetTDMm), "Set1"), scale = c(3.5, 0.75))

# Make a bigram cloud
bi_tweetTDM <- TermDocumentMatrix(txt_corpus, control = list(tokenize = bigramTokens))
bi_tweetTDMm <- as.matrix(bi_tweetTDM)

# Look up some bi-grams
grep_patterns <- c('covid test', 'police officer', 'white supremacy')
lapply(grep_patterns, function(pattern) grep(pattern, rownames(bi_tweetTDMm)))

# Get Row Sums & organize
tweetTDMv <- sort(rowSums(bi_tweetTDMm), decreasing = TRUE)
tweetDF <- data.frame(word = names(tweetTDMv), freq = tweetTDMv, row.names = NULL)

# Choose a color & drop light ones
pal <- brewer.pal(8, "PRGn")[-(5:7)]

set.seed(1234)
wordcloud(tweetDF$word, tweetDF$freq, max.words = 50, random.order = FALSE, colors = pal, scale = c(3, 0.5))

#### WORD ASSOCIATIONS AND DENDROGRAM ####

# Frequency Data Frame
tweet_freq <- data.frame(word = names(rowSums(txt_tdmM)), frequency = rowSums(txt_tdmM), row.names = NULL)

# Simple barplot; values greater than 90
top_words <- subset(tweet_freq, frequency >= 90)
top_words <- top_words[order(top_words$frequency, decreasing = FALSE),]
top_words$word <- factor(top_words$word, levels = unique(as.character(top_words$word)))

ggplot(top_words, aes(x = word, y = frequency)) + geom_bar(stat = "identity", fill = 'darkred') + coord_flip() + theme_gdocs() + geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 5.0)

# qdap version (a bit too much)
plot(freq_terms(tweets_train$text, top = 35, at.least = 2, stopwords = custom_stopwords))

# Inspect word associations
associations <- findAssocs(txt_tdm, 'trump', 0.15)

# Organize the word associations
trump_df <- data.frame(terms = names(associations[[1]]), value = unlist(associations))
trump_df$terms <- factor(trump_df$terms, levels = trump_df$terms)
trump_sample <- sample_n(trump_df, 15)

# Make a dot plot
ggplot(trump_sample, aes(y = terms)) + geom_point(aes(x = value), col = '#c00c00') + theme_gdocs() + geom_text(aes(x = value, label = value), colour = "red", hjust = "inward", vjust = "inward")

# Reduce TDM
vtweet_TDM2 <- removeSparseTerms(vtweet_TDM, sparse = 0.985)
vtweet_TDM2 <- as.data.frame(as.matrix(vtweet_TDM2))

# Basic Hierarchical Clustering
hc <- hclust(dist(vtweet_TDM2))
plot(hc, yaxt = 'n')

# Improved visual
ggdendrogram(hc, rotate = FALSE) + ggtitle('Dendrogram - word frequency clusters')

# Word associate
set.seed(1234)
random_sample <- tweets_train[sample(nrow(tweets_train), 300), ]
assoc_text <- rm_url(random_sample$text)

# Word association network (pops up in a different window)
word_associate(assoc_text, match.string = "trump", stopwords = custom_stopwords, network.plot = T, cloud.colors = c("darkblue", "darkred"))

#### ASKING SOME QUESTIONS ####

# How many hashtags are there in each post?
tweets_train$number.of.hash <- str_count(tweets_train$text, "#")

# How many mentions or "@" are there in each post?
tweets_train$mentions <- str_count(tweets_train$text, "@")

# What is the character length of the posts?
tweets_train$num.char <- nchar(tweets_train$text)

# How many times is the former president mentioned?
tweets_train$trump <- str_count(tweets_train$text, "(?i)trump")

# How many times is black lives matter mentioned?
tweets_train$blm <- str_count(tweets_train$text, "(?i)blm") + str_count(tweets_train$text, "(?i)blacklivesmatter")

#### GGPLOT PLAYGROUND ####

dev.off()

# Number of characters by number of hashtags
ggplot(tweets_train, aes(x = num.char, y = number.of.hash)) + geom_point()

# Number of hashtags
ggplot(tweets_train, aes(number.of.hash)) + geom_bar(position = "stack")

# Number of hashtags
ggplot(data = tweets_train, mapping = aes(x = number.of.hash)) + geom_freqpoly(binwidth = 0.5)

inspect(txt_dtm)

#### WHAT ABOUT EMOJIS? ####

# Transform tweet data to ASCII to get it in the right format for extracting emojis
emoji_tweets$text <- stri_trans_general(emoji_tweets$text, "latin-ascii")
emoji_tweets$text <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", " ", emoji_tweets$text)

# Substitute the UTF-8 code for the emoji description and place between two *'s (takes a minute)
sub_text <- mgsub(emoji_tweets$text, emojis$code, paste0(' *', emojis$description, '* '))

# Remove other junk
sub_text <- gsub("[][!#$%(),.:;<=>@^_|~.{}]", " ", sub_text)

# Remove everything but the emoji descriptions between the two symbols
just_emojis <- rm_between(sub_text, '*', '*', extract = TRUE)
just_emojis <- matrix(unlist(just_emojis), ncol = 1, nrow = 1600)
just_emojis <- as.data.frame(just_emojis)
just_emojis <- just_emojis[!apply(is.na(just_emojis) | just_emojis == "", 1, all), ]

# Emoji Counts
emoji_counts <- sapply(c("raised fist", "grinning face", "rolling on the floor laughing", "face with tears of joy", "thinking face", "black heart"), function(emoji) sum(str_count(just_emojis, emoji)))

# Make an emoji corpus
emoji_corpus <- VCorpus(VectorSource(just_emojis))

# Make TDM
emoji_TDM <- TermDocumentMatrix(emoji_corpus)
emoji_TDMm <- as.matrix(emoji_TDM)

# Frequency Data Frame
emoji_freq <- data.frame(word = names(rowSums(emoji_TDMm)), frequency = rowSums(emoji_TDMm), row.names = NULL)

# Simple barplot; values greater than 10
top_words <- subset(emoji_freq, frequency >= 10)
top_words <- top_words[order(top_words$frequency, decreasing = FALSE), ]
top_words$word <- factor(top_words$word, levels = unique(as.character(top_words$word)))

# GGplot for "emoji word" frequency
ggplot(top_words, aes(x = word, y = frequency)) + geom_bar(stat = "identity", fill = '#1b7ea1') + coord_flip() + theme_gdocs() + geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 5.0)

# End - this was fun!!
