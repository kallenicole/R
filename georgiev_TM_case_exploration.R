#' Author: Kalle Georgiev
#' Date: 11/22/2021
#' Purpose: Case II, CSCIE-96
#' "Make it nice or make it twice." -Daniel Humm

#set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/II Gov Contractor")

# import libraries
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
#library(emo)

#Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

#Custom cleaning function
clean_text<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

tryTolower <- function(x){
  #return NA when there is an error
  y = NA
  #tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  #if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#Cleaning function
clean_corpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom_stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

#Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

#Create custom stop words
custom_stopwords <- c(stopwords('english'), 'rt', 'lol', 'smh', 'just', 'can', 'us', 'i\'m', 'it\'s', 'amp', 'httpstco')
custom_stopwords

################################# SAMPLE #######################################
#get the training data for exploration.

#tweets_train <- readr::read_csv("student_tm_case_training_data.csv")

#tweets <- read.csv('student_tm_case_training_data.csv', encoding = "UTF-8" )
tweets <- read.csv('student_tm_case_training_data.csv') #this is the only way that the emojis will work

#Partition schema
splitPercent <- round(nrow(tweets) %*% .8)
totalRecords <- 1:nrow(tweets)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

tweets_train <- tweets[idx,]
tweets_test  <- tweets[-idx,]

#edit column names
names(tweets_train)[1] <- 'doc_id'
names(tweets_train)[2] <- 'text'

#get an uncleaned copy of training data for emoji mining use
emoji_tweets <- tweets_train

#Further removal of junk
tweets_train$text <- str_replace(tweets_train$text, "\\.\\.\\.","")
tweets_train$text <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", tweets_train$text)
tweets_train$text <- gsub("<U\\+\\w+>","", tweets_train$text)

#Make a volatile corpus
txt_corpus <- VCorpus(DataframeSource(tweets_train))

#Preprocess the corpus
txt_corpus <- clean_corpus(txt_corpus)

#Compare a single tweet before and after cleaning. looks pretty clean. 
tweets_train$text[6]
content(txt_corpus[[6]])

#Make a Document Term Matrix or Term Document Matrix depending on analysis
txt_dtm  <- DocumentTermMatrix(txt_corpus)
txt_tdm  <- TermDocumentMatrix(txt_corpus)
txt_dtmM <- as.matrix(txt_dtm)
txt_tdmM <- as.matrix(txt_tdm)

#Vector source corpus
vtext_corpus <- VCorpus(VectorSource(tweets_train$text))
vtext_corpus <- clean_corpus(vtext_corpus, custom_stopwords)

#DTM from Vector source
vtweet_TDM <- TermDocumentMatrix(vtext_corpus)

################################# EXPLORE #######################################

#what is in this data?
names(tweets_train)
summary(tweets_train)
head(tweets_train)
tail(tweets_train)

#Logical T/F vector that a string appears at least ONCE
protest    <- grepl("protest", tweets_train$text, ignore.case=TRUE)
covid <- grepl("covid", tweets_train$text, ignore.case=TRUE)
trump <- grepl("trump|antifa", tweets_train$text, ignore.case=TRUE)

#Find the row positions of a specific word appearing at least ONCE
grep("dude", tweets_train$text, ignore.case=TRUE)
grep("rainbow", tweets_train$text, ignore.case=TRUE)
grep("black", tweets_train$text, ignore.case=TRUE)
grep("realdonaldtrump", tweets_train$text, ignore.case=TRUE)

#Grep for indexing
tweets_train[grep('white', tweets_train$text),]

#Logical T/F for one word OR another appears at least ONCE
keywords <-"resist|blacklivesmatter|trump"
resistblmtrump <-grepl(keywords, tweets_train$text,ignore.case=TRUE)

#Calculate the % of times words show up among all tweets
sum(protest) / nrow(tweets_train)
sum(covid) / nrow(tweets_train)
sum(resistblmtrump) / nrow(tweets_train)
sum(trump) / nrow(tweets_train)

#### SENTIMENT ####

##### POLARITY of political vs non-political tweets ####
political <- filter(tweets_train, label == 1)
political$text <- clean_text(political$text)

non_political <- filter(tweets_train, label == 0)
non_political$text <- clean_text(non_political$text)

#political tweets are negative -1.117 (this makes sense unfortunately)
polarity(political[2])

#non-political tweets are more neutral to positive 0.184
polarity(non_political[2])

#### NRC SENTIMENT and RADAR CHART ####
#Mohammad, Saif M. and Turney, Peter D.
#http://saifmohammad.com/WebPages/lexicons.html
#Get the NRC data
nrc <- textdata::lexicon_nrc()
head(nrc)

#Group tweets by 0 or 1 and collapse rows into a single row of tweets
pol_tweets <- tweets_train %>% 
  group_by(label) %>% 
  summarise(across(everything(), str_c, collapse="")) 

#Don't need the doc_id
pol_tweets$doc_id <- NULL

#Separate into political/non political
tweets_political <- filter(pol_tweets, label == 1)
tweets_non_political <- filter(pol_tweets, label == 0)

#Don't need the label column
tweets_political$label <- NULL
tweets_non_political$label <- NULL
  
#Read in the tweets for processing 
sent_text <- c(tweets_political$text, tweets_non_political$text)

#Name the columns
doc_names <- c("political", "non-political") 

#Clean sentiment corpus
sent_corpus <- VCorpus(VectorSource(sent_text))
sent_corpus <- clean_corpus(sent_corpus, custom_stopwords)

#DTM
sent_DTM    <- DocumentTermMatrix(sent_corpus)
sent_DTM
#Dimensions of the document term matrix
dim(sent_DTM)

#Tidy the corpus
tidyCorp <- tidy(sent_DTM)

#Do inner join on NRC and tweet words
nrc_sent <- inner_join(tidyCorp, nrc, by=c('term' = 'word'))
nrc_sent

#Drop pos/neg leaving only the emotion
nrc_sent <- nrc_sent[-grep('positive|negative', nrc_sent$sentiment),]

#Check that the pos/neg has dropped
table(nrc_sent$sentiment,nrc_sent$document)
tail(nrc_sent)

#Manipulate for radarchart
nrc_sent_radar <- as.data.frame.matrix(table(nrc_sent$sentiment, nrc_sent$document))
nrc_sent_radar

#Paste column names 
colnames(nrc_sent_radar) <- doc_names

#Normalize for length; prop.table needs a "matrix" class
nrc_sent_radar <- prop.table(as.matrix(nrc_sent_radar),2)
nrc_sent_radar

#Check that columns = 1. Success.
colSums(nrc_sent_radar)

#Organize
nrc_sent_radar <- data.frame(labels = rownames(nrc_sent_radar),
                           nrc_sent_radar)
rownames(nrc_sent_radar) <- NULL
nrc_sent_radar

#The Radar Chart
chartJSRadar(scores = nrc_sent_radar, labelSize = 10, showLegend = T)

#### WORDCLOUD ####

#Vector Corpus
political_c       <- VCorpus(VectorSource(political$text))
non_political_c <- VCorpus(VectorSource(non_political$text))

#Cleaning
political_c       <- clean_corpus(political_c, customStopwords)
non_political_c <- clean_corpus(non_political_c, customStopwords)

#Extract plain clean text out of each corpus same as calling content() on an individual tweet
political_c       <- sapply(political_c, content)
non_political_c <- sapply(non_political_c, content)

#Combine all subject matter tweets into single document encompassing all tweets
political_c       <- paste(political_c, collapse=" ")
non_political_c <- paste(non_political_c, collapse=" ")

#To make it clear we now have a single document of all political tweets
political_c
length(political_c)

#Make a combined corpus of 3 subject matters
all_tweets   <- c(political_c, non_political_c)
tweet_corpus <- VCorpus(VectorSource(all_tweets))
tweet_corpus

#Make TDM
tweetTDM  <- TermDocumentMatrix(tweet_corpus)
tweetTDMm <- as.matrix(tweetTDM)

#Label the new TDM
colnames(tweetTDMm) = c("Political", "NonPolitical")
tweetTDMm[50:55,1:2]

#Make commonality cloud
commonality.cloud(tweetTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors=brewer.pal(8, "Spectral"),
                  scale=c(3.5,0.5))

# Make comparison cloud
comparison.cloud(tweetTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=1.5,
                 colors=brewer.pal(ncol(tweetTDMm),"Set1"),
                 scale=c(3.5,0.75))

#Make a bigram cloud
# Make bi-gram TDM according to the tokenize control & convert it to matrix
bi_tweetTDM  <- TermDocumentMatrix(txt_corpus, 
                               control=list(tokenize=bigramTokens))
bi_tweetTDMm <- as.matrix(bi_tweetTDM)

# look up some bi-grams
grep('covid test', rownames(bi_tweetTDMm))
grep('police officer', rownames(bi_tweetTDMm))
grep('white supremacy', rownames(bi_tweetTDMm))

# Get Row Sums & organize
tweetTDMv <- sort(rowSums(bi_tweetTDMm), decreasing = TRUE)
tweetDF   <- data.frame(word = names(tweetTDMv), 
                       freq = tweetTDMv,
                       row.names = NULL)

#What are some of the bi-grams
#Apparently we area headed towards the zombie apocalypse
head(tweetDF)

#Choose a color & drop light ones
pal <- brewer.pal(8, "PRGn")
pal <- pal[-(5:7)]

set.seed(1234)
wordcloud(tweetDF$word,
          tweetDF$freq,
          max.words=50,
          random.order=FALSE,
          colors=pal,
          scale=c(3,0.5))

#### WORD ASSOCIATIONS AND DENDROGRAM ####

#Frequency Data Frame
tweet_freq <- rowSums(txt_tdmM)
tweet_freq <- data.frame(word=names(tweet_freq),
                       frequency=tweet_freq, 
                       row.names = NULL)

#Simple barplot; values greater than 90 
top_words      <- subset(tweet_freq, tweet_freq$frequency >= 90) 
top_words      <- top_words[order(top_words$frequency, decreasing=F),]

#Change to factor for ggplot
top_words$word <- factor(top_words$word, 
                        levels=unique(as.character(top_words$word))) 

ggplot(top_words, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

#qdap version (a bit too much)
plot(freq_terms(tweets_train$text, top=35, at.least=2, stopwords = custom_stopwords))

#Inspect word associations
associations <- findAssocs(txt_tdm, 'trump', 0.15)
associations

#Organize the word associations
trump_df <- data.frame(terms=names(associations[[1]]),
                       value=unlist(associations))
trump_df$terms <- factor(trump_df$terms, levels=trump_df$terms)
trump_df
trump_sample <- sample_n(trump_df, 15)

#Make a dot plot
ggplot(trump_sample, aes(y=terms)) +
  geom_point(aes(x=value), data=trump_sample, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" )

#Reduce TDM
vtweet_TDM2 <- removeSparseTerms(vtweet_TDM, sparse=0.985) 
vtweet_TDM2

#Organize the smaller TDM
vtweet_TDM2 <- as.data.frame(as.matrix(vtweet_TDM2))

#Basic Hierarchical Clustering
hc <- hclust(dist(vtweet_TDM2))
plot(hc,yaxt='n')

#Improved visual
ggdendrogram(hc, rotate=FALSE) + ggtitle('Dendrogram - word frequency clusters')


#Word associate
set.seed(1234)
random_sample <- tweets_train[sample(nrow(tweets_train), "300"), ]
assoc_text <- rm_url(random_sample$text)

#word association network (pops up in a different window)
word_associate(assoc_text,
               match.string = "trump",
               stopwords = custom_stopwords,
               network.plot = T,
               cloud.colors = c("darkblue", "darkred"))

#### ASKING SOME QUESTIONS ####

#How many hashtags are there in each post?
tweets_train$number.of.hash <- str_count(tweets_train$text, "#")

#How many mentions or "@" are there in each post?
tweets_train$mentions <- str_count(tweets_train$text, "@")

#What is the character length of the posts?
tweets_train$num.char <-nchar(tweets_train$text)

#How many times is the former president mentioned?
tweets_train$trump <- str_count(tweets_train$text, "(?i)trump")

#How many times is black lives matter mentioned?
tweets_train$blm <-str_count(tweets_train$text, "(?!)blm") + 
  str_count(tweets_train$text, "(?!)blacklivesmatter")

#### GGPLOT PLAYGROUND ####
dev.off()
#Number of characters by number of hashtags
ggplot(tweets_train, 
       aes(x = num.char, y = number.of.hash)) +
  geom_point()

#Number of hashtags
ggplot(tweets_train, aes(number.of.hash)) + 
  geom_bar(position = "stack")

#Number of hashtags
ggplot(data = tweets_train, mapping = aes(x = number.of.hash)) +
  geom_freqpoly(binwidth = 0.5)

inspect(txt_dtm)


#### WHAT ABOUT EMOJIS? ####

#Transform tweet data to ASCII to get it in the right format for extracting emojis
emoji_tweets$text <- stri_trans_general(emoji_tweets$text, "latin-ascii")
emoji_tweets$text <- gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", " ", emoji_tweets$text)

#The emoji package
head(emojis)
emojis[10:20,]

#substitute the UTF-8 code for the emoji description and place between two *'s (takes a minute)
sub_text <- mgsub(emoji_tweets$text, emojis$code, paste0(' *', emojis$description,'* '))
#Remove other junk
sub_text <- gsub("[][!#$%(),.:;<=>@^_`|~.{}]", " ", sub_text)

#Inspect some tweets with emojis and see if the mgsub worked
emoji_tweets[16,]
sub_text[c(16, 25)]

tail(sub_text)
head(sub_text)

#Remove everything but the emoji descriptions between the two symbols
just_emojis <- rm_between(sub_text, '*', '*', extract=TRUE)
just_emojis[c(16,25)]

#Change to a matrix with 1600 rows
just_emojis <- matrix(unlist(just_emojis), ncol = 1, nrow = 1600)
just_emojis <- as.data.frame(just_emojis)

#Remove NAs and empty rows
just_emojis <- just_emojis[!apply(is.na(just_emojis)| just_emojis == "", 1, all),]

#Emoji Counts
sum(str_count(just_emojis, "raised fist")) 
sum(str_count(just_emojis, "grinning face")) 
sum(str_count(just_emojis, 'rolling on the floor laughing')) 
sum(str_count(just_emojis, 'face with tears of joy'))
sum(str_count(just_emojis, 'thinking face'))
sum(str_count(just_emojis, 'black heart'))

#Make an emoji corpus
emoji_corpus <- VCorpus(VectorSource(just_emojis))
emoji_corpus

#Make TDM
emoji_TDM  <- TermDocumentMatrix(emoji_corpus)
emoji_TDMm <- as.matrix(emoji_TDM)

#Frequency Data Frame
emoji_freq <- rowSums(emoji_TDMm)
emoji_freq <- data.frame(word=names(emoji_freq),
                         frequency=emoji_freq, 
                         row.names = NULL)

#Simple barplot; values greater than 10 
top_words      <- subset(emoji_freq, emoji_freq$frequency >= 10) 
top_words      <- top_words[order(top_words$frequency, decreasing=F),]

#Change to factor for ggplot
top_words$word <- factor(top_words$word, 
                         levels=unique(as.character(top_words$word))) 

#GGplot for "emoji word" frequency...would have liked to have kept the emoji phrases together, 
#but that task will take me more time/research. Just happy that I got here. :)
ggplot(top_words, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#1b7ea1') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

#End - this was fun!!