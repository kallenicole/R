#' Author: Kalle Georgiev
#' Date: 10-4-2021
#' Purpose: Case I: okcupid
#' “Far better an approximate answer to the right question, which is often vague, 
#' than an exact answer to the wrong question, which can always be made precise.” — John Tukey
#' Side note: my husband and I met on okcupid. 
#' I checked for our profiles, but they are not in the data set. ;)

############################# SCRIPT SETUP ######################################

# load necessary libraries needed for script
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(maps)
library(mapproj)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(hexbin)
library(radiant.data)
library(DataExplorer)

options(scipen=999)

# set working directory
setwd("~/Documents/CSCIE-96/Harvard_DataMining_Business_Student/Cases/I Ok Cupid")

# pull in the CSV data for okcupid, location, and address
profiles <- read.csv("profiles.csv")
latlon<- read.csv("LatLon.csv")
addr <- read.csv("addr.csv")

################################ BASIC EDA PLAYGROUND ###########################
# basic EDA to explore individual variables and what's in each variable

summary(profiles)
head(profiles)
tail(profiles)

#overall structure of data
str(profiles)

#overall dimensions of data, which can also be seen in global environment pane
dim(profiles)

#what are the variables
names(profiles)

#how many unique values are in each variable
length(unique(profiles$offspring)) 
nlevels(as.factor(profiles$offspring))

#what is correlation between age and income?
cor(profiles$age, profiles$income, use = 'complete.obs')

#looking at the makeup of different column data
table(profiles$orientation)
table(profiles$drinks)
table(profiles$job)
table(profiles$status)
table(profiles$offspring)

#there are 218 different ethnic combinations listed (wow)!
profiles %>% 
  count(ethnicity) 

#mostly athletic/fit
profiles %>% 
  count(body_type) 

#mostly younger users
hist(profiles$age) 
plot_density(profiles$age) 

#females and males plotted by age
age_group <- group_by(profiles[, c("sex", "age")], sex, age) %>% summarize(n_age = n())

age_graph <- ggplot(age_group, aes(x= age, y = n_age, color = sex)) + 
  geom_line(size = 1.5)  +
  ggtitle("Males and females by age") +
  scale_color_manual(values = c("#CCCC52", "#DD3569")) + 
  ylab("number") + 
  xlab("age") +
  theme(legend.position="right")

age_graph

# moving to a more complicated version of EDA where I examine more variables against one another
table(profiles$age, profiles$orientation)
table(profiles$religion, profiles$drugs)
table(profiles$offspring, profiles$drinks)
table(profiles$smokes, profiles$drugs)
table(profiles$education, profiles$drinks)


# looking at proportions of two different variables together
sex_drugs <- table(profiles$sex, profiles$drugs)
prop.table(sex_drugs, 1) #every row sums up to 1 (so I'm separating males and females).
prop.table(sex_drugs, 2) #If you want to separate the proportions along the columns, add a 2 to the prop.table instead of 1. 

#answering example question: what percentage of ppl who have kids never drink 
offspring_drinks <- table(profiles$offspring, profiles$drinks)
prop.table(offspring_drinks, 1) 

# feature engineering of different relationships to find connections
profiles$statEDU <- paste(profiles$status, profiles$education, sep = '_') #relationship status/education
table(profiles$statEDU)

############################### CLEAN UP AREA ########################################
#' my approach with numeric values is to replace with the mean.
#' my approach with categorical values is to say that the user was "silent"
#' because not responding to a question is a response in itself especially on a dating profile.
#' i don't believe that taking the average of people who take drugs for example and imputing it onto 
#' 81% of users who didn't respond would be the best approach. also, you can learn from users' silence.
#' i have sorted the variables here by how unwilling users were to respond on a particular question. 

# clean up income variable by replacing mean for NA values
sum(is.na(profiles$income)) # there were 48,442 missing values (wow, 81% of users were silent)
profiles$income[is.na(profiles$income)] <- mean(profiles$income, na.rm=TRUE)

#replaced no offspring response with "silent"
#I'll filter later for wants/doesn't want/has.
sum(is.na(profiles$offspring)) #there were 35561 missing values (59% of users were silent)
profiles$offspring[is.na(profiles$offspring)] <- "silent"

#replaced no diet response as "silent"
sum(is.na(profiles$diet)) #there were 24395 missing values (41% of users were silent)
profiles$diet[is.na(profiles$diet)] <- "silent"

#replaced no religion response with "silent"
sum(is.na(profiles$religion)) #there were 20226 missing values (34% of users were silent)
profiles$religion[is.na(profiles$religion)] <- "silent"
#stripped off the devotion to the particular religion
profiles <- profiles %>% mutate(affiliation = gsub(" [A-z ]*", "", religion))
profiles <- profiles %>% filter(affiliation != "other")

#replaced no pets response as "silent"
sum(is.na(profiles$pets)) #there were 19921 missing values (33% of users were silent)
profiles$pets[is.na(profiles$pets)] <- "silent"

#replaced drug use missing values with "silent" as in the user did not respond
sum(is.na(profiles$drugs)) #there were 14080 missing values (23% of users were silent)
profiles$drugs[is.na(profiles$drugs)] <- "silent"

#replaced no sign response with "silent"
#there are lots of categories here, but I did not focus on sign (more of a fun metric)
sum(is.na(profiles$sign)) #there were 20226 missing values (18% of users were silent)
profiles$sign[is.na(profiles$sign)] <- "silent"

#replaced no job response with "silent"
sum(is.na(profiles$job)) #there were 8198 missing values (14% of users were silent)
profiles$job[is.na(profiles$job)] <- "silent"

#replaced no education response with "silent"
sum(is.na(profiles$education)) #there were 6628 missing values (11% of users were silent)
profiles$education[is.na(profiles$education)] <- "silent"

#clean up ethnicity by replacing with mean 
sum(is.na(profiles$ethnicity)) #there were 5680 missing values (9% of users were silent)
profiles$ethnicity[is.na(profiles$ethnicity)] <- "silent"

#replaced no religion response with "silent"
sum(is.na(profiles$smokes)) #there were 5512 missing values (9% of users were silent)
profiles$smokes[is.na(profiles$smokes)] <- "silent"

#fixing the essay with the wordcloud functions later
sum(is.na(profiles$essay0)) #there were 5485 missing values (9% of users were silent)

#replaced no body type response with "silent"
sum(is.na(profiles$body_type)) #there were 5296 missing values (9% of users were silent)
profiles$body_type[is.na(profiles$body_type)] <- "silent"

#replaced drinking with "silent" for no response
sum(is.na(profiles$drinks)) #there were 2985 missing values (only 5% were silent)
profiles$drinks[is.na(profiles$drinks)] <- "silent"

#clean up height by replacing with mean 
sum(is.na(profiles$height)) #there were 3 missing values (most people wanted to share)
profiles$height[is.na(profiles$height)] <- mean(profiles$height, na.rm=TRUE)

#clean up age by replacing the obvious outliers over 100 with the mean age
sum(is.na(profiles$age)) #there were 0 missing values
mean(profiles$age) #32 is the mean age
profiles["age"][profiles["age"] > 100] <- 32

# these variables had no missing values
sum(is.na(profiles$location)) #there were 0 missing values
sum(is.na(profiles$last_online)) #there were 0 missing values
sum(is.na(profiles$orientation)) #there were 0 missing values
sum(is.na(profiles$sex)) #there were 0 missing values


############################# GGPLOT PLAYGROUND #####################################
## RELATIONSHIPS BETWEEN...
#what is the relationship between orientation and drinking?
ggplot(profiles, 
       aes(x = orientation, fill = drinks)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#what is the relationship between sex and wanting kids?
ggplot(profiles, 
       aes(x = sex, fill = offspring)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#what is the relationship between sex and religion (way too many categories for religion)?
ggplot(profiles, 
       aes(x = sex, fill = affiliation)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#what is the relationship between orientation and diet?
ggplot(profiles, 
       aes(x = orientation, fill = diet)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#what is the relationship between drinking and drugs? 
#most people who were silent on drugs were also silent on drinking.
ggplot(profiles, 
       aes(x = drinks, fill = drugs)) +
  geom_bar(position = "fill") + 
  ylab("proportion")


##FILTER FOR AGE GROUPS##
#zoom into older users
over_fifty <- profiles %>% 
  filter(age >= 50)

#And we want to add the counter group of under 50
under_fifty <- profiles %>% 
  filter(age < 50)

#what about the over 50 crowd? What's their deal?
summary(over_fifty)
table(profiles$offspring)
table(over_fifty$orientation)

#what is the ratio of age to silent responses
sum(apply(over_fifty, 1, function(r) any(r %in% c("silent")))) #over 50 have 2463 silent responses
sum(apply(under_fifty, 1, function(r) any(r %in% c("silent")))) #under 50 have 44096 silent responses


#plotting sexual orientation of older users over 50
ggplot(data = over_fifty, mapping = aes(x = age, colour = orientation)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drinking of older users over 50
ggplot(data = over_fifty, mapping = aes(x = age, colour = drinks)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drugs of older users over 50
ggplot(data = over_fifty, mapping = aes(x = age, colour = drugs)) +
  geom_freqpoly(binwidth = 0.5)

#plot the users OVER 50
ggplot(data = over_fifty, mapping = aes(x = age)) +
  geom_histogram(binwidth = 0.3)

#why are half of the muslim users over 50 married...?
ggplot(over_fifty, 
       aes(x = affiliation, fill = status)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#what percentage of offspring (or insert variable) responses are silent by age group?
over_fifty %>% 
  group_by (over_fifty$offspring == "silent") %>% 
  summarise (percent = 100 * n() / nrow(over_fifty))


#zoom into users BETWEEN 30-50
thirty_fifty <- profiles %>% 
  filter(age < 50 & age > 30)

#plotting sexual orientation of users 31-49
ggplot(data = thirty_fifty, mapping = aes(x = age, colour = orientation)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drinking of users 31-49
ggplot(data = thirty_fifty, mapping = aes(x = age, colour = drinks)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drugs of users 31-49
ggplot(data = thirty_fifty, mapping = aes(x = age, colour = drugs)) +
  geom_freqpoly(binwidth = 0.5)

#plot the users 31-49
ggplot(data = thirty_fifty, mapping = aes(x = age)) +
  geom_histogram(binwidth = 0.3)

#what percentage of diet (or insert variable) responses are silent by age group?
thirty_fifty %>% 
  group_by (thirty_fifty$diet == "silent") %>% 
  summarise (percent = 100 * n() / nrow(thirty_fifty))


#zoom into younger users UNDER 30
thirty_under <- profiles %>% 
  filter(age <= 30)

#plotting sexual orientation of users under 30
ggplot(data = thirty_under, mapping = aes(x = age, colour = orientation)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drinking of users under 30
ggplot(data = thirty_under, mapping = aes(x = age, colour = drinks)) +
  geom_freqpoly(binwidth = 0.5)

#plotting drugs of users under 30
ggplot(data = thirty_under, mapping = aes(x = age, colour = drugs)) +
  geom_freqpoly(binwidth = 0.5)

#what percentage of diet (or insert variable) responses are silent by age group?
under_fifty %>% 
  group_by (under_fifty$diet == "silent") %>% 
  summarise (percent = 100 * n() / nrow(under_fifty))


##FILTER BY SEX##
males <- filter(profiles, sex == "m")
females <- filter(profiles, sex == "f")

hist(females$age)
hist(males$age)

#boxplots
ggplot(data = males, mapping = aes(x = drinks, y = age)) +
  geom_boxplot()

ggplot(data = females, mapping = aes(x = drinks, y = age)) +
  geom_boxplot()

#covariation between categorical variables
ggplot(data = males) +
  geom_count(mapping = aes(x = drinks, y = age))

ggplot(data = females) +
  geom_count(mapping = aes(x = affiliation, y = offspring))

#relationship between age and income for males and females
#Because there is so little data on income and 81% of people have the mean income, 
#this plot is not so useful.
ggplot(data = males) +
  geom_hex(mapping = aes(x = age, y = income))


##OFFSPRING CATEGORIES EXPLORATION##
##(I am *sure* there is an easier way to do this..I am repeating myself which is never good.)
has <- filter(profiles, offspring == "has a kid, and might want more" |
                offspring == "has a kid, but doesn't want more" |
                offspring == "has kids, and might want more" |
                offspring == "has a kid, and wants more" |
                offspring == "has a kid" |
                offspring == "has kids" |
                offspring == "has kids, but doesn't want more" |
                offspring == "has kids, and wants more"
)
doesnt <- filter(profiles, offspring == "doesn't have kids" | 
                   offspring == "doesn't have kids, and doesn't want any" |
                   offspring == "doesn't have kids, but might want them" |
                   offspring == "doesn't want kids")
wants_doesnt_have <- filter(profiles, offspring == "wants kids" |
                              offspring == "might want kids" |
                              offspring == "doesn't have kids, but wants them")
plot_histogram(wants_doesnt_have)
plot_density(wants_doesnt_have)

#feature engineering of offspring and sex
profiles$offspring_sex <- paste(profiles$sex, profiles$offspring, sep = '_')
table(profiles$offspring_sex)

#finding proportion of females and wanting kids
wants_kids <- table(profiles$offspring, profiles$sex)
prop.table(wants_kids, 2) #answering example question: what percentage of females want kids? 

#making a graph of the relationship between sex and wanting offspring
kid_table = prop.table(table(profiles$sex,profiles$offspring), margin=1)
kid_table = as.data.frame(kid_table)
ggplot(kid_table,aes(x=Var2,y=Freq,fill=Var1)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

#barplot of sex/offspring
barplot(prop.table(table(profiles$sex,profiles$offspring), margin=1))

##GAY USERS##
gay <- filter(profiles, orientation == "gay")
table(gay$sex)
table(gay$affiliation)
plot_histogram(gay)

##BISEXUAL USERS##
bisexual <- filter(profiles, orientation == "bisexual")
table(bisexual$sex)
table(profiles$education)

#what is the relationship between bisexual users, status, and religion
#here again with the married muslim users..
ggplot(bisexual, 
       aes(x = affiliation, fill = status)) +
  geom_bar(position = "fill") + 
  ylab("proportion")


##PROFILE SILENCE##
#Look at the proportion of silence in males/females and <50 & >50. 
sum(apply(males, 1, function(r) any(r %in% c("silent")))) #males have 28309 silent responses 
sum(apply(females, 1, function(r) any(r %in% c("silent")))) #females have 18250 silent responses
sum(apply(over_fifty, 1, function(r) any(r %in% c("silent")))) #over 50 have 2463 silent responses
sum(apply(under_fifty, 1, function(r) any(r %in% c("silent")))) #under 50 have 44096 silent responses

# filtering for empty responses for 4 most blank categories (not income because that was 81% of people)
#ASIDE: it turns out there are 116 users who are silent on 9 variables!
silent <- filter(profiles, offspring == "silent")
silent <- filter(silent, diet == "silent")
silent <- filter(silent, religion == "silent")
silent <- filter(silent, pets == "silent")

#silent EDA
summary(silent)
head(silent)
hist(silent$age)
table(silent$sex)
table(silent$orientation)
table(profiles$orientation)

#how does missing profile information relate to education?
silent_grad <- silent[silent$education %in% c('graduated from college/university', 
                                              'graduated from law school',
                                              'graduated from masters program',
                                              'graduated from ph.d program',
                                              'graduated from med school'), ]
silent_dropped <- silent[silent$education %in% c('dropped out of college/university',
                                                 'dropped out of law school',
                                                 'dropped out of masters program',
                                                 'dropped out of ph.d program',
                                                 'dropped out of two year college',
                                                 'dropped out of space camp',
                                                 'dropped out of high school'), ]
silent_working <- silent[silent$education %in% c('working on college/university',
                                                 'college/university',
                                                 'working on two year degree',
                                                 'two year college',
                                                 'working on law school',
                                                 'law school',
                                                 'working on masters program',
                                                 'masters program',
                                                 'working on ph.d',
                                                 'ph.d',
                                                 'working on med school',
                                                 'med school'
                                                 ),]
  
#what proportion of users with missing information (4 variables) are male/female and what education?
ggplot(silent_working, 
       aes(x = education, fill = drugs)) +
  geom_bar(position = "fill") + 
  ylab("stack") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Missing profile information vs In School vs Drugs")


#most silent are 27 year olds
ggplot(data = silent, mapping = aes(x = age)) +
  geom_histogram(binwidth = 0.1) + 
  labs(title ="Most silent age")
  

#what is the age makeup of these people who don't complete their profiles?
ggplot(data = silent, mapping = aes(x=age)) + geom_bar(position = "stack")


##WEALTHY USERS
mean(profiles$income)
wealthy <- filter(profiles, income > 200000)

#EDA wealthy
wealthy %>% count(sex)
profiles %>% count(body_type)
table(wealthy$offspring)

#answering example question: what percentage of wealthy users who have kids never drink 
offspring_drinks <- table(wealthy$offspring, wealthy$status)
prop.table(offspring_drinks, 2) #answering example question: what percentage of wealthy ppl who have kids never drink 

#ggplot wealthy
ggplot(wealthy, 
       aes(x = drinks, fill = smokes)) +
  geom_bar(position = "fill") + 
  ylab("proportion")

#ages of these wealthy users
ggplot(data = wealthy, mapping = aes(x = age)) +
  geom_histogram(binwidth = 0.1)


############################ DATA ENRICHMENT #######################################

#joining the location and address CSV to profiles 
plus_location <- left_join(profiles, latlon, by ='location')
plus_location <- left_join(plus_location, addr, by ='location')
head(plus_location)
summary(plus_location)


############################ MAPPING PLAYGROUND ####################################

## Regular plot map of okcupid users around the country
map('usa')
points(plus_location$lon,plus_location$lat, col='green')

## ggplot map
stateData <- map_data("state")
head(stateData)

#state and county data
counties <- map_data("county")
CAcounty <- subset(counties, region == "california")
head(CAcounty)
onlyCA   <- subset(plus_location, plus_location$state == "California")
head(onlyCA)

#users not in CA
notCA <- subset(plus_location, plus_location$state != "California")
summary(notCA)

# State/county lines, then layer user location
ca <-ggplot() + geom_map(data  =  CAcounty, map = CAcounty,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) + 
                         coord_map('albers', lat0 = 39, lat1 = 45) +
                         theme_map() 
ca
# Add the points (most people in the bay area)
ca + geom_point(data=onlyCA, aes(x=lon, y=lat, group=157), 
                color='red', alpha=0.15)


########################### WORD CLOUD PLAYGROUND ###################################

#MAKING WORD CLOUDS WITH DATA SUBSETS
# create a vector containing only the essay text
text <- over_fifty$essay0 #replace the data with whatever subset you want

# create a corpus  
docs <- Corpus(VectorSource(text))

# strip out unnecessary white space, punctuation, numbers
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument,language = "english")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

#create a data frame of the most used words and their frequency
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#use wordcloud to show the most used essay words
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


## The end. This was so much fun!
