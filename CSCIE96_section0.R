#Author: Kalle Georgiev
#CSCIE-96 Section 1, September 30, 2023
######################################## DIVIDE SECTIONS LIKE THIS TO ORGANIZE ###################################

#set working directory
setwd("~/Documents/DataCamp/data_sets")
options(stringsAsFactors = FALSE)
options(scipen=999)
set.seed(123)


library(dplyr) #data manipulation, uses the pipe symbol: %>%
library(ggplot2) #visualizations
library(stringr) #string manipulation, pattern matching, text extraction
library(lubridate) #everything dates, which is normally a mess


#A few basic things to know about R:
#R is case sensitive (even column names) and starts indexing at 1. 
#Everything is rows (1), columns (2)


#creating objects: two main objects - vector & data frame
#vector example
my_vector <- c(12, 15, 61, 20, 34, 14)
mean(my_vector)

#data frame example
transactions <- data.frame(
  acct_num = c("9182723", "9182724", "9182725", "9182726"),
  purchase_amt = c(167.88, 86.52, 809.34, 40.50),
  purchase_date = c("08/10/2023", "08/08/2023", "08/15/2023", "08/11/2023"),
  transaction_id = c(109221, 188280, 196233, 197873),
  stringsAsFactors = FALSE  
)

#this is also a vector: transactions$purchase_amt
mean(transactions$purchase_amt)
median(transactions$purchase_amt)
max(transactions$purchase_amt)
min(transactions$purchase_amt)


############################## sample data set: food inspections #####################################
#read in food inspection data CSV from Kaggle (read.csv = base R)
inspections <- read.csv("kg_inspections.csv")

summary(inspections)
names(inspections)

#filtered data frame with filter() from dplyr + str_detect() from stringr
#all inspections with the word "code" in the "Violations" column
code_violations <- inspections %>%
  filter(str_detect(Violations, "code"))

#cleaning the column values with gsub from base R and a regular expression
#regex: beginning with a number "^\d+"
#regex: ending with a letter (any case): "[a-zA-Z]$"
inspections$Risk <- gsub("^Risk \\d+ \\(|\\)$", "", inspections$Risk)

#changing risk levels to a factor
inspections$Risk <- factor(inspections$Risk, levels = c("Low", "Medium", "High", "All"))

#Advantages of factors over characters according to ChatGPT:
#Ordered Factors: Factors can be ordered. If your data has an inherent order, such as "Low" < "Medium" < "High," you can specify this order with ordered factors. This can be particularly useful for data visualization and modeling.
#Levels Management: You can easily manage and modify the levels of a factor. You can reorder, add, or remove levels as needed. This can be useful for data cleaning and preparation.
#Data Validation: Factors help with data validation. You can ensure that only specific levels are allowed in the variable, reducing the chances of incorrect data entry.
#Statistical Analysis: Factors are essential for statistical modeling in R. Many statistical functions and packages require variables to be factors, especially when you are dealing with categorical predictors in regression models.
#Easy Summarization and Aggregation: When working with factors, you can use functions like table() or summary() to easily summarize and aggregate data by levels.
#Data Visualization: Factors can be directly used in various data visualization packages like ggplot2 for creating bar plots, box plots, and other visualizations based on categorical data.
#Logical Operations: You can perform logical operations directly on factors. For example, you can subset your data based on specific factor levels.
#Improved Memory and Performance: Factors can be more memory-efficient and can sometimes lead to faster calculations compared to character vectors.
#Default Labeling: Factors have built-in labeling of levels, which can make your code and visualizations more informative without the need for additional mapping.

#base R
#subsetting and creating a data frame of all low risk facilities (notice the comma)
low_risk <- inspections[inspections$Risk == 'Low', ]


#can I search the Violations for specific words? 
#grepl (logical) from base R
#assign some key words I want to search on. 
keywords <-"rats|cockroaches|garbage"
gross_violations <-grepl(keywords, inspections$Violations, ignore.case=TRUE)
gross_violations

#what data type is this? 
class(gross_violations) 
#which ones are TRUE? 
which(gross_violations)
#how many are TRUE? 
sum(gross_violations)

#can i subset my inspections where the gross violations are TRUE? 
subset_gross <- inspections[gross_violations, ]

#grep for index positions (global regular expression print)
grease <- grep("grease", inspections$Violations, ignore.case = TRUE)
grease


############################## a little advanced R : writing a function ##########################

#remove all characters from column names that are not letters, numbers, or underscores
remove_special_characters <- function(df) {
  colnames(df) <- gsub("[^A-Za-z0-9_]", "", colnames(df))
  return(df)
}

#call the function and pass in whichever data frame you would like
code_violations <- remove_special_characters(code_violations)
inspections <- remove_special_characters(inspections)

#you can also rename column names like this with dplyr: 
code_violations <- code_violations %>% rename(inspection_type = InspectionType)

#########################################  #library(stringr)  #####################################

users <- data.frame(zip=c("23656", "01221", "80906", "68135"),
                    phone=c("555-421-2222", "555-123-4124", 
                            "555-122-0192", "555-226-0167"),
                    buy=c(TRUE, TRUE, FALSE, TRUE))
users  

weather <- data.frame(temp = c(99, 'error', 89, 
                               92, 92, 90, 88), 
                      direction = c('east','west','west',
                                    'north', 'east', 'north', 'south'))

str_detect(weather$temp, "[[:digit:]]")

temps <- c(1, 7, 6, 0, 32, 
           8, 1, 4, 9, 32)
str_count(temps, "[0-9]")

userdata <- data.frame(phone = c("(555)214-0165", "555.455.1234", 
                                 "555-912-2129", "(555)231-1145", 
                                 "555-314-2568", "555-609-0001"))

userdata$phone <- str_replace_all(userdata$phone, "[[:punct:]]", "")

################################## back to inspections ###################################


#inspecting intersections of two variables (y, x)
table(inspections$Risk, inspections$Results)
table(inspections$Results, inspections$Risk)

#changing date field to a date type using lubridate (dmy, mdy, ymd)
inspections$InspectionDate <- mdy(inspections$InspectionDate)

#how many NAs in each column?
colSums(is.na(inspections))


#Can I engineer a variable? What does that mean? 

#vermin? 
inspections$vermin_flag <- ifelse(grepl("vermin", 
                                  inspections$Violations, 
                                  ignore.case = TRUE), 1, 0)

#all violations with vermin present failed the inspection...that is an insight
#we engineered ourselves and we have data to back it up. 
#this insight was not given to us initially in the data itself.
#it would not be accurate to say all inspection failures had vermin, 
#instead there is a 100% correlation between the presence of vermin and failing an inspection 
table(inspections$vermin_flag, inspections$Results)

# subsetting a data frame using dplyr
# zoom into high risk restaurants... and create a data frame called restaurants_high
restaurants_high <- inspections %>% 
  filter(Risk == 'High' & FacilityType == 'Restaurant')

#what percentage of high risk restaurants passed the inspection?
#output is a "tibble" which is a different data structure created by the tidyverse packages
#tidyr, readr, dplyr, stringr, lubridate, ggplot2
restaurants_high %>% 
  group_by (restaurants_high$Results == "Pass") %>% 
  summarise (percent = 100 * n() / nrow(restaurants_high))


#another way to engineer a variable based on age cuts
#binning ages for example
users <- data.frame(
  user_id = 1:100,
  age = sample(1:100, 100, replace = TRUE)
)

#default for cut is a factor
users$category <- as.character(cut(users$age, breaks = c(0, 18, 35, 60, 100), 
                                   labels = c("child", "young", "adult", "senior")))
############################################# Visualizations #####################################

#What is the frequency of restaurants within each Results category?
ggplot(inspections, aes(Results)) + 
  geom_bar() +
  labs(title="Inspection Results", x="Result", y="Frequency")

#Can we add the facility type to the Results bars? 
ggplot(inspections, aes(x=Results, fill=FacilityType)) + 
  geom_bar() +
  labs(title="Inspection Results", x="Result", y="Frequency")

#That's volume, what about proportion? 
ggplot(inspections, aes(x = Results, fill = Risk)) + 
  geom_bar(position = "fill") +
  labs(title="Inspection Results", x="Result", y="Proportion")



#histogram with simulated numeric grade data
grades <- data.frame(values = (rnorm(1000, mean = 80, sd = 7)))

mean(grades$values)
median(grades$values)

#histogram with binwidth
ggplot(grades, aes(x = values)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color="darkred") + 
  theme_linedraw() +
  labs(title = 'Grades for 1,000 Students')


#boxplot with course data
grades <- data.frame(
  values = c(rnorm(1000, mean = 80, sd = 9),
             rnorm(1000, mean = 70, sd = 4),
             rnorm(1000, mean = 85, sd = 2)),
  courses = rep(c("Physics", "Biology", "Chemistry"), each = 1000)
)

#box plot (Median, IQR, outliers)
#you can use dplyr with ggplot as well
grades %>%
  ggplot(aes(x = courses, y = values, fill = courses)) +
         geom_boxplot()

#what is the type of data, and which visualization is appropriate to tell the story? 
# recommend: https://r-graph-gallery.com/

