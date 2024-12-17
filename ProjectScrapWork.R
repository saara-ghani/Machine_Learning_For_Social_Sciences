# CRIM 4012
# Homework 9: Final Prediction Challenge
# Saara Ghani - 15486636

# Set Up
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")
load("data/helpwanted.RData")

library(tidyr)
library(dplyr)
library(tm)



#### EXPLORING THE DATA ----

head(dJobsPred)
head(dJobsTrain)


dJobsTrain |>
  filter(fraudulent == 1) |>
  select(requirements) |>
  head()

dJobsTrain |>
  filter(fraudulent == 0) |>
  select(requirements) |>
  head()

mean(dJobsTrain$has_company_logo)

# Lets look at the differences between data collected for fraudulent postings 
# and non-fraudulent postings

# Difference in metnion of telecommuting
round((prop.table(table(dJobsTrain$telecommuting == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Difference in use of company logo
round((prop.table(table(dJobsTrain$has_company_logo == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Difference in whether the application has questions
round((prop.table(table(dJobsTrain$has_questions == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Employee Type
round((prop.table(table(dJobsTrain$employment_type, dJobsTrain$fraudulent), 
    margin = 2) * 100), digits = 2)

# Required Experience
round((prop.table(table(dJobsTrain$required_experience, dJobsTrain$fraudulent), 
    margin = 2) * 100), digits = 2)


# Required Education
round((prop.table(table(dJobsTrain$required_education, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Industry
round((prop.table(table(dJobsTrain$industry, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Function
round((prop.table(table(dJobsTrain$function., dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Salary Range
round((prop.table(table(dJobsTrain$salary_range, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)


#### ----

# Explore the Hashtag
dJobsTrain$hashtag <- 0
dJobsTrain$hashtag[grep("#", dJobsTrain$company_profile)] <- 1
round((prop.table(table(dJobsTrain$hashtag == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Explore the Exclamation
dJobsTrain$exclaim <- 0
dJobsTrain$exclaim[grep("!", dJobsTrain$company_profile)] <- 1
round((prop.table(table(dJobsTrain$exclaim == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

# Explore the URL
dJobsTrain$URL <- 0
dJobsTrain$URL[grep("URL", dJobsTrain$company_profile)] <- 1
round((prop.table(table(dJobsTrain$URL == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)


# Explore the &
dJobsTrain$amp <- 0
dJobsTrain$amp[grep("&", dJobsTrain$company_profile)] <- 1
round((prop.table(table(dJobsTrain$amp == 1, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)

#


# Explore the location
dJobsTrain$country <- sub(",.*", "", dJobsTrain$location)
round((prop.table(table(dJobsTrain$country, dJobsTrain$fraudulent), 
                  margin = 2) * 100), digits = 2)




#### Natural Language Processing ----

tolower(dJobsTrain$company_profile)

# create a function to get the top 10 most used fords:
topTenWords <- function(dfCol) {
  corpus <- Corpus(VectorSource(dfCol))
  
  # Pre-processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Tokenization
  dtm <- DocumentTermMatrix(corpus)
  
  # Convert DocumentTermMatrix to a matrix
  matrix <- as.matrix(dtm)
  
  # Get word frequencies
  word_freq <- colSums(matrix)
  
  # Show top 10 most frequent words
  head(sort(word_freq, decreasing = TRUE), 10)
}

# Top 10 Words in the Company Profile
topTenWords(dJobsTrain$company_profile)

# Top 10 words in the Company Profile for Scams
dJobsTrain |>
  filter(fraudulent == 1) |>
  select(company_profile) |>
  topTenWords()



# Now that you have the top ten words of the scams, make a new column for each 
# word and check whether it is present in that column or not!

profileKeyWords <- dJobsTrain |>
                      filter(fraudulent == 1) |>
                      select(company_profile) |>
                      topTenWords() |>
                      names()

# how many of the job postings contain scam trigger words?

for(i in 1:length(profileKeyWords)) {
  print(paste(profileKeyWords[i], ":", 
        length(grep(profileKeyWords[i], 
                    tolower(dJobsTrain$company_profile)))))
}


# # Function that counts scam words per cell
# scamWordFrequency <- function(dfCol) {
#   
#   keyWordsList <- dJobsTrain |>
#     filter(fraudulent == 1) |>
#     select(all_of(dfCol)) |>
#     topTenWords() |>
#     names()
#   
#   # how many of the job postings contain scam trigger words?
#   print(paste("Column Name:", dfCol))
#   a <- dfCol
#   for(i in 1:length(keyWordsList)) {
#     print(paste(keyWordsList[i], ":", 
#                 length(grep(keyWordsList[i], 
#                             tolower(dJobsTrain[[a]])))))
#   }
# }
# 
# scamWordFrequency("company_profile")



# Now lets make a function that does this for all four columns:
# 1. company_profile
# 2. description
# 3. requirements
# 4. benefits

colList <- c("company_profile", "description", "requirements", "benefits")

scamWordFrequency <- function(dfColList) {

  for(i in 1:length(dfColList)) {

    keyWordsList <- dJobsTrain |>
      filter(fraudulent == 1) |>
      select(all_of(dfColList[i])) |>
      topTenWords() |>
      names()

    # how many of the job postings contain scam trigger words?
    print(paste("Column Name:", dfColList[i]))
    a <- dfColList[i]
    for(i in 1:length(keyWordsList)) {
      print(paste(keyWordsList[i], ":",
                  length(grep(keyWordsList[i],
                              tolower(dJobsTrain[[a]])))))
    }
    print("-")
  }
}
scamWordFrequency(colList)


# Now we are going to create40 new columns! For each feature (company_profile,
# description, requirements and benefits) we will create 10 additional columns, 
# one for each of the top 10 words.
# each colum will have a 1 (the word occurs) or a 0 (the word does not occur)


# # start with one feature! (company_profile)
# companyProfileKeyWords <- dJobsTrain |>
#   filter(fraudulent == 1) |>
#   select(company_profile) |>
#   topTenWords() |>
#   names()
# 
# 
# for (i in 1:length(profileKeyWords)) {
#   dJobsTrain <- dJobsTrain |>
#     mutate(!!paste("company_profile", 
#                    "_KEYWORD_", 
#                    profileKeyWords[i], 
#                    sep = "") 
#            := as.numeric(grepl(profileKeyWords[i], 
#                                tolower(dJobsTrain$company_profile)))) 
# }
# 




## Create the 4 lists of key words
keyWordGenerator <- function(colName) {
  dJobsTrain |>
    filter(fraudulent == 1) |>
    select(all_of(colName)) |>
    topTenWords() |>
    names()
}

companyProfileKeyWords <- keyWordGenerator("company_profile")
descriptionKeyWords <- keyWordGenerator("description")
requirementsKeyWords <- keyWordGenerator("requirements")
benefitsKeyWords <- keyWordGenerator("benefits")

# Function to create 10 new columns (10 key words for each above feature)
keywordColCreator <- function(featureColName, keyWordsList) {
  
  print(featureColName)
  print(keyWordsList)
  
  for (i in 1:length(keyWordsList)) {
    dJobsTrain <- dJobsTrain |>
      mutate(!!paste(featureColName, 
                     "_KEYWORD_", 
                     keyWordsList[i], 
                     sep = "") 
             := as.numeric(grepl(keyWordsList[i], 
                                 tolower(dJobsTrain[[featureColName]])))) 
  }
  return(dJobsTrain)
}


# Run the function to create your columns
dJobsTrain <- keywordColCreator("company_profile", companyProfileKeyWords)
dJobsTrain <- keywordColCreator("description", descriptionKeyWords)
dJobsTrain <- keywordColCreator("requirements", requirementsKeyWords)
dJobsTrain <- keywordColCreator("benefits", benefitsKeyWords)













#