# CRIM 4012
# Homework 9: Final Prediction Challenge
# Saara Ghani - 15486636

#### Set Up ----
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")
load("data/helpwanted.RData")

# Load the required packages
library(tidyr)
library(dplyr)
library(tm)

#### Natural Language Processing ----

# The following code will take the four long text columns: 'company_profile',
# 'description', 'requirements', and 'benefits' and determine the 10 most common
# words for rows that have been marked as scams. It then creates a column for each
# word and checks the text to see if the word occurs, leaving a "1" if the word
# is present, and a "0" is the word is not present. These 0s and 1s will then be used,
# along with other numeric data in the data set, to create a model that predicts
# whether the job posting is a scam.
# Later, I will use the same words on the dJobsPred (prediction) dataset, to predict
# which postings are scams and which are real.


# Function to get the top 10 most used words
topTenWords <- function(dfCol) {
  corpus <- Corpus(VectorSource(dfCol))
  
  # Pre-processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Tokenization
  matrix <- as.matrix(DocumentTermMatrix(corpus))
  
  # Show top 10 most frequent words
  head(sort(colSums(matrix), decreasing = TRUE), 10)
}

# Now we are going to create40 new columns! For each feature (company_profile,
# description, requirements and benefits) we will create 10 additional columns,
# one for each of the top 10 words. Each column will have a 1 (the word occurs)
# or a 0 (the word does not occur)
 


## Create the 4 lists of key words (these 4 lists will be used later on the 
# test data set to make our predictions)
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

# View the lists!
companyProfileKeyWords
descriptionKeyWords 
requirementsKeyWords
benefitsKeyWords


# A function to create 10 new columns (10 key words for each above feature)
keywordColCreator <- function(df, featureColName, keyWordsList) {
  
  for (i in 1:length(keyWordsList)) {
    df <- df |>
      mutate(!!paste(featureColName, 
                     "_KEYWORD_", 
                     keyWordsList[i], 
                     sep = "") 
             := as.numeric(grepl(keyWordsList[i], 
                                 tolower(df[[featureColName]])))) 
  }
  return(df)
}


# Run the function to create your columns
dJobsTrain <- keywordColCreator(dJobsTrain, "company_profile", companyProfileKeyWords)
dJobsTrain <- keywordColCreator(dJobsTrain, "description", descriptionKeyWords)
dJobsTrain <- keywordColCreator(dJobsTrain, "requirements", requirementsKeyWords)
dJobsTrain <- keywordColCreator(dJobsTrain, "benefits", benefitsKeyWords)


# Now you have 40 additional columns in the data set that can be used to train
# a machine learning model to predict scam job alerts!


#### Update Prediction Data Set ----

# Add all of the new columns to the prediction data set
dJobsPred <- keywordColCreator(dJobsPred, "company_profile", companyProfileKeyWords)
dJobsPred <- keywordColCreator(dJobsPred, "description", descriptionKeyWords)
dJobsPred <- keywordColCreator(dJobsPred, "requirements", requirementsKeyWords)
dJobsPred <- keywordColCreator(dJobsPred, "benefits", benefitsKeyWords)




#### Prediction Option 1: KNN Model ----

# Chosen Model: K-Nearest Neighbors
# The first model I have chosen to investigate is the KNN prediction model. I 
# will build and test the model, assessing the Bernoulli Log Likelihood value.
# After, I will build and test another model, choosing which one has better performance
# and then making my predictions on the dJobsPred (predictiond) data set.

# Load the required packages
library(FNN)
library(dplyr)
library(tidyr)
library(doParallel)

# Prepare the data
X <- dJobsTrain[, c(10:12,19:58)]
y <- dJobsTrain$fraudulent

# Determining the optimal value for K
set.seed(9292)
i <- c(1:nrow(X))
kval <- round(seq(1,100,length=50))
cl <- makeCluster(12)
registerDoParallel(cl)

bernLL <- foreach(j=1:length(kval), .combine = c, .packages = "FNN") %dopar%
  {
    knnPred <- FNN::knn.cv(train = X[i, ], 
                           cl = y[i], 
                           k = kval[j],
                           prob = TRUE)
    
    # Extract the predicted probabilities
    p <- attr(knnPred, "prob")
    p <- ifelse(knnPred==1, p, 1-p)
    p <- (p*kval[j] + 1)/(kval[j] + 2)
    
    # Compute average Bernoulli log-likelihood
    avebernLL <- mean(ifelse(y[i]==1, log(p), log(1-p)))
    
    return(avebernLL)
  }

stopCluster(cl)
kBest <- kval[which.max(bernLL)]
kBest # 58
max(bernLL) # -0.1303

#### KNN Model Prediction ----
knn1 <- knn(train   = X[i,],
            test    = dJobsPred[, c(10:12,19:58)],
            cl      = y[i],
            k       = kBest,
            prob    = TRUE)

p <- attr(knn1, "prob")
p <- ifelse(knn1==1, p, 1-p)
p <- (p*kBest + 1)/(kBest + 2)
p # probability of fraud job posting

# Add predictions to data frame
dJobsPred$fraudulent <- p
mean(dJobsPred$fraudulent)


#### Prediction Option 2: Decision Tree ----

# Load the required packages
library(rpart)

# Fit a regression tree
tree1 <- rpart(fraudulent ~ telecommuting+has_company_logo+has_questions+
                 company_profile_KEYWORD_candidates+company_profile_KEYWORD_services+
                 company_profile_KEYWORD_recruiting+company_profile_KEYWORD_bonus+
                 company_profile_KEYWORD_business+company_profile_KEYWORD_new+
                 company_profile_KEYWORD_solutions+company_profile_KEYWORD_experience+
                 company_profile_KEYWORD_aptitude+company_profile_KEYWORD_staffing+
                 description_KEYWORD_work+description_KEYWORD_will+description_KEYWORD_amp+
                 description_KEYWORD_team+description_KEYWORD_business+description_KEYWORD_position+
                 description_KEYWORD_management+description_KEYWORD_project+description_KEYWORD_customer+
                 description_KEYWORD_company+requirements_KEYWORD_experience+requirements_KEYWORD_skills+
                 requirements_KEYWORD_work+requirements_KEYWORD_ability+requirements_KEYWORD_years+
                 requirements_KEYWORD_knowledge+requirements_KEYWORD_amp+requirements_KEYWORD_must+
                 requirements_KEYWORD_communication+requirements_KEYWORD_management+
                 benefits_KEYWORD_benefits+benefits_KEYWORD_company+benefits_KEYWORD_training+
                 benefits_KEYWORD_time+benefits_KEYWORD_paid+benefits_KEYWORD_work+
                 benefits_KEYWORD_environment+benefits_KEYWORD_can+benefits_KEYWORD_opportunity+
                 benefits_KEYWORD_working, 
               data = dJobsTrain, method = "anova")

# Determine the best sized tree
plotcp(tree1)
bestCP <- with(tree1, cptable[which.min(cptable[,"xerror"]),"CP"])

# Obtain the best tree based on the minimum cross-validated error
treeBest <- prune(tree1, cp = bestCP)

# Display the pruned tree
par(xpd=NA)
plot(treeBest, uniform=TRUE)
text(treeBest, cex=0.4)


# Compute Bernoulli log-likelihood
predictions <- predict(treeBest, data = dJobsTrain, type = "vector")
mean(ifelse(y == 1, log(predictions), log(1 - predictions))) 
# -0.1349


#### Decision Tree Prediction ----
predictions <- predict(treeBest, newdata = dJobsPred, type = "vector")
mean(predictions)



#### Assemble Submission Docs ----
dJobsP <- data.frame("job_id" = dJobsPred$job_id, p = dJobsPred$fraudulent)

#