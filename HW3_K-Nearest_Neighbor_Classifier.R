# Name: Saara Ghani
# Penn ID: 15486636
# CRIM4012 HW 3: K-Nearest Neighbor Classifier and the Georgia Parolee Data

# Task:
# For this assignment you will build and evaluate a k-nearest neighbor 
# classifier for the Georgia Parolee data (NIJ challenge).

#### QUESTION 1 ----
# 1. Set Up
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")

library(FNN)
library(dplyr)
library(ggplot2)
library(naivebayes)

nij.df <- read.csv("NIJ_s_Recidivism_Challenge_Full_Dataset_20240131.csv")


#### QUESTION 2 ----
# 2. Convert categorical features to 0/1 indicators
X <- model.matrix(~Gender+Race+Age_at_Release+Gang_Affiliated+
                    Supervision_Level_First+Education_Level+Dependents+
                    Prison_Offense+Prison_Years,
                  data=nij.df)
X <- X[,-1]


#### QUESTION 3 ----
# 3. Make y be the 0/1 outcome for recidivism within 3 years
y <- as.numeric(nij.df$Recidivism_Within_3years == "true")


#### QUESTION 4 ----
# 4. k-Nearest Neighbor Classification Cross-Validation: Using only the 
# training sample (Training_Sample==1), use leave-one-out cross-validation to 
# determine the optimal value for k, the number of neighbors

log_likelihood <- c()
k_values <- seq(250, 3000, by = 250)
i <- which(nij.df$Training_Sample==1)

# Finding the best value for K
for(k in k_values){
  pred <- FNN::knn.cv(X[i,],
                      cl = y[i],
                      k  = k,
                      prob = TRUE)
  
  p <- attr(pred, "prob")
  log_likelihood <- c(log_likelihood, mean(y[i]*log(p) + (1-y[i])*log(1-p)))
  print(log_likelihood)
}

print(log_likelihood)
k_values[which.max(log_likelihood)] # Best K Value: 1750

# (save the log_likelihood just in case, so you don't have to rerun):
# -0.6644408 -0.6604603 -0.6587887 -0.6579851 -0.6570973 -0.6568356 -0.6565006 -0.6566058 -0.6566913 -0.6567430 -0.6569144 -0.6570677

# Calculate the Bernouli Log Likelihood with the best value of K
pred <- FNN::knn.cv(X[i,],
                 cl = y[i],
                 k  = 1750,
                 prob = TRUE)

p <- attr(pred, "prob")
mean(y[i]*log(p) + (1-y[i])*log(1-p)) 
# Ans: -0.6565006


#### QUESTION 5 ----
# 5. Create a plot with various choices of k on the x-axis and the average 
#    Bernoulli log-likelihood on the y-axis.

avgBern <- data_frame(k_values, log_likelihood)

ggplot(data = avgBern,
       aes(x = k_values, y = log_likelihood)) + 
  geom_point() +
  geom_smooth(se=FALSE, color = "purple1") +
  labs(title = "Bernoulli Log-Likelihood by Values of K",
       x = "K-Values",
       y = "Log Likelihood") + 
  theme_bw()


#### QUESTION 6 ----
# 6. Evaluate the performance of the knn classifier on the validation dataset 
# (Training_Sample==0) using the k that you found maximized the average 
# Bernoulli log-likelihood. You should be using knn() for this exercise and
# not knn.cv(). Generate the predicted probabilities for the validation dataset. 
# Compute the average Bernoulli log-likelihood of the validation dataset.

k_values[which.max(log_likelihood)] # K-Value: 1750

i <- which(nij.df$Training_Sample==1)
j <- which(nij.df$Training_Sample==0)

pred <- FNN::knn(train = X[i,],
                 test = X[j,],
                 cl = y[i],
                 k  = 1750,
                 prob = TRUE)

p <- attr(pred, "prob")
mean(y[j]*log(p) + (1-y[j])*log(1-p)) 
# Ans: -0.6571569


#### QUESTION 7 ----
# 7. Compare the knn’s performance to the naive Bayes performance. Specifically,
# fit a naive Bayes classifier to the training dataset. Use the estimated naive 
# Bayes model to get predicted probabilities for the validation dataset. 
# Evaluate the predicted probabilities with the average Bernoulli 
# log-likelihood. Make sure to recode any blank or missing values to a value 
# like “Missing”.

# Recode blank values
nij.df <- nij.df |>
  mutate(Gang_Affiliated=case_match(Gang_Affiliated,"" ~ NA,
                                    .default=Gang_Affiliated),
         Supervision_Level_First=case_match(Supervision_Level_First,"" ~ NA,
                                    .default=Supervision_Level_First),
         Prison_Offense=case_match(Prison_Offense,"" ~ NA,
                                    .default=Prison_Offense))


# Create the Naive Bayes Model from the Training Data
nb1 <- naive_bayes((Recidivism_Within_3years=="true")~Gender+
                     Race+Age_at_Release+Gang_Affiliated+
                     Supervision_Level_First+Education_Level+
                     Dependents+Prison_Offense+Prison_Years,
                   data=subset(nij.df,Training_Sample==1),
                   laplace=1)

# Fit the Naive Bayes model to the validation data
validation_data <- nij.df |> filter(Training_Sample==0)
nb_pred <- predict(nb1, newdata=validation_data, 
                   type="prob")[,2]

# Evaluate the predicted probabilities with the average Bernoulli log-likelihood
y <- as.numeric(validation_data$Recidivism_Within_3years == "true")
mean(y*log(nb_pred) + (1-y)*log(1-nb_pred)) 
# -Ans: -0.6403583


#### QUESTION 8 ----
# 8. Remember that the k-nearest neighbor classifier is sensitive to the 
# particular magnitude and scale of the features in X. Rerun the knn.cv() and 
# knn() again, only this time first center and scale each column of X (that is, 
# subtract the mean and divide by the standard deviation). The function scale() 
# can do this for you. Does knn have better performance when using this 
# normalized version of X?

X2 <- scale(X) # normalizing X
y <- as.numeric(nij.df$Recidivism_Within_3years == "true")
i <- which(nij.df$Training_Sample==1)

# Use knn.cv with scaled version of x
pred <- FNN::knn.cv(X2[i,],
                    cl = y[i],
                    k  = 1750,
                    prob = TRUE)

p <- attr(pred, "prob")
mean(y[i]*log(p) + (1-y[i])*log(1-p))
# Answer before scaling: -0.6565006
# Answer after scaling: -0.6546351


# Use knn with scaled version of x
i <- which(nij.df$Training_Sample==1)
j <- which(nij.df$Training_Sample==0)

pred <- FNN::knn(train = X2[i,],
                 test = X2[j,],
                 cl = y[i],
                 k  = 1750,
                 prob = TRUE)

p <- attr(pred, "prob")
mean(y[j]*log(p) + (1-y[j])*log(1-p))
# Answer before scaling: -0.6571569
# Answer after scaling: -0.6554437

# Answer: 
# Both the knn.cv and the knn answers are larger after scaling each column of X, 
# therefore we can assume that the performance will be better on the normalized 
# version of X.

#### QUESTION 9 ----
# 9. List some reasons why the two versions of knn and the naive Bayes perform 
# differently on this dataset. Why do you think one is better than another in 
# terms of predictive performance? What is it about the models or the data that 
# make one perform better than another?

# Answer:

#### Pro Scaled-KNN over Not-Scaled-KNN:
#
# By scaling the data, all features are treated fairly (with the same amount of 
# magnitude) regardless of their "scale" (range). Therefore, when using a KNN
# model we need to make sure that we properly scale the data before running the
# model.

#### Pro Naive-Bayes:
#
# Missing Values: Naive-Bayes will handle missing values by assigning them a 
# weight of evidence of "0", whereas, when using KNN the presence of missing 
# values will throw an error for which you will have to troubleshoot and make 
# adjustment to your data.
#
# Speed: Naive-Bayes is faster (when applied to large datasets) than KNN, which 
# is slower due to the large number of individual calculations required. 
# Therefore if speed is a concern, one should favour the Naive-Bayes classifier.

##### Pro KNN:
#
# Independent Variables: Naive-Bayes assumes that each feature is independent
# (which is often unrealistic), whereas KNN does not make this assumption and 
# does not treat them as independent, making the prediction potentially more 
# accurate.
#
# Training: Naive-Bayes models require training, whereas KNN is a lazy learning 
# model and therefore does not need to process training data in order to make a 
# prediction.

#### Performance of my model:
#
# The Naive-Bayes model produce a prediction of -0.6403583 which is larger than 
# the prediction made by the KNN model (scaled) which was -0.6554437. Therefore 
# the prediction performance of my Naive-Bayes classifier is slightly better 
# than that of my KNN model.
#
