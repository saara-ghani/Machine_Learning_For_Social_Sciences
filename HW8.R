# CRIM 4012
# HW8 L1 Regularization, Boosting, Propensity Scores
# Submission # 2
# Saara Ghani

# Set Up
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")
library(dplyr)
library(lubridate)
library(glmnet)
library(doParallel)
# remotes::install_github("gbm-developers/gbm3", 
#                         build_vignettes = TRUE, force = TRUE)
library(gbm3)
dStops <- read.csv("./data/car_ped_stops.csv.gz")



# Additional Data Preparation
dStops <- dStops |>
  # just focus on stops of black, Latino, and white drivers/pedestrians
  filter(race %in% c("Black - Latino","Black - Non-Latino",
                     "White - Latino","White - Non-Latino") &
           !is.na(age) &
           # drop those outside a Philadelphia bounding box
           lat > 39.859691 & lng > -75.284893 &
           lat < 40.141624 & lng < -74.954617) |>
  mutate(frisksearch = as.numeric(individual_frisked==1 |
                                    individual_searched==1 |
                                    vehicle_frisked==1 |
                                    vehicle_searched==1),
         datetimeoccur = ymd_hms(datetimeoccur),
         # make time a continuous 24-hour measure
         time = hour(datetimeoccur) + minute(datetimeoccur)/60,
         month = month(datetimeoccur, label = TRUE),
         month = factor(month, ordered = FALSE),
         weekday=factor(weekday),
         districtoccur = factor(districtoccur),
         psa = factor(psa),
         inside_or_outside=factor(inside_or_outside), 
         stoptype=factor(stoptype), 
         gender=factor(gender), 
         race=factor(race))


# Randomly select 80% of the cases to be training observations. The remaining 
# observations we will save for the validation set.
set.seed(20240329)
iTrain <- sample(1:nrow(dStops), 
                 size=round(0.8*nrow(dStops)), 
                 replace=FALSE)





# QUESTION # 1
# Train glmnet() on the training data to predict frisksearch from time, weekday, 
# month, districtoccur, psa, lat, lng, inside_or_outside, stoptype, gender, 
# race, and age. Remember, an important part of using glmnet() is determining 
# the optimal value of [lamda]. How many of the features does glmnet() include 
# in the optimal model?

features <- c("time", "weekday", "month", "districtoccur", "psa", "lat", "lng", 
              "inside_or_outside", "stoptype", "gender", "race", "age")

X <- as.matrix(dStops[iTrain, features])
y <- dStops[iTrain, "frisksearch"]

# Train the glmnet model
set.seed(20240312)
lasso1 <- cv.glmnet(X, 
                    y, 
                    family="binomial",  # logistic regression
                    alpha=1)
with(lasso1, lambda[which.min(cvm)]) |> log() |> round(1)
coef(lasso1, s = "lambda.min") # Answer: included 5 features



# QUESTION # 2
# Use your glmnet model to get predicted probabilities for all the observations 
# in the validation dataset (-iTrain). Evaluate the quality of the predicted 
# probabilities using the Bernoulli log-likelihood.
valData <- dStops[-iTrain,]
valX <- as.matrix(dStops[-iTrain, features])
valY <- dStops[-iTrain, "frisksearch"]

# Calculate predicted probabilities for the validation dataset
predicted_probs <- predict(lasso1, newx=valX, type = "response")

# Calculate Bernoulli log-likelihood
log_likelihood <- sum(valY * log(predicted_probs) + 
                        (1 - valY) * log(1 - predicted_probs))
log_likelihood # -30396.79




# QUESTION # 3
# Use gbmt() to fit a gradient boosted model to the training dataset predicting 
# frisksearch from the same features used in your glmnet model.

set.seed(20240316)
gbm1 <- gbmt(frisksearch~time+weekday+month+districtoccur+psa+lat+lng+
               inside_or_outside+stoptype+gender+race+age,
             data = dStops[iTrain,],
             distribution=gbm_dist("Bernoulli"),
             train_params = training_params(
               num_trees = 5000,
               shrinkage = 0.2,
               bag_fraction = 0.5,
               num_train = nrow(dStops[iTrain,]),
               min_num_obs_in_node = 10,
               interaction_depth = 3,
               num_features = 12),
             cv_folds=10,
             par_details=gbmParallel(num_threads=12),
             is_verbose = TRUE)





# QUESTION # 4
# For k-nearest neighbors, the number of neighbors, k, controlled the 
# complexity of the model. For decision trees, the number of splits controlled 
# the complexity of the model. For L1/LASSO, [lamda] controlled the complexity 
# of the model. What tuning parameter controls the complexity of a gradient 
# boosted model? 
# Find the value of that tuning parameter that optimizes the gbm model fit to 
# the PPD stop data.

# Answer: The number of trees control the complexity of the gradient boosted model.
bestNTree <- gbmt_performance(gbm1,method="cv") 
plot(bestNTree)
# Answer: The best cross-validation iteration was 4978.



# QUESTION # 5
# Use your gbm model to get predicted probabilities for all the observations 
# in the validation dataset (-iTrain). Evaluate the quality of the predicted 
# probabilities using the Bernoulli log-likelihood. Does glmnet() or gbmt() 
# have better predictive performance on the test dataset in terms of Bernoulli 
# log-likelihood?

valData <- dStops[-iTrain,]
valX <- dStops[-iTrain, features]
valY <- dStops[-iTrain, "frisksearch"]

predicted_probs <- predict(gbm1, newdata = valData, 
                           n.trees = bestNTree, type = "response")

# Calculate Bernoulli log-likelihood
log_likelihood <- sum(valY * log(predicted_probs) + 
                        (1 - valY) * log(1 - predicted_probs))
log_likelihood # -26447.97

  # Bernoulli log-likelihood from glmnet() model: -30396.79
  # Bernoulli log-likelihood from gbmt() model: -26447.97
  # The gbmt() model has a better predictive performance on the test dataset in 
  # terms of Bernoulli log-likelihood, since the likelihood is higher for the 
  # gbmt() model.





# QUESTION # 6
# Show a plot of the relative influence of the stop features. What are the 
# top three most important stop features for predicting whether a stop or a 
# frisk occurs?

par(mar=0.1+c(5,7,4,2))
relInf <- summary(gbm1, num_trees=bestNTree)
relInf$var[1:3]
# Answer: The three most important stop features for predicting whether a 
# stop or a frisk occurs are: stop type, age and district of occurrence.





# QUESTION # 7
# For the 8 most important (in terms of relative influence) stop features, show 
# their partial dependence plots.
relInf
relInf$var[1:8]
par(mfrow=c(3,3), mai=c(0.7,0.6,0.1,0.1))
for(xj in relInf$var[1:8])
  plot(gbm1, var_index = xj, num_trees = bestNTree)





# QUESTION # 8
# Estimate ATT using boosted propensity scores to assess race disparities in 
# the search rate comparing black and white drivers and pedestrians. To do so, 
  # 1) subset the data to just black and white drivers. 
  # 2) Use boosting to estimate the propensity scores, the probability that the 
  #    individual stopped by the police is black from the stop features. 
  # 3) For stop features use only time, weekday, month, districtoccur, psa, lat, 
  #    lng, inside_or_outside, and stoptype. You are welcome to use fastDR() to 
  #    facilitate this.
library(fastDR)
library(survey)
library(kableExtra)

subdata <- dStops |> filter(race %in% c("Black - Non-Latino", 
                                        "White - Non-Latino"))
subdata$raceBlack <- ifelse(subdata$race == "Black - Non-Latino", 1, 0)

set.seed(20240316)
ps2 <- fastDR(list(y.form = ~frisksearch, 
                   t.form = ~raceBlack, # 0 if white, 1 if black
                   x.form = ~time + weekday + month + districtoccur + psa + lat + lng + inside_or_outside + stoptype,
                   key.form = ~id), 
              data = subdata, 
              shrinkage = 0.003, 
              n.trees = 3000, 
              y.dist = "quasibinomial", 
              verbose = TRUE)




# QUESTION # 9
# Show the balance table and assess whether the estimated propensity scores 
# have adequately made the stops of white drivers/pedestrians similar to the 
# stops of black pedestrians. 

# (Bonus: To really practice your R skills 
  # 1) show that the entire time distribution matches between the black 
  # drivers/pedestrians and the weighted white drivers/pedestrians, 
  # 
  # 2) show that the spatial distribution of black drivers/pedestrians matches 
  # the weighted spatial distribution of white drivers/pedestrians).

ps2$balance.tab |> 
  kbl(digits = 3) |> 
  kable_classic(lightable_options="striped")

# Answer: The Kolmogorov-Smirnov (KS) statistic shows the difference between the 
# stops for white drivers and the stops for black drives. These differences are 
# all small (almost all less that 1%). Therefore, the propensity scores have 
# successfully made the stops of white and black drivers/pedestrians similar.




# QUESTION # 10
# What is the estimated disparity in frisk/search rate between black and white 
# drivers/pedestrians in Philadelphia accounting for the time, place, and 
# context in which the stop occurred?

ps2$effects |> 
  kbl(digits = 3) |> 
  kable_classic(lightable_options="striped")

# Answer: The treatment effect for the doubly robust analysis is 0.040, which 
# means that the estimated disparity in frisk/search rate is 4.0%. The estimated 
# frisk/search rate for Black individuals is 13.4%, and the rate for white
# individuals is 9.4%

#