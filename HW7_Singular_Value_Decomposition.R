# CRIM 4012 - Machine Learning For Social Science
# Homework # 7
# Student name: Saara Ghani
# Student ID: 15486636


# Set Up
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")

library(dplyr)
# one time make sure to get the vdemdata
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
varsID <- c("country_name","histname","country_text_id","year")
varsMedia <- c("v2mecenefm","v2mecenefi","v2mecenefibin","v2mecrit",
               "v2merange","v2mefemjrn","v2meharjrn","v2meslfcen",
               "v2mebias","v2mecorrpt")
dMedia <- vdem[c(varsID,varsMedia)] |>
  filter(year==2022)
X <- data.matrix(dMedia[varsMedia]) |> scale() |> t()




# Q1. Using R, compute the singular value decomposition of X.
svdX <- svd(X)




# Q2. Show that UDv1, where v1 is the first row of V, reconstructs the media 
# data for the first column of X.
UDv1 <- with(svdX, u %*% diag(d) %*% v[1,])
X[,1]
all(abs(UDv1 -  X[,1]) < 1e-10)



# Q3. Compute U'U and V'V. What kind of matrices are these?
with(svdX, t(u) %*% u) |> zapsmall()
with(svdX, t(v) %*% v) |> zapsmall()
# Answer: They are both 10x10 square matrices with ~0 values and ~1 on the 
# diagonal




# Q4. Plot the diagonal elements of E. How many left singular vectors seem 
# important?
svdX$d
plot(svdX$d, type = "o", log = "y", 
     xlab = "Singular value order",
     ylab = "Singular values",
     main = "Diagonal Elements of E",
     pch = 20)
# Answer: One left singular vector lies above 20 on the y axis (which are log 
# values), and therefore seems very important. 2 points lie above 10 on the 
# y axis, and could be argued to be important as well. The remaining 7 points 
# lie below 8 and can therefore be argued to be not as important.




# Q5. Create a barplot of the first left singular vector as (code given). 
# Judging by which bars are higher or lower, what is the first singular vector 
# capturing? Describe in terms of media measurements (not mathematics).
svdX$u[,1]
barplot(svdX$u[,1],
        names.arg = varsMedia, 
        cex.names=0.5, 
        las = 2, 
        main = "First Left Singular Vector")
# Answer: The first singular vector is showing which features that have the most 
# significant impact on the most dominant variation of the data. The features 
# with the most influence appear to be:
  # Government censorship effort (v2mecenefm)
  # Print/broadcast media that criticize the government (v2mecrit)
  # Harassment of journalists (v2meharjrn)
# These are followed closely by:
  # Print/broadcast media wide range of political perspectives (v2merange)
  # Media self-censorship (v2meslfcen)
  # Media self-censorship (v2meslfcen)
# And these are followed closely by:
  # Internet censorship effort (v2mecenefi)
  # Internet censorship effort (v2mecenefi)
# And the two features with very low influence in the first singular vector are:
  # Internet availability (v2mecenefibin)
  # Female journalists (v2mefemjrn)




# Q6. The first column of V indicates how much weight each country puts on the 
# first left singular vector. What are the five countries with the largest 
# values in this first column of V? What are the five countries with the 
# smallest values in this first column of V? Do the countries listed align with 
# your interpretation in (5)?
dMedia$v1 <- svdX$v[,1]
head(dMedia$country_name[order(-dMedia$v1)],5) # Greatest Weight
tail(dMedia$country_name[order(-dMedia$v1)],5) # Least Weight 
# Answer: The 5 countries with the largest values in the first column of V are 
# North Korea, Turkmenistan, Eritrea, Syria, and Laos. This countries have 
# historically had issues with freedom of speech and press perpetrated by the 
# government. North Korea is ranked lowest in the World Press Freedom Index 2023.
# On the other hand, the 5 countries with the lowest values in the first column 
# of V are Estonia, Belgium, Sweden, Switzerland, Denmark - all of which have 
# strong laws to promote freedom of press. Sweden was actually the first country 
# in the world to adopt a press freedom law (in 1766). 
# Moreover, the countries with larger weights are all autocracies/dictatorships,
# while the countries with the smaller weight are all working democracies. This 
# aligns with the study's measurement of democracies.
# Therefore, the countries listed do align with my interpretation in Q5.




# Q7. Which component of the second left singular vector is strikingly 
# different from the rest?
barplot(svdX$u[,2],
        names.arg = varsMedia, 
        cex.names=0.5, 
        las = 2, 
        main = "Second Left Singular Vector")
# Answer: Internet Availability (v2mecenefibin) is striking different, as it 
# is significantly lower than the other components. Female journalists 
# (v2mefemjrn) is also quite different for the second left singular vector. Both
# of these features' weight are significantly lower than the weights of the other 
# features, which lie betwee, -0.2 and +0.2.




# Q8. How would you describe the third left singular vector?
barplot(svdX$u[,3],
        names.arg = varsMedia, 
        cex.names=0.5, 
        las = 2, 
        main = "Third Left Singular Vector")
# Answer: The third left singular vector is seemingly mostly insignificant, as
# most of the values are between -0.2 and +0.2. However, there are two features
# that do have a seemingly significant weight. Internet Availability
# (v2mecenefibin) has a very high value at above 0.4. Harassment of Journalists
# (v2meharjrn) has a very low value, at -0.8. For this model, internet
# availability is high, but journalist harassment is a significant issue.




# Q9. Create a scatterplot of svdX$v[,1] and svdX$v[,2]. These are the weights
# that each country applies to the first and second singular vectors. You may
# wish to use dMedia$country_text_id to mark each point so that you can
# determine which country is which.
plot(svdX$v[,1], svdX$v[,2],
     xlab="Weights on 1st Right Singular Vector",
     ylab="Weights on 2nd Right Singular Vector",
     main="Comparison of Weights of Countries on Singular Vectors",
     pch=19, col="mediumpurple1")
text(svdX$v[,1], 
     svdX$v[,2], 
     labels = dMedia$country_text_id, 
     pos = 3, cex = 0.6)




# Q10. In the plot generated in (9), which countries are substantially separated
# from the others? What is the main feature that separates them from the other
# countries?
tempCountries <- c("CAF", "SDN", "ETH", "NIC", "ERI", "NIC", "ERI", "PRK")
dMedia$country_name[which(dMedia$country_text_id %in% tempCountries)]

for(i in 1:length(tempCountries)) {
  print(paste(dMedia$country_name[which(dMedia$country_text_id %in% 
                                          tempCountries[i])],
              "---",
              varsMedia[which.max(abs(svdX$v[which(
                dMedia$country_text_id == tempCountries[i]),]))]))
}
rm(tempCountries)
# Answer: The countries that are separated from the others are: Sudan,
# Ethiopia, North Korea, Nicaragua, Central African Republic, and Eritrea.
# The main feature that separates these countries from the others is Internet
# Censorship Effort (v2mecenefi)




# Q11. Create a world map that shades each country based on the first column
# of V, the weight each country places on the first left singular vector. There
# are a variety of ways to do this in R, but the most straightforward may be to
# use ggplot2 and map_data().
library(ggplot2)
world <- map_data("world")
# To help you further, you may wish to attach the first right singular vector 
# to dMedia.
dMedia$v1 <- svdX$v[,1]
# Some country names in world are not spelled exactly the same as in the V-Dem 
# data. You should first correct the spelling differences before joining with 
# dMedia.
world <- world |>
  mutate(region=case_match(region,
                           "USA" ~ "United States of America",
                           "UK" ~ "United Kingdom",
                           "Republic of Congo" ~ "Republic of the Congo",
                           "Myanmar" ~ "Burma/Myanmar",
                           "Czech Republic" ~ "Czechia",
                           .default = region)) |>
  left_join(dMedia |> select(country_name, v1),
            by=join_by(region==country_name))

# Answer: Generate the map:
ggplot(world, aes(x = long, y = lat, group = group, fill = v1)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(name = "Weight",
                      low = "lightpink", high = "purple4") +
  labs(title = "Weight on First Left Singular Vector By Country") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title





# Q12. The following countries are former British colonies.
formerUKcolony <- c("AUS","AFG","BGD","BHR","BRB","BWA","CAN","CYP","EGY",
                    "SWZ","FJI","GHA","GUY","IND","IRL","IRQ","ISR","JAM",
                    "JOR","KEN","KWT","LSO","LBY","MWI","MDV","MLT","MUS",
                    "MMR","NGA","NZL","OMN","PAK","QAT","SYC","SLE","SLB",
                    "SML","YEM","LKA","SSD","GMB","TTO","UGA","ARE","USA",
                    "VUT","ZAF","ZMB","ZZB","ZWE")
# Use any machine learning method that we have previously studied to predict 
# prior status as a British colony from the 10 right singular vectors. First, 
# be sure to attach V to dMedia.
dMedia <- cbind(dMedia, V=svdX$v)


# Selected Machine Learning Technique: 
# K-Nearest Neighbors



# MY ANSWER (The following has not been added to my R.Markdown):-


# Step 1. Create a UKcolony Column (0 = No, 1 = Yes)
dMedia$ukColony <- 0
dMedia$ukColony[which(dMedia$country_text_id %in% formerUKcolony)] <- 1

# Load the required packages
library(FNN)
library(dplyr)
library(tidyr)
library(doParallel)

# Prepare the data
X <- dMedia[, c("V.1", "V.2", "V.3", "V.4", "V.5", "V.6", "V.7", "V.8", "V.9", "V.10")] # Select the 10 V features
y <- dMedia$ukColony # outcome


# determining the optimal value for k
set.seed(9292)
# i <- sample(nrow(X), 0.8 * nrow(X))  # 80% training data -----> [if you want, or have time] You could try leave one out cross validation and refit the model 100 times ----> KNN CV WILL DO THAT FOR YOU
i <- c(1:nrow(X)) #<-- maybe go with this
kval <- round(seq(1,100,length=50))
cl <- makeCluster(12)
registerDoParallel(cl)

bernLL <- foreach(j=1:length(kval), .combine = c, .packages = "FNN") %dopar%
  {
    knnPred <- FNN::knn.cv(train = X[i, ], 
                           cl = y[i], 
                           k = kval[j],
                           prob = TRUE)
    
    # extract the predicted probabilities
    p <- attr(knnPred, "prob")
    p <- ifelse(knnPred==1, p, 1-p)
    p <- (p*kval[j] + 1)/(kval[j] + 2) # "smooth" the predicted probabilities
    
    # compute average Bernoulli log-likelihood
    avebernLL <- mean(ifelse(y[i]==1, log(p), log(1-p)))
    
    return(avebernLL)
  }
stopCluster(cl)
kBest <- kval[which.max(bernLL)]
kBest # 27
max(bernLL) # -0.5605454

# Evaluate the performance of the knn classifier on the test dataset
knn_model <- knn(train = X[i,],
                 test = X, 
                 cl = y[i],
                 k = kBest,
                 prob = TRUE)
p <- attr(knn_model, "prob")
p <- ifelse(knn_model==1, p, 1-p)
p <- (p*kBest + 1)/(kBest + 2)
#mean(ifelse(y[-i]==1, log(p), log(1-p))) # Answer: -0.5166949 -- YOU DONT NEED THIS MAYBE

# Predicting Prior Status as a British Colony
predicted_colony <- knn(train = X[i,],
                        test = X, 
                        cl = y[i],
                        k = kBest)

dMedia$predicted_ukColony <- predicted_colony
dMedia[,c("country_name","country_text_id","ukColony","predicted_ukColony")]
mean(dMedia$ukColony == dMedia$predicted_ukColony) # 74% of predictions are correct



# Its is oky if these predictions are bad! Sometimes you may just not be able to predict it,
# orrrrr maybe this is just not the best model to use. That is fine!


