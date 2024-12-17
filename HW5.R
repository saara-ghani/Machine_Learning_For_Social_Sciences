# CRIM 4012
# Assignment #5
# Saara Ghani; 15486636

setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012")
library(rpart)

#### Q4 ----
bodyfat <- 
  read.table("http://jse.amstat.org/datasets/fat.dat.txt", 
             col.names = c("case", "brozek", "siri","density", "age",
                           "weight_lbs","height_in", "bmi",
                           "fat_free_weight","neck_cm","chest_cm",
                           "abdomen_cm","hip_cm","thigh_cm","knee_cm",
                           "ankle_cm","biceps_cm","forearm_cm",
                           "wrist_cm"))

# Q4a. Fit a regression tree
set.seed(20240214)
tree1 <- rpart(brozek~age+weight_lbs+height_in+neck_cm+chest_cm+abdomen_cm+hip_cm+
                 thigh_cm+knee_cm+ankle_cm+biceps_cm+forearm_cm+wrist_cm,
               data = bodyfat)

# plot the tree
par(xpd=NA)
plot(tree1, uniform=TRUE, compress=TRUE)
text(tree1, cex = 0.8)

# Determine the Best Sized Tree
plotcp(tree1) # Answer = 6

bestCP <- function(myTree)
{
  cpTable <- myTree$cptable
  i <- which.min(cpTable[,"xerror"])
  return( cpTable[i,"CP"] )
}

# Q4c. Obtain what you think is the best tree based on part (b)
treeFinal <- prune(tree1, cp=bestCP(tree1))

# Q4d. Display the tree
plot(treeFinal, uniform=TRUE, compress=TRUE)
text(treeFinal, cex = 0.8)

#### Q5 ----
# Explore additional properties of tree-structured models using the bodyfat data

# Q5a. Randomly select half of the observations
set.seed(6245)
indices <- sample(1:nrow(bodyfat))
half_data <- bodyfat[indices[1:(nrow(bodyfat)/2)], ]

# Q5b. Fit a regression tree to this subsample 
# (selecting the appropriate tree size is part of this process)
tree2 <- rpart(brozek~age+weight_lbs+height_in+neck_cm+chest_cm+abdomen_cm+hip_cm+
                 thigh_cm+knee_cm+ankle_cm+biceps_cm+forearm_cm+wrist_cm,
               data = half_data)

plotcp(tree2) # Best size = 8
treeFinal <- prune(tree2, cp=bestCP(tree2))

# Q5c. Plot the tree.
plot(treeFinal, uniform=TRUE, compress=TRUE)
text(treeFinal, cex = 0.8)


# Q5d. Repeat steps a, b, and c about 4-8 times, each time selecting a 
# different subsample of the dataset. Plot the trees together on a common 
# page and comment on what you see. Are you surprised or concerned by the 
# results?

# Create a function that samples the data and creates a tree based on that 
# sample
halfDataTree <- function() {
  indices <- sample(1:nrow(bodyfat))
  half_data <- bodyfat[indices[1:(nrow(bodyfat)/2)], ]
  
  tempTree <- rpart(brozek~age+weight_lbs+height_in+neck_cm+chest_cm+abdomen_cm+
                      hip_cm+thigh_cm+knee_cm+ankle_cm+biceps_cm+forearm_cm+
                      wrist_cm,
                 data = half_data)
  
  tempTree <- prune(tempTree, cp=bestCP(tempTree))
  return(tempTree)
}

# Create a list to store the trees in
tree_list <- list()
for (i in 1:8) {
  tree_list[[i]] <- halfDataTree()
}

# Plot all 8 trees side by side
par(mfrow = c(2, 4), mar = c(2, 2, 4, 1), oma = c(4, 3, 2, 2))  

for (i in 1:8) {
  par(xpd=NA)
  plot(tree_list[[i]], uniform = TRUE, compress = TRUE, main = paste("Tree ", i), cex = 0.6)
  text(tree_list[[i]], cex = 0.4)
}
mtext("Pruned Regression Trees from Random Sample", outer = TRUE, cex = 1.2)

# Answer:
# The trees take different optimal sizes with different values for where the 
# nodes split. This is not surprising because each tree is modeled from different
# subsets of the data. It is, however, concerning that randomly chosen samples from
# the data set provide such a wide variety of regression trees.
