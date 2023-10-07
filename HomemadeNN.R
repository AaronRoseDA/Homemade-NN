rm(list = ls())
options(scipen = 999)
options("print.matrix" = FALSE)

library(ggplot2)

# Define the compute function outside the loop to avoid redefining
compute_Function <- function(train, L1, B1, L2, B2, L3, B3, O1, OB, bestModel = NULL) {
  
  L1_A <- 1 / (1 + exp(-train %*% L1 - B1))
  L2_A <- 1 / (1 + exp(-L1_A %*% L2 - B2))
  L3_A <- 1 / (1 + exp(-L2_A %*% L3 - B3))
  O_A <- 1 / (1 + exp(-L3_A %*% O1 - OB))
  return(which(O_A == max(O_A), arr.ind=TRUE)[2])
}
predict_Function <- function(DATA, bestModel) {
  
  L1_A <- 1 / (1 + exp(-DATA %*% bestModel$L1 - bestModel$B1))
  L2_A <- 1 / (1 + exp(-L1_A %*% bestModel$L2 - bestModel$B2))
  L3_A <- 1 / (1 + exp(-L2_A %*% bestModel$L3 - bestModel$B3))
  O_A <- 1 / (1 + exp(-L3_A %*% bestModel$O1 - bestModel$OB))
  return(which(O_A == max(O_A), arr.ind=TRUE)[2])
}

data <- iris
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
trainMaster  <- data.matrix(data[sample,],rownames.force = T)
test   <- data.matrix(data[!sample,],rownames.force = T)

accuracy <- 0
AccuracyTable <- data.frame(time = 1, Accuracy = accuracy)
bestModel <- list()

layerTime <- 0
computeTime <- 0
accuracyTime <- 0
time <- 1

startTime <- Sys.time()

for (i in 1:100000) {
  # i = 1
  start <- Sys.time()
  L1 <- matrix(runif(n = 16, min = -10, max = 10), nrow = 4, ncol = 4)
  B1 <- runif(4, min = -10, 10)
  L2 <- matrix(runif(n = 16, min = -10, max = 10), nrow = 4, ncol = 4)
  B2 <- runif(4, min = -10, 10)
  L3 <- matrix(runif(n = 16, min = -10, max = 10), nrow = 4, ncol = 4)
  B3 <- runif(4, min = -10, 10)
  O1 <- matrix(runif(n = 12, min = -10, max = 10), nrow = 4, ncol = 3)
  OB <- runif(3, min = -10, 10)
  layerTime <- layerTime + start - Sys.time()
  
  start <- Sys.time()
  table <- cbind(trainMaster[,5],
                 apply(trainMaster[,1:4], MARGIN=1, FUN=compute_Function, L1=L1, B1=B1, L2=L2, B2=B2, L3=L3, B3=B3, O1=O1, OB=OB))
  computeTime <- computeTime + start - Sys.time()
  
  start <- Sys.time()
  proportion_matching <- sum(table[,1] == table[,2]) / nrow(table)
  accuracyTime <- accuracyTime + start - Sys.time()
  
  if (proportion_matching > accuracy) {
    accuracy <- proportion_matching
    bestModel <- list(accuracy = proportion_matching,
                      layerTime = layerTime,
                      computeTime = computeTime,
                      accuracyTime = accuracyTime,
                      L1 = L1, 
                      B1 = B1, 
                      L2 = L2, 
                      B2 = B2, 
                      L3 = L3, 
                      B3 = B3, 
                      O1 = O1, 
                      OB = OB)
    time <- time + 1

    AccuracyTable <-  rbind(AccuracyTable, data.frame(time = time, Accuracy = accuracy))

    plot <- ggplot(AccuracyTable, aes(x = time, y = Accuracy)) +
      geom_line() + # Use points to represent data
      labs(title = "Accuracy over Time", x = "Time", y = "Accuracy") + 
      theme_minimal()
    print(plot)
  }
}

# bestModel_BACKUP <- bestModel

# Print best model after the loop
print(startTime - Sys.time())
print(paste(bestModel$accuracy, bestModel$layerTime, bestModel$computeTime, bestModel$accuracyTime))


table <- cbind(test[,5],
               apply(test[,1:4], MARGIN=1, FUN=predict_Function, bestModel))
proportion_matching <- sum(table[,1] == table[,2]) / nrow(table)




