rm(list=ls())
# @title 병렬 코딩
# @author MS.Lee
# @version 1.0 2017-08-13



# 01 Setting --------------------------------------------------------------
## library
library(data.table)
library(dplyr)
library(e1071)
library(randomForest)
library(parallel)
library(foreach)
library(doParallel)
library(rvest)
library(googlesheets)

## function
source("MS_model.R", encoding = 'utf-8')
# MS_model()
## var
# date
# date <- format(Sys.time(), "%y%m%d")
# dir.create(paste0("data/result/", date ))

# directory
# path <- paste0("data/result/", date)

set.seed(1)




# 02 Load Data ------------------------------------------------------------

# create data
x <- matrix(runif(1e6), 1000)
y <- gl(2, nrow(x)/2)

# sampling
ind <- sample(nrow(x), nrow(x) * 0.7)

## train / test
trainX <- x[ind, ]
trainY <- y[ind]
testX <- x[-ind, ]
testY <- y[-ind]




# 03-0 Basic SVM & RF ------------------------------------------------------------------
## rep 1
# SVM
svmModel   <- svm(trainX, trainY)
svmPrd <- predict(svmModel, testX)
svmAcc <- sum(svmPrd == testY)/length(testY)

# RF
ranModel <- randomForest(trainX, trainY)
ranPrd <- predict(ranModel, testX)
ranmAcc <- sum(ranPrd == testY)/length(testY)
svmAcc; ranmAcc


MS_model(x, y, 1, "SVM")


# 03-1 forLoop SVM & RF------------------------------------------------------------


## rep 10
t1 <- Sys.time()
result <- data.frame(SVM = NA, RF = NA)
for(i in 1:10){
  # modeling
  svmAcc <- MS_model(x, y, i, "SVM")
  ranAcc <- MS_model(x, y, i, "RF")
  
  # insert data
  result[i, ] <- c(svmAcc, ranAcc)
}
t2 <- Sys.time()
t2 - t1


# 03-2 parallel SVM & RF ----------------------------------------------------

## Setting parallel 
no_cores <- detectCores() - 1 # Calculate the number of cores
cl <- makeCluster(no_cores) # Initiate cluster

# code
clusterExport(cl, varlist=c("x", "y", "MS_model"))
clusterEvalQ(cl, library(e1071))
clusterEvalQ(cl, library(randomForest))

t3 <- Sys.time()
output <- parSapply(cl, 1:10, function(i){
  svmAcc <- MS_model(x, y, i, "SVM")
  ranAcc <- MS_model(x, y, i, "RF")
  result <- c(svmAcc, ranAcc)
  return(result)
})
t4 <- Sys.time()
stopCluster(cl)



# 05 foreach --------------------------------------------------------------

# parallel
no_cores <- detectCores() - 1 # Calculate the number of cores
cl <- makeCluster(no_cores) # Initiate cluster


doParallel::registerDoParallel(cl)


# code
clusterExport(cl, varlist=c("x", "y", "MS_model"))
clusterEvalQ(cl, library(e1071))
clusterEvalQ(cl, library(randomForest))
t1 <- Sys.time()
output <- foreach(i = 1:10, .combine = rbind) %dopar% {
  svmAcc <- MS_model(x, y, i, "SVM")
  ranAcc <- MS_model(x, y, i, "RF")
  result <- c(svmAcc, ranAcc)
  return(result)
}
t2 <- Sys.time()
stopCluster(cl)


# 03's practice ----------------------------------------------------------------

## for-loop
result <- data.frame(KNN = NA, SVM = NA)
for(i in 1:10){
  set.seed(i)
  ind <- sample(nrow(xP), nrow(xP)*0.7)
}
parallel::parSapply()


## data
data <- iris[rep(seq_len(nrow(iris)), 22), ] 
xP <- data[, 1:4]
yP <- data[, 5] # Species

# sampling
ind <- sample(nrow(xP), nrow(xP)*0.7)

#train / test
trainXP <- xP[ind, ]
trainYP <- yP[ind]
testXP <- xP[-ind, ]
testYP <- yP[-ind]

# knn
knnModel <- knn[trainXP, testXP, trainYP, k = 5]
knnAcc <- sum(knnModel == testYP)/length(testYP)

# SVM
svmModel <- svm(trainXP, trainYP)
svmPrd <- predict(svmModel, testXP)
svmAcc <- svm(svmPrd == testYP)/length(testYP)

#insert data
insert[i, ] <- c(knnAcc, svmAcc)

class::knn(train, test, cl, k = 5)
e1071::sym(x,y)