
setwd("~/Dropbox/adv.computing")
library(randomForest)
# Load data
readMNISTImages <- function(fname){
  imgCon = file(fname, "rb")
  magicNumber = readBin(imgCon, "int", n = 1, size = 4, endian = "big")
  nImg = readBin(imgCon, "int", n = 1, size = 4, endian = "big")
  nRow = readBin(imgCon, "int", n = 1, size = 4, endian = "big")
  nCol = readBin(imgCon, "int", n = 1, size = 4, endian = "big")
  
  imageList = lapply(1:nImg, function(x){
    matrix(as.numeric(readBin(imgCon, "int", n = nRow*nCol, size = 1, endian = "big")),
           nrow = nRow, ncol = nCol)
  })
  close(imgCon)
  imageList
}

readMNISTLabels <- function(fname){
  labCon = file(fname, "rb")
  magicNumber = readBin(labCon, "int", n = 1, size = 4, endian = "big")
  nItems = readBin(labCon, "int", n = 1, size = 4, endian = "big")
  
  imageLabels = sapply(1:nItems, function(x){
    readBin(labCon, "int", n = 1, size = 1, endian = "big", signed = FALSE)
  })
  close(labCon)
  imageLabels
}

train_img <- readMNISTImages("data/train-images-idx3-ubyte")
train_labels <- readMNISTLabels("data/train-labels-idx1-ubyte")

test_img <- readMNISTImages("data/t10k-images-idx3-ubyte")
test_labels <- readMNISTLabels("data/t10k-labels-idx1-ubyte")

# Turn training data into a matrix we can use in modeling
training_data <- matrix(unlist(train_img), nrow = length(train_img), byrow = TRUE)
training_data = data.frame(Y = as.factor(train_labels),
                           X = training_data)

test_data <- matrix(unlist(test_img), nrow = length(test_img), byrow = TRUE)
test_data = data.frame(Y = as.factor(test_labels),
                       X = test_data)

# Omit pixels with little to no information
keep_idx <- apply(training_data, 2, var) > 1
training_data <- training_data[,keep_idx]

#keep_idx <- apply(test_data, 2, var) > 1
test_data <- test_data[,keep_idx]

# Random Forest function
pred.loss <- function(param, n.itrs = 10){
  print(c(param))
  trials <- sapply(1:n.itrs, function(x){
    
    train.idx <- sample(1:nrow(training_data), size = 48000)
    test.idx <- setdiff(1:nrow(training_data), train.idx)
    MNIST.train <- training_data[train.idx,]
    MNIST.test <- training_data[test.idx,]
    
    rf.out <- randomForest(Y ~ ., 
                           data = MNIST.train, 
                           ntree = 100,
                           sampsize = param[1],
                           nodesize = param[2])
    
    
    rf.pred <- predict(rf.out, MNIST.test)
    1-mean(MNIST.test$Y == rf.pred)
  })
  mean(trials)
}

# Set grid
sampsizes <- c(50, 100,1000)
nodesizes <- c(1, 5, 10)

param.grid <- as.matrix(expand.grid(sampsizes, nodesizes))

# Grid search
rslt <- apply(param.grid, 1, pred.loss)

# Plot result
plotMatrix <- function(M){
  image(t(M[nrow(M):1,]))
}
M <- matrix(rslt, nrow = length(sampsizes), ncol = length(nodesizes))
plotMatrix(1/M)

# Best tunning parameter
param.grid[which.min(rslt),]

# Show results
rslt

# Additional test for larger samle size when fixing node size. 

sampsizes <- c(5000, 10000,15000)
nodesizes <- c(1)

param.grid <- as.matrix(expand.grid(sampsizes, nodesizes))

rslt <- apply(param.grid, 1, pred.loss)



