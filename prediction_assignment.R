library(randomForest)
library(corrplot)
library(caret)
set.seed(123)

setwd("C:/Users/lynnaj/Desktop/Desktop/coursera/machine learning/assignment")
dataTraining <- read.csv("pml-training.csv", na.strings= c("", " ", "NA"))

# clean the data by removing columns with NAs etc
NAdf <- NULL
for(i in 1:ncol(dataTraining)){
  NAdf[i] <- sum(is.na(dataTraining[,i]))
}


dataTraining <- dataTraining[,which(NAdf == 0)]

#delete first 7 columns as they're not applicable predictors.
dataTraining <- dataTraining[-c(1:7)]

#cross validation
inTrain <- createDataPartition(y = dataTraining$classe, p = 0.7, list = FALSE)
trainset <- dataTraining[inTrain, ]
crossset <- dataTraining[-inTrain, ]

#correlation between variables increase error rate, so we check 
#correlations <- cor(trainset[, -c(1, 4, 10, 53)])
#library(corrplot)
#corrplot(correlations)

trainset <- trainset[-c(1, 4, 10)]

#"accel_belt_x" is correlated with 3 other vars, so i'll delete this
#"roll_belt"
#"total_accel_belt"
modelFit <- randomForest(classe ~ ., data = trainset)
modelFit

# crossvalidate the model using the remaining 30% of data
predictCross <- predict(modelFit, crossset)
confusionMatrix(crossset$classe, predictCross)

# load the test data with 20 observations
dataTest <- read.csv("pml-testing.csv", na.strings= c("", " ", "NA"))

# predict the classes of the test set using the model
predictTest <- predict(modelFit, dataTest)
answers <- predictTest
answers <- as.character(answers)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
