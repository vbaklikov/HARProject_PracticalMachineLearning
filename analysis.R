library(caret)
library(ggplot2)

#read the training set, converting empty values to NAs when necessary
pmlData <- read.csv("./data/pml-training.csv", header=TRUE, na=c("#DIV/0!","NA"))


summary(pmlData)

inTrain <- createDataPartition(y=pmlData$classe, p=0.7, list=FALSE)
training <- pmlData[inTrain,]
testing <- pmlData[-inTrain,]


#remove near zero variation variables that do not add value to the model
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

training <- training[,!nsv$nzv]

#remove X column as it is useless for predictions
training <- training[-c(1)]

#remove columns with most NAs
nav <- sapply(colnames(training), function(x) if(sum(is.na(training[, x])) > 0.8*nrow(training)){return(T)}else{return(F)})
training <- training[, !nav]

#let's fit the model now using Random Forest and caret's train(). 
#Might take long time to run, but will yield a best fitting model
set.seed(12345)
modFit <- train(classe ~ ., data=training, method="rf", importance = TRUE, trControl = trainControl(method="oob",number=4))
modFit

#predict values for the remainder of the dataset, i.e. testing and analyze accuracy
predictions <- predict(modFit, newdata = testing)
confusionMatrix(predictions,testing$classe)

#predict values for the given validation set and produce files for submission

resData <- read.csv("./data//pml-testing.csv", header = TRUE)
result <- as.character(predict(modFit,newdata = resData))
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./prediction/problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

pml_write_files(result)