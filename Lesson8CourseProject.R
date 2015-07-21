library(caret)
library(randomForest)
library(corrplot)
library(rpart)
library(rpart.plot)

set.seed(12345)

#training and testing data files are assumed to be in the working directory
training <- read.csv("pml-training.csv", na.strings= c("NA",""," "))
testing <- read.csv("pml-testing.csv", na.strings= c("NA",""," "))

#keep only columns without NAs
training_clean <- training[,colSums(is.na(training)) == 0]

#remove the first 7 columns of the data which is not related to the accelerometers
training_clean <- training_clean[8:length(training_clean)]

#split the cleansed training data into training and cross validation data sets
inTrain <- createDataPartition(y = training_clean$classe, p = 0.7, list = FALSE)
training <- training_clean[inTrain, ]
crossvalidation <- training_clean[-inTrain, ]

#correlation matrix
correlationMatrix <- cor(training[, -length(training)])
corrplot(correlationMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))
#corrplot(correlationMatrix, method = "color")

#fit a model to predict the classe variable using all other variables as predictors
modFit <- randomForest(classe ~ ., data = training)

#crossvalidate the model using the remaining 30% of data
prediction <- predict(modFit, crossvalidation)
confusionMatrix(crossvalidation$classe, prediction)

#decision tree
treeModel <- rpart(classe ~ ., data=training, method="class")
prp(treeModel)

#apply same cleansing method to the testing data
testing_clean <- testing[,colSums(is.na(testing))==0]
testing_clean <- testing_clean[8:length(testing_clean)]

#predict the classe variable of the test set
predictionTest <- predict(modFit, testing_clean)
predictionTest

answers <- as.character(predictionTest)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)
