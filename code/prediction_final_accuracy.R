credit<-read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("dataset_imputed.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]

# intoroduce a new level for missing values
library(gdata)
orig$GOOD <- NAToUnknown(x = orig$GOOD, unknown = "other")

# get a split into test and training data
require(caret)
trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)

# train a random forest model
library(randomForest)
require(e1071)

accuracy <- data.frame("accuracy"= numeric(0), "upper" = numeric(0), "lower" = numeric(0), "x" = numeric(0))
train <- orig[trainIndex,]
test <- orig[-trainIndex,]
# run 100 times to get the difference in accuracy
for (i in 1:100) {
  rf.model <- randomForest(factor(GOOD)~., data = train, 
                           ntree = 482, 
                           mtry = 21, 
                           nodesize = 51, 
                           importance = T)
  rf.predict <- predict(rf.model, test)
  (cm <- confusionMatrix(test$GOOD, rf.predict))
  
  accuracy[nrow(accuracy)+1, ] <- c(cm$overall[1], cm$overall[3], cm$overall[4], i)
  print(i)
}
# save results
write.csv(accuracy, "final_prediction_accuracy.csv")
# plot differnce in values
plot(accuracy$accuracy,ylim = c(0.8,0.9))

# Box plot of values with confidence interaval
require(ggplot2)
ggplot(accuracy, aes(x=x, y=accuracy)) +
geom_point(size=1, pch = 19) +
geom_errorbar(aes(ymax=upper, ymin=lower)) +
xlab("random forest")

