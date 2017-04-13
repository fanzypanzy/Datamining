credit<-read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("trainimp_with_app_id.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]


library(gdata)
orig$GOOD <- NAToUnknown(x = orig$GOOD, unknown = "other")

library(caret)
library(randomForest)

# Testing whether the model performs better when columns with (originally) many NAs are deleted
creditNoResp <- credit[, -c(2,3,4)]
colSums(is.na(creditNoResp))
lotsofNAs <- sort(colSums(is.na(creditNoResp)), index.return = TRUE, decreasing = TRUE)$ix

colSums(is.na(creditNoResp[,lotsofNAs]))/nrow(creditNoResp)

library(HapEstXXR)

# Calculate accuracy with all columns in model for comparison

trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
dat <- orig
train <- dat[trainIndex,]
test <- dat[-trainIndex,]
rf.model <- randomForest(factor(GOOD)~., data = train,
                         ntree=487, mtry=22,nodesize=54)
rf.predict <- predict(rf.model, test)
cm <- confusionMatrix(test$GOOD, rf.predict)
accuracy_complete <- cm$overall[1]


# can't test all combinations to delete since too computationally expensive
# test 10 columns with most NAs
deleteCombinations <- powerset(lotsofNAs[1:10])

trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
accuracy_delete <- c()
for (i in 1:length(deleteCombinations)) {
  delete <- deleteCombinations[[i]]
  dat <- orig[,-c(delete, lotsofNAs[1:4])]
  train <- dat[trainIndex,]
  test <- dat[-trainIndex,]
  set.seed(2484)
  rf.model <- randomForest(GOOD~., data = train,
                           ntree=487, mtry=22,nodesize=54)
  rf.predict <- predict(rf.model, test)
  cm <- confusionMatrix(test$GOOD, rf.predict)
  accuracy_delete[i] <- cm$overall[1]
  print(i)
  print(accuracy_delete[i])
}

accuracy_delete[length(deleteCombinations)+1] <- accuracy_complete
which.max(accuracy_delete)
deleteCombinations[which.max(accuracy_delete)]
hist(accuracy_delete)
plot(accuracy_delete)

# accuracy seems randomly distributed. (Running it more times generated a different result.)


# Test: is there an actual difference between leaving out columns 1:14 (of lotsofNAs), or not?
# Do this by simulating
# (since randomForest is in fact random and produces slightly different results every time)

# 1) Delete all columns with NAs
accuracy_delete_all <- c()
for(i in 1:100){
  trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
  dat <- orig[,-lotsofNAs[1:14]]
  train <- dat[trainIndex,]
  test <- dat[-trainIndex,]
  rf.model <- randomForest(GOOD~., data = train,
                           ntree=487,nodesize=54)
  rf.predict <- predict(rf.model, test)
  cm <- confusionMatrix(test$GOOD, rf.predict)
  accuracy_delete_all[i] <- cm$overall[1]
  print(i)
  print(accuracy_delete_all[i])
}


# 2) Keep all columns
accuracy_delete_none <- c()
for(i in 1:100){
  trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
  dat <- orig
  train <- dat[trainIndex,]
  test <- dat[-trainIndex,]
  rf.model <- randomForest(GOOD~., data = train,
                           ntree=487,nodesize=54)
  rf.predict <- predict(rf.model, test)
  cm <- confusionMatrix(test$GOOD, rf.predict)
  accuracy_delete_none[i] <- cm$overall[1]
  print(i)
  print(accuracy_delete_none[i])
}

t.test(accuracy_delete_all, accuracy_delete_none)
# signigficant

# 3) Delete all columns with > 20% NAs, so most columns with NAs
accuracy_delete_most <- c()
for(i in 1:100){
  trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
  dat <- orig[,-lotsofNAs[1:11]]
  train <- dat[trainIndex,]
  test <- dat[-trainIndex,]
  rf.model <- randomForest(GOOD~., data = train,
                           ntree=487,nodesize=54)
  rf.predict <- predict(rf.model, test)
  cm <- confusionMatrix(test$GOOD, rf.predict)
  accuracy_delete_most[i] <- cm$overall[1]
  print(i)
  print(accuracy_delete_most[i])
}

write.csv(data.frame(deleteAll = accuracy_delete_all, deleteMost = accuracy_delete_most,
                     deleteNone = accuracy_delete_none),"accuracies.csv")

t.test(accuracy_delete_most, accuracy_delete_none)
t.test(accuracy_delete_most, accuracy_delete_all)

# Conclusion: Delete all columns with more than 20 % variables

