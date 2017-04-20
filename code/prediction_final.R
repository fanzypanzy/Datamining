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

# train a random forest model
library(randomForest)
final.model <- randomForest(factor(GOOD)~., data = orig, 
                         ntree = 482, 
                         mtry = 21, 
                         nodesize = 51, 
                         importance = T)

final.predict <- predict(final.model, holdout)

# add a BAd column
bad <- rep(0, times = 4981)
bad[final.predict==0] <- 1
bad[final.predict=="other"] <- '.'

good <- rep(1, times = 4981)
good[final.predict==0] <- 0
good[final.predict=="other"] <- '.'

predictNacional <- data.frame(GOOD = good, BAD = bad)

write.csv(predictNacional, "final_prediction_nacional.csv")
