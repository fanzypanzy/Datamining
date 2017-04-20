
mydat <- read.csv("dataset_imputed.csv", header = TRUE)
credit <- read.csv(read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999")))

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- credit$GOOD
orig <- orig[,-1]
holdout <- holdout[,-1]
cc<-is.na(orig$GOOD)
m<-which(cc==TRUE)

# set NA values in response to bad. We're not sure if this is the right approach, but we will know when we get 
# feedback. If it is the right approach, we expect accuracy ~80% if not ~65%. (Not setting NA to bad should give
# us ~69 % accuracy.)
orig$GOOD[m] <- 0

# Deleting columns with a lot of NAs (showed a little improvement when testing)
del <- c(15, 7, 13, 9, 14)
origDel <- orig[,-del]

# train model on all the original data
final.model <- randomForest(factor(GOOD)~., data = origDel, 
                            ntree = 443, 
                            mtry = 8, 
                            nodesize = 39, 
                            cutoff = c(0.465,0.535),
                            importance = T)

final.predict <- predict(final.model, holdout)

# add a BAd column
bad <- rep(0, times = 4981)
bad[final.predict==0] <- 1
predictNacional <- data.frame(GOOD = final.predict, BAD = bad)

write.csv(predictNacional, "predictionNacional.csv")


