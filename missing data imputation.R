##################Practical 2 by group Nacional #####################
                      ## Data import ##
credit<-read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
attach(credit)
                   ## Deal with the missing data ##

#find out how many NAs every column has
summary(credit)
!colSums(is.na(credit))
##so we don't have NAs in app_id,app_date,occ_code,disp_income,cust_age,I_06 and CA_03,CA_01,S_02

###Approach 1: with mice package to deal with the missing value by different methods

##combine GOOD and BAD as response factor and set good as 1 and bad as 0
##delete those observations with missing value on response value
cc<-is.na(credit$GOOD)
m<-which(cc==c('TRUE'))
credit<-credit[-m,]
credit$GOOD <- as.numeric(credit$GOOD)
credit$BAD <- as.numeric(credit$BAD)
credit$response <- credit$GOOD*(credit$BAD+1)
###ASSUMPTION2:We assume the application ID and application Data isn't useful to decide whether it is a good account
credit<-credit[,-c(1:4)]
#create dummy variable to fit model(because some model can't deal with datasets with character) 
require(mlr)
occ.code<-data.frame(var=credit$occ_code)
occ.code<-createDummyFeatures(occ.code,cols='var')
res.indicator<-data.frame(var=credit$res_indicator)
res.indicator<-createDummyFeatures(res.indicator,cols='var')
CA.01 <- data.frame(var = as.factor(credit$CA_01))
CA.01 <- createDummyFeatures(CA.01,cols="var")##but CA.01 is full so we don't need it to be predicted
creditdata<- cbind(credit,occ.code,res.indicator,CA.01)
###delete the response columns for mice to work
which(colnames(credit)=="response")
creditdata2<-creditdata[,-22]
require(mice)
# use mice try to impute by different method
response<-creditdata[,22]
mice.rf<-mice(creditdata2,m=1,method='rf')#random forest
mice.rf<- complete(mice.rf,1)
mice.rf$response <- response
write.csv(mice.rf,"mice by random forest.csv")

mice.pmm <- mice(creditdata2,m=1,meth='pmm') #predictive mean matching 
mice.pmm <- complete(mice.pmm,1) 
mice.pmm$response <- response
write.csv(mice.pmm,"mice by predictive mean matching.csv")

mice.cart<-mice(creditdata2,m=1,meth='cart')#Classification and regression trees (any)
mice.cart<-complete(mice.cart,1)
mice.cart$response<-response
write.csv(mice.cart,"mice by classification and regression trees.csv")

mice.sample<-mice(creditdata2,m=1,meth='sample')#Random sample from the observed values (any)
mice.sample<-complete(mice.sample,1)
mice.sample$response<-response
write.csv(mice.sample,"mice by random sample from the observed value.csv")


mice.fastpmm<-mice(creditdata2,m=1,meth='fastpmm')
mice.fastpmm<-complete(mice.fastpmm,1)#Experimental: Fast predictive mean matching using C++ (any)
mice.fastpmm$response<-response
write.csv(mice.fastpmm,"mice by fast predictive mean matching using c++.csv")

mice.norm.boot<-mice(creditdata2,m=1,mth='norm.boot')
mice.norm.boot<-complete(mice.norm.boot,1)#Linear regression using bootstrap 
mice.norm.boot$response<-response
write.csv(mice.norm.boot,"mice by linear regression using bootstrap.csv")


mice.2lonly.norm<-mice(creditdata2,m=1,mth='2lonly.norm')#Imputation at level-2 by Bayesian linear regression (numeric)
mice.2lonly.norm<-complete(mice.2lonly.norm,1)
mice.2lonly.norm$response<-response
write.csv(mice.2lonly.norm,"mice by level-2 by Bayesian linear regression.csv")


###############split data into train and test part by random forest(mice)
require(caret)
train_size<-floor(0.7*nrow(mice.rf))
set.seed(123)
train_ind<-sample(seq_len(nrow(mice.rf)),size=train_size)
train<-mice.rf[train_ind,]
test<-mice.rf[-train_ind,]
##################  Models  ################
require(randomForest)
randomforest.model <- randomForest(as.factor(train$response) ~ disp_income+cust_age+time_emp+I_01+I_02+I_03
                         +I_04+D_01+ER_01+ER_02+I_05+D_02+I_06+P_01+S_01+
                           CA_03+CA_02+S_02+var.FT+var.SA+ var.SB+ var.SC
                         + var.SD + var.SE + var.SF + var.SG + var.SH + var.SJ+ var.SK
                         + var.SL + var.SM + var.SN + var.SO + var.H + var.P
                         + var.R + var.1+var.2+var.3+var.4+var.5, data = train,
                         ntree = 2000)
randomforest.predict <- predict(randomforest.model, test)
confusionMatrix(test$response, randomforest.predict)
varImp(randomforest.model)
varImpPlot(randomforest.model,type=2)



