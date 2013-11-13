raw_trainset <- read.csv("train.csv")
raw_testset <- read.csv("test.csv")
sample <- read.csv("sampleSubmission.csv")

raw_testset$num_votes <- 0
raw_testset$num_comments <- 0
raw_testset$num_views <- 0

raw_fullset<-rbind(raw_testset,raw_trainset)

###################################
### change longitutue into city ###
###################################
#longitude < -100, Oakland
#-90 < longitude < -80, Chicago
#-80 < longitude < -75, Richmond
#otherwise New Haven.

train$city [train$longitude <  -100] <- "Oakland"
train$city [ train$longitude > -90] <- "Chicago"
train$city [ train$longitude > -80] <- "Richmond"
train$city [ train$longitude > -75] <- "New Haven"
train$city<-as.factor(train$city)

test$city [test$longitude <  -100] <- "Oakland"
test$city [test$longitude > -90] <- "Chicago"
test$city [test$longitude > -80] <- "Richmond"
test$city [test$longitude > -75] <- "New Haven"
test$city<-as.factor(test$city)


############################
### predict NA in source ###
############################
train_small <-na.omit(train)
test_small <-na.omit(test)

###prediction for source###
myFormula <- source ~ num_views + num_votes + num_comments + city 

source_ctree <- ctree(myFormula, data=train_small)
source_ctree <- ctree(myFormula, data=test_small)

train$source[is.na(train$source)] <- predict(source_ctree, newdata = train)[is.na(train$source)]
test$source[is.na(test$source)] <- predict(source_ctree, newdata = test)[is.na(test$source)]

###check NA in source###
train_miss_source<-subset(test, is.na(train$source))
test_miss_source<-subset(test, is.na(test$source))
