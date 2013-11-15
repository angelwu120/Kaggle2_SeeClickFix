library(randomForest)
library(imputation)
library(MASS)
library(party)

setwd("/Users/Angie/Desktop/RPI/Courses/MGMT 6963/assignment/2")
raw_trainset <- read.csv("train.csv")
raw_testset <- read.csv("test.csv")
sample <- read.csv("sampleSubmission.csv")

raw_testset$num_votes <- 0
raw_testset$num_comments <- 0
raw_testset$num_views <- 0

raw_fullset<-rbind(raw_testset,raw_trainset)

train <- raw_trainset
test <- raw_testset

train$miss_type <- is.na(train$tag_type)
train$miss_source <- is.na(train$source)

test$miss_type <- is.na(test$tag_type)
test$miss_source <- is.na(test$source)

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

########################################
### data clean! source and tag_type! ###
########################################

### can I just put NA into "other"?? ###
train$tag_type<-as.character(train$tag_type)
train$tag_type[is.na(train$tag_type)]<- "other"


test$tag_type<-as.character(test$tag_type)
test$tag_type[is.na(test$tag_type)]<- "other"


### check NA in tag_type ###
test_miss_type<-subset(test, is.na(test$tag_type))
train_miss_type<-subset(train, is.na(train$tag_type))

############################
### predict NA in source ###
############################
train_small <-na.omit(train)
test_small <-na.omit(test)

### prediction for source ###
myFormula <- source ~ num_views + num_votes + num_comments + city 

source_ctree_train <- ctree(myFormula, data=train_small)
source_ctree_test <- ctree(myFormula, data=test_small)

train$source[is.na(train$source)] <- predict(source_ctree_train, newdata = train)[is.na(train$source)]
test$source[is.na(test$source)] <- predict(source_ctree_test, newdata = test)[is.na(test$source)]

### check NA in source ###
train_miss_source<-subset(test, is.na(train$source))
test_miss_source<-subset(test, is.na(test$source))

####################################
### how to clean NA in tag_types ###
####################################

### train set 

train$tag_type[train$summary=="Abandoned Vehicle"]<-'abandoned_vehicles'
train$tag_type[train$summary=="Graffiti Removal"]<-'graffiti'
train$tag_type[train$summary=="Street Light 1 / Out"]<-'street_light'
train$tag_type[train$summary=="Street Light - Outage/Damaged"]<-'street_light'
train$tag_type[train$summary=="Traffic Signal Out"]<-'street_signal'
train$tag_type[train$summary=="Traffic Signal - Outage/Damaged"]<-'street_signal'
train$tag_type[train$desription=="This issue was reported to 
               the City of Oakland Public Works Agency via phone (510-615-5566), 
               email (pwacallcenter@oaklandnet.com), or web (www.oaklandpw.com)."]<-'trash'
train$tag_type[train$summary=="Trash/bulk Pick-ups"] <-'trash'
train$tag_type[train$summary=="Illegal Dumping - debris, appliances, etc."] <-'trash'
train$tag_type[train$summary=="brush"] <-'brush_bulk'
train$tag_type[train$summary=="Brush"] <-'brush_bulk'
train$tag_type[train$summary=="Bulk & brush"] <-'brush_bulk'
train$tag_type[train$summary=="bulk and brush"] <-'brush_bulk'
train$tag_type[train$summary=="Bulk and Brush"] <-'brush_bulk'
train$tag_type[train$summary=="bulk"] <-'brush_bulk'
train$tag_type[train$summary=="Bulk"] <-'brush_bulk'
train$tag_type[train$summary=="Streets - Potholes/Depression"] <-'pothole'
train$tag_type[train$summary=="Rodent Baiting / Rat Complaint"] <-'rodents'

## integer similar tags
train$tag_type[train$tag_type=='abandoned_vehicle']<-'abandoned_vehicles'
train$tag_type[train$tag_type=="roadkill"] <-'road_safety'
train$tag_type[train$tag_type=="pedestrian_light"] <-'street_light'
train$tag_type[train$tag_type=="rodents"] <-'animal_problem'
train$tag_type[train$tag_type=="bad_driving"] <-'road_safety'
train$tag_type[train$tag_type=="noise_complaint"] <-'noise_odor'
train$tag_type[train$tag_type=="odor"] <-'noise_odor'
train$tag_type[train$tag_type=="zoning"] <-'traffic'
train$tag_type[train$tag_type=="drain_problem"] <-'pothole'

## change "small tages" into "other"
train$tag_type[train$tag_type=="public_art"] <-'other'
train$tag_type[train$tag_type=="public_concern"] <-'other'
train$tag_type[train$tag_type=="lost_and_found"] <-'other'

#train.other<-subset(train,tag_type=="other")
                    
### test set 

test$tag_type[test$tag_type=='abandoned_vehicle']<-'abandoned_vehicles'
test$tag_type[test$summary=="Graffiti Removal"]<-'graffiti'
test$tag_type[test$summary=="Street Lights All / Out"]<-'street_light'
test$tag_type[test$summary=="Street Light 1 / Out"]<-'street_light'
test$tag_type[test$summary=="Street Light - Outage/Damaged"]<-'street_light'
test$tag_type[test$summary=="Traffic Signal Out"]<-'street_signal'
test$tag_type[test$summary=="Traffic Signal - Outage/Damaged"]<-'street_signal'
test$tag_type[test$desription=="This issue was reported to 
              the City of Oakland Public Works Agency via phone (510-615-5566), 
              email (pwacallcenter@oaklandnet.com), or web (www.oaklandpw.com)."]<-'trash'
test$tag_type[test$summary=="Trash/bulk Pick-ups"] <-'trash'
test$tag_type[test$summary=="Illegal Dumping - debris, appliances, etc."] <-'trash'
test$tag_type[test$summary=="brush"] <-'brush_bulk'
test$tag_type[test$summary=="Brush"] <-'brush_bulk'
test$tag_type[test$summary=="Bulk & brush"] <-'brush_bulk'
test$tag_type[test$summary=="bulk and brush"] <-'brush_bulk'
test$tag_type[test$summary=="Bulk and Brush"] <-'brush_bulk'
test$tag_type[test$summary=="bulk"] <-'brush_bulk'
test$tag_type[test$summary=="Bulk"] <-'brush_bulk'
test$tag_type[test$summary=="Pothole in Street"] <-'pothole'
test$tag_type[test$summary=="Streets - Potholes/Depression"] <-'pothole'
test$tag_type[test$summary=="Rodent Baiting / Rat Complaint"] <-'rodents'

## integer similar tags
test$tag_type[test$tag_type=="pedestrian_light"] <-'street_light'
test$tag_type[test$tag_type=="roadkill"] <-'road_safety'
test$tag_type[test$tag_type=="rodents"] <-'animal_problem'
test$tag_type[test$tag_type=="bad_driving"] <-'road_safety'
test$tag_type[test$tag_type=="noise_complaint"] <-'noise_odor'
test$tag_type[test$tag_type=="odor"] <-'noise_odor'
test$tag_type[test$tag_type=="zoning"] <-'traffic'
test$tag_type[test$tag_type=="drain_problem"] <-'pothole'

## change "small tages" into "other"
test$tag_type[test$tag_type=="bus_lane"] <-'other'

#test.other<-subset(test,tag_type=="other")


### combine into full data ###
train$tag_type<-as.factor(train$tag_type)
test$tag_type<-as.factor(test$tag_type)
fulldata<-rbind(test,train)




########################################
### Cross Validation---Random Forest ###
########################################
train.cv1 <- train[1:111564,]
train.cv2 <- train[111565:223128,]

## CV1
train.cv1.rf <- randomForest(formula<-num_views ~ city + source + tag_type , data=train.cv1)
train.cv2$pred <- predict(train.cv1.rf, train.cv2)

## CV2
train.cv2.rf <- randomForest(formula<-num_views ~ city + source + tag_type , data=train.cv2)
train.cv1$pred <- predict(train.cv2.rf, train.cv1)

### Test ###
train.rf.view <- randomForest(formula<-num_views ~ city + source + tag_type , data=train)
train.rf.vote <- randomForest(formula<-num_votes ~ city + source + tag_type + num_views, data=train)
train.rf.comm <- randomForest(formula<-num_comments ~ city + source + tag_type + num_views, data=train)

#train.rf$importance
#varImpPlot(train.rf)

test$num_views <- predict(train.rf.view, test)
test$num_votes <- predict(train.rf.vote, test)
test$num_comments <- predict(train.rf.comm, test)

write.csv(test[,c("id", "num_votes", "num_comments", "num_views")], 
          '~/Desktop/RPI/Courses/MGMT 6963/assignment/2/predict.csv')

