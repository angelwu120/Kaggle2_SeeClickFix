####################################
### how to clean NA in tag_types ###
####################################

### train set ###
train <- raw_trainset

train$tag_type[train$tag_type=='abandoned_vehicle']<-'abandoned_vehicles'
train$tag_type[train$summary=="Abandoned Vehicle"]<-'abandoned_vehicles'
train$tag_type[train$summary=="Graffiti Removal"]<-'graffiti'
train$tag_type[train$summary=="Pothole in Street"]<-'pavement'
train$tag_type[train$summary=="Street Light 1 / Out"]<-'street_light'
train$tag_type[train$summary=="Street Light - Outage/Damaged"]<-'street_light'
train$tag_type[train$summary=="Traffic Signal Out"]<-'street_signal'
train$tag_type[train$summary=="Traffic Signal - Outage/Damaged"]<-'street_signal'
train$tag_type[train$desription=="This issue was reported to 
               the City of Oakland Public Works Agency via phone (510-615-5566), 
               email (pwacallcenter@oaklandnet.com), or web (www.oaklandpw.com)."]<-'trash'
train$tag_type[train$summary=="Trash/bulk Pick-ups"] <-'trash'
train$tag_type[train$summary=="Illegal Dumping - debris, appliances, etc."] <-'trash'



### test set ###
test <- raw_testset

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



### can I just put NA into "other"?? ###

train$miss_type <- is.na(train$tag_type)
train$miss_source <- is.na(train$source)
train$tag_type<-as.character(train$tag_type)
train$tag_type[is.na(train$tag_type)]<- "other"
train$tag_type<-as.factor(train$tag_type)

test$miss_type <- is.na(test$tag_type)
test$miss_source <- is.na(test$source)
test$tag_type<-as.character(test$tag_type)
test$tag_type[is.na(test$tag_type)]<- "other"
test$tag_type<-as.factor(test$tag_type)

### check NA in tag_type ###
test_miss_type<-subset(test, is.na(test$tag_type))
train_miss_type<-subset(train, is.na(train$tag_type))


###combine into full data###
fulldata<-rbind(test,train)


