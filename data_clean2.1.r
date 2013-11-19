####################################
### how to clean NA in tag_types ###
####################################

### check NA in tag_type ###
test_miss_type<-subset(test, is.na(test$tag_type))
train_miss_type<-subset(train, is.na(train$tag_type))

train$tag_type<-as.character(train$tag_type)
test$tag_type<-as.character(test$tag_type)

### train set 

train$tag_type[train$summary=="Abandoned Vehicle"]<-'abandoned_vehicles'
train$tag_type[train$desription=="This issue was reported to 
               the City of Oakland Public Works Agency via phone (510-615-5566), 
               email (pwacallcenter@oaklandnet.com), or web (www.oaklandpw.com)."]<-'trash'
train$tag_type[train$summary=="Trash/bulk Pick-ups"] <-'trash'
train$tag_type[train$summary=="Illegal Dumping - debris, appliances, etc."] <-'trash'

train$tag_type[agrep("Rodent", train$summary, ignore.case=TRUE)] <-'rodents'
train$tag_type[agrep("tree", train$summary, ignore.case=TRUE)] <-'tree'
train$tag_type[agrep("limbs", train$summary, ignore.case=TRUE)] <-'tree'
train$tag_type[agrep("grass", train$summary, ignore.case=TRUE)] <-'tree'
train$tag_type[agrep("pothole", train$summary, ignore.case=TRUE)] <-'pothole'
train$tag_type[agrep("Graffiti", train$summary, ignore.case=TRUE)] <-'graffiti'
train$tag_type[agrep("brush", train$summary, ignore.case=TRUE)] <-'brush_bulk'
train$tag_type[agrep("bulk", train$summary, ignore.case=TRUE)] <-'brush_bulk'
train$tag_type[agrep("bulk", train$description, ignore.case=TRUE)] <-'brush_bulk'
train$tag_type[agrep("light", train$summary, ignore.case=TRUE)] <-'street_light'
train$tag_type[agrep("signal", train$summary, ignore.case=TRUE)] <-'street_signal'
train$tag_type[agrep("prostitutes", train$summary, ignore.case=TRUE)] <-'prostitutes'

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

train$tag_type[is.na(train$tag_type)]<- "other"
train.other<-subset(train,tag_type=="other")
                    
### test set 

test$tag_type[test$tag_type=='abandoned_vehicle']<-'abandoned_vehicles'
test$tag_type[test$desription=="This issue was reported to 
              the City of Oakland Public Works Agency via phone (510-615-5566), 
              email (pwacallcenter@oaklandnet.com), or web (www.oaklandpw.com)."]<-'trash'
test$tag_type[agrep("Trash/bulk", test$summary, ignore.case=TRUE)] <-'trash'
test$tag_type[agrep("Dumping", test$summary, ignore.case=TRUE)] <-'trash'
test$tag_type[agrep("Rodent", test$summary, ignore.case=TRUE)] <-'rodents'
test$tag_type[agrep("tree", test$summary, ignore.case=TRUE)] <-'tree'
test$tag_type[agrep("limbs", test$summary, ignore.case=TRUE)] <-'tree'
test$tag_type[agrep("grass", test$summary, ignore.case=TRUE)] <-'tree'
test$tag_type[agrep("pothole", test$summary, ignore.case=TRUE)] <-'pothole'
test$tag_type[agrep("Graffiti", test$summary, ignore.case=TRUE)] <-'graffiti'
test$tag_type[agrep("brush", test$summary, ignore.case=TRUE)] <-'brush_bulk'
test$tag_type[agrep("bulk", test$summary, ignore.case=TRUE)] <-'brush_bulk'
test$tag_type[agrep("bulk", test$description, ignore.case=TRUE)] <-'brush_bulk'
test$tag_type[agrep("light", test$summary, ignore.case=TRUE)] <-'street_light'
test$tag_type[agrep("signal", test$summary, ignore.case=TRUE)] <-'street_signal'
test$tag_type[agrep("prostitutes", test$summary, ignore.case=TRUE)] <-'prostitutes'

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

test$tag_type[is.na(test$tag_type)]<- "other"
test.other<-subset(test,tag_type=="other")


### combine into full data ###
train$tag_type<-as.factor(train$tag_type)
test$tag_type<-as.factor(test$tag_type)
fulldata<-rbind(test,train)
