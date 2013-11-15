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




