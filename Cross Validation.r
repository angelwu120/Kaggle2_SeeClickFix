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