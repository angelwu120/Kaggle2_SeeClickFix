

###clean NAs in tag_type, alternative method 

## i.e.

train$tag_type[agrep("tree", train$summary, ignore.case=TRUE)] <-ifelse(is.na(train$tag_type), 'tree', train$tag_type)
train$tag_type[agrep("pothole", train$summary, ignore.case=TRUE)] <-ifelse(is.na(train$tag_type), 'pothole', train$tag_type)
