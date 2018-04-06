library(dplyr)
#Load the training data and test data
xts <- read.table("X_test.txt")
yts <-  read.table("Y_test.txt")
subjts <- read.table("subject_test.txt")
xtr <- read.table("X_train.txt")
ytr <- read.table("y_train.txt")
subjtr <- read.table("subject_train.txt")
#Load the activity labels and features
featnames <- read.table("features.txt")
actlab <- read.table("activity_labels.txt")
#Merge the data
merge1 <- rbind(xtr,xts)
merge2 <- rbind(ytr,yts)
mergesub <- rbind(subjtr,subjts)
#Extracts only the measurements on the mean and standard deviation for each measurement
extvar <- featnames[grep("mean\\(\\)|std\\(\\)",featnames[,2]),]
merge1 <- merge1[,extvar[,1]]
#Uses descriptive activity names to name the activities in the data set
colnames(merge2) <- "activity"
merge2$activitylabel <- factor(merge2$activity, labels = as.character(actlab[,2]))
activitylabel <- merge2[,-1]
#Appropriately label the data set with descriptive variable names
colnames(merge1) <- featnames[extvar[,1],2]
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
colnames(mergesub) <- "subject"
total <- cbind(merge1, activitylabel, mergesub)
finmean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(finmean, file = "tidydata.txt", row.names = FALSE, col.names = TRUE)