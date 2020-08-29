##loading necessary packages 
library(dplyr)
library(reshape2)

##reading features and activity data
features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")


##reading test data 
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt",sep="")
test_x <- read.table("UCI HAR Dataset/test/X_test.txt",sep="")
test_y <- read.table("UCI HAR Dataset/test/Y_test.txt",sep="")
colnames(test_x) <- features$V2
test_x$activity <- test_y$V1
test_x$subject <- factor(test_subject$V1)

##reading train data
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt",sep="")
train_x <- read.table("UCI HAR Dataset/train/X_train.txt",sep="")
train_y <- read.table("UCI HAR Dataset/train/Y_train.txt",sep="")
colnames(train_x) <- features$V2
train_x$activity <- train_y$V1
train_x$subject <- factor(train_subject$V1)





##merging test and train datasets 
data <- rbind(test_x, train_x) 


##filtering column names 
colnames <- colnames(data)


##getting only columns for standard deviation and mean values, also saves activity and subject values 
colnamesfilt <- grep("std\\(\\)|mean\\(\\)|activity|subject", colnames, value=TRUE)
datasetfilt <- data[, colnamesfilt]

#adding descriptive values for activity labels 
datasetfilt$activitylabel <- factor(datasetfilt$activity, labels= c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))


#creating a tidy dataset with mean values for each subject and activity
features.colnames = grep("std\\(\\)|mean\\(\\)", colnames, value=TRUE)
data.melt <- melt(datasetfilt, id = c("activitylabel", "subject"), measure.vars=features.colnames)
tidy.table <- dcast(data.melt, subject + activitylabel ~ variable)
tidy.table.mean <- dcast(data.melt, subject + activitylabel ~ variable, mean)

##writing the data into a text file
write.table(tidy.table.mean, file = "tidyData.txt", row.names = FALSE)

