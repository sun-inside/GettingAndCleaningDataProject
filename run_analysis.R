##You should create one R script called run_analysis.R that does the following. 
##1 Merges the training and the test sets to create one data set.
##2 Extracts only the measurements on the mean and standard deviation for each measurement. 
##3 Uses descriptive activity names to name the activities in the data set
##4 Appropriately labels the data set with descriptive variable names. 
##5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable
##for each activity and each subject.

# Load pacakages
library(dplyr)
library(tidyr)

##1 Merges the training and the test sets to create one data set.
# Download data from the internet
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI_Dataset.zip"
if(!file.exists("UCI_dataset.zip")){
  download.file(fileUrl,destfile="UCI_dataset.zip",method = "curl")
  unzip(zipfile = "UCI_dataset.zip", files = NULL,unzip = "internal") 
}

# 1 Merge training and test data to create one set
# Read names and ids of features and activity
features <- read.table("UCI HAR Dataset//features.txt",col.names=c("id","vars"), stringsAsFactors = FALSE)
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("id","Activity"))

# Read train data and add indexes
train <- read.table("UCI HAR Dataset//train/X_train.txt", col.names=features$vars)
train <- mutate(train, id = 1:nrow(train))

# Read subject train data and add indexes
subj_train <- read.table("UCI HAR Dataset//train/subject_train.txt",col.names="Subject")
subj_train <- mutate(subj_train, id = 1:nrow(subj_train))

# Read activity train and add indexes
activity_train <- read.table("UCI HAR Dataset//train/y_train.txt", col.name="Activity")
activity_train <- mutate(activity_train, id = 1:nrow(activity_train))

# Merge experimental data with subjects and activities by id
train_subj <- merge(train, subj_train, by = "id", sort=FALSE)
train_result <- merge(train_subj, activity_train, by = "id", sort=FALSE)

# Read test data and add indexes
test <- read.table("UCI HAR Dataset//test/X_test.txt", col.names=features$vars)
test <- mutate(test, id = 1:nrow(test))

# Read subject test data and add indexes
subj_test <- read.table("UCI HAR Dataset//test/subject_test.txt",col.names="Subject")
subj_test <- mutate(subj_test, id = 1:nrow(subj_test))

# Read activity test and add indexes
activity_test <- read.table("UCI HAR Dataset//test/y_test.txt", col.name="Activity")
activity_test <- mutate(activity_test, id = 1:nrow(activity_test))

# Merge experimental data from test with subjects and activities by id
test_subj <- merge(test, subj_test, by = "id", sort=FALSE)
test_result <- merge(test_subj, activity_test, by = "id", sort=FALSE)

# Combine train and test data
DataSet <- rbind(train_result,test_result)

##2 Extracts only the measurements on the mean and standard deviation for each measurement. 
#filtering columns with other names
DataSetSelection <- select(DataSet, Subject, Activity, matches("mean"), matches("std"),-matches("angle"),-matches("meanFreq"))

##3 Uses descriptive activity names to name the activities in the data set
DataSetSelection$Activity <- as.factor(DataSetSelection$Activity)
levels(DataSetSelection$Activity) <- levels(sort(activity$Activity))

# 4. Appropriately label the data set with descriptive variable names:
colnames(DataSetSelection) <- sub("mean", " Mean ", colnames(DataSetSelection))
colnames(DataSetSelection) <- sub("std", " Standard Deviation ", colnames(DataSetSelection))

##5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable
##for each activity and each subject.
TidydataSet <- summarise_each(group_by(DataSetSelection,Subject,Activity), funs(mean))

# Write an ouptut text file using row.name=FALSE in wide tidy form
write.table(TidydataSet, file = "tidy_data.txt", row.name=FALSE)
