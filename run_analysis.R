
library(dplyr)

# Preparatory-step: Downloading the data
filename <- "Coursera_DC.zip"

if (!file.exists("./filename")) {
  fileUrl <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile = filename, method = "curl")
}

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Extracting dataframes of interest

## reading features and activity data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

## Test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
## Train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# Step 1: Merges the training and the test sets to create one data set.
## Binding rows between train and test as columns are identical in each of the 3 pairs of dataframes
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
### joining all columns from the3 obtained dataframes
Merged_Data <- cbind(Subject, Y, X)


# Step2 : Extracts only the measurements on the mean and standard deviation for each measurement.
## Select columns: subject, code, and columns with "std" or "mean" in their string character 
Data.filtered <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))



# Step3 : Uses descriptive activity names to name the activities in the data set
## Changing in the df Data.filtered the number coding the activity 
## by its name stored in the "activities" data.frame
Data.filtered$code <- activities[Data.filtered$code, 2]


# Step4 : Appropriately labels the data set with descriptive variable names.
## Changing to full names to be more readable
names(Data.filtered)[2] = "activity"
names(Data.filtered)<-gsub("Acc", "Accelerometer", names(Data.filtered))
names(Data.filtered)<-gsub("Gyro", "Gyroscope", names(Data.filtered))
names(Data.filtered)<-gsub("BodyBody", "Body", names(Data.filtered))
names(Data.filtered)<-gsub("Mag", "Magnitude", names(Data.filtered))
names(Data.filtered)<-gsub("^t", "Time", names(Data.filtered))
names(Data.filtered)<-gsub("^f", "Frequency", names(Data.filtered))
names(Data.filtered)<-gsub("tBody", "TimeBody", names(Data.filtered))
# sometimes the string to change may have some uppercase so use of "ignore.case=TRUE"
names(Data.filtered)<-gsub("-mean()", "Mean", names(Data.filtered), ignore.case = TRUE)
names(Data.filtered)<-gsub("-std()", "Std", names(Data.filtered), ignore.case = TRUE)
names(Data.filtered)<-gsub("-freq()", "Frequency", names(Data.filtered), ignore.case = TRUE)
names(Data.filtered)<-gsub("angle", "Angle", names(Data.filtered))
names(Data.filtered)<-gsub("gravity", "Gravity", names(Data.filtered))


# Step5: From the data set in step 4, creates a second, independent tidy data set 
#  with the average of each variable for each activity and each subject.
TidyData <- Data.filtered %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean = mean))

#editing a tidy data set file
write.table(TidyData,file = "TidyData.txt", row.names = FALSE )
