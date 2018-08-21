##########################################################################
#
#   	File: run_analysis.R
#
##########################################################################

library(dplyr)
library(Hmisc)

##########################################################################
#	Step 1: Get data
##########################################################################

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## File Documentation: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
download.file(fileUrl, destfile = "smartphones.zip", method = "curl")
dateDownloaded <- date()

##########################################################################
#	Step 2: Extract data
##########################################################################

unzip("smartphones.zip")
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainsub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainact <- read.table("./UCI HAR Dataset/train/y_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
testsub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testact <- read.table("./UCI HAR Dataset/test/y_test.txt")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")


##########################################################################
#	Step 3: Combining data
##########################################################################

alltrain <- cbind(trainsub, trainact, train)
names(alltrain) <- c("Subject", "ActivityID", unlist(strsplit(toString(features[,2]), split=", ")))
alltest <- cbind(testsub, testact, test)
names(alltest) <- c("Subject", "ActivityID", unlist(strsplit(toString(features[,2]), split=", ")))
alldata <- rbind(alltest, alltrain) # Completes Requirement 1

##########################################################################
#	Step 4: Selecting variables
##########################################################################

# According to the documentation in features_info.txt, the variables
# needed to complete requirement 2 will have "mean()" or "std()" as part 
# of the variable name.

allmeans <- alldata[,grepl("Subject|ActivityID|mean\\(\\)|std\\(\\)", names(alldata))] # Completes Requirement 2

##########################################################################
#	Step 5: Using descriptive activity names
##########################################################################

allmeans[,1] <- as.factor(allmeans[,1])
allmeans[,2] <- factor(as.factor(allmeans[,2]), levels = activity[, 1], labels = activity[, 2]) # Completes Requirement 3

##########################################################################
#	Step 6: Using descriptive variable names
##########################################################################

allnames <- names(allmeans)
allnames <- gsub("\\(\\)", "", allnames)
allnames <- gsub("^t", "TimeDomain", allnames)
allnames <- gsub("^f", "FrequencyDomain", allnames)
allnames <- gsub("mean", "Mean", allnames)
allnames <- gsub("std", "StandardDeviation", allnames)
allnames <- gsub("Acc", "Acceleration", allnames)
allnames <- gsub("Gyro", "Gyroscope", allnames)
allnames <- gsub("Jerk", "JerkSignal", allnames)
allnames <- gsub("Mag", "Magnitude", allnames)
allnames <- gsub("BodyBody", "Body", allnames)
allnames <- gsub("X$", "xAxis", allnames)
allnames <- gsub("Y$", "yAxis", allnames)
allnames <- gsub("Z$", "zAxis", allnames)
allnames[2] <- "ActivityName"

names(allmeans) <- allnames # Completes Requirement 4

##########################################################################
#	Step 7: Creating tidy data set
##########################################################################

alltidymeans <- group_by(allmeans, Subject, ActivityName)
alltidymeans <- summarise_all(alltidymeans, mean) # Completes Requirement 5

##########################################################################
#	Step 8: Exporting tidy data set
##########################################################################

write.table(alltidymeans, file = "TidyData.txt", row.name=FALSE)
