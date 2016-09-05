## run_analysis.R does the following

## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

## Step 0: Extract the data from files into tbls so we can work with it

## Unzip the data file

unzip("getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")

## Read the training and test sets into tables

xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")

## Convert the two tables into data frame tbls for easier processing using dplyr
xtrain <- tbl_df(xtrain)
xtest <- tbl_df(xtest)

## Read in the column headings
featureNames <- readLines("UCI HAR Dataset/features.txt")

## Apply the column headings
colnames(xtrain) <- featureNames
colnames(xtest) <- featureNames

## Read in the activities
xtrainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
xtestActivities <- read.table("UCI HAR Dataset/test/y_test.txt")

## Read in the activity names so we can provide desriptive names for the activities
activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")

## Add an Activity column to each of the tbls to identify the activity, joining on the activity names
## to provide descriptive activity names
xtrain$Activity <- left_join(xtrainActivities, activityNames)$V2
xtest$Activity <- left_join(xtestActivities, activityNames)$V2

## Step 1: Merge the training and test datasets
harData <- rbind(xtrain, xtest)

## Save memory by removing everything we don't need
rm(list = ls()[ls() != "harData"])

## Step 2: Extract only the measurements on the mean and standard deviation

harData <- select(harData, Activity, matches("std()"), matches("mean()"))

## Step 3: We previously provided descriptive activity names
