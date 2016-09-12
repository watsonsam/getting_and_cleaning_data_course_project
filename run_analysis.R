## run_analysis.R does the following

## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

## makeNiceName is used later to create more human-readable names for the variables. The '.'
## character is removed. Names are converted to lower case.  The '_' is used as a separator 
## between the type of variable (mean or std) and the other parts of the name. 'mag' is converted
## to 'magnitude' for further clarity.

makeNiceName <- function(colname) {
        parts <- strsplit(colname, "\\.")
        parts <- parts[[1]][parts[[1]] != ""]
        tmp <- if(length(parts) == 3) {
                paste(parts[2], parts[1], parts[3], "axis", sep = "_")
        } else if(length(parts) == 2) {
                paste(parts[2], parts[1], sep = "_")
        } else colname
        
        sub("mag", "magnitude", tolower(tmp))
}

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
featureNames <- make.names(read.table("UCI HAR Dataset/features.txt", stringsAsFactors = F)$V2, unique = T)

## Read in the subject information
xtrainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
xtestSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

## Add the subject data as a column to the main data tables
xtrain$subject <- xtrainSubjects$V1
xtest$subject <- xtestSubjects$V1

## Step 1: Merge the training and test datasets
harData <- rbind(xtrain, xtest)

## Apply the descriptive column names from the source data to the table, excluding the
## final column which contains our added subject variable.  (This partly fulfils the 
## requirement for step 4, labeling with descriptive variable names, but we do this step
## now while the data is in the original shape.)

names(harData)[1:ncol(harData) -1] <- featureNames

## Step 2: Extract only the measurements on the mean and standard deviation. We only include
## the main mean and std columns, ignoring the additional vectors relating to the the angle 
## variable (i.e. angle(...)) and the meanFreq variable.

harData <- select(harData, subject, matches("\\.std\\."), matches("\\.mean\\."))

## Step 3: Apply descriptive activity names

## Read in the activity names so we can provide descriptive names for the activities
activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")

## Read in the activities
xtrainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
xtestActivities <- read.table("UCI HAR Dataset/test/y_test.txt")

## Join the two sets of activities
Activities <- rbind(xtrainActivities, xtestActivities)

## Add an activity column to our main table to identify the activity, joining on the activity names
## to provide descriptive activity names
harData$activity <- left_join(Activities, activityNames)$V2
## Keep the subject and activity variables at the beginning
harData <- harData %>% select(subject, activity, everything())


## Step 4: We labeled the data set with variable names in step 1.  Now we tidy them up, removing
## the '.' character, making clear that the X, Y and Z refer to the X, Y, and Z axes.
names(harData) <- sapply(names(harData), makeNiceName)

## Step 5: From the data set in step 4, create a second, independent tidy data set with the average 
## of each variable for each activity and each subject.

## Group the data by activity and subject

hd2 <- group_by(harData, activity, subject)

## Use the summarise_all function from dplyr to create a tbl with the average 
## of each variable for each activity and each subject.

hd3 <- summarise_all(hd2, mean)

## rename the variables of the new tbl to emphasise and clarify that they are mean values

names(hd3)[3:length(names(hd3))] <- sapply(names(hd3)[3:length(names(hd3))], paste0, "_average")

write.table(hd3, file = "average_har_data.txt", row.names = F)