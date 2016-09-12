# Getting and Cleaning Data Course Project

This project fulfils the requirements for the course project for week 4 of the [Coursera/Johns Hopkins University "Getting and Cleaning Data" course](https://www.coursera.org/learn/data-cleaning/home/welcome).

Using the provided [data file](getdata%252Fprojectfiles%252FUCI%20HAR%20Dataset.zip) (the original file and more information about it can be found [here](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)) the run_analysis.R script does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set (these are taken from the activity_labels.txt file provided with the data file).
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The final data set has a header row, one variable per column, and all column names are lower case and use '_' as a separator where additional clarity is required.  The column names are explained in the included [code book](code_book.txt).

The dplyr function 'summarise_all' was used to apply the R mean function to grouped data to generate the averaged data in the final data set.

The final data set is called [average_har_data.txt](average_har_data.txt).  It can be read into R using the command
```
read.table("average_har_data.txt")
```
