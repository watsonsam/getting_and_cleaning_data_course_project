This code book describes the content of the data set average_har_data.txt produced by the R script run_analysis.R.

The data set contains average values by subject and activity for the the std() (standard deviation) and mean() variables in the UCI HAR data set.  Each row contains averaged data for one subject and activity.

* The subject and activity columns identify the subject and activity for the set of observations in that row.

* Each variable name starts with either 'mean_' or 'std_' indicating whether the data in that column is the average of the mean() or std() variable respectively.

* Where applicable, a variable name will indicate whether the reading was from the x, y, or z axis of the phone: it will contain the string 'x_axis', 'y_axis' or 'z_axis' respectively.

* Readings were taken from the accelerometer and gyroscope of the phone.  Variable names reflect this by containing the string 'acc' or 'gyro' respectively.

For further explanation of the variable names please see the features_info.txt file included with the HAR data set.