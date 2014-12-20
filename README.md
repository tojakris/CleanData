Getting and Cleaning Data Course Project

========================================

This repository contains the code for my solution to the assignment of the course "Getting and Cleaning Data" . 

## Description of the assignment

One of the most  exciting areas in all of data science right now is wearable computing - see for example this article ( http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/ ). Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked  to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where  the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##  Solution

To run the analysis, perform the following steps:
* Unzip the file getdata-projectfiles-UCI HAR Dataset.zip in your working directory.
* Place the scripts *run_analysis.R*, *analysis_functions.R* and *constants.R* in your working directory.
* If necessary, modify the file *constants.R* indicating the name of the directory containing the data, if it has been modified, and / or the delimiting character for your filesystem.
* Load the source *run_analysis.R* at your session in R, with **`source("run_analysis.R")`**
* Finally, execute **`run.analysis()`** in R.
* Be sure to have installed the packages data.table, plyr and reshape2.

run.analysis() uses the constants defined in constants.R and the functions from analysis_functions.R, and generates a file called new_dataset.txt in your working directory.


