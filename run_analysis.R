
# #Week-4-Getting-And-Cleaning-Data-Setup
# 
# # Unzip dataSet to /data directory
# unzip(zipfile="C:/Week 4 Project/Getting-and-Cleaning-Data-Week-4-Project/getdata_projectfiles_UCI HAR Dataset.zip",exdir="C:/Week 4 Project/Getting-and-Cleaning-Data-Week-4-Project")
# 
# # Checking new unzipped files in the new folder that has been unzipped
# list.files("C:/Week 4 Project/Getting-and-Cleaning-Data-Week-4-Project/")
# 
# # Defining a path to where the new folder has been unziped
# pathdata = file.path("C:/Week 4 Project/Getting-and-Cleaning-Data-Week-4-Project/", "UCI HAR Dataset")
# 
# # Creating a file which has the 28 file names
# files = list.files(pathdata, recursive=TRUE)
# 
# # Displaying file listing to console
# files


#install.packages("dplyr")
library(dplyr)

### 1. Output Steps - Here we begin how to create the data set of training and test
#Reading training tables - xtrain / ytrain, subject train

xtrain = read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"),header = FALSE)

#Reading the testing tables
xtest = read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(pathdata, "test", "subject_test.txt"),header = FALSE)

#Read the features data
features = read.table(file.path(pathdata, "features.txt"),header = FALSE)

#Read activity labels data
activityLabels = read.table(file.path(pathdata, "activity_labels.txt"),header = FALSE)

#Create column Values to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"

#Create column values to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"

#Create activity label column values
colnames(activityLabels) <- c('activityId','activityType')

###############################################################################
## Step 1 - Merge the Training and Test data to make one clean tidy data set ##
###############################################################################
#Merging the train and test data
mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)

#Create the main data table by merging both table tables
mainTDSet = rbind(mrg_train, mrg_test)

#####################################################################################################
## Step 2 - Extract only the measurements on the mean and standard deviation for each measurement. ##
#####################################################################################################

# Read all the values that are available
colNames = colnames(mainTDSet)
# Gathering a subset of all the mean and standard deviation for the corresponding activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
#A subtset has to be created to get the required dataset
setForMeanAndStd <- mainTDSet[ , mean_and_std == TRUE]

#####################################################################################
## Step 3 - Uses descriptive activity names to name the activities in the data set ##
#####################################################################################

## Majority of the activity column labelling was done above (prior to STEP 1) as it naturally seemed in better interest to identity and label this data before any manipulation.

#############################################################
## Step 4 - Label Data Set with descriptive variable names ##
#############################################################
setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)

#############################################################################################################################
## Step 5 - Create second, independent tidy data set with the average of each variable for each activity and each subject. ##
#############################################################################################################################
# New tidy set has to be created 
TDSet2 <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
TDSet2 <- TDSet2[order(TDSet2$subjectId, TDSet2$activityId),]

#The last step is to write the ouput to a text file 
write.table(TDSet2, "TidyData.txt", row.name=FALSE)
