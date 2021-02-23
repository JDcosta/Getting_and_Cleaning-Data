## R script run_analysis.R for getting and cleaning data project	   
## 	   
## Load R libraries	   
library(dplyr)	   
library(knitr)
library(reshape2)
library(stringr)

#PHASE 1: Preparing to obtain data for the analysis

#The dataset used for this project is UCI HAR Dataset
#Human Activity Recognition (HAR) using Smartphones database details of which are described at
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#The dataset is in a zip folder- "Dataset.zip" which is hosted at the following URL
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Housekeeping: The project dataset - UCI HAR Dataset,  will be in the directory
# .\\pData  (Windows relative path). The zipped folder Dataset.zip will unzip to 
# provide the project dataset in .\\pData\\UCI HAR Dataset )

#STEP01
#navigate to the working directory of your choosing for this project
working_directory <- getwd()

#the dataset will be stored in a sub-directory < "pData" > of the working_directory
# This is the project directory.Check if .\\pData exists. If not, create it.

if(!file.exists(".\\pData")){	   
  dir.create(".\\pData")	   
}	

#STEP02
#check  if pData directory folder already contains the unzipped folder
#UCI HAR Dataset. If it does then PHASE 1 is complete. Go to STEP03
if(!file.exists(".\\pData\\UCI HAR Dataset")){

#pData directory folder does not contain the unzipped folder
#UCI HAR Dataset. 
  
#Check if it contains the downloaded zip file Dataset.zip
#if it does not, download zip folder Dataset.zip
  if(!file.exists(".\\pData\\Dataset.zip")){
    
    download.file(URL,destfile=".\\pData\\Dataset.zip",mode = "wb")
  }
#At this point there is a Dataset.zip folder available in the project directory 
#unzip Dataset.zip to obtain the project dataset UCI HAR Dataset in pData
  
}
#From the details of the Human Activity Recognition (HAR) using Smartphones database, 
#a description of the files in the Dataset is available. From this description the
#following files were determined to be needed for the analysis. Only those files will
#be extracted from the zip folder. See README.txt or CODEBOOK.rmd for details.

fileset <-c("UCI HAR Dataset/activity_labels.txt","UCI HAR Dataset/features.txt", 
            "UCI HAR Dataset/test/subject_test.txt","UCI HAR Dataset/test/X_test.txt", 
            "UCI HAR Dataset/test/y_test.txt", "UCI HAR Dataset/train/subject_train.txt",
            "UCI HAR Dataset/train/X_train.txt","UCI HAR Dataset/train/y_train.txt")

#extract the data files into the project folder
unzip(zipfile="./pData/Dataset.zip",exdir="./pData", junkpaths=FALSE, files= fileset)

#STEP03
#Check out and list the contents of UCI HAR Dataset
datPath <- ".\\pData\\UCI HAR Dataset"
datFiles <- list.files(datPath, recursive= TRUE)
#print(datFiles)
#set up the R environment variables for appropriate representation of numeric data
#R write.table is internally coded to default to the representation of numeric data
#and the number of digits after the decimal based on the scipen environment variable
#archive the default scipen and digits 

#set the scipen and digits options that are effective for this dataset

options(scipen= 27, digits=8)   #default for scipen is 0

#STEP04 Read data from each of the files into respective data frames
#simultaneously change some of the original column names to descriptive activity names 

# read activity and feature labels
activitylabels <- read.table("./pData/UCI HAR Dataset/activity_labels.txt",col.names = c("activityidentifier", "activitytype"),colClasses = c("integer", "character"),stringsAsFactors=FALSE )
features <- read.table("./pData/UCI HAR Dataset/features.txt", as.is=TRUE, col.names =c ("featureidentifier", "featurename"), colClasses = c("integer", "character"),stringsAsFactors=FALSE)

#read data for the "Test" partition of the dataset
subjecttest <- read.table("./pData/UCI HAR Dataset/test/subject_test.txt", col.names = c("subjectidentifier"),colClasses = c("integer"),stringsAsFactors=FALSE)
xtest <- read.table("./pData/UCI HAR Dataset/test/X_test.txt",colClasses= c("numeric"),stringsAsFactors=FALSE)
ytest <- read.table("./pData/UCI HAR Dataset/test/y_test.txt", col.names = c("activityidentifier"), colClasses=c("integer"),stringsAsFactors=FALSE)

#set the preferred number of digits after the decimal point
#this is based on the further uses of the tidy data set. Not all applications
#need to process 11 digits after the decimal point (default for R)
xtest <- xtest %>% 
   dplyr::mutate(across(where(is.numeric), ~ round(., digits = 8)))


#read data for the "Train" partition of the dataset
subjecttrain <- read.table("./pData/UCI HAR Dataset/train/subject_train.txt", col.names = c("subjectidentifier"),colClasses = c("integer"),stringsAsFactors=FALSE)
xtrain <- read.table("./pData/UCI HAR Dataset/train/X_train.txt",colClasses= c("numeric"),stringsAsFactors=FALSE)
ytrain <- read.table("./pData/UCI HAR Dataset/train/y_train.txt", col.names = c("activityidentifier"), colClasses=c("integer"),stringsAsFactors=FALSE)
#set the preferred number of digits after the decimal point - same as the test partition
xtrain <- xtrain %>% 
   dplyr::mutate(across(where(is.numeric), ~ round(., digits = 8)))


#Step05: combine the subjecttest, ytest and Xtest dataframes into a "test" dataframe
test <- cbind(subjecttest, ytest, xtest)

#combine the subjecttrain, ytrain, and xtrain dataframes into a "train" dataframe
train <- cbind(subjecttrain, ytrain, xtrain)

#combine test and train datasets into a single compositedb data set.
#presumption that the "training" phase will precede the "testing" phase in the
#experiment

compositedb <- rbind (train, test) 

#Step06 rename column names of compositedb to more descriptive activity types
colnames(compositedb) <- c(colnames(compositedb[,1:2]), features$featurename)

#reduce the dataset by keeping only those columns that are needed for this project
#those are columns which contain "mean" and "Std" related data.
#this next step only provides the logical vector corresponding to columns to be retained

colindices <- sapply(colnames(compositedb), FUN = function(x) grepl("mean\\(\\)|std\\(\\)", x))

#make suitable modifications to account for the fact that two dataframes were 
#appended to the test and train data-frames from which the compositedb was obtained

#need to retain the subjectidentifier and activityidentifier columns
colindices[1:2] = TRUE 

#obtain the numerical position of the columns that will be retained by using the 
#logical vector computed earlier, Only retain the columns corresponding to those
#positions. Compositedb now has only the required columns.
compositedb <- compositedb[, which(colindices)]

#step07 clean the column names of the new smaller dataframe compositedb so that
#the variable names reflect descriptive names
colnames(compositedb) <- colnames(compositedb) %>%
  str_replace("^f", "Freq") %>%
  str_replace("^t", "Time") %>%
  str_replace("Acc", "Accel") %>%
  str_replace("Mag", "Magnitude") %>%
  str_replace("Gyro", "Gyroscope") %>%
  str_replace("BodyBody", "Body") %>%
  str_replace("[(][)]-?", "") %>%
  str_replace("X$", "_X") %>%
  str_replace("Y$", "_Y") %>%
  str_replace("Z$", "_Z") %>%
  str_replace("mean", "Mean") %>%
  str_replace("std", "STD") %>%
  str_replace_all("-", ".") 

#replace activityidentifier with activitytype in column 2 for tidy Data reporting purposes
#The factor activityidentifier used thus far carries no particular descriptive connotation
#except that it is linked to the activitytype through the activitylabel dataframe.
mutate(compositedb, as.character(activityidentifier))
compositedb[, "activityidentifier"]<- activitylabels$activitytype[compositedb[, "activityidentifier"]]
#change the column name of column 2 to 'activitytype' from 'activityidentifier'
names(compositedb)[2] <- c("activitytype")

#Step08 create factors for the Subjectidentifier and Activity variables
compositedb[, "subjectidentifier"] <- factor(compositedb[, "subjectidentifier"])
compositedb[, "activitytype"] <- factor(compositedb[, "activitytype"])


#Step09 Create an independent second tidy data set 'c' with the average of each
#variable for each activity and each subject

#x <- compositedb %>%
#            group_by(activitytype,subjectidentifier) %>%
#            dplyr::summarize(across(.cols=everything(), .fns=mean))

#output grouped by subjectidentifier in tidy_Data.txt
#output grouped by activitytype by switching the order of variables in 
#group_by

 tidyData <- compositedb %>%
  group_by(subjectidentifier, activitytype) %>%
  summarize_each(funs(mean))

#format the tidy data set  data frame to get the desired output 
format(tidyData, justify= "left", trim=FALSE, digit=8, nsmall=4)

#output the tidy dataset as file tidy_Data.txt
write.table(tidyData,file="tidy_Data.txt", quote= FALSE, row.names=FALSE, col.names=TRUE, append=FALSE, sep=" ")
#restore scipen and digits to their default values
options(scipen =0, digits=7)   #default for R

print("run_analysis.R ran successfully. Output written to tidy_Data.txt in the ")
print(working_directory)
