CODEBOOK
================

### Project Description

The Human Activity Recognition (HAR) using Smartphones project is
premised on recognizing the movements of a human (i.e. sitting, walking
etc.) through the processing of information retreived from body-worn
sensors. The sensors generate voluminous movement data (i.e raw data)
which is pre-processed and made available to the public as a
dataset(s).The ultimate goal is to use the data to develop computer
algorithms capable of human movement recogniton from real time data
streaming from body-worn sensors. In the present exercise, the goal is
to use the publicly available data- which is raw data or data which has
been pre-processed to some degree and generate tidy data sets suitable
for use as input to human movement recognition computer algorithms.

### Study design and data processing

The dataset for this project,
“Human+Activity+Recognition+Using+Smartphones” (HAR Dataset) is
described at the following link provided on the class webpage:
<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.
It was prepared and made available in 2012 by Davide Anguita, et
al. from the University of Genova, Italy and is described in full in
their 2013 paper “A Public Domain Dataset for Human Activity Recognition
Using Smartphones.” The experiments have been carried out with a group
of 30 volunteers within an age bracket of 19-48 years. Each person
performed six activities (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS,
SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on
the waist. The smartphone had embedded accelerometer and gyroscope
sensors which provided 3-axial linear acceleration and 3-axial angular
velocity readings at a constant rate of 50Hz. The experiments were
video-recorded to label the data manually. The resulting raw dataset was
randomly partitioned into two sets, where data from 70% of the
volunteers was considered to be the training data and the remainder 30%
the test data.

#### Collection of the raw data

The HAR Dataset was downloaded by the run\_analysis.R script (described
in detail in the accompanying README.txt file) from the following link
on the course web page:
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>.
The program downloads the HAR Dataset to the folder ‘UCI HAR Dataset’ in
the project directory structured as a subdirectory of the working
directory. The README.txt file documents the directory structure in
detail.

The UCI HAR Dataset contains the following files:

  - `X_train.txt` : Train features, each line is composed 561-feature
    vector with time and frequency domain variables.

  - `X_test.txt` : Test features, each line is composed 561-feature
    vector with time and frequency domain variables.

  - `y_train.txt`: train activity labels, its range is from 1 to 6

  - `y_test.txt`: test activity labels, its range is from 1 to 6

  - `subject_id_train.txt`: training subject identifiers, Its range is
    from 1 to 30

  - `subject_id_test.txt`: testing subject identifiers, Its range is
    from 1 to 30

  - `features_info.txt`: Shows information about the variables used on
    the feature vector.

  - `features.txt`: includes list of all 561 features

  - ‘activity\_labels.txt’: Links the class (i.e. Test/Train) labels
    with their activity type. activityidentifier activitytype 1 WALKING
    2 WALKING\_UPSTAIRS 3 WALKING\_DOWNSTAIRS 4 SITTING 5 STANDING 6
    LAYING

  - ‘README.md’

The features selected for this (raw data) database come from the
accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
These time domain signals (prefix ‘t’ to denote time) were captured at a
constant rate of 50 Hz. Then they were filtered using a median filter
and a 3rd order low pass Butterworth filter with a corner frequency of
20 Hz to remove noise. Similarly, the acceleration signal was then
separated into body and gravity acceleration signals (tBodyAcc-XYZ and
tGravityAcc-XYZ) using another low pass Butterworth filter with a corner
frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were
derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and
tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm (tBodyAccMag,
tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these
signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ,
fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to
indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for
each pattern:  
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ tGravityAcc-XYZ tBodyAccJerk-XYZ tBodyGyro-XYZ
tBodyGyroJerk-XYZ tBodyAccMag tGravityAccMag tBodyAccJerkMag
tBodyGyroMag tBodyGyroJerkMag fBodyAcc-XYZ fBodyAccJerk-XYZ
fBodyGyro-XYZ fBodyAccMag fBodyAccJerkMag fBodyGyroMag fBodyGyroJerkMag

The set of variables that were estimated from these signals and used in
this project are those related to mean and stdard deviation (std) :

mean(): Mean value std(): Standard deviation

Additional vectors obtained by averaging the signals in a signal window
sample. These are used on the angle() variable:

gravityMean tBodyAccMean tBodyAccJerkMean tBodyGyroMean
tBodyGyroJerkMean

#### Notes on the original (raw) data

The files in the train Inertial Signals and test Inertial Signals will
not be used in the analysis and are not discussed in this CODEBOOK -
Features are normalized and bounded within \[-1,1\]. - Each feature
vector is a row on the ‘X’ and ‘y’ files.
(i.e. X\_train.txt`,`X\_test.txt`,`y\_train.txt`,`y\_test.txt\`) - The
units used for the accelerations (total and body) are ’g’s (gravity of
earth -\> 9.80665 m/sec2). (sec is second in time units) - The gyroscope
units are rad/sec (sec is abbreviation for second in time units)

### Creating the tidy datafile

The process of creating the tidy datafile has been described in detail
in the README.txt filein this GitHub repo. For the sake of completeness
of the CODEBOOK.Rmd, the steps in creating the tidy Data file are
summarized below.

*1)* navigate RStudio to the working directory of your choosing for this
project \#through RStudio’s Session menu- Set Working Directory - Choose
Directory

*2)* run \>getwd() in the command line of RStudio and note the name of
the working directory.

*3)* install the script ‘run\_analysis.R’ in the working directory

*4)* run the script ‘run\_analysis.R’ in RStudio by issuing the command
\>source(“run\_analysis.R”)

*5)* the script will check for a project directory and if it is not
present it will create one (for Windows it is ./pData) the project
directory is a folder in the working directory

*6)* The script will download the raw HAR Dataset as a zip file and
unzip it in the project directory ./pData  
into a subfolder of .pData i.e. the UCI HAR Dataset (‘.pData/UCI HAR
Dataset’) The UCI HAR Dataset has the directory structure and contents
described in the preceding paragraphs \#\#\#Study design and data
processing

*7)* The script will read the activity and feature labels from
“activity\_labels.txt” and “features.txt” respectively.

*8)* Read data for the “Test” partition of the dataset (i.e. create
dataframes for each “subject\_test.txt”,“y\_test.txt”,“X\_test.txt” and
column bind the three dataframes to create the “test” dataframe

*9)* Repeat step 8 for the “Train” partition of the dataset and create
the “train” dataframe

*10)* Merge the train and test dataframes through a rowbind operation
into a composite database (referred to as “compositedb” in
“run\_analysis.R”)

*11)* Except for the first two columns, The column names of the combined
dataframe (compositedb in this code)are the same generic names (V1, V2
and so forth) inherited from the the original dataframes created from
the files in the raw Dataset in steps *8* and *9*.

*12)* Rename column names of compositedb to more descriptive column
names. Use the file names suggested by features.txt for example.

*13)* Trim this compositedb (which now features descriptive column
names) so that only those columns that are needed for this project
remain. For this project, these columns are the ones whose column names
contain “mean” and “Std” strings. It is presumed that these column names
are indicative of the information the columns contain.

*14)* The trimmed, smaller dataframe, i.e. compositedb with a
reduced-set of columns is cleaned so the column names are transformed
into more descriptive column names. The material displayed within the
*SELECTION A* box below are the column names of the reduced-set

(or column-trimmed) original dataframe columndb from *paragraph 11*
above.

*15)* The material displayed within the SELECTION B box below are the
column names in paragraph 14 cleaned or transformed so that these
columns now have more descriptive names.

#### SELECTION A (BEGIN) Column names of the reduced-column original data frame compositedb

> names(compositedb) \[1\] “subjectidentifier” “activityidentifier”  
> \[3\] “tBodyAcc-mean()-X” “tBodyAcc-mean()-Y”  
> \[5\] “tBodyAcc-mean()-Z” “tBodyAcc-std()-X”  
> \[7\] “tBodyAcc-std()-Y” “tBodyAcc-std()-Z”  
> \[9\] “tGravityAcc-mean()-X” “tGravityAcc-mean()-Y”  
> \[11\] “tGravityAcc-mean()-Z” “tGravityAcc-std()-X”  
> \[13\] “tGravityAcc-std()-Y” “tGravityAcc-std()-Z”  
> \[15\] “tBodyAccJerk-mean()-X” “tBodyAccJerk-mean()-Y”  
> \[17\] “tBodyAccJerk-mean()-Z” “tBodyAccJerk-std()-X”  
> \[19\] “tBodyAccJerk-std()-Y” “tBodyAccJerk-std()-Z”  
> \[21\] “tBodyGyro-mean()-X” “tBodyGyro-mean()-Y”  
> \[23\] “tBodyGyro-mean()-Z” “tBodyGyro-std()-X”  
> \[25\] “tBodyGyro-std()-Y” “tBodyGyro-std()-Z”  
> \[27\] “tBodyGyroJerk-mean()-X” “tBodyGyroJerk-mean()-Y”  
> \[29\] “tBodyGyroJerk-mean()-Z” “tBodyGyroJerk-std()-X”  
> \[31\] “tBodyGyroJerk-std()-Y” “tBodyGyroJerk-std()-Z”  
> \[33\] “tBodyAccMag-mean()” “tBodyAccMag-std()”  
> \[35\] “tGravityAccMag-mean()” “tGravityAccMag-std()”  
> \[37\] “tBodyAccJerkMag-mean()” “tBodyAccJerkMag-std()”  
> \[39\] “tBodyGyroMag-mean()” “tBodyGyroMag-std()”  
> \[41\] “tBodyGyroJerkMag-mean()” “tBodyGyroJerkMag-std()”  
> \[43\] “fBodyAcc-mean()-X” “fBodyAcc-mean()-Y”  
> \[45\] “fBodyAcc-mean()-Z” “fBodyAcc-std()-X”  
> \[47\] “fBodyAcc-std()-Y” “fBodyAcc-std()-Z”  
> \[49\] “fBodyAccJerk-mean()-X” “fBodyAccJerk-mean()-Y”  
> \[51\] “fBodyAccJerk-mean()-Z” “fBodyAccJerk-std()-X”  
> \[53\] “fBodyAccJerk-std()-Y” “fBodyAccJerk-std()-Z”  
> \[55\] “fBodyGyro-mean()-X” “fBodyGyro-mean()-Y”  
> \[57\] “fBodyGyro-mean()-Z” “fBodyGyro-std()-X”  
> \[59\] “fBodyGyro-std()-Y” “fBodyGyro-std()-Z”  
> \[61\] “fBodyAccMag-mean()” “fBodyAccMag-std()”  
> \[63\] “fBodyBodyAccJerkMag-mean()” “fBodyBodyAccJerkMag-std()”  
> \[65\] “fBodyBodyGyroMag-mean()” “fBodyBodyGyroMag-std()”  
> \[67\] “fBodyBodyGyroJerkMag-mean()” “fBodyBodyGyroJerkMag-std()”

#### SELECTION A (END)

#### SELECTION B (BEGIN) Column names in selection A above transformed into more descriptive form

These are the manes of the variables in the final tiny\_data.txt file

The tidy Data dataframe has the following characteristics: The variable
names are as follows: \[1\] “subjectidentifier” “activitytype”  
\[3\] “TimeBodyAccel.Mean\_X” “TimeBodyAccel.Mean\_Y”  
\[5\] “TimeBodyAccel.Mean\_Z” “TimeBodyAccel.STD\_X”  
\[7\] “TimeBodyAccel.STD\_Y” “TimeBodyAccel.STD\_Z”  
\[9\] “TimeGravityAccel.Mean\_X” “TimeGravityAccel.Mean\_Y”  
\[11\] “TimeGravityAccel.Mean\_Z” “TimeGravityAccel.STD\_X”  
\[13\] “TimeGravityAccel.STD\_Y” “TimeGravityAccel.STD\_Z”  
\[15\] “TimeBodyAccelJerk.Mean\_X” “TimeBodyAccelJerk.Mean\_Y”  
\[17\] “TimeBodyAccelJerk.Mean\_Z” “TimeBodyAccelJerk.STD\_X”  
\[19\] “TimeBodyAccelJerk.STD\_Y” “TimeBodyAccelJerk.STD\_Z”  
\[21\] “TimeBodyGyroscope.Mean\_X” “TimeBodyGyroscope.Mean\_Y”  
\[23\] “TimeBodyGyroscope.Mean\_Z” “TimeBodyGyroscope.STD\_X”  
\[25\] “TimeBodyGyroscope.STD\_Y” “TimeBodyGyroscope.STD\_Z”  
\[27\] “TimeBodyGyroscopeJerk.Mean\_X” “TimeBodyGyroscopeJerk.Mean\_Y”  
\[29\] “TimeBodyGyroscopeJerk.Mean\_Z” “TimeBodyGyroscopeJerk.STD\_X”  
\[31\] “TimeBodyGyroscopeJerk.STD\_Y” “TimeBodyGyroscopeJerk.STD\_Z”  
\[33\] “TimeBodyAccelMagnitude.Mean” “TimeBodyAccelMagnitude.STD”  
\[35\] “TimeGravityAccelMagnitude.Mean”
“TimeGravityAccelMagnitude.STD”  
\[37\] “TimeBodyAccelJerkMagnitude.Mean”
“TimeBodyAccelJerkMagnitude.STD”  
\[39\] “TimeBodyGyroscopeMagnitude.Mean”
“TimeBodyGyroscopeMagnitude.STD”  
\[41\] “TimeBodyGyroscopeJerkMagnitude.Mean”
“TimeBodyGyroscopeJerkMagnitude.STD” \[43\] “FreqBodyAccel.Mean\_X”
“FreqBodyAccel.Mean\_Y”  
\[45\] “FreqBodyAccel.Mean\_Z” “FreqBodyAccel.STD\_X”  
\[47\] “FreqBodyAccel.STD\_Y” “FreqBodyAccel.STD\_Z”  
\[49\] “FreqBodyAccelJerk.Mean\_X” “FreqBodyAccelJerk.Mean\_Y”  
\[51\] “FreqBodyAccelJerk.Mean\_Z” “FreqBodyAccelJerk.STD\_X”  
\[53\] “FreqBodyAccelJerk.STD\_Y” “FreqBodyAccelJerk.STD\_Z”  
\[55\] “FreqBodyGyroscope.Mean\_X” “FreqBodyGyroscope.Mean\_Y”  
\[57\] “FreqBodyGyroscope.Mean\_Z” “FreqBodyGyroscope.STD\_X”  
\[59\] “FreqBodyGyroscope.STD\_Y” “FreqBodyGyroscope.STD\_Z”  
\[61\] “FreqBodyAccelMagnitude.Mean” “FreqBodyAccelMagnitude.STD”  
\[63\] “FreqBodyAccelJerkMagnitude.Mean”
“FreqBodyAccelJerkMagnitude.STD”  
\[65\] “FreqBodyGyroscopeMagnitude.Mean”
“FreqBodyGyroscopeMagnitude.STD”  
\[67\] “FreqBodyGyroscopeJerkMagnitude.Mean”
“FreqBodyGyroscopeJerkMagnitude.STD”

#### SELECTION B—(END)

*16)* At this point in the tidy Data set creation, the original
dataframe with raw data (compositedb) has been transformed in that it
now has a reduced number of columns corresponding to the purpose for
which the dataset will be used and secondly, the columns of this trimmed
dataframe have been assigned more descriptive column nemes. The values
in the rows of the columns remain unaltered from their values in the raw
dataset.

#### The variables present in the dataset at this stage

*17)* The first two columns are “subjectidentifier” and “activitytype”.
Each row of the reduced compositedb pertains to a specific “subject”
identified by the subjectidentifier, performing a specific activity (or
activitytype) to generate an observation comprising of numerical values
each of which is in some form relaed to mean or standard deviation. The
units of the data items are as described in the paragraph “Notes on the
original (raw) data” in a preceding paragraph.

*18* The dimensions of the dataset compositedb are ’‘’dim(compositedb)
\[1\] 10299 68’’’ Of the 68 columns, the first two columns are the
subjectidentifier and the activitytype columns whereas the rest of the
66 columns contain measurements obtained from the smartphone.

*19*. The tidy data frame ‘tidyData’ is obtained by using the summarize
function of dplyr (‘dplyr::summarize’) to compute the mean of each
measurement variable (columns 3 to 68) for each combination of
subjectidentifier and activitytype.

*20*. The dimensions of the tiny data dataset tiny Data are \>
dim(tidyData) \[1\] 180 68 ( Of the 68 columns, the first two columns
are the subjectidentifier and the activitytype columns. Columns 3:68
contain the mean of the measurement variables for that column computed
for each combination of the subjectidentifier and activitytype. There
are 30 subjects each performing all of the activitytype movements (6)

#### Sources

All of the datasets described herin, including the HAR dataset, are
provied AS-IS. Any use of the daasets must be accompanied by the
following attribution: Davide Anguita, Alessandro Ghio, Luca Oneto,
Xavier Parra and Jorge L. Reyes-Ortiz. A Public Domain Dataset for Human
Activity Recognition Using Smartphones. 21th European Symposium on
Artificial Neural Networks, Computational Intelligence and Machine
Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.
