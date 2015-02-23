# Download necessary files:
if (!file.exists("getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, method="curl", destfile = "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
  print("Dataset downloaded!")
}

# Unzip dataset
if (!file.exists("UCI HAR Dataset/")) {
  unzip("getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
  print("Dataset unziped!")
}

# You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.

xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
X <- rbind(xtrain, xtest)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
S <- rbind(subject_train, subject_test)

ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")
ytest <- read.table("UCI HAR Dataset/test/y_test.txt")
Y <- rbind(ytrain, ytest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names. 

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "averages.txt")








