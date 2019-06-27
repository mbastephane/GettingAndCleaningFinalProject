library(dplyr)


# 1.Merges the training and the test sets to create one data set.

#Read Test data
subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/x_test.txt")
y_test <- read.table("test/y_test.txt")

#Read training data
subject_train <- read.table("train/subject_train.txt")
y_train <- read.table("train/y_train.txt")
x_train <- read.table("train/X_train.txt")

#Read metadata
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) <- c("label_code","label")

features <- read.table("features.txt")

#Build the columns' names vector
data_names <- c("subject",as.character(features[,2]),"label_code")

#Build the data training set with their label codes
data_train <- data.frame(subject_train,x_train,y_train)
names(data_train) <- data_names

#Build the data testing set with their label codes
data_test <- data.frame(subject_test,x_test,y_test)
names(data_test) <- data_names

#Merging the data training and testing sets
data_whole <- rbind(data_test, data_train, all=TRUE)



#2.Extracts only the measurements on the mean and standard deviation for each measurement.
cols_of_means <- grep("mean",data_names)
cols_of_std <- grep("std",data_names)
cols_of_subject_label <- grep("subject|label",data_names)

data_col_mean_std <- data_whole[, c(cols_of_means, cols_of_std,cols_of_subject_label)]




#3. Uses descriptive activity names to name the activities in the data set
data_labelled <- data_col_mean_std
data_labelled$label <- factor(data_labelled$label_code, levels = activity_labels[, 1], labels = activity_labels[, 2])
data_labelled$label_code <- NULL



#4. Appropriately labels the data set with descriptive variable names.
#
# get column names
data_labelled_cols <- colnames(data_labelled)

# remove special characters
data_labelled_cols <- gsub("[\\(\\)-]", "", data_labelled_cols)

# expand abbreviations and clean up names
data_labelled_cols <- gsub("^f", "frequencyDomain", data_labelled_cols)
data_labelled_cols <- gsub("^t", "timeDomain", data_labelled_cols)
data_labelled_cols <- gsub("Acc", "Accelerometer", data_labelled_cols)
data_labelled_cols <- gsub("Gyro", "Gyroscope", data_labelled_cols)
data_labelled_cols <- gsub("Mag", "Magnitude", data_labelled_cols)
data_labelled_cols <- gsub("Freq", "Frequency", data_labelled_cols)
data_labelled_cols <- gsub("mean", "Mean", data_labelled_cols)
data_labelled_cols <- gsub("std", "StandardDeviation", data_labelled_cols)

# correct typo
data_labelled_cols <- gsub("BodyBody", "Body", data_labelled_cols)

# use new labels as column names
colnames(data_labelled) <- make.names(data_labelled_cols, unique = TRUE)





#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
#
data_labelled_means <- data_labelled %>% 
    group_by(subject, label) %>%
    summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(data_labelled_means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

data_labelled_means
