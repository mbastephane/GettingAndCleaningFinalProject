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
cols_of_means <- setdiff(grep("mean",data_names),grep("meanF",data_names))
cols_of_std <- grep("std",data_names)

data_col_mean_std <- data_whole[, c(cols_of_means, cols_of_std)]




#3. Uses descriptive activity names to name the activities in the data set
data_labelled <- data_whole
data_labelled$label <- factor(data_labelled$label_code, levels = activity_labels[, 1], labels = activity_labels[, 2])




#4. Appropriately labels the data set with descriptive variable names.
#
# get column names
data_labelled_cols <- colnames(data_labelled)

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
