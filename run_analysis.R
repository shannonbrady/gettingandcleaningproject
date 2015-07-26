## Train Data
#Load in raw data
traindata <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/train/X_train.txt")
#Load activity labels
trainlabels <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/train/y_train.txt")
#Load subject identifier
trainsubjects <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/train/subject_train.txt")

#Add column of activity and column of subject
alltrain <- cbind(traindata, trainlabels)
colnames(alltrain)[562] <- "labels"
alltrain <- cbind(alltrain, trainsubjects)
colnames(alltrain)[563] <- "subject"

## Test Data
#Load in raw data
testdata <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/test/X_test.txt")
#Load activity labels
testlabels <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/test/y_test.txt")
#Load subject identifier
testsubjects <- read.table("/Users/brady.shannon/Downloads/UCI HAR Dataset/test/subject_test.txt")

#Add column of activity and column of subject
alltest <- cbind(testdata, testlabels)
colnames(alltest)[562] <- "labels"
alltest <- cbind(alltest, testsubjects)
colnames(alltest)[563] <- "subject"

#Merge test and train together
alldata <- rbind(alltrain, alltest)

#Select only mean and standard dev measurements
mydata <- alldata%>%
  select(as.numeric(c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543,562:563)))

#Create a list of column labels that are descriptive
mylabels <- c("Body Acceleration mean.X", "Body Acceleration mean.Y", "Body Acceleration mean.Z",
              "Body Acceleration std.X", "Body Acceleration std.Y", "Body Acceleration std.Z",
              "Gravity Acceleration mean.X", "Gravity Acceleration mean.Y", "Gravity Acceleration mean.Z",
              "Gravity Acceleration std.X", "Gravity Acceleration std.Y", "Gravity Acceleration std.Z",
              "Body Acceleration Jerk mean.X", "Body Acceleration Jerk mean.Y", "Body Acceleration Jerk mean.Z",
              "Body Acceleration Jerk std.X", "Body Acceleration Jerk std.Y", "Body Acceleration Jerk std.Z",
              "Body Gyro mean.X", "Body Gyro mean.Y", "Body Gyro mean.Z",
              "Body Gyro std.X", "Body Gyro std.Y", "Body Gyro std.Z",
              "Body Gyro Jerk mean.X", "Body Gyro Jerk mean.Y", "Body Gyro Jerk mean.Z",
              "Body Gyro Jerk std.X", "Body Gyro Jerk std.Y", "Body Gyro Jerk std.Z",
              "Body Acceleration Magnitude mean", "Body Acceleration Magnitude std",
              "Gravity Acceleration Magnitude mean", "Gravity Acceleration Magnitude std",
              "Body Acceleration Jerk Magnitude mean", "Body Acceleration Jerk Magnitude std",
              "Body Gyro Magnitude mean", "Body Gyro Magnitude std",
              "Body Gyro Jerk Magnitude mean", "Body Gyro Jerk Magnitude std",
              "fBody Acceleration mean.X", "fBody Acceleration mean.Y", "fBody Acceleration mean.Z",
              "fBody Acceleration std.X", "fBody Acceleration std.Y", "fBody Acceleration std.Z",
              "fBody Acceleration Jerk mean.X", "fBody Acceleration Jerk mean.Y", "fBody Acceleration Jerk mean.Z",
              "fBody Acceleration Jerk std.X", "fBody Acceleration Jerk std.Y", "fBody Acceleration Jerk std.Z",
              "fBody Gyro mean.X", "fBody Gyro mean.Y", "fBody Gyro mean.Z",
              "fBody Gyro std.X", "fBody Gyro std.Y", "fBody Gyro std.Z",
              "fBody Acceleration Magnitude mean", "fBody Acceleration Magnitude std",
              "fBody Body Acceleration Jerk Magnitude mean", "fBody Body Acceleration Jerk Magnitude std",
              "fBody Body Gyro Magnitude mean", "fBody Body Gyro Magnitude std",
              "fBody Body Gyro Jerk Magnitude mean", "fBody Body Gyro Jerk Magnitude std", "Activity", "Subject")
#Put descriptive column labels on data set
colnames(mydata) <- mylabels

#Make descriptive activity names
for (i in 1:nrow(mydata)) {
ifelse(mydata$Activity[[i]]==1, mydata$Activity[[i]] <- "Walking",
       ifelse(mydata$Activity[[i]]==2, mydata$Activity[[i]] <- "Walking Upstairs",
              ifelse(mydata$Activity[[i]]==3, mydata$Activity[[i]] <- "Walking Downstairs",
                     ifelse(mydata$Activity[[i]]==4, mydata$Activity[[i]] <- "Sitting",
                            ifelse(mydata$Activity[[i]]==5, mydata$Activity[[i]] <- "Standing",
                                   ifelse(mydata$Activity[[i]]==6, mydata$Activity[[i]] <- "Laying",NA))))))}

tidy <- mydata%>%
  group_by(Subject,Activity)%>%
  summarise_each(funs(mean))

write.table(tidy, file="./coursera/tidy.txt", row.name=FALSE)

