run_analysis<- function() {
  
  library(dplyr)
  col_names<-read.table("features.txt", header=F,sep="")
  # subsetting only second column in my data frame of colums names
  col_names1<-col_names[,"V2"]
  
  rawdata<-read.table("train/X_train.txt", header=F,sep="")
  person<-read.table("train/subject_train.txt", header=F,sep="")
  activity<-read.table("train/y_train.txt", header=F,sep="")
  
  #replacing numbers of activity with the actual name of the activity in train 
  activity<-replace(activity,activity==1,'WALKING') 
  activity<-replace(activity,activity==2,'WALKING_UPSTAIRS')
  activity<-replace(activity,activity==3,'WALKING_DOWNSTAIRS') 
  activity<-replace(activity,activity==4,'SITTING')
  activity<-replace(activity,activity==5,'STANDING')
  activity<-replace(activity,activity==6,'LAYING')
  
  # naming the colums in the train data file
  colnames(rawdata)<-col_names1
  # removing the colums not related to the "mean" data we need
  rawdata1<-rawdata[ , grepl( "mean" , names( rawdata ) ) ]
  # binding the data in train data frame
  DF_1<-cbind(activity,person,rawdata1)
  
  rawdata_test<-read.table("test/X_test.txt", header=F,sep="")
  person_test<-read.table("test/subject_test.txt", header=F,sep="")
  activity_test<-read.table("test/y_test.txt", header=F,sep="")
  # naming the colums in the test data file
  colnames(rawdata_test)<-col_names1
  # removing the colums not related to the "mean" data we need
  rawdata_test1<-rawdata_test[ , grepl( "mean" , names( rawdata_test ) ) ]
  #replacing numbers of activity with the actual name of the activity in test
  activity_test<-replace(activity_test,activity_test==1,'WALKING') 
  activity_test<-replace(activity_test,activity_test==2,'WALKING_UPSTAIRS')
  activity_test<-replace(activity_test,activity_test==3,'WALKING_DOWNSTAIRS') 
  activity_test<-replace(activity_test,activity_test==4,'SITTING')
  activity_test<-replace(activity_test,activity_test==5,'STANDING')
  activity_test<-replace(activity_test,activity_test==6,'LAYING')
  
  # binding the data in test data frame
  DF_2<-cbind(activity_test,person_test,rawdata_test1)
  
  #binding both data frames
  DF<-rbind(DF_1,DF_2)
  
  #changing first and second column name in test data
  names(DF)[1]<-paste("activities")
  names(DF)[2]<-paste("person")
  
  # sorting the data by person and by activity
  DF<-data.frame(DF[order(DF$activities,DF$person),],row.names=NULL)
  DF<-DF[,1:5]
  names(DF)[3]<-paste("tBodyAcc.mean.X")
  names(DF)[4]<-paste("tBodyAcc.mean.Y")
  names(DF)[5]<-paste("tBodyAcc.mean.Z")
  
  DF<-group_by(DF,person,activities)
  DF<-summarize(DF,mean(tBodyAcc.mean.X),mean(tBodyAcc.mean.Y),mean(tBodyAcc.mean.Z))
  write.table(DF, file = "tidy_data.txt", row.names = FALSE,col.names=TRUE)
  
}