library(tidyverse)

#this sets the wd to whichever file you want to extract files from
setwd("/Users/bridgerhuhn/Documents/Research/Endemic Plant/Yermo Data/yermo/IRGA_2021")

### creates function that gets all your .csv files and puts them into one data frame
MegaFrame <- function(){
  #gets all files with the given file type
  allFiles<-list.files(pattern = "*.csv")
  
  outDF<- data.frame() ### creates a dataframe to store
  for (i in 1:length(allFiles)){
    #for the current file read it in
    dat<- read.csv(allFiles[i], row.names = NULL)
    
    #puts meta data in a column
    meta<- dat[1,1]
    #stores the dat in which this file was created
    dat$meta <- meta 
    
    ## renames columns using by finding "obs"
    names(dat)<- as.character(unlist(dat[min(which(dat[,1] == "Obs")),])) 
    
    ### some irgas have Mch columns, this removes those
    dat <-dat[,-(which(grepl("Mch",names(dat))))]
    
    #binds data frames together
    outDF<- rbind(outDF, dat)
  }
  
}

## this function gets rid of rows that aren't necessary
LC<-function(dat){
  #creates a comments column
  dat$comment<-"kitten"
  
  
  #This is to put comments in comment column####
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (dat[i,1]=="Remark=" & !grepl(pattern = "\"", dat[i,2])) {
        dat$comment[i] <- dat[i,2]
      } else {
        dat$comment[i]<- dat$comment[i-1]
      }
    }
  }
  
  #deletes rows where Remarks don't take measuremetns (Remarks that we didn't type in on the machine)
  todelete<- c()
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (dat[i,1]=="Remark=" & grepl(pattern = "\"", dat[i,2])) {## any row with remark, and a " in it are put into a list
        todelete <- append(todelete,i)
      }
    }
  }
  
  dat <- dat[-todelete,]
  
  ## deletes leading rows that where named "kitten" earlier ^^^^
  dat <- dat[-which(dat$comment=="kitten"),] 
  dat <- dat[which(grepl(pattern = ":", dat[,2])),]
  #makes all numbers numeric and non numbers NAs
  dat[,1]<-as.numeric(as.character(unlist(dat[,1])))
  
  return(dat)
}
