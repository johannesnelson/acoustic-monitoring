
###Script for Downloading Data

# First, read all the functions into the environment, ten begin by using the nameFiles function to rename and move the audiomoth
# files to a directory of your choice. Note this will only work for audiomoth files naming convention, but could be modified to fit others.
# Then, you can use the dbFolder to add all the files to the database

#Checklist:
# -rename files according to location and equipmentID
# -copy files to program archive in cloud
# -append recordings table in SQL database
# -back files up on SDD
# -THEN AND ONLY THEN DELETE THE FILES


library(AMMonitor)
library(AMModels)
library(tidyverse)
library(seewave)
library(warbleR)
library(tuneR)
library(lubridate)

db.path <- 'database/Capstone.sqlite'

conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# Turn the SQLite foreign constraints on
RSQLite::dbExecute(conn = conx, statement = 
                     "PRAGMA foreign_keys = ON;"
)



##### This will name all files in a directory according to the arguments of locID and eqID

nameFiles <- function(directory, new_directory, locID, eqID) {
  
  file.vector <- list.files(directory) 
  current.dir <- getwd()
  setwd(directory)
  for (x in file.vector) {
    file.rename(from = x, to = paste(locID, eqID, basename(x), sep = '_'))
  }
  new.file.vector <- list.files()
  file.copy(new.file.vector, new_directory)
  setwd(current.dir)
}
 


###This defines the first function, which can be applied to single files
dbFile <- function(fpath) {
  file.name <- basename(fpath)
  sp1 <- unlist(strsplit(file.name, split = '_', fixed = TRUE))
  
  sp2 <- unlist(strsplit(sp1[4] , split = '.', fixed = TRUE))
  
  
  recording.insert <<- data.frame(recordingID = file.name,
                                  locationID = sp1[1],
                                  equipmentID = sp1[2],
                                  startDate = as.character(ymd(sp1[3])),
                                  startTime = sp2[1],
                                  format = sp2[2],
                                  tz = Sys.timezone(),
                                  timestamp = as.character(Sys.time()))
  RSQLite::dbWriteTable(conn = conx, name = 'recordings', value = recording.insert, append = TRUE)
  
  
}



#####This defines the second function which runs it along the list of filenames in a folder.
dbFolder <- function(dir.path) {
  all.names <- list.files(dir.path)
  sapply(all.names, FUN = dbFile)
  
}




