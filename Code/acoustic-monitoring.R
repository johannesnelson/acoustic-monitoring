#### BirdNET data manipulation functions
library(NSNSDAcoustics)
library(tidyverse)
library(RSQLite)
library(AMMonitor)
library(seewave)

data.fp <- "D:/Capstone/Audio_data"
results.fp <- "D:/Capstone/BirdNET_results"

db.path <- 'database/resultsDB.sqlite'

bndb <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)


# This gathers all and writes it to a new, full csv file in the same directory

gather.all <- function(directory, filename) {
  all.results <- birdnet_gather(results.directory = directory, formatted = FALSE)
  write.csv(all.results, paste(directory, filename, sep = '/'), row.names = FALSE)
  return(all.results)
}



# This preps the data frame for input into the function birdnet_plot()

plot.prep <- function(x) {
  mutate(x, timezone = "America/New_York",
         verify = NA,
         recordingID = basename(filepath))
  
}


### Functions for adding columns that allow exploration by location, date, time

# Add locationID based on locationID tag in filename
add.locationID <- function(x) {
  
  newframe <- mutate(x, locationIDvector = strsplit(basename(x$filepath), split = '_'))
  
  locvector <- c()
  
  for (i in seq(newframe$locationIDvector)){
    locvector <- append(locvector, newframe$locationIDvector[[i]][1])
    if(length(locvector) == length(newframe$locationIDvector)) {break}
    
    
  }
  
  newframe <-  mutate(x, locationID = locvector)
}


# Add date based on date info in filename
add.date <- function(x) {
  
  newframe <- mutate(x, locationIDvector = strsplit(basename(x$filepath), split = '_'))
  
  datevector <- c()
  
  for (i in seq(newframe$locationIDvector)){
    
    datevector <- append(datevector, newframe$locationIDvector[[i]][c(3)])
    
    if(length(datevector) == length(newframe$locationIDvector)) {break}
  }
  
  newframe <-  mutate(x, date = ymd(datevector))
  
}

# Add time based on time info in filename (needs to be made better with a lubridate style function)
add.time <- function(x) {
  
  newframe <- mutate(x, locationIDvector = strsplit(basename(x$filepath), split = '_'))
  
  timevector <- c()
  
  for (i in seq(newframe$locationIDvector)){
    timevector <- append(timevector, gsub(".WAV","", newframe$locationIDvector[[i]][c(4)]))
    if(length(timevector) == length(newframe$locationIDvector)) {break}
  }
  
  newframe <-  mutate(x, time = timevector)
}


######## Filtering made easier 

filter.species <- function(data, species){
  filter(data, common_name == species)
}

filter.confidence <- function(data, minconf){
  filter(data, confidence >= minconf)
}

species.count <- function(data, num.obs = 20){
  setDT(data)
  setorder(data, common_name, -confidence)
  specvec <- unique(data[,common_name])
  high.conf.mean <- c()
  for (i in 1:length(specvec)) {
    value <- mean(data[common_name == specvec[i]][1:num.obs,confidence])
    high.conf.mean[i]<- value
  }
  
  spec.conf.table <- data.table(common_name = specvec, high.conf.mean = high.conf.mean)

  spec.table <- data %>% 
   group_by(common_name) %>% 
   summarise(count = n(), 
             max.conf = max(confidence), 
             min.conf = min(confidence),
             mean.conf = mean(confidence), 
           sd.conf = sd(confidence))
    setDT(spec.table)
    spec.table <- full_join(spec.table, spec.conf.table )

  
 # spec.table <- full_join(spec.table, specDT, by = common_name)
  return(spec.table)
  
}


### NEEDS REVIEW NOT WORKING AS EXPECTED
high.mean.conf.filter <- function(data, cutoff, num.obs = 20){
  setDT(data)
  setorder(data, common_name, -confidence)
  specvec <- unique(data[,common_name])
  high.conf.mean <- c()
  for (i in 1:length(specvec)) {
    value <- mean(data[common_name == specvec[i]][1:num.obs,confidence])
    high.conf.mean[i] <- value
  }
  spec.conf.table <- as.data.table(cbind(specvec, high.conf.mean))
  final_specvec <- spec.conf.table[high.conf.mean >= cutoff,1]
  data <- data[common_name %in% final_specvec[,specvec]]
  return(data)
}

#### Write BirdNET detections to ordered wave files that should correspond to spectrogram order if you want to visually scan as well. This expect

makeWaves <- function(x, dir) {
  dir.create(dir, recursive = TRUE)
  dir.fp <- dir
  for (i in 1:nrow(x)){
    wave <- readWave(x$filepath[i], from = x$start[i]-1, to = x$end[i]+1, units = 'seconds')
    writeWave(wave, filename = paste(dir.fp, paste(i, basename(x$filepath[i]), x$start[i], ".wav",  sep = '_'), sep = "/"))
    
  }
  
}

### This is only a slightly modified birdnet_gather that removes the key() arugment for ordering purposes
ordered.plot <- function (data, audio.directory, title, frq.lim = c(0, 12), new.window = TRUE, 
                          spec.col = monitoR::gray.3(), box = TRUE, box.lwd = 1, box.col = "black", 
                          title.size = 1) 
{
  if (all(c("common.name", "start.s", "end.s") %in% 
          colnames(data))) {
    colnames(data)[colnames(data) == "common.name"] <- "common_name"
    colnames(data)[colnames(data) == "start.s"] <- "start"
    colnames(data)[colnames(data) == "end.s"] <- "end"
  }
  if (length(unique(data$common_name)) > 1) {
    stop("Please input data for one species at a time. You have input a dataset with ", 
         length(unique(data$common_name)), " species.")
  }
  owd <- setwd(getwd())
  on.exit(setwd(owd))
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, "/")
  }
  data <- as.data.table(data)
  
  all.wav <- list.files(audio.directory, full.names = TRUE, 
                        recursive = TRUE)
  rec.ids <- unique(data$recordingID)
  wav.paths <- unique(grep(paste(rec.ids, collapse = "|"), 
                           all.wav, value = TRUE))
  checker <- readWave(filename = wav.paths[1], from = 0, to = 3, 
                      units = "seconds")
  check.sp <- monitoR:::spectro(wave = checker)
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] & check.sp$freq <= 
                            frq.lim[2])
  nrows <- length(which.frq.bins)
  ncols <- length(check.sp$time)
  mats <- array(data = 0, dim = c(nrows, ncols, nrow(data)))
  cat("\nGathering plot data...\n")
  for (n in 1:nrow(data)) {
    cat(n, " ")
    dat <- data[n]
    det <- readWave(filename = wav.paths[grepl(pattern = dat$recordingID, 
                                               x = wav.paths)], from = dat$start, to = dat$end, 
                    units = "seconds")
    det.spec <- monitoR:::spectro(wave = det, tlab = paste0(dat$recordingID, dat$start))
    mats[, , n] <- det.spec$amp[which.frq.bins, ]
  }
  if (new.window) 
    dev.new()
  sqrdim <- ceiling(sqrt(dim(mats)[3]))
  if (sqrdim^2%%dim(mats)[3] > sqrdim) {
    dim1 <- sqrdim - 1
    dim2 <- sqrdim
  }
  else {
    dim1 <- dim2 <- sqrdim
  }
  par(mar = rep(0, 4), mfrow = c(dim1, dim2))
  cat("\nPlotting...\n")
  for (pl in 1:dim(mats)[3]) {
    image(x = 1:ncols, y = 1:nrows, t(mats[, , pl]), yaxt = "n", 
          xaxt = "n", xlab = "", ylab = "", 
          col = spec.col)
    if (box == TRUE) {
      box(col = box.col, lwd = box.lwd)
    }
  }
  if (!(missing(title))) 
    mtext(title, side = 3, line = -2, outer = TRUE, cex = title.size)
  cat("\nDone plotting detections.")
}


##### This will create subset dfs of all the rails in a formatted df

subset.rails.conf <- function(x) {
  
  VIRA <<- filter.species(x, "Virginia Rail")
  KIRA <<- filter.species(x, "King Rail")
  BLRA <<- filter.species(x, "Black Rail")
  SORA <<- filter.species(x, "Sora")
  
  VIRA <<- VIRA[order(-VIRA$confidence),]
  KIRA <<- KIRA[order(-KIRA$confidence),]
  BLRA <<- BLRA[order(-BLRA$confidence),]
  SORA <<- SORA[order(-SORA$confidence),]
}

subset.rails <- function(x) {
  
  VIRA <<- filter.species(x, "Virginia Rail")
  KIRA <<- filter.species(x, "King Rail")
  BLRA <<- filter.species(x, "Black Rail")
  SORA <<- filter.species(x, "Sora")
  
}

####### Full filter function

#### Filtering BN results
filter.results <- function(dataframe, conf.level, count.cutoff) {
  
  dataframe %>% 
    group_by(common_name) %>% 
    filter(confidence > conf.level) %>% 
    filter(n() > count.cutoff)
}



### Awesome verification function





verify.results.simple <- function (x) {
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a")
  
  if(!"verification" %in% names(x)){
    x <- mutate(x, verification = NA)
  }
  
  if(!"notes" %in% names(x)){
    x<- mutate(x, notes = NA)
  }
  
  
  for (i in 1:nrow(x)) {
    if(!is.na(x$verification[i])){
      cat(paste("\n Verification for", basename(x$filepath[i]), "at", x$start[i], "seconds already exists. Moving onto next detection...\n"))
      verifs <- append(verifs, x$verification[i])
      next}
    
    
    repeat{
      
      wave.obj <- readWave(x$filepath[i], from = x$start[i], to = x$end[i], units = 'seconds')
      #spectro( wave = wave.obj )
      viewSpec(x$filepath[i], start.time = x$start[i]-1, page.length = 5, units = 'seconds'  )
      cat(paste("\n Showing detection", i, "out of", nrow(x), "from", basename(x$filepath[i]), "at", x$start[i], "seconds. Confidence:", x$confidence[i], "\n"))
      
      cat(paste( "Enter \n 'y' for yes,\n",  
                 "'n' for no,\n",
                 "'r' for review,\n",
                 "'p' to play audio segment,\n", 
                 "'w' to write segment as wav file to working directory,\n",
                 "'s' to skip to next segment (and log as NA)",
                 "'a' to add a note \n",
                 "'q' for quit."))
      
      answer <- readline( prompt = paste0(paste("Is this a(n)", x$common_name[i]), "?")) 
      
      
      if(answer %in% verif.options) break
      
      if(answer == "p") {
        tempwave <- readWave(x$filepath[i], from = x$start[i] - 1, to = x$end[i] + 1, units = "seconds")
        play(tempwave)
      }
      
      if(answer == "w") {
        filename <- paste0(paste(gsub(pattern = ".WAV", "", basename(x$filepath[i])), x$start[i], sep = "_"), ".WAV")
        tempwave <<- readWave(x$filepath[i], from = x$start[i] - 1, to = x$end[i] + 1, units = "seconds")
        writeWave(tempwave, filename)
        cat("\n Writing wav file to working directory...")
      }
      
      if(answer == "a") {
        
        note <- readline(prompt = "Add note here: ")
        x$notes[i] <- note
      }
      
      
      if(!answer %in% all.options){
        cat("\n Response not recognized, please input correct response...\n")
      }
      
    }
    
    if(answer %in% c("y", "n", "r")) {
      cat("\n Adding result to verification data...\n ")
      verifs <- append(verifs, answer)
    }
    
    if(answer == "s") {
      verifs <- (append(verifs, NA))
      cat("Skipping to next detection...")
    }
    
    if(answer == "q") {
      
      verifs <- append(verifs, x$verification[i:nrow(x)])
      break}
    
  }
  
  
  mutate(x, verification = verifs)
} 



##### This option adds more functionality 


verify.results.species <- function (x, species = "all", conf = 0) {
  
  if(species == "all") {
    
  }
  
  if(!species == "all") {
    x <-  filter(x, common_name == species)
    
  }
  x <- filter(x, confidence > conf)
  
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a")
  
  if(!"verification" %in% names(x)){
    x <- mutate(x, verification = NA)
  }
  
  if(!"notes" %in% names(x)){
    x<- mutate(x, notes = NA)
  }
  
  
  for (i in 1:nrow(x)) {
    if(!is.na(x$verification[i])){
      cat(paste("\n Verification for", basename(x$filepath[i]), "at", x$start[i], "seconds already exists. Moving onto next detection...\n"))
      x$verification[i] <- x$verification[i]
      next}
    
    
    repeat{
      
      wave.obj <- readWave(x$filepath[i], from = x$start[i], to = x$end[i], units = 'seconds')
      #spectro( wave = wave.obj )
      viewSpec(x$filepath[i], start.time = x$start[i]-1, page.length = 5, units = 'seconds'  )
      cat(paste("\n Showing detection", i, "out of", nrow(x), "from", basename(x$filepath[i]), "at", x$start[i], "seconds. Confidence:", x$confidence[i], "\n"))
      
      cat(paste( "Enter \n 'y' for yes,\n",  
                 "'n' for no,\n",
                 "'r' for review,\n",
                 "'p' to play audio segment,\n", 
                 "'w' to write segment as wav file to working directory,\n",
                 "'s' to skip to next segment (and log as NA)",
                 "'a' to add a note \n",
                 "'q' for quit."))
      
      answer <- readline( prompt = paste0(paste("Is this a(n)", x$common_name[i]), "?")) 
      
      
      if(answer %in% verif.options) break
      
      if(answer == "p") {
        tempwave <- readWave(x$filepath[i], from = x$start[i] - 1, to = x$end[i] + 1, units = "seconds")
        play(tempwave)
      }
      
      if(answer == "w") {
        filename <- paste0(paste(gsub(pattern = ".WAV", "", basename(x$filepath[i])), x$start[i], sep = "_"), ".WAV")
        tempwave <<- readWave(x$filepath[i], from = x$start[i] - 1, to = x$end[i] + 1, units = "seconds")
        writeWave(tempwave, filename)
        cat("\n Writing wav file to working directory...")
      }
      
      if(answer == "a") {
        
        note <- readline(prompt = "Add note here: ")
        x$notes[i] <- note
      }
      
      
      if(!answer %in% all.options){
        cat("\n Response not recognized, please input correct response...\n")
      }
      
    }
    
    if(answer %in% c("y", "n", "r")) {
      cat("\n Adding result to verification data...\n ")
      x$verification[i] <- answer
    }
    
    if(answer == "s") {
      x$verification[i] <- NA
      cat("Skipping to next detection...")
    }
    
    if(answer == "q") {
      
      x$verification[i:nrow(x)] <- x$verification[i:nrow(x)]
      break}
    
  }
  saveask <- readline(prompt = "Would you like to save results as a csv file? \n Input 'y' for yes:")
  if(saveask == "y") {
    fname <- readline(prompt = "What would you like to name the file?")
    
    write.csv(x, paste0(fname, ".csv"), row.names = FALSE)
  }
  return(x)
  
} 


######################## For quick verifs of full dataset by count

verify.results.count <- function (df, spec.count = NULL) {
  
  setDT(df)
  setkey(df, common_name)
  spec.list <- unique(df[,common_name])
  spec.no <- length(spec.list)
  
  final_dt <- data.table()
  for(i in 1:length(spec.list)) {
    spec.subset <- df[common_name == spec.list[i],]
    if(nrow(spec.subset) >= spec.count){
      spec.subset <- spec.subset[sample(1:nrow(spec.subset), spec.count), ]
    } 
    final_dt <- rbind(final_dt, spec.subset)
  }
  df <- final_dt
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a")
  
  if(!"verification" %in% names(df)){
    df[, verification := NA]
  }
  
  if(!"notes" %in% names(df)){
    df[,notes := NA]
  }
  
  
  for (i in 1:nrow(final_dt)) {
    if(!is.na(df$verification[i])){
      cat(paste("\n Verification for", basename(df$filepath[i]), "at", df$start[i], "seconds already exists. Moving onto next detection...\n"))
      df$verification[i] <- df$verification[i]
      next}
    
    
    repeat{
      
      wave.obj <- readWave(df$filepath[i], from = df$start[i], to = df$end[i], units = 'seconds')
      #spectro( wave = wave.obj )
      viewSpec(df$filepath[i], start.time = df$start[i]-1, page.length = 5, units = 'seconds'  )
      cat(paste("\n Showing detection", i, "out of", nrow(df), "from", basename(df$filepath[i]), "at", df$start[i], "seconds. Confidence:", df$confidence[i], "\n"))
      
      cat(paste( "Enter \n 'y' for yes,\n",  
                 "'n' for no,\n",
                 "'r' for review,\n",
                 "'p' to play audio segment,\n", 
                 "'w' to write segment as wav file to working directory,\n",
                 "'s' to skip to next segment (and log as NA)",
                 "'a' to add a note \n",
                 "'q' for quit."))
      
      answer <- readline( prompt = paste0(paste("Is this a(n)", df$common_name[i]), "?")) 
      
      
      if(answer %in% verif.options) break
      
      if(answer == "p") {
        tempwave <- readWave(df$filepath[i], from = df$start[i] - 1, to = df$end[i] + 1, units = "seconds")
        play(tempwave)
      }
      
      if(answer == "w") {
        filename <- paste0(paste(gsub(pattern = ".WAV", "", basename(df$filepath[i])), df$start[i], sep = "_"), ".WAV")
        tempwave <- readWave(df$filepath[i], from = df$start[i] - 1, to = df$end[i] + 1, units = "seconds")
        writeWave(tempwave, filename)
        cat("\n Writing wav file to working directory...")
      }
      
      if(answer == "a") {
        
        note <- readline(prompt = "Add note here: ")
        df$notes[i] <- note
      }
      
     # if(answer == 'l') {
        
      #  i <- i - 1
    #  }
      
      
      if(!answer %in% all.options){
        cat("\n Response not recognized, please input correct response...\n")
      }
      
    }
    
    if(answer %in% c("y", "n", "r")) {
      cat("\n Adding result to verification data...\n ")
      df$verification[i] <- answer
    }
    
    if(answer == "s") {
      df$verification[i] <- NA
      cat("Skipping to next detection...")
    }
    
    if(answer == "q") {
      
      df$verification[i:nrow(df)] <- df$verification[i:nrow(df)]
      break}
    
  }
  saveask <- readline(prompt = "Would you like to save results as a csv file? \n Input 'y' for yes:")
  if(saveask == "y") {
    fname <- readline(prompt = "What would you like to name the file?")
    
    write.csv(df, paste0(fname, ".csv"), row.names = FALSE)
  }
  return(df)
  
} 




#### Function to pull out, visualize, and listen to a detection by number -- taking args of datagrame and rownumber


pullWave <- function (x, det_num, time.buffer = 0, start_buffer = 0, end_buffer = 0, listen = TRUE) {
  
  tempwave <- readWave(filename = x$filepath[det_num], 
                       from = x$start[det_num] - (time.buffer + start_buffer), 
                       to = x$end[det_num] + (time.buffer+ end_buffer), 
                       units = 'seconds')
  
  viewSpec(clip = x$filepath[det_num],
           start.time = x$start[det_num] - (time.buffer + start_buffer),
           page.length = ((x$end[det_num] + (time.buffer + end_buffer) - (x$start[det_num] - (time.buffer + start_buffer)))),
           units = 'seconds')
  cat("Birdnet confidence is", x$confidence[det_num])
  if (listen == TRUE){
    play(tempwave)}
  
  return(tempwave)
}

##### Good batch loading from a folder

batch_load <- function (dir) {
  
  filenames <- list.files(dir)
  filenames <- paste(dir, filenames, sep = '\\')
  mergeddata <- vroom(filenames)
  return(mergeddata)
}




#######################

extractFeatures <- function(data, flim = NULL, amp.data = TRUE, zcr.data = TRUE, acoustat.data = TRUE, mfcc = TRUE, scaled = FALSE) {
  
  full_data <- data.table()
  
  amp_full_data <- data.table()
  
  if (amp.data == TRUE) {
    for (x in 1:length(data$filepath)) {
      
      # Read in temporary wave object
      temp.wave <- readWave(data$filepath[x], from = data$start[x], to = data$end[x], units = 'seconds' )
      
      # Create temporary spectrogram
      temp.spec <- spectro(wave = temp.wave, flim = flim, plot = FALSE)
      
      # Extraction and preparation of taw amp data
      temp.data <- as.vector(t(temp.spec$amp))
      temp.data <- as.data.frame(t(temp.data))
      amp_full_data <- rbind(amp_full_data, temp.data)
      
    }
    
    # add names to columns with amp.'x'
    col_labels <- c()
    for (i in 1:length(amp_full_data)) {
      col_labels <- append(col_labels, paste("amp", i, sep = "."))
    }
    colnames(amp_full_data) <- col_labels
    full_data <- cbind(full_data, amp_full_data)
    
  }
  
  
  # extract zero crossing rate data
  
  if(zcr.data == TRUE) {
    zcr_full_data <- data.table()
    
    for (x in 1:length(data$filepath)){
      temp.wave <- readWave(data$filepath[x], from = data$start[x], to = data$end[x], units = 'seconds' )
      
      zcr.data <- zcr(temp.wave, plot = FALSE)
      zcr.data <- as.data.table(zcr.data[,'zcr'])
      zcr.data <- as.data.table(t(zcr.data))
      zcr_full_data <- rbind(zcr_full_data, zcr.data)
    }
    
    #name zcr cols
    zcr_col_labels <- c()
    for (i in 1:length(zcr_full_data)) {
      zcr_col_labels <- append(zcr_col_labels, paste("zcr", i, sep = "."))
    }
    colnames(zcr_full_data) <- zcr_col_labels
    
    full_data <- cbind(full_data, zcr_full_data)
    
  }
  
  ## MFCC 
  if(mfcc == TRUE) {
    mfcc_full_data <- data.table()
    
    for (i in 1:length(data$filepath)) {
      temp.wave <- readWave(data$filepath[i], from = data$start[i], to = data$end[i], units = 'seconds' )
      mfcc_data <- melfcc(temp.wave)
      mfcc_data <- as.vector(t(mfcc_data))
      mfcc_data <- as.data.table(t(mfcc_data))
      mfcc_full_data <- rbind(mfcc_full_data, mfcc_data)
    }
    mfcc_col_labels <- c()
    for (i in 1:length(mfcc_full_data)) {
      mfcc_col_labels <- append(mfcc_col_labels, paste('mfcc', i, sep = "."))
    }
    setnames(mfcc_full_data, old = colnames(mfcc_full_data), new = mfcc_col_labels)
    full_data <- cbind(full_data, mfcc_full_data)
  }
  ### add acoustat extraction here
  
  if(acoustat.data == TRUE) {
    aco_time_data <- data.table()
    
    for (x in 1:length(data$filepath)){
      temp.wave <- readWave(data$filepath[x], from = data$start[x], to = data$end[x], units = 'seconds' )
      
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      aco.time <- as.data.table(aco.data$time.contour[, 2])
      aco.time <- as.data.table(t(aco.time))
      aco_time_data <- rbind(aco_time_data, aco.time)
    }
    
    #naming tc cols 
    aco_col_labels <- c()
    for (i in 1:length(aco_time_data)) {
      aco_col_labels <- append(aco_col_labels, paste("tc", i, sep = "."))
    }
    
    colnames(aco_time_data) <- aco_col_labels
    
    full_data <- cbind(full_data, aco_time_data)
    
    ### add acoustat extraction here
    
    aco_freq_data <- data.table()
    
    for (x in 1:length(data$filepath)){
      temp.wave <- readWave(data$filepath[x], from = data$start[x], to = data$end[x], units = 'seconds' )
      
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      aco.freq <- as.data.table(aco.data$freq.contour[, 2])
      aco.freq <- as.data.table(t(aco.freq))
      aco_freq_data <- rbind(aco_freq_data, aco.freq)
    }
    
    #naming tc cols 
    aco_freq_labels <- c()
    for (i in 1:length(aco_freq_data)) {
      aco_freq_labels <- append(aco_freq_labels, paste("fc", i, sep = "."))
    }
    
    colnames(aco_freq_data) <- aco_freq_labels
    
    full_data <- cbind(full_data, aco_freq_data)
    
    ##### Final acoustat shit
    aco_misc_data <- data.table()
    
    for (x in 1:length(data$filepath)){
      temp.wave <- readWave(data$filepath[x], from = data$start[x], to = data$end[x], units = 'seconds' )
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      misc.data <- as.data.table(aco.data[3:length(aco.data)])
      aco_misc_data <- rbind(aco_misc_data, misc.data)
      
    }
    full_data <- cbind(full_data, aco_misc_data)
    
  }
  #final column for verification results
  verif.vector <- c()
  
  for (i in 1:length(data$verification)) {
    verif.vector <- append(verif.vector, data$verification[i])
  }
  
  full_data <- cbind(full_data, verif.vector)
  
  if (scaled == TRUE) {
    
    scale.cols <- colnames(full_data)
    
    scale.cols <- scale.cols[-length(scale.cols)]
    full_data[, (scale.cols) := lapply(.SD, scale), .SDcols = scale.cols]
  }
  
  return(full_data)
}


#########################
# Raw extraction of full audio file
totalExtract <- function(filepath, flim = NULL, amp.data = TRUE, zcr.data = TRUE, acoustat.data = TRUE, mfcc = TRUE, scaled = FALSE) {
  fullwave <- readWave(filepath)          
  wav.dur <- length(fullwave@left)/fullwave@samp.rate
  wave.seq <- seq(0, wav.dur, 3)
  full_data <- data.table()
  
  if (amp.data == TRUE) {
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds')
      temp.spec <- spectro(wave = temp.wave, flim = flim, plot = FALSE)
      
      # Extraction and preparation of taw amp data
      temp.data <- as.vector(t(temp.spec$amp))
      temp.data <- as.data.table(t(temp.data))
      full_data <- rbind(full_data, temp.data)
      
    }
    
    # add names to columns with amp.'x'
    col_labels <- c()
    for (i in 1:length(full_data)) {
      col_labels <- append(col_labels, paste("amp", i, sep = "."))
    }
    colnames(full_data) <- col_labels
    
  }
  ##zcr extraction begin here
  if(zcr.data == TRUE) {
    zcr_full_data <- data.table()
    
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds')    
      zcr.data <- zcr(temp.wave, plot = FALSE)
      zcr.data <- as.data.table(zcr.data[,'zcr'])
      zcr.data <- as.data.table(t(zcr.data))
      zcr_full_data <- rbind(zcr_full_data, zcr.data)
    }
    
    #name zcr cols
    zcr_col_labels <- c()
    for (i in 1:length(zcr_full_data)) {
      zcr_col_labels <- append(zcr_col_labels, paste("zcr", i, sep = "."))
    }
    colnames(zcr_full_data) <- zcr_col_labels
    
    full_data <- cbind(full_data, zcr_full_data)
    
  }
  #### MFCC
  if(mfcc == TRUE) {
    mfcc_full_data <- data.table()
    
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds') 
      mfcc_data <- melfcc(temp.wave)
      mfcc_data <- as.vector(t(mfcc_data))
      mfcc_data <- as.data.table(t(mfcc_data))
      mfcc_full_data <- rbind(mfcc_full_data, mfcc_data)
    }
    mfcc_col_labels <- c()
    for (i in 1:length(mfcc_full_data)) {
      mfcc_col_labels <- append(mfcc_col_labels, paste('mfcc', i, sep = "."))
    }
    setnames(mfcc_full_data, old = colnames(mfcc_full_data), new = mfcc_col_labels)
    full_data <- cbind(full_data, mfcc_full_data)
  }
  ####Acoustat functions here
  
  if (acoustat.data == TRUE) {
    aco_time_data <- data.table()
    
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds')  
      
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      aco.time <- as.data.table(aco.data$time.contour[, 2])
      aco.time <- as.data.table(t(aco.time))
      aco_time_data <- rbind(aco_time_data, aco.time)
    }
    
    #naming tc cols 
    aco_col_labels <- c()
    for (i in 1:length(aco_time_data)) {
      aco_col_labels <- append(aco_col_labels, paste("tc", i, sep = "."))
    }
    
    colnames(aco_time_data) <- aco_col_labels
    
    full_data <- cbind(full_data, aco_time_data)
    
    
    
    ### add acoustat extraction here
    
    aco_freq_data <- data.table()
    
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds')  
      
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      aco.freq <- as.data.table(aco.data$freq.contour[, 2])
      aco.freq <- as.data.table(t(aco.freq))
      aco_freq_data <- rbind(aco_freq_data, aco.freq)
    }
    
    #naming tc cols 
    aco_freq_labels <- c()
    for (i in 1:length(aco_freq_data)) {
      aco_freq_labels <- append(aco_freq_labels, paste("fc", i, sep = "."))
    }
    
    colnames(aco_freq_data) <- aco_freq_labels
    
    full_data <- cbind(full_data, aco_freq_data)
    
    
    ##### Final acoustat shit
    aco_misc_data <- data.table()
    
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + 3, units = 'seconds')
      aco.data <- acoustat(temp.wave, flim = flim, plot = FALSE)
      misc.data <- as.data.table(aco.data[3:length(aco.data)])
      aco_misc_data <- rbind(aco_misc_data, misc.data)
      
    }
    
    full_data <- cbind(full_data, aco_misc_data)
    
  }
  # Add filepath info
  full_data[,filepath := filepath]
  
  ##Scale
  
  if (scaled == TRUE) {
    
    scale.cols <- colnames(full_data)
    
    scale.cols <- scale.cols[-length(scale.cols)]
    full_data[, (scale.cols) := lapply(.SD, scale), .SDcols = scale.cols]
  }
  
  return(full_data)
}


############ Easy exploration 
pullGoodWave <- function(data, species, det_num, start_buffer = 0, end_buffer = 0, time_buffer = 0 ) {
  pullWave(data[common_name == species][order(-confidence)], det_num, start_buffer = start_buffer, end_buffer = end_buffer, time.buffer = time_buffer)
}