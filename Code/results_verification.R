#### These functions can help handle, visualize, and validate results from BirdNET, but will work with any
#### results formatted with the following columns:

####        -filepath: character string path to audio file
####        -start: start time in seconds of detection
####        -end: end time in seconds from detection

library(NSNSDAcoustics)
library(tidyverse)
library(RSQLite)
library(AMMonitor)
library(seewave)
library(data.table)




# This function allows easy visual scanning of results with options to play, save wave files, write notes, etc. Be sure either to 
# assign the result to an object or to choose the save as csv option at the end. If you 'esc' mid verification, you will lose
# your progress. It is the primary tool I use to go through detection results. As long as the dataframe has filepaths that correspond
# to the audio directory, start and end times, it should work. If doing multiple species detections, defining a common_name column with
# species names will be necessary. 

verify_results <- function (data, species = "all", conf = 0, temp.length = 'none', template.mode = FALSE) {
  
  setDT(data)
  if(!species == "all") {
    data <- data[common_name == species,]
    
  }
  data <- data[confidence >= conf,]
  
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a", "t")
  
 if (template.mode == TRUE) {
   template_DT <- data.table()
 }
  if(!"verification" %in% names(data)){   #Adds verification column
    data[,verification := NA]
  }
  
  if(!"notes" %in% names(data)){         #Adds notes column
    data[,notes := NA]
  }
  
  if(!"confidence" %in% names(data)){         #Adds confidence column with 0s. Only really relevant if using non-birdNET detections (e.g. monitoR, or a specific ML model)
    data[,confidence := 0]
  }
  
  # Skip over observations where verification is already defined
  
  for (i in 1:nrow(data)) {
    if(!is.na(data$verification[i])){
      cat(paste("\n Verification for", basename(data$filepath[i]), "at", data$start[i], "seconds already exists. Moving onto next detection...\n"))
      data$verification[i] <- data$verification[i]
      next}
    
    # Begin main repeat loop where spectrograms are shown with options for user to input into console
    
    repeat{
      
      wave.obj <- readWave(data$filepath[i], from = data$start[i], to = data$end[i], units = 'seconds')
      #spectro( wave = wave.obj )
      viewSpec(data$filepath[i], start.time = data$start[i]-1, page.length = 5, units = 'seconds'  )
      cat(paste("\n Showing detection", i, "out of", nrow(data), "from", basename(data$filepath[i]), "at", data$start[i], "seconds. Confidence:", data$confidence[i], "\n"))
      
      cat(paste( "Enter \n 'y' for yes,\n",  
                 "'n' for no,\n",
                 "'r' for review,\n",
                 "'p' to play audio segment,\n", 
                 "'w' to write segment as wav file to working directory,\n",
                 "'s' to skip to next segment (and log as NA)",
                 "'a' to add a note \n",
                 "'q' for quit."))
      
      answer <- readline( prompt = paste0(paste("Is this a(n)", data$common_name[i]), "?")) 
      
      
      if(answer %in% verif.options) break
      
      # Option to play sound
      if(answer == "p") {
        tempwave <- readWave(data$filepath[i], from = data$start[i] - 1, to = data$end[i] + 1, units = "seconds")
        play(tempwave)
      }
      # Option to write sound to wav file in working directory
      if(answer == "w") {
        filename <- paste0(paste(gsub(pattern = ".WAV", "", basename(data$filepath[i])), data$start[i], sep = "_"), ".WAV")
        tempwave <- readWave(data$filepath[i], from = data$start[i] - 1, to = data$end[i] + 1, units = "seconds")
        writeWave(tempwave, filename)
        cat("\n Writing wav file to working directory...")
      }
      
      # Option to add a note in the notes column
      if(answer == "a") {
        
        note <- readline(prompt = "Add note here: ")
        data$notes[i] <- note
      }
      
      # This 'template mode' section is still in development. It allows you to click on the center of a detection and specify a temp.length. 
      # When quitting out of function, it will save a template_DT which is effectively a new set of detections, but with
      # a smaller time window (end - start) will be equal to temp.length and centered on the actual detection. The reason
      # for this is when extracting features to fit ML models, the 3 seconds birdNET window is often too large and has 
      # a lot of unwanted information. This function should allow you to use high confidnce birdNET detections to create
      # a stronger training set for ML purposes. As it is, it is finnicky and the visualization/locator() selection 
      # piece could be refined. 
      
      if(answer == "t") {
        if(template.mode == FALSE) {
          template_DT <- data.table()
        }
        repeat {
          wave.obj.2 <- readWave(data$filepath[i], from = data$start[i] - 1, to = data$end[i] + 1, units = 'seconds')
          tempSpec <- spectro(wave.obj.2, fastdisp = TRUE)
          t.bins <- tempSpec$time
          n.t.bins <- length(t.bins)
          which.t.bins <- 1:n.t.bins
          which.frq.bins <- which(tempSpec$freq >= 0)
          frq.bins <- tempSpec$freq
          amp <- round(tempSpec$amp[which.frq.bins, ], 2)
          n.frq.bins <- length(frq.bins)
          ref.matrix <- matrix(0, nrow = n.frq.bins, ncol = n.t.bins)
          
          
          if (temp.length == 'none') {
            t.value <- as.numeric(readline("How many seconds long would you like the templates to be?"))
          }else {
            t.value <- temp.length
          }
          # f.min <- as.numeric(readline("What would you like the minimum frequency to be in Hz?"))
          # f.max <- as.numeric(readline("What would you like the maximum frequency to be in Hz?"))
          cat("Click the plot where you would like to center this template")
          ctr.pt <- locator(n = 1)
          
          temp.DT <- data.table(filepath = data[i, filepath], 
                                common_name = data[i, common_name], 
                                start = (data[i, start] -1) + ctr.pt$x - (t.value/2), 
                                end = (data[i, start] -1) + ctr.pt$x +(t.value/2),
                                center.freq = ctr.pt$y
                                # frq.min = f.min, 
                                # frq.max = f.max)
          )
          template_DT <-  rbind(template_DT, temp.DT)
          
          # image(ref.matrix, 
          #     xlim = c(ctr.pt$x - (.5*t.value), ctr.pt$x + (.5*t.value)),
          #   ylim = c(f.min, f.max),
          #   col = 'orange',
          #   add = TRUE)
          {break}
        }
        dev.off()
      }
      
      
      if(!answer %in% all.options){
        cat("\n Response not recognized, please input correct response...\n")
      }
      
    }
    
    # add verification character to verification column
    if(answer %in% c("y", "n", "r")) {
      cat("\n Adding result to verification data...\n ")
      data$verification[i] <- answer
    }
    # skip observation (leave as NA)
    if(answer == "s") {
      data$verification[i] <- NA
      cat("Skipping to next detection...")
    }
    
    # quitting will lead to csv saving options
    if(answer == "q") {
      
      data$verification[i:nrow(data)] <- data$verification[i:nrow(data)]
  
      break}
    
  }
  saveask <- readline(prompt = "Would you like to save results as a csv file? \n Input 'y' for yes:")
  if(saveask == "y") {
    fname <- readline(prompt = "What would you like to name the file?")
    
    write.csv(data, paste0(fname, ".csv"), row.names = FALSE)
  }
  
  saveask2 <- readline(prompt = "Would you like to save the template data to the environment? \n Input 'y' for yes:")
  if(saveask2 == 'y'){
    template_DT <<-template_DT
  }
  return(data)
  
} 




# This will name all files in a directory (usually SD card) according to the arguments of locID and eqID 
# so that "locationID_deviceID_YYYYMMDD_HHMMSS.WAV" becomes the naming convention. This will help a great
# deal with organization and later analysis, since each filename is encoded with a lot of crucial information


nameFiles <- function(directory, new_directory, locID, eqID) {
  
  file.vector <- list.files(directory) 
  current.dir <- getwd()
  setwd(directory)
  for (oldname in file.vector) {
    file.rename(from = oldname, to = paste(locID, eqID, basename(oldname), sep = '_'))
  }
  new.file.vector <- list.files()
  file.copy(new.file.vector, new_directory)  #note, the original files are not deleted so that you can verify successful copying before wiping sd card clean
  setwd(current.dir)
}


# This just adds the 'write' step to the NDSA acoustics function which gathers all BirdNET csv's into a single data.table

gather_write <- function(directory, filename) {
  all.results <- birdnet_gather(results.directory = directory, formatted = FALSE)
  write.csv(all.results, paste(directory, filename, sep = '/'), row.names = FALSE)
  return(all.results)
}


# The NSNSDA function birdnet_plot() requires certain columns to exist. This will create them. 
# User defines timezone in tz.

plot_prep <- function(data, tz) {
  data[, ':=' (timezone = tz, verify = NA, recordingID = basename(filepath))]
  return(data)
}


# Functions for adding columns for location, date, time. This assumes the filename convention "locationID_deviceID_YYYYMMDD_HHMMSS.WAV"
# see function nameFiles for an easy way to batch name all audio files as you download them of SDcards. These take along time to run
# on large data.tables, but you only need to run them once and save to csv or SQLite table, etc.

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

# Add time based on time info in filename (time gets converted to character here)
add.time <- function(x) {
  
  newframe <- mutate(x, locationIDvector = strsplit(basename(x$filepath), split = '_'))
  
  timevector <- c()
  
  for (i in seq(newframe$locationIDvector)){
    timevector <- append(timevector, gsub(".WAV","", newframe$locationIDvector[[i]][c(4)]))
    if(length(timevector) == length(newframe$locationIDvector)) {break}
  }
  
  newframe <-  mutate(x, time = timevector)
}


######## Some summarizing functions

# This function creates a table of all species detected, the number of detections, the max, min, mean, and sd of the confidence value,
# as well as a 'high confidence mean' which takes n observations (where n is defined by the numobs argument) with the greatest confidence
# score for each species and gives the mean. Since there are often so many low confidence detections, this pulls the mean confidence
# way down and doesn't always give an accurate idea of probability of presence just at a glance. 

species_count <- function(data, num.obs = 20){
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


# This filters the data to show only observations where the mean confidence of the highest confidence values for a certain number (num.obs)
# of species detections

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

#### Write BirdNET detections to ordered wave files that should correspond to spectrogram order (created with ordered_plot) if you want to visually scan as well.

makeWaves <- function(x, dir) {
  dir.create(dir, recursive = TRUE)
  dir.fp <- dir
  for (i in 1:nrow(x)){
    wave <- readWave(x$filepath[i], from = x$start[i]-1, to = x$end[i]+1, units = 'seconds')
    writeWave(wave, filename = paste(dir.fp, paste(i, basename(x$filepath[i]), x$start[i], ".wav",  sep = '_'), sep = "/"))
    
  }
  
}

### This is only a slightly modified birdnet_gather that removes the key() arugment for ordering/ scanning purposes
ordered_plot <- function (data, audio.directory, title, frq.lim = c(0, 12), new.window = TRUE, 
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


# Simple function that allows you to filter all results by a confidence threshold and a count cutoff. Saves some time.
filter.results <- function(dataframe, conf.level, count.cutoff) {
  
  dataframe %>% 
    group_by(common_name) %>% 
    filter(confidence > conf.level) %>% 
    filter(n() > count.cutoff)
}





# Function to pull out, visualize, and listen to a detection by row index number time_buffer adds a pad of seconds on either side, start and end
# buffers scooch the window one way or the other. Easy for quick visualizations/listens and also easier to call readWAve without having to specify
# start and end each time. 

pullWave <- function (data, det_num, time.buffer = 0, start_buffer = 0, end_buffer = 0, listen = TRUE) {
  
  tempwave <- readWave(filename = data$filepath[det_num], 
                       from = data$start[det_num] - (time.buffer + start_buffer), 
                       to = data$end[det_num] + (time.buffer+ end_buffer), 
                       units = 'seconds')
  
  viewSpec(clip = data$filepath[det_num],
           start.time = data$start[det_num] - (time.buffer + start_buffer),
           page.length = ((data$end[det_num] + (time.buffer + end_buffer) - (data$start[det_num] - (time.buffer + start_buffer)))),
           units = 'seconds')
  cat("Birdnet confidence is", data$confidence[det_num])
  if (listen == TRUE){
    play(tempwave)}
  
  return(tempwave)
}

# This just modifies pullWave a bit to order the results by confidence so that you can see a 'prime' example. Your det_num in this case
# will be ordinal, meaning 1 will pull the highest confidence example for that species. It is not necessary, but I have wanted to pull good examples
# for visualizations in presentations enough times that it is worth including for me. 

pullGoodWave <- function(data, species, det_num, start_buffer = 0, end_buffer = 0, time_buffer = 0 ) {
  pullWave(data[common_name == species][order(-confidence)], det_num, start_buffer = start_buffer, end_buffer = end_buffer, time.buffer = time_buffer)
}

# I can't remember why I thought I needed this, but I am leaving it here until I do

batch_load <- function (dir, pattern = NULL) {
  
  filenames <- list.files(dir, pattern = pattern )
  filenames <- paste(dir, filenames, sep = '\\')
  mergeddata <- vroom(filenames)
  return(mergeddata)
}


### This function allows some basic annotation by letting you page through a recording with a specified page length, 
### labeling a verification column with either 'y' or 'n' (or some other options), indicating presence or absence
### of a target species. An option 'a' allows to annotate a notes column with a character string

annotate_recording <- function (file_path, past_annos = NULL, page_length, template_mode = FALSE, temp_length = 3, species = NULL) {
  
  wave_obj <- readWave(file_path)
  wave_dur <- length(wave_obj@left)/wave_obj@samp.rate
  wave_seq <- seq(0, wave_dur, page_length)
  
  if(is.null(past_annos)){
  annotationDT <- data.table(filepath = file_path, common_name = species, start = wave_seq, end = wave_seq + page_length, verification = NA, notes = NA)
  }
  
  if(!is.null(past_annos)){
    annotationDT <- past_annos
  }
  
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a", "t")
  if (template_mode == TRUE) {
    template_DT <- data.table()
  }
  
  # Skip over observations where verification is already defined
  
  for (i in 1:nrow(annotationDT)) {
    if(!is.na(annotationDT$verification[i])){
      cat(paste("\n Verification for", basename(annotationDT$filepath[i]), "at", annotationDT$start[i], "seconds already exists. Moving onto next detection...\n"))
      annotationDT$verification[i] <- annotationDT$verification[i]
      next}
    
    # Begin main repeat loop where spectrograms are shown with options for user to input into console
    
    repeat{
      
      viewSpec(annotationDT$filepath[i], start.time = annotationDT$start[i]-1, page.length = page_length, units = 'seconds'  )
      cat(paste("\n Showing detection", i, "out of", nrow(annotationDT), "from", basename(annotationDT$filepath[i]), "at", annotationDT$start[i], "seconds. Confidence:", annotationDT$confidence[i], "\n"))
      
      cat(paste( "Enter \n 'y' for yes,\n",  
                 "'n' for no,\n",
                 "'r' for review,\n",
                 "'p' to play audio segment,\n", 
                 "'w' to write segment as wav file to working directory,\n",
                 "'s' to skip to next segment (and log as NA)",
                 "'a' to add a note \n",
                 "'q' for quit."))
      
      answer <- readline( prompt = paste0(paste("Is the target species present?")))
      
      
      if(answer %in% verif.options) break
      
      # Option to play sound
      if(answer == "p") {
        tempwave <- readWave(annotationDT$filepath[i], from = annotationDT$start[i], to = annotationDT$end[i], units = "seconds")
        play(tempwave)
      }
      # Option to write sound to wav file in working directory
      if(answer == "w") {
        filename <- paste0(paste(gsub(pattern = ".WAV", "", basename(annotationDT$filepath[i])), annotationDT$start[i], sep = "_"), ".WAV")
        tempwave <- readWave(annotationDT$filepath[i], from = annotationDT$start[i] - 1, to = annotationDT$end[i] + 1, units = "seconds")
        writeWave(tempwave, filename)
        cat("\n Writing wav file to working directory...")
      }
      
      # Option to add a note in the notes column
      if(answer == "a") {
        
        note <- readline(prompt = "Add note here: ")
        annotationDT$notes[i] <- note
      }
      
      # This 'template mode' section is still in development. It allows you to click on the center of a detection and specify a temp.length. 
      # When quitting out of function, it will save a template_DT which is effectively a new set of detections, but with
      # a smaller time window (end - start) will be equal to temp.length and centered on the actual detection. The reason
      # for this is when extracting features to fit ML models, the 3 seconds birdNET window is often too large and has 
      # a lot of unwanted information. This function should allow you to use high confidnce birdNET detections to create
      # a stronger training set for ML purposes. As it is, it is finnicky and the visualization/locator() selection 
      # piece could be refined. 
      
      if(answer == "t") {
        if(template_mode == FALSE) {
          template_DT <- data.table()
        }
        repeat {
          wave.obj.2 <- readWave(annotationDT$filepath[i], from = annotationDT$start[i] - 1, to = annotationDT$end[i] + 1, units = 'seconds')
          tempSpec <- spectro(wave.obj.2, fastdisp = TRUE)
          t.bins <- tempSpec$time
          n.t.bins <- length(t.bins)
          which.t.bins <- 1:n.t.bins
          which.frq.bins <- which(tempSpec$freq >= 0)
          frq.bins <- tempSpec$freq
          amp <- round(tempSpec$amp[which.frq.bins, ], 2)
          n.frq.bins <- length(frq.bins)
          ref.matrix <- matrix(0, nrow = n.frq.bins, ncol = n.t.bins)
          
          
          if (temp_length == 'none') {
            t.value <- as.numeric(readline("How many seconds long would you like the templates to be?"))
          }else {
            t.value <- temp_length
          }
          # f.min <- as.numeric(readline("What would you like the minimum frequency to be in Hz?"))
          # f.max <- as.numeric(readline("What would you like the maximum frequency to be in Hz?"))
          cat("Click the plot where you would like to center this template")
          ctr.pt <- locator(n = 1)
          
          temp.DT <- data.table(filepath = annotationDT[i, filepath], 
                                common_name = annotationDT[i, common_name], 
                                start = (annotationDT[i, start] -1) + ctr.pt$x - (t.value/2), 
                                end = (annotationDT[i, start] -1) + ctr.pt$x +(t.value/2),
                                center.freq = ctr.pt$y
                                # frq.min = f.min, 
                                # frq.max = f.max)
          )
          template_DT <-  rbind(template_DT, temp.DT)
          
          # image(ref.matrix, 
          #     xlim = c(ctr.pt$x - (.5*t.value), ctr.pt$x + (.5*t.value)),
          #   ylim = c(f.min, f.max),
          #   col = 'orange',
          #   add = TRUE)
          {break}
        }
        dev.off()
      }
      
      
      if(!answer %in% all.options){
        cat("\n Response not recognized, please input correct response...\n")
      }
      
    }
    
    # add verification character to verification column
    if(answer %in% c("y", "n", "r")) {
      cat("\n Adding result to verification data...\n ")
      annotationDT$verification[i] <- answer
    }
    # skip observation (leave as NA)
    if(answer == "s") {
      annotationDT$verification[i] <- NA
      cat("Skipping to next detection...")
    }
    
    # quitting will lead to csv saving options
    if(answer == "q") {
      
      annotationDT$verification[i:nrow(annotationDT)] <- annotationDT$verification[i:nrow(annotationDT)]
      
      break}
    
  }
  saveask <- readline(prompt = "Would you like to save results as a csv file? \n Input 'y' for yes:")
  if(saveask == "y") {
    fname <- readline(prompt = "What would you like to name the file?")
    
    write.csv(annotationDT, paste0(fname, ".csv"), row.names = FALSE)
  }
  
  saveask2 <- readline(prompt = "Would you like to save the template data to the environment? \n Input 'y' for yes:")
  if(saveask2 == 'y'){
    csv_name <- readline(prompt = 'What would you like to name it?')
    write.csv(template_DT, paste0(csv_name, ".csv"), row.names = FALSE)
  }
  return(annotationDT)
  
} 

