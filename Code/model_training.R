### THIS IS NOT A WORKSPACE, THESE FUNCTIONS ARE THE FINALIZED FORM OF WHAT WAS DEVELOPED IN SCRATCHPAD AND SERVE TO ASSIST SUPERVISED ML 



# This functions will extract acoustic features from a df based on filepath, start, and end time. 
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


## This function enables the raw extraction of a full audio file. This does it in 3-second chunks to match with BirdNET

totalExtract <- function(filepath, flim = NULL, amp.data = TRUE, zcr.data = TRUE, acoustat.data = TRUE, mfcc = TRUE, scaled = FALSE, temp.length = 3) {
  fullwave <- readWave(filepath)          
  wav.dur <- length(fullwave@left)/fullwave@samp.rate
  wave.seq <- seq(0, wav.dur, temp.length)
  full_data <- data.table()
  
  if (amp.data == TRUE) {
    for (x in wave.seq[-length(wave.seq)]) {
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds')
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
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds')    
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
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds') 
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
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds')  
      
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
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds')  
      
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
      temp.wave <- readWave(filepath, from = x, to = x + temp.length, units = 'seconds')
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


###########################

## This is a modified version of my 'verify' function, which allows for template creation. 
## In the end, one main function with all funcitonality should be decided upon
verify.results.template <- function (x, species = "all", conf = 0, temp.length = 'none') {
  
  if(species == "all") {
    
  }
  
  if(!species == "all") {
    x <-  filter(x, common_name == species)
    
  }
  x <- filter(x, confidence > conf)
  
  template_DT <- data.table()
  verifs <- c()
  verif.options <- c("y", "n", "r", "q", "s")
  all.options <- c("y", "n", "r", "q", "p", "s", "w", "a", "t")
  
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
      
      if(answer == "t") {
        repeat {
          wave.obj.2 <- readWave(x$filepath[i], from = x$start[i] - 1, to = x$end[i] + 1, units = 'seconds')
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
          
          temp.DT <- data.table(filepath = x[i, filepath], 
                                common_name = x[i, common_name], 
                                start = (x[i, start] -1) + ctr.pt$x - (t.value/2), 
                                end = (x[i, start] -1) + ctr.pt$x +(t.value/2),
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
      template_DT <<-template_DT
      break}
    
  }
  saveask <- readline(prompt = "Would you like to save results as a csv file? \n Input 'y' for yes:")
  if(saveask == "y") {
    fname <- readline(prompt = "What would you like to name the file?")
    template_DT <<- template_DT
    write.csv(x, paste0(fname, ".csv"), row.names = FALSE)
  }
  return(x)
  
} 
