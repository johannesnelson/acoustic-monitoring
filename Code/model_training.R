library(data.table)
library(tuneR)
library(seewave)


## All of these functions are in development. The goal is to have an easy way to do feature extraction, model training, and predicting
## in order to be able to create ML classifiers that might perform better than birdNET on certain species (e.g. rails, owls)



# This functions will extract acoustic features from a dataframe based on filepath, start, and end time. amp.data is raw amplitude data
# and takes a long time to run. It also may be unnecessary. zcr.data will extrract zero crossing rate info using seewave's zcr() function,
# acoustat data uses seewave's acoustat function, mfcc gives the mel frequency ceptral coefficients. 

#flim defines frequncy limits and takes a vector with two values -- c(lower limit, upperlimit) in kHz. e.g. c(2, 6)
#scaled is set to FALSE by default, but most often will probably want to be set to TRUE to scale the features

#Be sure to assign it to an object when calling. 
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


## This function enables the raw extraction of a full audio file. This does it in 3-second chunks to match with BirdNET, but the temp.length
## can be changed to match a classifier created with a different sized timewindow (using verify_results in template mode)

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


##################

#Takes a filepath to a wav file, breaks it into temp.length sized chunks and extracts specified features in order to be then analyzed my 
# a classifier that used the same temp.length and features when it was created. I would lik to find a way to store temp.lengths and features
# in the model object to not have to remember or keep records elsewhere. 


predict_format <- function (model, file.path, flim = NULL, amp.data = TRUE, zcr.data = TRUE, acoustat.data = TRUE, mfcc = TRUE, scaled = TRUE, temp.length = 3) {
  
  results_df <- data.table()
  features_df <- totalExtract(filepath = file.path, flim = flim, amp.data = amp.data, zcr.data = zcr.data, acoustat.data = acoustat.data, mfcc = mfcc, scaled = scaled, temp.length = temp.length)
  
  if (class(model) == 'train') {
    prediction_df <- predict(model, features_df, type = 'prob')
    setDT(prediction_df)
    prediction_df[,start := seq(0, nrow(prediction_df) * temp.length - 1, temp.length)]
    prediction_df[,end := start + temp.length]
    prediction_df[,filepath := file.path]
    prediction_df[,confidence := y]
    
  } else {
    prediction_df <- predict(model, features_df, type = 'raw')
    setDT(prediction_df)
    prediction_df[,start := seq(0, nrow(prediction_df) * temp.length - 1, temp.length)]
    prediction_df[,end := start + temp.length]
    prediction_df[,filepath := file.path]
    prediction_df[,confidence := y]
  }
  
  return(prediction_df)
  
}


### predict results using a model and format them with timestamps, filepaths, etc.

predict_results <- function(model, features_df, temp.length) {
  
  if (class(model) == 'train') {
    prediction_df <- predict(model, features_df, type = 'prob')
    setDT(prediction_df)
    prediction_df[,start := seq(0, nrow(prediction_df) * temp.length - 1, temp.length)]
    prediction_df[,end := start + temp.length]
    prediction_df[,filepath := features_df[,filepath]]
    
  } else {
    prediction_df <- predict(model, features_df, type = 'raw')
    setDT(prediction_df)
    prediction_df[,start := seq(0, nrow(prediction_df) * temp.length - 1, temp.length)]
    prediction_df[,end := start + temp.length]
    prediction_df[,filepath := features_df[,filepath]]
  }
  
  return(prediction_df)
  
}
