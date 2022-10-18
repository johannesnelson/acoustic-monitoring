### These functions interact with monitoR in away that allows you to batch process and generate detection data
### that will also work with the verify_results function in results_verification.R


##### Testing with also keeping peaks frame
batchPatternMatch <- function (dir, temp, results = 'both') {
  
  file.list <- list.files(dir, pattern = ".wav", ignore.case = TRUE)
  file.list <- paste(dir, file.list, sep = '\\')
  all.detections <- data.table()
  all.peaks <- data.table()
  
  for (x in 1:length(file.list)) {
    scores <- corMatch(survey = file.list[x], templates = temp, show.prog = TRUE)
    peaks.obj <- findPeaks(scores)
    filepath.peaks <- rep(file.list[x], times = nrow(peaks.obj@peaks[[1]]))
    temp.peaks <- peaks.obj@peaks[[1]]
    temp.peaks <- cbind(filepath.peaks, temp.peaks)
    setDT(temp.peaks)
    temp.peaks[,':='(start = time, end = time + 3, confidence = score)]
    all.peaks <- rbind(all.peaks, temp.peaks)
    
    detections <- getDetections(peaks.obj)
    filepath <- rep(file.list[x], times = nrow(detections))
    detections <- cbind(filepath, detections)
    setDT(detections)
    detections[,':='(start = time, end = time + 3, confidence = score)]
    all.detections <- rbind(all.detections, detections)
    
  }
  
  colnames(all.detections)[colnames(all.detections) == 'score'] <- 'confidence'
  
  if (results == 'both'){
    return(list(all.detections, all.peaks))
  }else if (results == 'peaks') {
    return(all.peaks)
  }else {
    return(all.detections)
  }
}





