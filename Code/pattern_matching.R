### These functions interact with monitoR in away that allows you to batch process and generate detection data
### that will also work with the verify_results function in results_verification.R


# Testing with also keeping peaks frame. 'temp' is a monitoR object of the class corTemplateList. This should be read into your environment. 
# score.cutoff will allow you to set the score cutoff for the template prior to processing. This is helpful if you will look only at detections.
# results argument lets you specify if you want the peaks data.table or the detections data.table. Peaks is more informative and allows you to filter
# all peaks by score (effectively creating detections anyway). Detections will only show peaks that are above the cutoff.

batchPatternMatch <- function (dir, temp, score.cutoff = 'default', results = 'both') {
  
  file.list <- list.files(dir, pattern = ".wav", ignore.case = TRUE)
  file.list <- paste(dir, file.list, sep = '\\')
  all.detections <- data.table()
  all.peaks <- data.table()
  
  if (!score.cutoff == 'default') {
    templateCutoff(temp) <- c(default = score.cutoff)
  }
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

