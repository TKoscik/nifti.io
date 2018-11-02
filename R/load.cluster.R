load.cluster <- function(data.4d, data.mask, mask.vol, verbose=FALSE) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  data.4d <- parse.4d(data.4d)[[1]]
  n.4d <- length(data.4d)
  
  stopifnot(all(mask.vol > 0))
  
  n.masks <- length(mask.vol)
  mask <- vector("list", n.masks)
  for (i in 1:n.masks) {
    mask[[i]] <- read.nii.volume(data.mask, mask.vol[i])
  }
  
  fmri <- vector("list", length(mask.vol))
  for (i in 1:n.4d) {
    n.vols <- nii.dims(data.4d[i])[4]
    for (j in 1:n.vols) {
      fmri.temp <- read.nii.volume(data.4d[i], j)
      for (k in 1:n.masks) {
        fmri[[k]] <- c(fmri[[k]], mean(fmri.temp[mask[[k]]!=0], na.rm=TRUE))
        if (verbose) {
          print(paste0("Extracting ", k, " of ", n.masks, " ROIs from ", j, " of ", n.vols, " volumes from ", i, " of ", n.4d, " 4D files."))
        }
      }
    }
  }
  return(fmri)
}
