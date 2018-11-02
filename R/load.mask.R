load.mask <- function(data.mask, ...) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  stopifnot(!missing(data.mask))
  
  temp.mask <- data.mask
  which.voxels <- list()
  n.masks <- 0
  for (i in 1:length(temp.mask)) {
    if (dir.exists(temp.mask[i])) {
      fls <- list.files(temp.mask[i], pattern="*.nii$", full.names=TRUE)
      for (j in 1:length(fls)) {
        mask.dims <- nii.dims(fls[j])
        for (k in 1:mask.dims[4]) {
          n.masks <- n.masks + 1
          mask.temp <- read.nii.volume(fls[j], k)
          which.voxels[[n.masks]] <- which(mask.temp != 0, arr.ind=TRUE)
        }
      }
    } else if (file.exists(temp.mask[i])) {
      file.ext <- tail(unlist(strsplit(data.mask, "[.]")), n=1)
      if (file.ext=="nii") {
        mask.dims <- nii.dims(temp.mask[i])
        for (k in 1:mask.dims[4]) {
          n.masks <- n.masks + 1
          mask.temp <- read.nii.volume(temp.mask[i], k)
          which.voxels[[n.masks]] <- which(mask.temp != 0, arr.ind=TRUE)
        }
      } else if (file.ext=="csv") {
        n.masks <- n.masks + 1
        which.voxels[[n.masks]] <- read.csv(temp.mask[i], header=FALSE)
      } else {
        stop(sprintf("Cannot use file of %s for mask", file.ext))
      }
    } else if (is.null(temp.mask[i])) {
      n.masks <- n.masks + 1
      mask.dims <- nii.dims(data.4d[1])
      which.voxels[[n.masks]] <- which((array(1, dim=mask.dims[1:3])) != 0, arr.ind=TRUE)
    } else {
      stop("Cannot parse mask input, check to make sure directories and files exist.")
    }
  }
  
  # close(fid)
  return(which.voxels)
}