load.voxel <- function(data.nii, coords) {
  
  # Parse input ----
  if (is.character(data.nii)) {
    temp.nii <- data.nii
    data.nii <- list()
    if (length(temp.nii) == 1) { # Get filelist from specified directory
      if(!dir.exists(temp.nii)) {
        data.nii[[1]] <-temp.nii
      } else {
        data.nii[[1]] <- list.files(path=temp.nii, pattern="*.nii$", full.names=TRUE)
      }
    } else {
      data.nii[[1]] <- temp.nii
    }
    for (i in 1:length(data.nii[[1]])) { # check if nii files exist
      stopifnot(file.exists(data.nii[[1]][i]))
    }
  } else if (is.list(data.nii)) {
    n.nii <- length(data.nii)
    temp.nii <- data.nii
    data.nii <- vector("list", n.nii)
    for (i in 1:n.nii) {
      if (length(temp.nii[[i]]) == 1) { # Get filelist from specified directory
        if(!dir.exists(temp.nii[[1]])) {
          data.nii[[1]] <- temp.nii[[i]]
        } else {
          data.nii[[1]] <- list.files(path=temp.nii[[i]], pattern="*.nii$", full.names=TRUE)
        }
      } else {
        data.nii[[i]] <- temp.nii[[i]]
      }
      for (j in 1:length(data.nii[[i]])) { # check if nii files exist
        stopifnot(file.exists(data.nii[[i]][j]))
      }
    }
  }
  # ----
  
  n.nii <- length(data.nii)
  nii.temp <- vector("list", n.nii)
  for (i in 1:n.nii) {
    nii.temp[[i]] <- numeric()
    for (j in 1:length(data.nii[[i]])) {
      nii.temp[[i]] <- c(nii.temp[[i]], read.nii.voxel(data.nii[[i]][j], coords = c(coords, Inf)))
    }
  }
  nii <- matrix(unlist(nii.temp), ncol=n.nii)
  if (n.nii == 1) { colnames(nii) <- "nii"
  } else { colnames(nii) <- paste0("nii", 1:n.nii) }
  
  return(nii)
}
