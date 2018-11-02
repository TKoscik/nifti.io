load.voxel <- function(data.4d, coords) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  # add check if data.4d is list or character vector
  data.4d <- parse.4d(data.4d)
  
  n.4d <- length(data.4d)
  fmri.temp <- vector("list", n.4d)
  for (i in 1:n.4d) {
    fmri.temp[[i]] <- numeric()
    for (j in 1:length(data.4d[[i]])) {
      fmri.temp[[i]] <- c(fmri.temp[[i]],
                          read.nii.voxel(data.4d[[i]][j], coords = c(coords, Inf)))
    }
  }
  fmri <- matrix(unlist(fmri.temp), ncol=n.4d)
  if (n.4d == 1) { colnames(fmri) <- "fmri"
  } else { colnames(fmri) <- paste0("fmri", 1:n.4d) }
  
  return(fmri)
}
