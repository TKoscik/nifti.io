load.cluster <- function(data.nii, data.mask, mask.values="all", verbose=FALSE) {
  
  n.nii <- length(data.nii)
  
  mask <- read.nii.volume(data.mask, 1)
  if (mask.values == "all") {
    mask.values <- sort(unique(as.numeric(mask)))[-1]
  }
  n.masks <- length(mask.values)
  mask.idx <- vector("list", n.masks)
  for (i in 1:length(mask.values)) {
    mask.idx[[i]] <- which(mask == mask.values[i], arr.ind=TRUE)
  }
  
  
  nii <- vector("list", length(mask.vol))
  for (i in 1:n.nii) {
    n.vols <- nii.dims(data.nii[i])[4]
    for (j in 1:n.vols) {
      nii.temp <- read.nii.volume(data.nii[i], j)
      for (k in 1:n.masks) {
        nii[[k]] <- c(nii[[k]], mean(nii.temp[mask.idx[[k]]], na.rm=TRUE))
        if (verbose) {
          print(paste0("Extracting ", k, " of ", n.masks, " ROIs from ", j, " of ", n.vols, " volumes from ", i, " of ", n.nii, " NII files."))
        }
      }
    }
  }
  return(nii)
}
