get.voxel.sphere <- function(sphereRadius, voxelSz, center, volumeDims) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  radius <- round(sphereRadius / voxelSz)
  actualRadius <- voxelSz[1] * radius[1] 
  
  x <- c(seq(from=-1*actualRadius, to=-1*voxelSz[1], by=voxelSz[1]), seq(from=0, to=actualRadius, by=voxelSz[1]))
  y <- c(seq(from=-1*actualRadius, to=-1*voxelSz[2], by=voxelSz[2]), seq(from=0, to=actualRadius, by=voxelSz[2]))
  z <- c(seq(from=-1*actualRadius, to=-1*voxelSz[3], by=voxelSz[3]), seq(from=0, to=actualRadius, by=voxelSz[3]))
  
  roi <- matrix(0, nrow=0, ncol=3)
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      for (k in 1:length(z)) {
        if (as.numeric(((x[i]^2+y[j]^2+z[k]^2) ^ 0.5) <= sphereRadius)) {
          roi <- rbind(roi, c((center[1] + x[i]/voxelSz[1]), (center[2] + y[j]/voxelSz[2]), (center[3] + z[k]/voxelSz[3])))
        }
      }
    }
  }
  roi[which(roi < 0)] <- NA
  roi <- na.omit(roi)
  return(roi)
  
}