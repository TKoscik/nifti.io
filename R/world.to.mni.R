world.to.mni <- function(coords, tform.xyz=NULL, nii.file=NULL) {
  
  if (is.null(tform.xyz)) {
    if (is.null(nii.file)) {
      stop("Affine transform must be provided or read from nifti file")
    } else {
      tform <- nii.hdr(nii.file, c("srow_x", "srow_y", "srow_z"))
      tform <- rbind(tform$srow_x, tform$srow_y, tform$srow_z, c(0,0,0,1))
    }
  } else {
    stopifnot(nrow(tform.xyz) == 3)
    stopifnot(ncol(tform.xyz) == 4)
    tform <- rbind(tform.xyz, c(0,0,0,1))
  }
  n.coords <- nrow(coords)
  mni.coords <- t(tform %*% rbind(coords[ ,1]-1,
                                  coords[ ,2]-1,
                                  coords[ ,3]-1,
                                  rep(1,n.coords)))[ ,-4]
  
  return(mni.coords)
}
