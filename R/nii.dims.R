nii.dims <- function(nii.file) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  stopifnot(file.exists(nii.file))
  
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian
  hdr.size <- readBin(fid, integer(), size=4, endian=endian)
  seek(fid, where=40, origin="start", size=1)
  img.dims <- readBin(fid, integer(), 8, size=2, endian=endian)
  img.dims <- img.dims[2:5]
  close(fid)
  return(img.dims)
}