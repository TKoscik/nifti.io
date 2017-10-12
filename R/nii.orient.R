nii.orient <- function(nii.file) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  stopifnot(file.exists(nii.file))
  
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian
  hdr.size <- readBin(fid, integer(), size=4, endian=endian)
  
  nim <- list()
  seek(fid, where=252, origin="start", size=1)
  nim$qform_code <- readBin(fid, integer(), size = 2, endian = endian)
  nim$sform_code <- readBin(fid, integer(), size = 2, endian = endian)
  nim$quatern_b <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$quatern_c <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$quatern_d <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$qoffset_x <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$qoffset_y <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$qoffset_z <- readBin(fid, numeric(), size = 4, endian = endian)
  nim$srow_x <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  nim$srow_y <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  nim$srow_z <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  close(fid)
  return(nim)
}