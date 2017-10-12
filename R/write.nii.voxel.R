write.nii.voxel <- function(nii.file, coords, value) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  # Check inputs ---------------------------------------------------------------
  stopifnot(!missing(nii.file), file.exists(nii.file),
            !missing(coords), !missing(value))
  
  # Get necessary NII file info ------------------------------------------------
  dims <- nii.dims(nii.file)
  hdr <- nii.hdr(nii.file, field = c("vox_offset", "datatype", "bitpix"))
  
  # Check if coordinates are in range ------------------------------------------
  for (i in 1:length(coords)) {
    stopifnot(coords[i] <= dims[i])
  }
  
  # Convert value to format from NII file --------------------------------------
  value <- switch(as.character(hdr$datatype),
                  `2` = as.integer(value), 
                  `4` = as.integer(value), 
                  `8` = as.integer(value), 
                  `16` = as.double(value),
                  `64` = as.double(value), 
                  stop("Error converting value format"))
  
  # Calculate write location ---------------------------------------------------
  n <- length(coords)
  dimorder <- 1:n
  coords <- matrix(coords, nrow=1, ncol=length(coords))
  coords <- coords
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  loc <- as.integer(colSums(t(coords[ , , drop=FALSE]-1) * cdim) + 1L)
  
  # Initialize NII file for binary reading and writing -------------------------
  fid <- file(nii.file, "r+b")
  endian <- .Platform$endian
  
  # Write value to file --------------------------------------------------------
  seek(fid,
       where=(hdr$vox_offset + (loc-1) * (hdr$bitpix/8)),
       origin="start",
       rw="write")
  
  # Write Value ----------------------------------------------------------------
  switch(as.character(hdr$datatype),
         `2` = writeBin(value, fid, size=hdr$bitpix/8, endian=endian),
         `4` = writeBin(value, fid, size=hdr$bitpix/8, endian=endian),
         `8` = writeBin(value, fid, size=hdr$bitpix/8, endian=endian),
         `16` = writeBin(value, fid, size=hdr$bitpix/8, endian=endian),
         `64` = writeBin(value, fid, size=hdr$bitpix/8, endian=endian),
         stop("Error writing to file."))
  
  close(fid)
}