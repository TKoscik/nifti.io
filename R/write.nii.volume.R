write.nii.volume <- function(nii.file, vol.num, values) {
  # Check inputs -------------------------------------------------------------------------
  stopifnot(!missing(nii.file), file.exists(nii.file),
            !missing(vol.num), !missing(values))

  # Get necessary NII file info ----------------------------------------------------------
  dims <- info.nii(nii.file, field="dim")[2:5]
  hdr <- info.nii(nii.file, field=c("datatype", "bitpix", "vox_offset"))

  # Convert value to format from NII file ------------------------------------------------
  values <- switch(as.character(hdr$datatype),
                  `2` = as.integer(values),
                  `4` = as.integer(values),
                  `8` = as.integer(values),
                  `16` = as.double(values),
                  `32` = as.double(values),
                  `64` = as.double(values),
                  `128` = as.integer(values),
                  `256` = as.integer(values),
                  `512` = as.integer(values),
                  `768` = as.integer(values),
                  `1024` = as.integer(values),                   
                  `1280` = as.integer(values),
                  stop("Error converting value format"))

  # Calculate write location -------------------------------------------------------------
  n <- length(dims)
  dimorder <- 1:n
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  coord <- matrix(c(1,1,1, vol.num), nrow=1)
  loc <- as.integer(colSums(t(coord[ , 1:n, drop=FALSE]-1) * cdim) + 1L)

  # Initialize NII file for binary reading and writing -----------------------------------
  fid <- file(nii.file, "r+b")
  endian <- .Platform$endian

  seek(fid, where=(hdr$vox_offset + (loc-1) * (hdr$bitpix/8)), origin="start", rw="write")

  # Write value to file ------------------------------------------------------------------
  #switch(as.character(hdr$datatype),
  #       `2` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `4` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `8` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `16` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `32` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `64` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `128` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `256` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `512` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `768` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `1024` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       `1280` = writeBin(values, fid, size=hdr$bitpix/8, endian=endian),
  #       stop("Error writing to file."))
  writeBin(values, fid, size=hdr$bitpix/8, endian=endian)
  close(fid)
}
