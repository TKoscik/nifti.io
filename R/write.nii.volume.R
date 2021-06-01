write.nii.volume <- function(nii.file, vol.num, values) {
  # Check inputs -------------------------------------------------------------------------
  stopifnot(!missing(nii.file), file.exists(nii.file),
            !missing(vol.num), !missing(values))

  # Get necessary NII file info ----------------------------------------------------------
  dims <- info.nii(nii.file, field="dim")[2:5]
  hdr <- info.nii(nii.file, field=c("datatype", "bitpix", "vox_offset"))

  # Convert value to format from NII file ------------------------------------------------
  if (hdr$datatype %in% c(2,4,8,128,256,512,768,1024,1280)) {
    values <- as.integer(values)
  } else if (hdr$datatype %in% c(16, 32, 64)) {
    values <- as.double(values)
  } else {
    stop("Error converting value format")
  }

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
  writeBin(values, fid, size=hdr$bitpix/8, endian=endian)
  close(fid)
}
