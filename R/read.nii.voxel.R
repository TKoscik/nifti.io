read.nii.voxel <- function(nii.file, coords) {

  # Check inputs -----------------------------------------------------------------
  stopifnot(!missing(nii.file), file.exists(nii.file),
            !missing(coords), any(length(coords)==3, length(coords==4)))

  # Get necessary NII file info --------------------------------------------------
  dims <- info.nii(nii.file, field = "dim")[2:5]
  hdr <- info.nii(nii.file, field = c("vox_offset", "datatype", "bitpix"))

  # Check if coordinates are in range
  for (i in 1:length(coords)) {
    if (i < 4) {
      stopifnot(coords[i] <= dims[i])
    } else if (!is.infinite(coords[i])) {
      stopifnot(coords[i] <= dims[i])
    }
  }

  # Initialize NII file for binary reading -------------------------------------
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian

  # Get data -------------------------------------------------------------------
  n <- length(dims)
  dimorder <- 1:4
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  if (length(coords) == 4 & is.infinite(coords[4])) {
    t.count <- 1:dims[4]
  } else if (length(coords) == 4) {
    t.count <- coords[4]
  } else {
    t.count <- 1
  }
  data <- numeric(length(t.count))
  counter <- 0
  for (i in t.count) {
    counter <- counter + 1
    coord <- matrix(c(coords[1:3],i), nrow=1)
    loc <- as.integer(colSums(t(coord[ ,1:4, drop=FALSE]-1) * cdim) + 1L)
    seek(fid, where=(hdr$vox_offset + (loc-1) * (hdr$bitpix/8)), origin="start")
    data[counter] <- switch(
      as.character(hdr$datatype),
      `2` = readBin(fid, integer(), 1, hdr$bitpix/8, signed=FALSE, endian=endian),
      `4` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `8` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `16` = readBin(fid, double(), 1, hdr$bitpix/8, endian=endian),
      `32` = readBin(fid, double(), 1, hdr$bitpix/8, endian=endian),
      `64` = readBin(fid, double(), 1, hdr$bitpix/8, endian=endian),
      `128` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `256` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `512` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `768` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `1024` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      `1280` = readBin(fid, integer(), 1, hdr$bitpix/8, endian=endian),
      stop(paste("Data type", hdr$datatype, "unsupported in", nii.file)))
  }
  close(fid)
  return(data)
}
