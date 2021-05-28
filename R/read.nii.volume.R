read.nii.volume <- function(nii.file, vol.num) {

  # Check inputs ---------------------------------------------------------------
  stopifnot(!missing(nii.file), file.exists(nii.file))
  stopifnot(!missing(vol.num), is.numeric(vol.num), length(vol.num)==1)

  # Get necessary NII file info ------------------------------------------------
  dims <- info.nii(nii.file, field="dim")[2:5]
  hdr <- info.nii(nii.file, field = c("vox_offset", "datatype", "bitpix"))

  # Check if volume number is in range -----------------------------------------
  if (vol.num > dims[4]) {stop("Volume number exceeds number of volumes")}

  n <- length(dims)
  dimorder <- 1:4
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  coord <- matrix(c(1,1,1, vol.num), nrow=1)
  loc <- as.integer(colSums(t(coord[ , 1:4, drop=FALSE]-1) * cdim) + 1L)

  # Initialize NII file for binary reading -------------------------------------
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian

  seek(fid, where=(hdr$vox_offset + (loc-1) * (hdr$bitpix/8)), origin="start")
  data <- switch(
    as.character(hdr$datatype),
    `2` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, signed=FALSE, endian=endian),
    `4` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `8` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `16` = readBin(fid, double(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `32` = readBin(fid, double(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `64` = readBin(fid, double(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `128` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `256` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `512` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `768` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `1024` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    `1280` = readBin(fid, integer(), prod(dims[1:3]), hdr$bitpix/8, endian=endian),
    stop(paste("Data type", as.character(hdr$datatype), "unsupported in", nii.file)))
  data <- array(data, dim=dims[1:3])

  close(fid)
  return(data)
}
