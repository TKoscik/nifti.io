init.nii <- function(new.nii, ref.nii=NULL, dims, pixdim=NULL, orient=NULL, datatype=16, init.value=NA) {
  fid <- file(new.nii, "w+b")
  
  if (!is.null(ref.nii)) {
    dims <- info.nii(ref.nii, "xyz")
    pixdim <- info.nii(ref.nii, "pixdim")
    orient <- info.nii(ref.nii, "orient")
    datatype <- info.nii(ref.nii, "datatype")
  } else {
    if (is.null(pixdim)) { pixdim <- c(-1,2,2,2,1,0,0,0) }
    if (is.null(orient)) {
      orient$qform_code <- 2
      orient$sform_code <- 2
      orient$quatern_b <- 0
      orient$quatern_c <- 1
      orient$quatern_d <- 0
      orient$qoffset_x <- 90
      orient$qoffset_y <- -126
      orient$qoffset_z <- -72
      orient$srow_x <- c(-2,0,0,90)
      orient$srow_y <- c(0,2,0,-126)
      orient$srow_z <- c(0,0,2,-72)
    }
  }

  datatype.lut <- c(2L, 4L,  8L,  16L, 32L, 64L, 128L, 256L, 512L, 768L, 1024L, 1280L)
  bitpix.lut <-   c(8L, 16L, 32L, 32L, 64L, 64L,  24L,   8L,  16L,  32L,   64L,   64L)
  idx <- which(datatype.lut == datatype)
  if (length(idx)==0) { stop(sprintf("Datatype %s is not supported.", datatype)) }
  datatype <- as.integer(datatype)
  bitpix <- bitpix.lut[idx]
    
  writeBin(348L, fid, size = 4) #sizeof_hdr
  suppressWarnings(writeChar("", fid, nchars = 10, eos = NULL)) #data_type
  suppressWarnings(writeChar("", fid, nchars = 18, eos = NULL)) #db_name
  writeBin(0L, fid, size = 4) #extents
  writeBin(0L, fid, size = 2) #session_error
  writeChar("r", fid, nchars = 1, eos = NULL) #regular
  writeBin(0L, fid, size = 1) #dim_info

  img.dims <- rep(1,8)
  img.dims[1] <- length(dims)
  img.dims[2:(1+length(dims))] <- dims
  writeBin(as.integer(img.dims), fid, size = 2) #dim

  writeBin(as.double(0), fid, size = 4) #intent_p1
  writeBin(as.double(0), fid, size = 4) #intent_p2
  writeBin(as.double(0), fid, size = 4) #intent_p2
  writeBin(0L, fid, size = 2) #intent_code

  writeBin(datatype, fid, size = 2) #datatype
  writeBin(bitpix, fid, size = 2) #bitpix
  writeBin(1L, fid, size = 2) #slice_start

  writeBin(as.double(pixdim), fid, size = 4) #pixdim

  writeBin(as.double(352), fid, size = 4) #vox_offset
  writeBin(as.double(1), fid, size = 4) #scl_slope
  writeBin(as.double(0), fid, size = 4) #scl_inter
  writeBin(0L, fid, size = 2) #slice_end
  writeBin(0L, fid, size = 1) #slice_code
  writeBin(2L, fid, size = 1) #xyzt_units
  writeBin(as.double(0), fid, size = 4) #cal_max
  writeBin(as.double(0), fid, size = 4) #cal_min
  writeBin(as.double(0), fid, size = 4) #slice_duration
  writeBin(as.double(0), fid, size = 4) #toffset
  writeBin(0L, fid, size = 4) #glmax
  writeBin(0L, fid, size = 4) #glmin
  suppressWarnings(writeChar("R_nifti_io", fid, nchars = 80, eos = NULL)) #descrip
  suppressWarnings(writeChar("", fid, nchars = 24, eos = NULL)) #aux_file

  writeBin(as.integer(orient$qform_code), fid, size = 2)
  writeBin(as.integer(orient$sform_code), fid, size = 2)
  writeBin(as.double(orient$quatern_b), fid, size = 4)
  writeBin(as.double(orient$quatern_c), fid, size = 4)
  writeBin(as.double(orient$quatern_d), fid, size = 4)
  writeBin(as.double(orient$qoffset_x), fid, size = 4)
  writeBin(as.double(orient$qoffset_y), fid, size = 4)
  writeBin(as.double(orient$qoffset_z), fid, size = 4)
  writeBin(as.double(orient$srow_x), fid, size = 4)
  writeBin(as.double(orient$srow_y), fid, size = 4)
  writeBin(as.double(orient$srow_z), fid, size = 4)

  suppressWarnings(writeChar("0", fid, nchars = 16, eos = NULL)) #intent_name
  suppressWarnings(writeChar("n+1", fid, nchars = 4, eos = NULL)) #magic

  writeBin(c(0L,0L,0L,0L), fid, size = 1)
 
  data <- array(init.value, dim=dims[1:3])
  if (length(dims==3)) { dims <- c(dims, 1) }
  for (i in 1:dims[4]) {
    #writeBin(as.double(data), fid, size=8)
    switch(as.character(datatype),
      `2` = writeBin(as.integer(data), fid, size = bitpix/8),
      `4` = writeBin(as.integer(data), fid, size = bitpix/8),
      `8` = writeBin(as.integer(data), fid, size = bitpix/8),
      `16` = writeBin(as.double(data), fid, size = bitpix/8),
      `32` = writeBin(as.double(data), fid, size = bitpix/8),
      `64` = writeBin(as.double(data), fid, size = bitpix/8),
      `128` = writeBin(as.integer(data), fid, size = bitpix/8),
      `256` = writeBin(as.integer(data), fid, size = bitpix/8),
      `512` = writeBin(as.integer(data), fid, size = bitpix/8),
      `768` = writeBin(as.integer(data), fid, size = bitpix/8),
      `1024` = writeBin(as.integer(data), fid, size = bitpix/8),
      `1280` = writeBin(as.integer(data), fid, size = bitpix/8))
  }
  close(fid)
}
