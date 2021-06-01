info.nii <- function(nii.file, field="hdr") {
  
  stopifnot(file.exists(nii.file))
  
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian
  
  info <- list()
  
  # special cases ------------------------------------------------------------
  chk.ls <- c("dimensions", "dims", "size", "sz", "voxels", "vxls", "xyz")
  if (field[1] %in% chk.ls) {
    invisible(seek(fid, 40, "start", "rb"))
    info$voxel_dimensions <- readBin(fid, integer(), 8, size = 2, endian = endian)[2:4]
    names(info$voxel_dimensions) <- c("x", "y", "z")
  }

  chk.ls <- c("space", "spacing")
  if (field[1] %in% chk.ls) {
    invisible(seek(fid, 76, "start", "rb"))
    tmp <- readBin(fid, numeric(), 8, size = 4, endian = endian)[2:4]
    bad_vals = !is.finite(tmp)
    if (any(bad_vals)) { tmp[bad_vals] = 1 }
    info$spacing <- tmp
    names(info$spacing) <- c("x", "y", "z")
  }
  
  chk.ls <- c("volumes", "vols", "trs")
  if (field[1] %in% chk.ls) {
    invisible(seek(fid, 40, "start", "rb"))
    tmp <- readBin(fid, integer(), 8, size = 2, endian = endian)
    info$num_volumes <- tmp[4]
  }
  
  if (field[1] %in% c("orient", "orientation")) {
    seek(fid, where=252, origin="start", size=1)
    info$qform_code <- readBin(fid, integer(), size = 2, endian = endian)
    info$sform_code <- readBin(fid, integer(), size = 2, endian = endian)
    info$quatern_b <- readBin(fid, numeric(), size = 4, endian = endian)
    info$quatern_c <- readBin(fid, numeric(), size = 4, endian = endian)
    info$quatern_d <- readBin(fid, numeric(), size = 4, endian = endian)
    info$qoffset_x <- readBin(fid, numeric(), size = 4, endian = endian)
    info$qoffset_y <- readBin(fid, numeric(), size = 4, endian = endian)
    info$qoffset_z <- readBin(fid, numeric(), size = 4, endian = endian)
    info$srow_x <- readBin(fid, numeric(), 4, size = 4, endian = endian)
    info$srow_y <- readBin(fid, numeric(), 4, size = 4, endian = endian)
    info$srow_z <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  # general header information --------------------------------------------------
  if (field[1] %in% c("all", "hdr", "header")) {
    field <- c("sizeof_hdr", "data_type", "db_name", "extents", "session_error",
               "regular", "dim_info", "dim", "intent_p1", "intent_p2", "intent_p3",
               "intent_code", "datatype", "bitpix", "slice_start", "pixdim",
               "vox_offset", "scl_slope", "scl_inter", "slice_end", "slice_code",
               "xyzt_units", "cal_max", "cal_min", "slice_duration", "toffset",
               "glmax", "glmin", "descrip", "aux_file", "qform_code", "sform_code",
               "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y",
               "qoffset_z", "srow_x", "srow_y", "srow_z", "intent_name", "magic")
  }
  
  if ("sizeof_hdr" %in% field) {
    invisible(seek(fid, 0, "start", "rb"))
    info$sizeof_hdr <- readBin(fid, integer(), size=4, endian=endian)
  }
  
  if ("data_type" %in% field) {
    invisible(seek(fid, 4, "start", "rb"))
    txt <- readBin(fid, "raw", 10)
    info$data_type <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("db_name" %in% field) {
    invisible(seek(fid, 14, "start", "rb"))
    txt <- readBin(fid, "raw", 18)
    info$db_name <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("extents" %in% field) {
    invisible(seek(fid, 32, "start", "rb"))
    info$extents <- readBin(fid, integer(), size = 4, endian = endian)
  }
  
  if ("session_error" %in% field) {
    invisible(seek(fid, 36, "start", "rb"))
    info$session_error <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("regular" %in% field) {
    invisible(seek(fid, 38, "start", "rb"))
    txt <- readBin(fid, "raw", 1)
    info$regular <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("dim_info" %in% field) {
    invisible(seek(fid, 39, "start", "rb"))
    # txt <- readBin(fid, "raw", 1)
    # info$dim_info<- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
    info$dim_info<- readBin(fid, integer(), size = 1, endian = endian)
  }
  
  if ("dim" %in% field) {
    invisible(seek(fid, 40, "start", "rb"))
    info$dim <- readBin(fid, integer(), 8, size = 2, endian = endian)
  }
  
  if ("intent_p1" %in% field) {
    invisible(seek(fid, 56, "start", "rb"))
    info$intent_p1 <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("intent_p2" %in% field) {
    invisible(seek(fid, 60, "start", "rb"))
    info$intent_p2 <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("intent_p3" %in% field) {
    invisible(seek(fid, 64, "start", "rb"))
    info$intent_p3 <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("intent_code" %in% field) {
    invisible(seek(fid, 68, "start", "rb"))
    info$intent_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("datatype" %in% field) {
    invisible(seek(fid, 70, "start", "rb"))
    info$datatype <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("bitpix" %in% field) {
    invisible(seek(fid, 72, "start", "rb"))
    info$bitpix <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("slice_start" %in% field) {
    invisible(seek(fid, 74, "start", "rb"))
    info$slice_start <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("pixdim" %in% field) {
    invisible(seek(fid, 76, "start", "rb"))
    info$pixdim <- readBin(fid, numeric(), 8, size = 4, endian = endian)
  }
  bad_pixdim = !is.finite(info$pixdim)
  if (any(bad_pixdim)) { info$pixdim[bad_pixdim] = 1 }
  
  if ("vox_offset" %in% field) {
    invisible(seek(fid, 108, "start", "rb"))
    info$vox_offset <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("scl_slope" %in% field) {
    invisible(seek(fid, 112, "start", "rb"))
    info$scl_slope <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("scl_inter" %in% field) {
    invisible(seek(fid, 116, "start", "rb"))
    info$scl_inter <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("slice_end" %in% field) {
    invisible(seek(fid, 120, "start", "rb"))
    info$slice_end <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("slice_code" %in% field) {
    invisible(seek(fid, 122, "start", "rb"))
    info$slice_code  <- readBin(fid, integer(), size = 1, signed = FALSE, endian = endian)
  }
  
  if ("xyzt_units" %in% field) {
    invisible(seek(fid, 123, "start", "rb"))
    info$xyzt_units <- readBin(fid, integer(), size = 1, signed = FALSE, endian = endian)
  }
  
  if ("cal_max" %in% field) {
    invisible(seek(fid, 124, "start", "rb"))
    info$cal_max  <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("cal_min" %in% field) {
    invisible(seek(fid, 128, "start", "rb"))
    info$cal_min <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("slice_duration" %in% field) {
    invisible(seek(fid, 132, "start", "rb"))
    info$slice_duration <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("toffset" %in% field) {
    invisible(seek(fid, 136, "start", "rb"))
    info$toffset <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("glmax" %in% field) {
    invisible(seek(fid, 140, "start", "rb"))
    info$glmax <- readBin(fid, integer(), size = 4, endian = endian)
  }
  
  if ("glmin" %in% field) {
    invisible(seek(fid, 144, "start", "rb"))
    info$glmin <- readBin(fid, integer(), size = 4, endian = endian)
  }
  
  if ("descrip" %in% field) {
    invisible(seek(fid, 148, "start", "rb"))
    txt <- readBin(fid, "raw", 80)
    info$descrip <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("aux_file" %in% field) {
    invisible(seek(fid, 228, "start", "rb"))
    txt <- readBin(fid, "raw", 224)
    info$aux_file <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("qform_code" %in% field) {
    invisible(seek(fid, 252, "start", "rb"))
    info$qform_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("sform_code" %in% field) {
    invisible(seek(fid, 254, "start", "rb"))
    info$sform_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("quatern_b" %in% field) {
    invisible(seek(fid, 256, "start", "rb"))
    info$quatern_b <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("quatern_c" %in% field) {
    invisible(seek(fid, 260, "start", "rb"))
    info$quatern_c <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("quatern_d" %in% field) {
    invisible(seek(fid, 264, "start", "rb"))
    info$quatern_d <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_x" %in% field) {
    invisible(seek(fid, 268, "start", "rb"))
    info$qoffset_x <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_y" %in% field) {
    invisible(seek(fid, 272, "start", "rb"))
    info$qoffset_y <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_z" %in% field) {
    invisible(seek(fid, 276, "start", "rb"))
    info$qoffset_z <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("srow_x" %in% field) {
    invisible(seek(fid, 280, "start", "rb"))
    info$srow_x <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("srow_y" %in% field) {
    invisible(seek(fid, 296, "start", "rb"))
    info$srow_y <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("srow_z" %in% field) {
    invisible(seek(fid, 312, "start", "rb"))
    info$srow_z <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("intent_name" %in% field) {
    invisible(seek(fid, 328, "start", "rb"))
    txt <- readBin(fid, "raw", 16)
    info$intent_name <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("magic" %in% field) {
    invisible(seek(fid, 344, "start", "rb"))
    txt <- readBin(fid, "raw", 4)
    info$magic <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  close(fid)
  if (length(info) == 1) { info <- unlist(info) }
  return(info)
}
