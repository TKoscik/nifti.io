nii.hdr <- function(nii.file, field="all") {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  stopifnot(file.exists(nii.file))
  
  fid <- file(nii.file, "rb")
  endian <- .Platform$endian
  
  if (field[1] == "all") {
    field <- c("sizeof_hdr", "data_type", "db_name", "extents", "session_error",
               "regular", "dim_info", "dim", "intent_p1", "intent_p2", "intent_p3",
               "intent_code", "datatype", "bitpix", "slice_start", "pixdim",
               "vox_offset", "scl_slope", "scl_inter", "slice_end", "slice_code",
               "xyzt_units", "cal_max", "cal_min", "slice_duration", "toffset",
               "glmax", "glmin", "descrip", "aux_file", "qform_code", "sform_code",
               "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y",
               "qoffset_z", "srow_x", "srow_y", "srow_z", "intent_name", "magic")
  }
  
  hdr <- list()
  
  if ("sizeof_hdr" %in% field) {
    invisible(seek(fid, 0, "start", "rb"))
    hdr$sizeof_hdr <- readBin(fid, integer(), size=4, endian=endian)
  }
  
  if ("data_type" %in% field) {
    invisible(seek(fid, 4, "start", "rb"))
    txt <- readBin(fid, "raw", 10)
    hdr$data_type <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("db_name" %in% field) {
    invisible(seek(fid, 14, "start", "rb"))
    txt <- readBin(fid, "raw", 18)
    hdr$db_name <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }

  if ("extents" %in% field) {
    invisible(seek(fid, 32, "start", "rb"))
    hdr$extents <- readBin(fid, integer(), size = 4, endian = endian)
  }
  
  if ("session_error" %in% field) {
    invisible(seek(fid, 36, "start", "rb"))
    hdr$session_error <- readBin(fid, integer(), size = 2, endian = endian)
  }

  if ("regular" %in% field) {
    invisible(seek(fid, 38, "start", "rb"))
    txt <- readBin(fid, "raw", 1)
    hdr$regular <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("dim_info" %in% field) {
    invisible(seek(fid, 39, "start", "rb"))
    # txt <- readBin(fid, "raw", 1)
    # hdr$dim_info<- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
    hdr$dim_info<- readBin(fid, integer(), size = 1, endian = endian)
  }

  if ("dim" %in% field) {
    invisible(seek(fid, 40, "start", "rb"))
    hdr$dim <- readBin(fid, integer(), 8, size = 2, endian = endian)
  }

  if ("intent_p1" %in% field) {
    invisible(seek(fid, 56, "start", "rb"))
    hdr$intent_p1 <- readBin(fid, numeric(), size = 4, endian = endian)
  }

  if ("intent_p2" %in% field) {
    invisible(seek(fid, 60, "start", "rb"))
    hdr$intent_p2 <- readBin(fid, numeric(), size = 4, endian = endian)
  }

  if ("intent_p3" %in% field) {
    invisible(seek(fid, 64, "start", "rb"))
    hdr$intent_p3 <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("intent_code" %in% field) {
    invisible(seek(fid, 68, "start", "rb"))
    hdr$intent_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("datatype" %in% field) {
    invisible(seek(fid, 70, "start", "rb"))
    hdr$datatype <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("bitpix" %in% field) {
    invisible(seek(fid, 72, "start", "rb"))
    hdr$bitpix <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("slice_start" %in% field) {
    invisible(seek(fid, 74, "start", "rb"))
    hdr$slice_start <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("pixdim" %in% field) {
    invisible(seek(fid, 76, "start", "rb"))
    hdr$pixdim <- readBin(fid, numeric(), 8, size = 4, endian = endian)
  }
  bad_pixdim = !is.finite(hdr$pixdim)
  if (any(bad_pixdim)) { hdr$pixdim[bad_pixdim] = 1 }

  if ("vox_offset" %in% field) {
    invisible(seek(fid, 108, "start", "rb"))
    hdr$vox_offset <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("scl_slope" %in% field) {
    invisible(seek(fid, 112, "start", "rb"))
    hdr$scl_slope <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("scl_inter" %in% field) {
    invisible(seek(fid, 116, "start", "rb"))
    hdr$scl_inter <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("slice_end" %in% field) {
    invisible(seek(fid, 120, "start", "rb"))
    hdr$slice_end <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("slice_code" %in% field) {
    invisible(seek(fid, 122, "start", "rb"))
    hdr$slice_code  <- readBin(fid, integer(), size = 1, signed = FALSE, endian = endian)
  }
  
  if ("xyzt_units" %in% field) {
    invisible(seek(fid, 123, "start", "rb"))
    hdr$xyzt_units <- readBin(fid, integer(), size = 1, signed = FALSE, endian = endian)
  }
  
  if ("cal_max" %in% field) {
    invisible(seek(fid, 124, "start", "rb"))
    hdr$cal_max  <- readBin(fid, numeric(), size = 4, endian = endian)
  }
   
  if ("cal_min" %in% field) {
    invisible(seek(fid, 128, "start", "rb"))
    hdr$cal_min <- readBin(fid, numeric(), size = 4, endian = endian)
  }
   
  if ("slice_duration" %in% field) {
    invisible(seek(fid, 132, "start", "rb"))
    hdr$slice_duration <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("toffset" %in% field) {
    invisible(seek(fid, 136, "start", "rb"))
    hdr$toffset <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("glmax" %in% field) {
    invisible(seek(fid, 140, "start", "rb"))
    hdr$glmax <- readBin(fid, integer(), size = 4, endian = endian)
  }
   
  if ("glmin" %in% field) {
    invisible(seek(fid, 144, "start", "rb"))
    hdr$glmin <- readBin(fid, integer(), size = 4, endian = endian)
  }
  
  if ("descrip" %in% field) {
    invisible(seek(fid, 148, "start", "rb"))
    txt <- readBin(fid, "raw", 80)
    hdr$descrip <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("aux_file" %in% field) {
    invisible(seek(fid, 228, "start", "rb"))
    txt <- readBin(fid, "raw", 224)
    hdr$aux_file <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("qform_code" %in% field) {
    invisible(seek(fid, 252, "start", "rb"))
    hdr$qform_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("sform_code" %in% field) {
    invisible(seek(fid, 254, "start", "rb"))
    hdr$sform_code <- readBin(fid, integer(), size = 2, endian = endian)
  }
  
  if ("quatern_b" %in% field) {
    invisible(seek(fid, 256, "start", "rb"))
    hdr$quatern_b <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("quatern_c" %in% field) {
    invisible(seek(fid, 260, "start", "rb"))
    hdr$quatern_c <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("quatern_d" %in% field) {
    invisible(seek(fid, 264, "start", "rb"))
    hdr$quatern_d <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_x" %in% field) {
    invisible(seek(fid, 268, "start", "rb"))
    hdr$qoffset_x <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_y" %in% field) {
    invisible(seek(fid, 272, "start", "rb"))
    hdr$qoffset_y <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("qoffset_z" %in% field) {
    invisible(seek(fid, 276, "start", "rb"))
    hdr$qoffset_z <- readBin(fid, numeric(), size = 4, endian = endian)
  }
  
  if ("srow_x" %in% field) {
    invisible(seek(fid, 280, "start", "rb"))
    hdr$srow_x <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("srow_y" %in% field) {
    invisible(seek(fid, 296, "start", "rb"))
    hdr$srow_y <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("srow_z" %in% field) {
    invisible(seek(fid, 312, "start", "rb"))
    hdr$srow_z <- readBin(fid, numeric(), 4, size = 4, endian = endian)
  }
  
  if ("intent_name" %in% field) {
    invisible(seek(fid, 328, "start", "rb"))
    txt <- readBin(fid, "raw", 16)
    hdr$intent_name <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  if ("magic" %in% field) {
    invisible(seek(fid, 344, "start", "rb"))
    txt <- readBin(fid, "raw", 4)
    hdr$magic <- iconv(rawToChar(txt[txt!=as.raw(0)]), to="UTF-8")
  }
  
  close(fid)
  return(hdr)
}