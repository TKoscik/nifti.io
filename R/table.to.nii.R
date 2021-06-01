table.to.nii <- function(in.table, coords, save.dir, prefix=NULL, do.log=TRUE, model.string=NULL, ref.nii=NULL, img.dims=NULL, pixdim=NULL, orient=NULL) {
  
  tbl.name <-  deparse(substitute(in.table))
  if (!is.matrix(in.table) | !is.data.frame(in.table)) { in.table <- as.matrix(in.table) }
  
  # if prefix is not provided, use name of input object
  if (is.null(prefix)) {
    prefix <- tbl.name
    prefix <- gsub("[[:punct:]]", "", prefix)
    prefix <- gsub("[[:space:]]", "", prefix)
  }
  
  # write log if desired and if does not exist
  if (do.log) {
    log.name <- paste0(save.dir, "/", prefix, "_", "log.txt")
    if (!file.exists(log.name)) {
      log.id <- file(log.name, "wt")
      writeLines(paste0("MODEL:", model.string), con=log.id)
      writeLines(paste0("TABLE:", tbl.name), con=log.id)
      writeLines(paste0("NIIFILE:", colnames(in.table)), con=log.id)
      writeLines(paste0("VOLUME", 1:nrow(in.table), ":", rownames(in.table)), con=log.id)
      close(log.id)
    }
  }
  
  # rename table to remove bad characters
  colnames(in.table) <- gsub("[[:punct:]]", "", colnames(in.table))
  colnames(in.table) <- gsub("[[:space:]]", "", colnames(in.table))
  
  # output data                 
  for (i in 1:ncol(in.table)) {
    fname <- paste0(save.dir, "/", prefix, "_", colnames(in.table)[i], ".nii")
    if (!file.exists(fname)) {
      if (!is.null(ref.nii)) {
        img.dims <- info.nii(ref.nii, "xyz")
        pixdim <- info.nii(ref.nii, "pixdim")
        orient <- info.nii(ref.nii, "orientation")
      }
      init.nii(new.nii = fname,
               dims = c(img.dims[1:3], nrow(in.table)),
               pixdim = pixdim,
               orient = orient)
    }
    for (j in 1:nrow(in.table)) {
      write.nii.voxel(nii.file=fname, coords=c(coords,j), value=in.table[j,i])
    }
  }
}
