table.to.nii <- function(in.table, coords, img.dims, save.dir, prefix, model, pixdim=NULL, orient=NULL) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  var.name <- deparse(substitute(in.table)) # retrieve table name
  
  if (!is.matrix(in.table) | !is.data.frame(in.table)) {
    in.table <- as.matrix(in.table)
  }
  
  log.name <- paste0(save.dir, "/", prefix, ".", var.name, ".log.txt")
  if (!file.exists(log.name)) {
    if (!missing(model)) {
      write.table(paste0("Model: ", deparse(formula(model))), log.name,
                  append=TRUE, quote=FALSE, sep=",",
                  row.names=FALSE, col.names=FALSE)
    } else {
      write.table("Model: NA", log.name,
                  append=TRUE, quote=FALSE, sep=",",
                  row.names=FALSE, col.names=FALSE)
    }
    
    write.table(paste0("Table: ", var.name), log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
    
    write.table(paste0("NII File: ", colnames(in.table)), log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
    
    write.table(paste0("Volume ", 1:nrow(in.table), ": ", rownames(in.table)), log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
  }
  
  for (i in 1:ncol(in.table)) {
    if (!is.null(colnames(in.table))) {
      suffix <- colnames(in.table)[i]
      suffix <- gsub("[[:punct:]]", "", suffix) # Remove invalid characters
      suffix <- gsub("[[:space:]]", "", suffix)
    } else {
      suffix <- paste0("X", i) # if table is unnamed call it X
    }
    fname <- paste0(save.dir, "/", prefix, ".", var.name, ".", suffix)
    if (!file.exists(paste0(fname, ".nii"))) {
      init.nii(file.name=paste0(fname, ".nii"),
               dims=c(img.dims[1:3], nrow(in.table)),
               pixdim=pixdim, orient=orient)
    }
    for (j in 1:nrow(in.table)) {
      if (!is.na(in.table[j,i])) {
        write.nii.voxel(paste0(fname, ".nii"), coords=c(coords,j), value=in.table[j,i])
      }
    }
  }
}