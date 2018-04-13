label.summary <- function(label.sjx, id.ls=NULL, label.csv=NULL, return.df=FALSE, save.name=NULL) {

  n.sjx <- length(label.sjx)
  label.df <- read.csv(label.csv, header=TRUE, as.is=TRUE)

  col.labels <- character(0L)
  for (i in 2:ncol(label.df)) {
    col.labels <- c(col.labels, unique(label.df[ ,i]))
  }
  col.labels <- na.omit(col.labels)

  if (!is.null(id.ls)) {
    id.ls <- label.sjx
  }

  if (length(id.ls) != length(label.sjx)) {
    stop("ID list and file list do not match")
  }

  df <- data.frame(id=id.ls,
                   matrix(as.numeric(NA), ncol=length(col.labels), nrow=length(id.ls)))
  colnames(df) <- c("id", col.labels)

  for (i in 1:n.sjx) {
    labels <- read.nii.volume(label.sjx[i], 1)
    pixdim <- prod(unlist(nii.hdr(label.sjx[i], "pixdim")[2:4]))

    out.vec <- numeric(0L)
    if (ncol(label.df)){
      for (j in 2:ncol(label.df)) {
        merge.labels <- na.omit(unique(label.df[ ,j]))
        for (k in 1:length(merge.labels)) {
          out.vec <- c(out.vec, sum(labels %in% label.df[which(label.df[ ,j] == merge.labels[k]),1]))
        }
      }
    }

    out.vec <- out.vec / pixdim

    df[i,2:ncol(df)] <- out.vec
    df[i, 
  }

  if (!is.null(save.name)) {
    write.table(x = df,
                file = save.name,
                quote = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE)
  } else {
    return.df <- TRUE
  }

  if (return.df == TRUE) {
    return(df[i, ])
  }
}
