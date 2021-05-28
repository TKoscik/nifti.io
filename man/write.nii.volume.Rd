\name{write.nii.volume}
\alias{write.nii.volume}
\title{Write NII Volume}
\description{}
\usage{
  write.nii.volume(nii.file, vol.num, values)
}
\arguments{
  \item{nii.file}{Full directory listing to a NIFTI file.  File must not be gzipped.}
  \item{vol.num}{An integer indicating which volume to read.}
  \item{values}{an array or vector of values to be written}
}
\details{
NIFTI files need to be unzipped before using this function or any other portions of the nifti.io package. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function).
}
\value{
Output directly to NIFTI file.
}
\author{
  Timothy R. Koscik <timkoscik+nifti.io@gmail.com>
}
