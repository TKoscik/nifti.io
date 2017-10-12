\name{read.nii.volume}
\alias{read.nii.volume}
\title{
Read NII Volume
}
\description{
Read indicated volume from NIFTI files.
}
\usage{
read.nii.volume(nii.file, vol.num)
}
\arguments{
  \item{nii.file}{
Full directory listing to a NIFTI file.  File must not be gzipped.
}
  \item{vol.num}{
An integer indicating which volume to read.
}
}
\details{
NIFTI files need to be unzipped before using this function or any other portions of the mRi package. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function).
}
\value{
An array containing values from NIFT volume.
}
\author{
Timothy R. Koscik <timkoscik+mripkg@gmail.com>
}