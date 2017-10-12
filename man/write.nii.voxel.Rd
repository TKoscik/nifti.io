\name{write.nii.voxel}
\alias{write.nii.voxel}
\title{
Write NII Voxel
}
\description{
Write NII Voxel
}
\usage{
write.nii.voxel(nii.file, coords, value)
}
\arguments{
  \item{nii.file}{
Full directory listing to a NIFTI file.  File must not be gzipped.
}
  \item{coords}{
A numeric vector conatining x,y,z,t coordinates indicating the location to write values
}
  \item{value}{
A numeric value to write
}
}
\details{
NIFTI files need to be unzipped before using this function or any other portions of the mRi package. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function).
}
\value{
Output directly to NIFTI file.
}
\author{
Timothy R. Koscik <timkoscik+mripkg@gmail.com>
}