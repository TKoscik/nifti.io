\name{read.nii.voxel}
\alias{read.nii.voxel}
\title{
Read NII Voxel
}
\description{
Read values from NIFTI files at given coordinates (x,y,z,t).
}
\usage{
read.nii.voxel(nii.file, coords)
}
\arguments{
  \item{nii.file}{
Full directory listing to a NIFTI file.  File must not be gzipped.
}
  \item{coords}{
A numeric vector conatining x,y,z,t coordinates indicating the location to read values
}
}
\details{
NIFTI files need to be unzipped before using this function or any other portions of the mRi package. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function).

Values for t coordinates (coords[4]) may be Inf to retrieve all timepoints for the given x,y,z coordinates.  If data is 4D and only x, y, z coordinates are given, the value at those coordinates for the first volume only is returned.
}
\value{
A number or a numeric vector containing values for all timepoints.
}
\author{
Timothy R. Koscik <timkoscik+mripkg@gmail.com>
}