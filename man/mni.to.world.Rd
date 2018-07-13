\name{mni.to.world}
\alias{mni.to.world}
\title{Convert coordinate systems}
\description{Convert between MM cordinates in NIFTI file to voxel coordinates based on SFORM transformation}
\usage{mni.to.world(coords, tform.xyz=NULL, nii.file=NULL)}
\arguments{
  \item{coords}{3-column matrix of MNI x,y,z coordinates}
  \item{tform.xyz}{Transformation matrix}
  \item{nii.file}{NIFTI file from which to read the transformation matrix}
}
\author{
Timothy R. Koscik <timkoscik+nifti@gmail.com>
}