\name{world.to.mni}
\alias{world.to.mni}
\title{Convert coordinate systems}
\description{Convert between voxel cordinates in NIFTI file to mm coordinates based on SFORM transformation}
\usage{mni.to.world(coords, tform.xyz=NULL, nii.file=NULL)}
\arguments{
  \item{coords}{3-column matrix of voxel x,y,z coordinates}
  \item{tform.xyz}{Transformation matrix}
  \item{nii.file}{NIFTI file from which to read the transformation matrix}
}
\author{
Timothy R. Koscik <timkoscik+nifti@gmail.com>
}