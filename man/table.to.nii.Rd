\name{table.to.nii}
\alias{table.to.nii}
\title{table.to.nii}
\description{Write Dataframe to NII file}
\usage{
  table.to.nii(in.table, coords, img.dims, save.dir, prefix, model, pixdim=NULL, orient=NULL)
}
\arguments{
  \item{in.table}{dataframe or matrix object to output to NII file}
  \item{coords}{voxel coordinates of location in NII file to write values}
  \item{img.dims}{voxel dimensions of target NII file}
  \item{save.dir}{directory location to save outpuit}
  \item{prefix}{prefix to be appended to the filename}
  \item{model}{the model that was run, optional, will be added as a line in the log file that accompanies NII output}
  \item{pixdim}{voxel dimensions of NII output}
  \item{orient}{orientation list object of NII output}
}
\value{}
\author{
  Timothy R. Koscik <timkoscik+nifti.io@gmail.com>
}
\examples{}
