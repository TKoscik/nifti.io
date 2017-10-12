\name{nii.orient}
\alias{nii.orient}
\title{
Read Orientation from NIFTI Header
}
\description{
Read Orientation from NIFTI Header
}
\usage{
nii.orient(nii.file)
}
\arguments{
  \item{nii.file}{
Full directory listing to NIFTI file.
}
}
\value{
A named list containing NIFTI orientation information (qform and sform)
}
\author{
Timothy R. Koscik <timkoscik+mripkg@gmail.com>
}