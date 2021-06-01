\name{write.nii.volume}
\alias{write.nii.volume}
\title{Write NIFTI-1 Volume}
\description{Write values to a specific volume in a NIFTI-1 file.}
\usage{
  write.nii.volume(nii.file, vol.num, values)
}
\arguments{
  \item{nii.file}{Full directory listing to a NIFTI file. File must not be gzipped.}
  \item{vol.num}{An integer indicating which volume to read.}
  \item{values}{an array or vector of values to be written}
}
\details{
NIFTI files need to be unzipped before using this function or any other portions of the nifti.io package. This is necessary given the inconsistent way in which gzipped files are indexed.
}
\value{
Output directly to NIFTI file.
}
\author{
  Timothy R. Koscik <timothy-koscik@uiowa.edu>
}
\examples{
\dontrun{
reference.image = "~/reference_image.nii"
pixdim = info.nii(nii.file = reference.image, field="pixdim")
orientation = info.nii(nii.file = reference.image, field="orientation")
init.nii(file.name = "~/example_image.nii",
         dims = c(100,100,100),
         pixdim = pixdim,
         orient = orientation)
new.values <- rnorm(prod(c(100,100,100)))
write.nii.volume(nii.file = "~/example_image.nii",
                 vol.num = 1,
                 values = new.values)
}
}
