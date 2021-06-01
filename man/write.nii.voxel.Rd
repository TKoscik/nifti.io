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
Full directory listing to a NIfTI file.  File must not be gzipped.
}
  \item{coords}{
A numeric vector conatining x,y,z,t coordinates indicating the location to write values
}
  \item{value}{
A numeric value to write
}
}
\details{
NIfTI files need to be unzipped before using this function or any other portions of the nifti.io package. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function).
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
coordinates <- c(50,50,50)
new.value <- rnorm(1)
write.nii.volume(nii.file = "~/example_image.nii",
                 coords = coordinates,
                 value = new.value)
}
}
