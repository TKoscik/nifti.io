\name{init.nii}
\alias{init.nii}
\title{Initialize Empty NIFTI File}
\description{This function is used to initialize an empty NIFTI file. It creates the file volume-by-volume rather than a bulk array write operation such that massive arrays do not need to be loaded into memory}
\usage{init.nii(nii.file, dims, pixdim=NULL, orient=NULL, datatype=16, init.value=NA)}
\arguments{
  \item{nii.file}{Full file path for the file to be written, must have .nii extension}
  \item{dims}{a numeric vector specifying the image dimensions, e.g., c(X,Y,Z,T)}
  \item{pixdim}{An 8 element numeric vector specifying the pixel dimensions image to be created. Should be in the format (a,x,y,z,t,b,c,d), the output from info.nii("example_file.nii", "pixdim") is the correct format}
  \item{orient}{A named list with the following elements, qform_code [integer(1)], sform_code [integer(1)], quatern_b [numeric(1)], quatern_c [numeric(1)], quatern_d [numeric(1)], qoffset_x [numeric(1)], qoffset_y [numeric(1)], qoffset_z [numeric(1)], srow_x [numeric(3)], srow_y [numeric(3)], srow_z [numeric(3)]}
  \item{datatype}{a numeric value indicating the type of tdata to be used in the new NII file, default=16}
  \item{init.value}{value to use for all voxels to initialize NII file, default=NA}
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
}
}
