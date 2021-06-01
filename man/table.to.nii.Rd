\name{table.to.nii}
\alias{table.to.nii}
\title{Table to NII file}
\description{Write Dataframe or Matrix to NII file}
\usage{
  table.to.nii(in.table, coords, save.dir, prefix=NULL, do.log=TRUE, model.string=NULL, ref.image=NULL, img.dims=NULL, pixdim=NULL, orient=NULL)
}
\arguments{
  \item{in.table}{dataframe or matrix object to output to NII file}
  \item{coords}{voxel coordinates of location in NII file to write values}
  \item{save.dir}{directory location to save output}
  \item{prefix}{prefix to be the base of filenames, default is the name of the input table object}
  \item{do.log}{logical, whether or not to write log file providing details about what the output contents are. this may be helpful for sorting through volumes.}
  \item{model.string}{A string to wirte to the log file specifying describing or specifying the model that was run}
  \item{ref.image}{a reference NII image to draw image properties from (e.g., dimensions and orientation)}
  \item{img.dims}{voxel dimensions of target NII file}
  \item{pixdim}{voxel dimensions of NII output}
  \item{orient}{orientation list object of NII output}
}
\value{Output directly to log.text file and write to specified coordinates in a set of NII files.}
\author{
  Timothy R. Koscik <timkoscik+nifti.io@gmail.com>
}
\examples{
  \dontrun{
    mdl <- lm(mpg ~ cyl, mtcars)
    mdl.coef <- as.data.frame(summary(mdl)$coef))
    table.to.nii(in.table=mdl.coef, coords=c(50,50,50), save.dir="~", prefix="exampleModel_coef", model.string="lm(mpg ~ cyl, mtcars)", ref.image="~/reference_image.nii")
  }
}
