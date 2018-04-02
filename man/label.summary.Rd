\name{label.summary}
\alias{label.summary}
\title{
Summarize JLF Labels
}
\description{}
\usage{
label.summary(label.sjx, id.ls=NULL, label.csv=NULL, return.df=FALSE, save.name=NULL)
}
\arguments{
  \item{label.sjx}{A charcter vector indicating the full file path to each file you want to include in the summary}
  \item{id.ls}{A character vector indicating the subject identifier to be included in the output dataframe or file}
  \item{label.csv}{A character vector of length 1 indicating the full file path to comma-separated file that has the following columns (in order, and column labels). Column 1 = numeric label value for label, Column 2 - N = labels to include in summary, if label is repeated then the sum of all labelled voxels sharing this value are included.}
  \item{return.df}{A logical indicating whether or not to return the dataframe to the R environment, default = FALSE unless save.name = NULL}
  \item{save.name}{A character vector indicating the full file path to save the output}
}
\value{A dataframe or .csv file containing volumes (in mm) for each region}
\author{
Timothy R. Koscik <timkoscik+mripkg@gmail.com>
}
\examples{}
