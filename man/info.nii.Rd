\name{info.nii}
\alias{info.nii}
\title{Retreive NIFTI-1 Header Information}
\description{Retreive information from the NIFTI-1 header of the specified file. The entire header can be retreived as a list, or a single or subset of items can be specified as a character vector to get specific information. There are several special cases implemented including getting number of voxels, number of volumes, image spacing, and orientation.}
\usage{info.nii(nii.file = nii.file, field="hdr")}
\arguments{
  \item{nii.file}{Full directory listing to NIFTI file.}
  \item{field}{A character vector of field names to be returned.
     Number of voxels in XYZ planes: "dimensions", "dims", "size", "sz", "voxels", "vxls", "xyz"
     Voxel spacing: "space", "spacing"
     Number of Volumes: "volumes", "vols", "trs"
     Orientation fields: "orientation" [partial matching]
     Entire Header as list object: "hdr"
     Subset of specific header items: "sizeof_hdr", "data_type", "db_name", "extents", "session_error", "regular", "dim_info", "dim", "intent_p1", "intent_p2", "intent_p3", "intent_code", "datatype", "bitpix", "slice_start", "pixdim", "vox_offset", "scl_slope", "scl_inter", "slice_end", "slice_code", "xyzt_units", "cal_max", "cal_min", "slice_duration", "toffset", "glmax", "glmin", "descrip", "aux_file", "qform_code", "sform_code", "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y", "qoffset_z", "srow_x", "srow_y", "srow_z", "intent_name", "magic")}
}
\value{A numeric vector or named list containing the requested output}
\author{
Timothy R. Koscik <timkoscik+nifti.io@gmail.com>
}
\examples{
  \dontrun{
    # specify directory listing to NII file as a string
    nii.file <- "~/file.nii"
    
    # retrieve the number of voxels in the 3-dimensional volume of the specified image
    image.size <- info.nii(nii.file = nii.file, field = "dimensions")
    
    # retreive the size in mm of the volume
    image.spacing <- info.nii(nii.file = nii.file, field = "spacing")
    
    # retreive the number of volumes
    image.volumes <- info.nii(nii.file = nii.file, field = "volumes")
    
    # retreive list containing orientation parameters
    image.orientation <- info.nii(nii.file = nii.file, field = "orientation")
    
    # retreive entire header from NII file
    image.hdr <- info.nii(nii.file = nii.file, field = "hdr")
    
    # retreive a specific element in the header
    image.hdr.datatype <- info.nii(nii.file = nii.file, field = "datatype")
  }
}
