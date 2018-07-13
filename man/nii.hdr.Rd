\name{nii.hdr}
\alias{nii.hdr}
\title{
Read NIFTI-1 Header Information
}
\description{Read NIFTI-1 Header Information}
\usage{
nii.hdr(nii.file, field="all")
}
\arguments{
  \item{nii.file}{Full directory listing to NIFTI file.}
  \item{field}{A character vector of field names to be returned. Acceptable valuse include: "all", "sizeof_hdr", "data_type", "db_name", "extents", "session_error", "regular", "dim_info", "dim", "intent_p1", "intent_p2", "intent_p3", "intent_code", "datatype", "bitpix", "slice_start", "pixdim", "vox_offset", "scl_slope", "scl_inter", "slice_end", "slice_code", "xyzt_units", "cal_max", "cal_min", "slice_duration", "toffset", "glmax", "glmin", "descrip", "aux_file", "qform_code", "sform_code", "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y", "qoffset_z", "srow_x", "srow_y", "srow_z", "intent_name", "magic")}
}
\value{A named list containing the requested output}
\author{
Timothy R. Koscik <timkoscik+nifti@gmail.com>
}