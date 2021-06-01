# nifti.io
## A set of tools for manipulating NIFTI-1 files, typically used for neuroimaging.

These tools include functions to manipulate NIFTI-1 files without having to load the entire object into RAM. As a consequence of this NIFTI files must be decompressed from their typical *.nii.gz* state. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function). These read/write operations thus are **very fast**, faster than typical indexing of R objects, and require **minimal RAM** to work.

NIFTI files can be initialized with the <init.nii> function, and this proceeds volume-wise to reduce RAM to a minimum even when initializing nifti files with massive dimensions.

