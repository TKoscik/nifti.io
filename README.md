# nifti.io
## A set of tools for manipulating NIfTI-1 files, typically used for neuroimaging.

These tools include functions to manipulate NIfTI-1 files (Neuroimaging Informatics Technology Initiative) without having to load the entire object into RAM. As a consequence of this NIfTI files must be decompressed from their typical *.nii.gz* state. This is necessary given the inconsistent way in which gzipped files are indexed (Some information on this is given in the documentation for the readBin function). These read/write operations thus are **very fast**, faster than typical indexing of R objects, and require **minimal RAM** to work.

NIfTI files can be initialized with the <init.nii> function, and this proceeds volume-wise to reduce RAM to a minimum even when initializing nifti files with massive dimensions.

