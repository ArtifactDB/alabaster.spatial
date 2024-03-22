# Save `SpatialExperiment`s to file

|Environment|Status|
|---|---|
|[BioC-release](https://bioconductor.org/packages/release/bioc/html/alabaster.spatial.html)|[![Release OK](https://bioconductor.org/shields/build/release/bioc/alabaster.spatial.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/alabaster.spatial/)|
|[BioC-devel](https://bioconductor.org/packages/devel/bioc/html/alabaster.spatial.html)|[![Devel OK](https://bioconductor.org/shields/build/devel/bioc/alabaster.spatial.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/alabaster.spatial/)|

The **alabaster.spatial** package implements methods for saving and loading `SpatialExperiment` objects under the **alabaster** framework.
This includes the spatial coordinates and images associated with each sample.
To get started, install the package and its dependencies from [Bioconductor](https://bioconductor.org/packages/alabaster.spatial):

```r
# install.packages("BiocManager")
BiocManager::install("alabaster.spatial")
```

In the example below, we save a `SpatialExperiment` object to file:

```r
library(SpatialExperiment)
example(SpatialExperiment, echo=FALSE) # can't be bothered to copy it here.
spe
## class: SpatialExperiment
## dim: 50 99
## metadata(0):
## assays(1): counts
## rownames(50): ENSMUSG00000051951 ENSMUSG00000089699 ...
##   ENSMUSG00000005886 ENSMUSG00000101476
## rowData names(1): symbol
## colnames(99): AAACAACGAATAGTTC-1 AAACAAGTATCTCCCA-1 ...
##   AAAGTCGACCCTCAGT-1 AAAGTGCCATCAATTA-1
## colData names(4): in_tissue array_row array_col sample_id
## reducedDimNames(0):
## mainExpName: NULL
## altExpNames(0):
## spatialCoords names(2) : pxl_col_in_fullres pxl_row_in_fullres
## imgData names(4): sample_id image_id data scaleFactor

library(alabaster.spatial)
tmp <- tempfile()
saveObject(spe, tmp)

roundtrip <- readObject(tmp)
class(roundtrip)
## [1] "SpatialExperiment"
## attr(,"package")
## [1] "SpatialExperiment"
```
