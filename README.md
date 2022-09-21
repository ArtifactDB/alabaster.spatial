# Save `SpatialExperiment`s to file

The **alabaster.spatial** package implements methods for saving and loading `SpatialExperiment` objects under the **alabaster** framework.
It provides a language-agnostic method for serializing experimental data and annotations in these objects, including the genomic coordinates in a `RangedSpatialExperiment`.
To get started, install the package and its dependencies from GitHub:

```r
devtools::install_github("ArtifactDB/alabaster.schemas")
devtools::install_github("ArtifactDB/alabaster.base")
devtools::install_github("ArtifactDB/alabaster.ranges")
devtools::install_github("ArtifactDB/alabaster.matrix")
devtools::install_github("ArtifactDB/alabaster.se")
devtools::install_github("ArtifactDB/alabaster.sce")
devtools::install_github("ArtifactDB/alabaster.spatial")
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
dir.create(tmp)
meta <- stageObject(spe, tmp, "spe")
meta[["$schema"]]
## [1] "spatial_experiment/v1.json"

roundtrip <- loadObject(meta, tmp)
class(roundtrip)
## [1] "SpatialExperiment"
## attr(,"package")
## [1] "SpatialExperiment"
```
