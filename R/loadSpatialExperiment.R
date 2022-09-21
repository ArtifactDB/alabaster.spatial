#' Load a spatial experiment
#'
#' Load a \linkS4class{SpatialExperiment} object from its constituent files in DataSetDB.
#'
#' @param exp.info Named list of metadata for a spatial 'omics experiment.
#' @inheritParams alabaster.base::loadObject
#'
#' @return 
#' A \linkS4class{SpatialExperiment} object.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(SpatialExperiment)
#' example(read10xVisium, echo=FALSE)
#' colnames(spe) <- make.unique(colnames(spe)) # forcing unique column names.
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' meta <- stageObject(spe, tmp, "experiment-1")
#'
#' meta$path <- "experiment-1/experiment.json"
#' loadSpatialExperiment(meta, tmp)
#'
#' @export
#' @importFrom alabaster.base acquireMetadata .loadObject
#' @importFrom S4Vectors DataFrame I
#' @importFrom alabaster.sce loadSingleCellExperiment
#' @importFrom SummarizedExperiment colData<- colData
#' @import SpatialExperiment
loadSpatialExperiment <- function(exp.info, project) {
    sce <- loadSingleCellExperiment(exp.info, project)

    # Loading spatial stats.
    coord.data <- acquireMetadata(project, exp.info$spatial_experiment$coordinates$resource$path)
    mat <- .loadObject(coord.data, project)
    coords <- as.matrix(mat)

    img.data <- exp.info$spatial_experiment$image_data
    sids <- iids <- character(length(img.data))
    sfs <- numeric(length(img.data))
    imgs <- vector("list", length(img.data)) 
    for (x in seq_along(img.data)) {
        current <- img.data[[x]]
        sids[x] <- current$sample_id
        iids[x] <- current$image_id
        sfs[x] <- current$scale_factor

        img.meta <- acquireMetadata(project, current$data$resource$path)
        imgs[[x]] <- .loadObject(img.meta, project)
    }
    img.df <- DataFrame(sample_id=sids, image_id=iids, data=I(imgs), scaleFactor=sfs)

    toSpatialExperiment(sce, spatialCoords = coords, imgData = img.df)
}
