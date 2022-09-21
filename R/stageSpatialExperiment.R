#' Stage a spatial experiment
#'
#' Stage a \linkS4class{SpatialExperiment} object.
#'
#' @param x A \linkS4class{SpatialExperiment} object.
#' @inheritParams alabaster.base::stageObject
#' 
#' @author Aaron Lun
#'
#' @return
#' A named list of the same form as that returned by the \code{\link{stageObject}} method for a SingleCellExperiment,
#' but containing additional fields for the spatial data.
#' A directory is created at \code{path} inside \code{dir} and is populated with the contents of \code{x}.
#'
#' @examples
#' library(SpatialExperiment)
#' example(read10xVisium, echo=FALSE)
#' colnames(spe) <- make.unique(colnames(spe)) # forcing unique column names.
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' stageObject(spe, tmp, "experiment-1")
#' list.files(tmp, recursive=TRUE)
#' 
#' @export
#' @rdname stageSpatialExperiment
#' @importFrom alabaster.base .writeMetadata stageObject .stageObject
#' @importFrom jsonlite toJSON
#' @import SpatialExperiment 
#' @import methods
setMethod("stageObject", "SpatialExperiment", function(x, dir, path, child=FALSE, ...) {
    meta <- callNextMethod()
    meta[["$schema"]] <- "spatial_experiment/v1.json"

    spatmeta <- list()
    spatdir <- file.path(path, "spatial")
    dir.create(file.path(dir, spatdir))

    coord.path <- file.path(spatdir, "coords")
    coord.info <- .stageObject(spatialCoords(x), dir, coord.path, child=TRUE)
    coord.res <- .writeMetadata(coord.info, dir=dir)
    spatmeta[["coordinates"]] <- list(type="point", resource=coord.res)

    image.dir <- file.path(spatdir, "images")
    dir.create(file.path(dir, image.dir))

    images <- imgData(x)
    image.meta <- list()
    for (i in seq_len(nrow(images))) {
        image.path <- file.path(image.dir, paste0("image-", i))
        image.info <- .stageObject(images$data[[i]], dir, image.path, child=TRUE)
        image.res <- .writeMetadata(image.info, dir=dir)

        image.meta[[i]] <- list(
            sample_id = images$sample_id[i],
            image_id = images$image_id[i],
            scale_factor = images$scaleFactor[i],
            data = list(resource=image.res)
        )
    }
    spatmeta[["image_data"]] <- image.meta

    meta[["spatial_experiment"]] <- spatmeta
    meta
})
