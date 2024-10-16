#' Read a SpatialExperiment from disk
#'
#' Read a \linkS4class{SpatialExperiment} object from its on-disk representation.
#'
#' @param path String containing a path to a directory, itself created using the \code{\link{saveObject}} method for \linkS4class{SpatialExperiment} objects.
#' @param metadata Named list of metadata for this object, see \code{\link{readObjectFile}} for details.
#' @param ... Further arguments passed to \code{\link{readSingleCellExperiment}} and internal \code{\link{altReadObject}} calls.
#'
#' @return 
#' A \linkS4class{SpatialExperiment} object.
#'
#' @seealso
#' \code{"\link{saveObject,SpatialExperiment-method}"}, to save a SpatialExperiment to disk.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(SpatialExperiment)
#' example(read10xVisium, echo=FALSE)
#'
#' tmp <- tempfile()
#' saveObject(spe, tmp)
#' readObject(tmp)
#'
#' @export
#' @aliases loadSpatialExperiment
#' @importFrom S4Vectors DataFrame I
#' @import SpatialExperiment alabaster.base rhdf5
#' @importFrom alabaster.sce readSingleCellExperiment
readSpatialExperiment <- function(path, metadata, ...) {
    sce <- readSingleCellExperiment(path, metadata, ...) # see comments in readRangedSummarizedExperiment in alabaster.se.

    coord.data <- altReadObject(file.path(path, "coordinates"), ...)
    coords <- as.matrix(coord.data)

    img.dir <- file.path(path, "images")
    fhandle <- H5Fopen(file.path(img.dir, "mapping.h5"))
    on.exit(H5Fclose(fhandle))
    ghandle <- H5Gopen(fhandle, "spatial_experiment")
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    sample.names <- h5_read_vector(ghandle, "sample_names")
    column.samples <- h5_read_vector(ghandle, "column_samples")
    sce$sample_id <- sample.names[column.samples + 1L] # just to be sure.

    image.samples <- h5_read_vector(ghandle, "image_samples")
    img.df <- DataFrame(
        sample_id=sample.names[image.samples + 1L],
        image_id=h5_read_vector(ghandle, "image_ids"),
        data=logical(length(image.samples)),
        scaleFactor=h5_read_vector(ghandle, "image_scale_factors")
    )

    all.formats <- h5_read_vector(ghandle, "image_formats")
    imgs <- vector("list", nrow(img.df)) 
    for (i in seq_along(imgs)) {
        cur.format <- all.formats[i]
        if (cur.format == "OTHER") {
            imgs[[i]] <- altReadObject(file.path(img.dir, i - 1L), ...)
        } else {
            if (cur.format == "PNG") {
                suffix <- "png"
            } else if (cur.format == "TIFF") {
                suffix <- "tif"
            } else {
                stop("unknown format '", cur.format, "'")
            }
            target <- file.path(img.dir, paste0(i - 1L, ".", suffix))
            imgs[[i]] <- SpatialImage(target, is.url=FALSE)
        }
    }
    img.df$data <- imgs

    toSpatialExperiment(sce, spatialCoords = coords, imgData = img.df)
}

##################################
######### OLD STUFF HERE #########
##################################

#' @export
#' @importFrom alabaster.sce loadSingleCellExperiment
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
