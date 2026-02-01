#' Save a spatial experiment
#'
#' Save a \linkS4class{SpatialExperiment} object to its on-disk representation.
#'
#' @param x A \linkS4class{SpatialExperiment} object.
#' @inheritParams alabaster.base::saveObject
#' @param SpatialExperiment.reuse.images Whether to re-use existing files on disk (e.g., by copying or linking to them in \code{path})
#' when an image in \code{\link[SpatialExperiment]{imgData}(x)} contains a reference to its original file location (e.g., via \code{\link[SpatialExperiemnt]{imgSource}}).
#' This can be any of the values described for \code{action=} in \code{\link[alabaster.base]{cloneFile}}.
#' @param ... Further arguments to pass to internal \code{\link{saveObject}} calls.
#' 
#' @author Aaron Lun
#'
#' @details
#' Currently, only PNG and TIFF image formats are supported in the \code{\link{imgData}}.
#' All other images will be re-saved as PNG.
#'
#' @return \code{x} is saved to \code{path} and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readSpatialExperiment}}, to read the SpatialExperiment back into the R session.
#'
#' @examples
#' library(SpatialExperiment)
#' example(read10xVisium, echo=FALSE)
#'
#' tmp <- tempfile()
#' saveObject(spe, tmp)
#' list.files(tmp, recursive=TRUE)
#' 
#' @export
#' @rdname saveSpatialExperiment 
#' @aliases stageObject,SpatialExperiment-method
#' @import SpatialExperiment alabaster.base methods rhdf5
#' @importMethodsFrom alabaster.sce saveObject
setMethod("saveObject", "SpatialExperiment", function(x, path, SpatialExperiment.reuse.images="link", ...) {
    callNextMethod() # see comments in saveObject,RangedSummarizedExperiment-method in alabaster.se.

    coord.path <- file.path(path, "coordinates")
    saveObject(spatialCoords(x), coord.path, ...)

    images <- imgData(x)
    if (nrow(images) > 0L) {
        img.dir <- file.path(path, "images")
        dir.create(img.dir)

        fhandle <- H5Fcreate(file.path(img.dir, "mapping.h5"))
        on.exit(H5Fclose(fhandle))
        ghandle <- H5Gcreate(fhandle, "spatial_experiment")
        on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

        sample.names <- unique(images$sample_id)
        h5_write_vector(ghandle, "sample_names", sample.names)

        column.samples <- match(x$sample_id, sample.names) - 1L
        h5_write_vector(ghandle, "column_samples", column.samples, type="H5T_NATIVE_UINT32")

        image.samples <- match(images$sample_id, sample.names) - 1L
        h5_write_vector(ghandle, "image_samples", image.samples, type="H5T_NATIVE_UINT32")

        h5_write_vector(ghandle, "image_ids", images$image_id)
        h5_write_vector(ghandle, "image_scale_factors", images$scaleFactor, type="H5T_NATIVE_DOUBLE")

        actual.images <- images$data
        formats <- character(length(actual.images))
        for (i in seq_along(actual.images)) {
            cur.img <- actual.images[[i]]
            format <- NULL

            meth <- selectMethod("saveObject", class(cur.img), optional=TRUE)
            if (!is.null(meth)) {
                meth(cur.img, file.path(img.dir, i - 1L), ...)
                format <- "OTHER"
            } else {
                if (is(cur.img, "StoredSpatialImage")) {
                    format <- save_image(imgSource(cur.img), img.dir, i, action=SpatialExperiment.reuse.images)
                } else if (is(cur.img, "RemoteSpatialImage")) {
                    format <- save_image(imgSource(cur.img, path=TRUE), img.dir, i, action=SpatialExperiment.reuse.images)
                }

                if (is.null(format)) {
                    ras <- imgRaster(x)
                    Y <- col2rgb(as.matrix(ras))
                    Y <- t(Y)
                    Y <- Y / 255
                    dim(Y) <- c(dim(ras), ncol(Y)) 
                    dest <- file.path(img.dir, paste0(i-1L, ".png"))
                    png::writePNG(Y, target=dest)
                    format <- "PNG"
                }
            }

            formats[i] <- format
        }

        h5_write_vector(ghandle, "image_formats", formats)
    }

    meta <- readObjectFile(path)
    meta$spatial_experiment <- list(version="1.2")
    saveObjectFile(path, "spatial_experiment", meta)

    invisible(NULL)
})

#' @importFrom alabaster.base cloneFile
save_image <- function(src, dir, i, action) {
    handle <- magick::image_read(src)
    details <- magick::image_info(handle)
    format <- details$format

    if (format == "PNG") {
        suffix <- "png"
    } else if (format == "TIFF") {
        suffix <- "tif"
    } else {
        # For now. This can be extended to more supported formats, but until
        # then, we fall back to extracting a raster and re-saving as a PNG.
        return(NULL)
    }

    dest <- file.path(dir, paste0(i-1L, ".", suffix))
    cloneFile(src, dest, action=action)

    format
}

##################################
######### OLD STUFF HERE #########
##################################

#' @export
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
