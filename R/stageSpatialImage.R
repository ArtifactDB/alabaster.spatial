#' Stage images for upload 
#'
#' These methods are deprecated and are only documented here for back-compatibility purposes.
#'
#' @param x A \linkS4class{SpatialImage} object.
#' @param dir String containing a path to a directory.
#' @param path String containing a relative path inside a directory.
#' @param child Logical scalar indicating whether \code{x} is a child of another object.
#' @param ... Further arguments, ignored.
#'
#' @return An image file is created at \code{file.path(dir, path)}, possibly after appending an appropriate file extension.
#'
#' The return value should be a named list containing at least:
#' \itemize{
#' \item \code{$schema}, a string specifying the schema to use to validate the metadata.
#' This may have a \code{package} attribute to specify the package where the schema lives (in its \code{inst/schemas} directory).
#' \item \code{path}, a string containing the path to the file containing the assay contents.
#' This should start with the input \code{path} but can be followed by any necessary file extensions.
#' \item \code{child}, whether this is a child resource of a larger object.
#' }
#' Other fields can be provided and will be included in the metadata, provided that they are recognized by the specified schema.
#'
#' @author Aaron Lun
#'
#' @details
#' Each of the different methods will take advantage of any existing files to avoid an actual save.
#' For example, the \linkS4class{RemoteSpatialImage} method will download the file directly to \code{path},
#' while the \linkS4class{StoredSpatialImage} method will create a link or copy the file.
#' The \linkS4class{SpatialImage} method will fall back to saving the raster directly as a PNG.
#'
#' @examples
#' example(read10xVisium, echo=FALSE)
#' (img <- imgData(spe)$data[[1]])
#' 
#' # Doing a local run: 
#' tmp <- tempfile()
#' dir.create(tmp)
#' stageObject(img, tmp, "whee")
#'
#' # Forcing a re-save:
#' Y <- as(img, "LoadedSpatialImage")
#' stageObject(Y, tmp, "foo")
#' 
#' @name stageSpatialImage
NULL

#' @export
#' @rdname stageSpatialImage
#' @importFrom grDevices col2rgb
setMethod("stageObject", "VirtualSpatialImage", function(x, dir, path, child=FALSE, ...) {
    ras <- imgRaster(x)

    Y <- col2rgb(as.matrix(ras))
    Y <- t(Y)
    Y <- Y / 255
    dim(Y) <- c(dim(ras), ncol(Y)) 

    output <- paste0(path, ".png")
    png::writePNG(Y, target=file.path(dir, output))

    list(
        `$schema`="image_file/v1.json",
        path=output,
        image_file=list(
            width=ncol(ras), # yes, this is correct. Just think about it.
            height=nrow(ras),
            format="PNG"
        ),
        is_child=child
    )
})

.annotate_image <- function(src, path, child) {
    handle <- magick::image_read(src)
    details <- magick::image_info(handle)
    format <- details$format
    if (format == "JPG") {
        format <- "JPEG"
    } else if (!format %in% c("JPEG", "PNG", "BMP", "GIF", "TIFF", "WEBP")) {
        stop("unrecognized format for '", src, "'")
    }

    # Renaming, just because we can.
    newpath <- paste0(path, ".", tolower(format))
    newname <- paste0(src, ".", tolower(format))
    file.rename(src, newname)

    list(
        `$schema`="image_file/v1.json",
        path=newpath,
        image_file=list(
            width=details$width,
            height=details$height,
            format=format
        ),
        is_child=child
    )
}

#' @export
#' @rdname stageSpatialImage
setMethod("stageObject", "StoredSpatialImage", function(x, dir, path, child=FALSE, ...) {
    target <- file.path(dir, path)
    src <- imgSource(x)
    if (!file.link(src, target) && !file.copy(src, target)) {
        stop("failed to copy '", src, "'")
    }
    .annotate_image(target, path, child=child)
})

#' @export
#' @rdname stageSpatialImage
#' @importFrom utils download.file
setMethod("stageObject", "RemoteSpatialImage", function(x, dir, path, child=FALSE, ...) {
    target <- file.path(dir, path)
    src <- imgSource(x)
    if (!download.file(src, target)) {
        stop("failed to download '", src, "'")
    }
    .annotate_image(target, path, child=child)
})
