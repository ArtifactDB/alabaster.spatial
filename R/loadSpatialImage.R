#' Load a spatial image
#'
#' Load an image as a \linkS4class{SpatialImage} or subclass thereof.
#'
#' @param img.info Named list containing the metadata for this assay.
#' @inheritParams alabaster.base::loadObject
#'
#' @return A \linkS4class{SpatialImage} containing the image data (or a reference to it).
#' 
#' @author Aaron Lun
#'
#' @examples
#' example(read10xVisium, echo=FALSE)
#' img <- imgData(spe)$data[[1]]
#' 
#' tmp <- tempfile()
#' dir.create(tmp)
#' meta <- stageObject(img, tmp, "whee")
#'
#' out <- loadSpatialImage(meta, tmp)
#' 
#' @export
#' @importFrom alabaster.base acquireFile
#' @importFrom SpatialExperiment SpatialImage
loadSpatialImage <- function(img.info, project) {
    fpath <- acquireFile(project, img.info$path)
    SpatialImage(fpath, is.url=FALSE)
}
