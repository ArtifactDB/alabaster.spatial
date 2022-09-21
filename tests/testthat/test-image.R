# Tests that the images are staged correctly.
# library(testthat); library(alabaster.spatial); source("test-image.R")

library(SpatialExperiment)
example(read10xVisium, echo=FALSE)
img <- imgData(spe)$data[[1]]

tmp <- tempfile()
dir.create(tmp)

test_that("local saves work correctly", {
    meta <- stageObject(img, tmp, "whee")
    .writeMetadata(meta, tmp)

    expect_identical(meta$image_file$format, "PNG")
    expect_identical(digest::digest(file=file.path(tmp, meta$path)), digest::digest(file=imgSource(img)))

    X <- imgRaster(img)
    expect_identical(meta$image_file$width, ncol(X))
    expect_identical(meta$image_file$height, nrow(X))

    # Reload works correctly.
    roundtrip <- loadSpatialImage(meta, project=tmp)
    expect_identical(imgRaster(roundtrip), imgRaster(img))

    # Metadata saving works correctly
    expect_error(alabaster.base::.writeMetadata(meta, dir=tmp), NA)
})

test_that("forcible resaves work correctly", {
    Y <- as(img, "LoadedSpatialImage")

    meta <- stageObject(Y, tmp, "foo")
    .writeMetadata(meta, tmp)
    expect_identical(meta$image_file$format, "PNG")

    X <- magick::image_read(file.path(tmp, meta$path))
    expect_identical(imgRaster(Y), as.raster(X))

    info <- magick::image_info(X)
    expect_identical(meta$image_file$width, info$width)
    expect_identical(meta$image_file$height, info$height)
})
