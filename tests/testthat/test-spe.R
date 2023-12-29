# This tests the SpatialExperiment loading.
# library(testthat); library(alabaster.spatial); source("test-spe.R")

library(SpatialExperiment)
example(read10xVisium, echo=FALSE)
colnames(spe) <- make.unique(colnames(spe)) # forcing unique column names.

test_that("staging and loading works correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    meta <- stageObject(spe, tmp, "experiment-1")
    .writeMetadata(meta, tmp)

    # Passes metadata validity checks correctly.
    meta$path <- "experiment-1/experiment.json"
    expect_error(alabaster.base::.writeMetadata(meta, dir=tmp), NA)

    # Check that it gets loaded correctly.
    spe2 <- loadSpatialExperiment(meta, project=tmp)
    expect_identical(spatialCoords(spe), spatialCoords(spe2))
    expect_identical(colData(spe), colData(spe2)) 

    # Deprecated fields aren't saved.
    expect_false("spatial_data" %in% names(meta$spatial_experiment))

    # Check that images are loaded correctly.
    nodata <- c("sample_id", "image_id", "scaleFactor")
    expect_identical(imgData(spe)[,nodata], imgData(spe2)[,nodata])

    dat1 <- imgData(spe)[,"data"]
    dat2 <- imgData(spe2)[,"data"]
    expect_identical(imgRaster(dat1[[1]]), imgRaster(dat2[[1]]))
    expect_identical(imgRaster(dat1[[2]]), imgRaster(dat2[[2]]))
})

test_that("saving and reading works in the new world", {
    tmp <- tempfile()
    saveObject(spe, tmp)

    spe2 <- readObject(tmp)
    expect_identical(spatialCoords(spe), spatialCoords(spe2))
    expect_identical(colData(spe), colData(spe2)) 

    # Check that images are loaded correctly.
    nodata <- c("sample_id", "image_id", "scaleFactor")
    expect_identical(imgData(spe)[,nodata], imgData(spe2)[,nodata])

    dat1 <- imgData(spe)[,"data"]
    dat2 <- imgData(spe2)[,"data"]
    expect_identical(imgRaster(dat1[[1]]), imgRaster(dat2[[1]]))
    expect_identical(imgRaster(dat1[[2]]), imgRaster(dat2[[2]]))
})

test_that("saving and reading works with fully loaded images", {
    copy <- spe
    imgData(copy)$data <- lapply(imgData(copy)$data, function(x) as(x, "LoadedSpatialImage"))
    
    tmp <- tempfile()
    saveObject(copy, tmp)
    spe2 <- readObject(tmp)

    dat1 <- imgData(spe)[,"data"]
    dat2 <- imgData(spe2)[,"data"]
    expect_identical(imgRaster(dat1[[1]]), imgRaster(dat2[[1]]))
    expect_identical(imgRaster(dat1[[2]]), imgRaster(dat2[[2]]))
})
