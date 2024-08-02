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

test_that("saving and reading respects SCE fields", {
    tmp <- tempfile()
    reducedDim(spe, "TSNE") <- matrix(runif(ncol(spe) * 2), ncol=2)
    altExp(spe, "ERCC", withDimnames=FALSE) <- SummarizedExperiment(list(counts=matrix(rpois(10 * ncol(spe), lambda=1), ncol=ncol(spe))))
    saveObject(spe, tmp)

    spe2 <- readObject(tmp)
    expect_identical(reducedDim(spe, "TSNE"), as.matrix(reducedDim(spe2, "TSNE")))
    expect_identical(altExpNames(spe), "ERCC")
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

setClass("TestSpatialImage", contains="LoadedSpatialImage")

setMethod("saveObject", "TestSpatialImage", function(x, path, ...) {
    dir.create(path)
    saveObjectFile(path, "test_image") 
    ras <- imgRaster(x)
    Y <- col2rgb(as.matrix(ras))
    Y <- t(Y)
    Y <- Y / 255
    dim(Y) <- c(dim(ras), ncol(Y)) 
    dest <- file.path(path, "image.png")
    png::writePNG(Y, target=dest)
})

test_that("saving and reading works with custom image classes", {
    copy <- spe
    imgData(copy)$data <- lapply(imgData(copy)$data, function(x) as(as(x, "LoadedSpatialImage"), "TestSpatialImage"))

    registerValidateObjectFunction("test_image", function(path, metadata, ...) {})
    on.exit(registerValidateObjectFunction("test_image", NULL), add=TRUE, after=FALSE)
    registerValidateObjectSatisfiesInterface("test_image", "IMAGE")
    on.exit(registerValidateObjectSatisfiesInterface("test_image", "IMAGE", action="remove"))
    registerReadObjectFunction("test_image", function(path, metadata, ...) as(as(SpatialImage(file.path(path, "image.png")), "LoadedSpatialImage"), "TestSpatialImage"))
    on.exit(registerReadObjectFunction("test_image", NULL), add=TRUE, after=FALSE)

    tmp <- tempfile()
    saveObject(copy, tmp)
    spe2 <- readObject(tmp)

    dat1 <- imgData(spe)[,"data"]
    dat2 <- imgData(spe2)[,"data"]
    expect_s4_class(dat2[[1]], "TestSpatialImage")
    expect_s4_class(dat2[[2]], "TestSpatialImage")
    expect_identical(imgRaster(dat1[[1]]), imgRaster(dat2[[1]]))
    expect_identical(imgRaster(dat1[[2]]), imgRaster(dat2[[2]]))
})
