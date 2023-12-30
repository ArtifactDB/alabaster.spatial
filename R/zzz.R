.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_experiment", readSpatialExperiment)
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_experiment", NULL)
}
