\name{alabaster.spatial News}
\title{alabaster.spatial News}
\encoding{UTF-8}

\section{Version 1.6.0}{\itemize{
\item Respect application-level overrides when saving the SingleCellExperiment base from the SpatialExperiment's \code{saveObject} method.

\item Dispatch to \code{saveObject()} for any of the SpatialExperiment's images that implement a dedicated method for it.
}}
