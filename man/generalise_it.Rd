% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caricRture.R
\name{generalise_it}
\alias{generalise_it}
\title{Generalise an object}
\usage{
generalise_it(spdf,tol)
spdf %>% generalise_it(tol)
}
\arguments{
\item{spdf}{A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object}

\item{tol}{The tolerance of the generalising algorithm.  The higher this is,  the cruder the generalisation.  For caricaturing,  it should be reasonably high - eg 10000 for the Irish National Grid in metres.}
}
\value{
A generalised \link[sp]{SpatialPolygons} object
}
\description{
Apply a generalisation algorithm to a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object.
}
\examples{
data(RA)
RA.spdf \%>\% small_chop \%>\% generalise_it(10000) \%>\% plot
}

