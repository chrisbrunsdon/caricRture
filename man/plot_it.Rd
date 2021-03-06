% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caricRture.R
\name{plot_it}
\alias{plot_it}
\title{Plotting function - pipeline friendly}
\usage{
plot_it(spdf,...)
spdf %>% plot_it(...)
}
\arguments{
\item{spdf}{A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame}}

\item{...}{Other parameters to pass on to \link[graphics]{plot}}
}
\value{
as \code{spdf}
}
\description{
Works like \code{plot} but returns the input object (good in pipelines).
Requires \link{make_canvas} to have been called (probably in a pipeline).
}
\examples{
data(all_ireland)
all_ireland \%>\% make_canvas \%>\% plot_it
}

