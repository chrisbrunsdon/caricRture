% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caricRture.R
\name{make_backdrop}
\alias{make_backdrop}
\title{Make an OpenStreetMap, Stamen or other map backdrop}
\usage{
make_backdrop(spdf,bw,sepia,...)
spdf %>% make_backdrop(bw,sepia,...)
}
\arguments{
\item{spdf}{a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object}

\item{bw}{A variable indicating whether to create a monochrome backdrop - 0 indicates use colour, 1 is standard brightness, other values vary brightness}

\item{sepia}{A variable indicating whether to create a sepia backdrop - 0 indicates use colour, 1 is standard brightness, other values vary brightness - this overwrites \code{bw}}

\item{...}{parameters passed on to \link[OpenStreetMap]{openmap}}
}
\value{
Echos \code{spdf} - this is useful for pipelines using \code{\%>\%}
}
\description{
Create a backdrop for sketchy maps,  based on the extent of \code{spdf}.  The backdrop
can be from OpenStreetMap, Google, Stamen, ESRI, bing, Apple or other sources
depending on location.
}
\details{
This function requires that \code{spdf} has a well-defined \code{proj4string} in
order to match the backdrop map with the extent of the spatial object.
}
\examples{
# Here make_canvas is inserted in a pipeline after a simplified and tidied 
# map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
# backdrop of type osm-bw (monochrome OpenStreetMap)
data(RA) 
RA.spdf \%>\% small_chop \%>\% gSimplify(tol=11000) \%>\% tidy_it \%>\% 
  make_backdrop(type='osm',sepia=0.8) \%>\% sketch_it(col='darkred',lwd=2,rough=0.08)
}

