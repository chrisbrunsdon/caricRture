% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caricRture.R
\name{to_osm}
\alias{to_osm}
\title{Transform to OpenStreetMap (*Google Mercator*) Projection}
\usage{
to_osm(spdf)
spdf %>% to_osm
}
\arguments{
\item{spdf}{a \link[sp]{Spatial*} or \link[sp]{Spatial*DataFrame} object}
}
\value{
\code{spdf} transformed to 'Google Mercator' projection
}
\description{
Transforms a \link[sp]{Spatial} object 
to 'Google Mercator' projection - this makes the object work well with a
backdrop.
}
\examples{
data(all_ireland) 
# Transform to osm projection,  enjoy nice rectangular image...
all_ireland \%>\% to_osm \%>\% 
  make_backdrop(type='nps') \%>\% plot_it(border='navyblue')
}

