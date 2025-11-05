\name{OSM_Network}
\alias{OSM_Network}
\docType{data}
\title{
Retrieve network data from OpenStreetMap
}
\description{
This function first download the data based on given country and district name from Geofabrik.

}
\usage{
OSM_Network(country, district=NULL, bbox=NULL, out=FALSE)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{country}{The name of the country. Refer to \code{osm_data} for valid country names.}
  \item{district}{The name of the district. Refer to \code{osm_data} for valid district names. This argument is mandatory only if multiple districts exist in a country; otherwise, if set to \code{NULL} by default, the network dataset for the entire country will be parsed.}
  \item{bbox}{The boundary used to clip the OSM data when converting it into network data. It is highly recommended to specify this boundary to prevent potential out-of-memory issues.}
  \item{out}{The directory to export all files, including node, link, segment, and network data.}
}
\author{
Maintainer: Chia Jung, Yeh  \email{chia-jung.yeh@sydney.edu.au}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# Retrieve Greater Sydney Road Network

OSM_Network(country="Australia", bbox=NULL)
}
\keyword{planit}

