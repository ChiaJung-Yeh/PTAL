\name{OSM_Network}
\alias{OSM_Network}
\docType{data}
\title{
Retrieve network data from OpenStreetMap
}
\description{
This function first downloads the data for the specified country and district from \href{https://download.geofabrik.de/}{Geofabrik}.
It then clips the OSM data to the defined boundary to extract all highway features.
Finally, it converts the line geometries into a routable network dataset.
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
  \item{bbox}{The boundary used to clip the OSM data when converting it into network data. It is highly recommended to specify this boundary to prevent potential out-of-memory issues. The CRS of a boundary box should be \code{EPSG:4326}. This is an optional argument. If it is not set, the network dataset for the entire country/district will be parsed.}
  \item{out}{The directory to export all files, including node, link, segment, and network data.}
}
\author{
Maintainer: Chia-Jung (Robert), Yeh  \email{chia-jung.yeh@sydney.edu.au}
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
# Make sure the CRS of a box is EPSG:4326
area_box=st_bbox(st_transform(greater_sydney, crs=4326))

# Retrieve Greater Sydney road network
OSM_Network(country="Australia", bbox=area_box)
}
\keyword{network}

