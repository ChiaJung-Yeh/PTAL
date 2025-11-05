\name{read_gtfs}
\alias{read_gtfs}
\docType{data}
\title{
Read GTFS data
}
\description{
Read GTFS data from local directory. The output data includes \code{stops}, \code{stop_times}, \code{routes}, \code{calendar}, \code{calendar_dates}, and \code{trips}, which are the essential data to calculate PTAL.
}
\usage{
read_gtfs(path)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{path}{The directory path to the GTFS file. The path can point to a \code{.zip} file, which will be automatically extracted by this function, or to a directory containing the already extracted GTFS files.}
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
gtfs("full_greater_sydney_gtfs_static_0.zip")
}
\keyword{network}

