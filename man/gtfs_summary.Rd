\name{gtfs_summary}
\alias{gtfs_summary}
\docType{data}
\title{
Tidy GTFS data
}
\description{
This function retrieves the total number of trips within a specified date and time interval, along with the corresponding routes for each stop.
}
\usage{
gtfs_summary(gtfs, gtfs_mode, test_date, time_period, tz)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{gtfs}{A list of GTFS data, containing all the required elements (i.e., \code{stops}, \code{stop_times}, \code{routes}, \code{calendar}, \code{calendar_dates}, and \code{trips}). This can be generated using function \code{read_gtfs()}.}
  \item{gtfs_mode}{A user-defined table specifying the classification of transport modes. The table must include three fields: \code{route_type}, \code{mode_type}, and \code{mode}.\cr \code{route_type} corresponds to the field in the GTFS \code{routes.txt} file. \code{mode_type} follows the definitions in the \href{https://developers.google.com/transit/gtfs/reference/extended-route-types?hl=en}{Google Extended GTFS Route Types} specification and related GTFS documentation, such as the fileset consumer guide provided by \href{https://opendata.transport.nsw.gov.au/sites/default/files/2024-10/TfNSW_Realtime_Bus_Technical_Doc_v4.4.pdf}{Transport for New South Wales, Australia}. \code{mode} allows three values: \code{bus}, \code{rail}, and \code{ferry}.}
  \item{test_date}{The date used for PTAL calculation. The format must be \code{YYYY-MM-DD}. This can be set to the first Tuesday in December, implemented by \href{https://opendata.transport.nsw.gov.au/data/dataset/ptal-public-transport-accessibility-level/resource/2865ead7-6ad2-4d94-9907-19b62381d30d}{Transport for New South Wales} in Australia.}
  \item{time_period}{The time period for PTAL calculation. The format must be \code{hh:mm~hh:mm}. The default value is \code{08:15~09:15} suggested in \href{https://content.tfl.gov.uk/connectivity-assessment-guide.pdf}{PTAL Guidebook} published by Transport for London.}
  \item{tz}{The time zone specification in GTFS data. See \href{https://www.wikiwand.com/en/articles/List_of_tz_database_time_zones}{time zones}.}
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
gtfs_summary(gtfs, test_date="2025-12-02", time_period="08:00~09:00", tz="Australia/Sydney")
}
\keyword{network}

