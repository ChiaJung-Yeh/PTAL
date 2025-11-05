\name{Planit_Setup}
\alias{Planit_Setup}
\docType{data}
\title{
Set up PLANit-Python environment
}
\description{
Set up the environment for executing PLANit to extract the OSM network.
If an existing environment is detected, it will be reused automatically.
Otherwise, a new environment will be created and install all the packages required in python.
Please run this code only once per session.
If an error occurs, restart R before running it again.
}
\usage{
Planit_Setup(env)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{env}{The enviroment name for python to execute PLANit.}
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
# Set up the environment
Planit_Setup("OSM_Network")
}
\keyword{planit}

