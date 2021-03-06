\name{fullMissionsList}
\alias{fullMissionsList}
\docType{data}
\title{
Nested List containing NASA Missions
}
\description{
This list is used to lookup specific datasets IDs in order to get access to their FTP path.
}
\usage{data(fullMissionsList)}
\format{
  A nested list containing NASA's PDS FTP paths.
}
\source{
\itemize{
\item{Atmospheres Node \url{ftp://pds-atmospheres.nmsu.edu/}}
\item{Geosciences Node \url{ftp://pds-geosciences.wustl.edu/}}
\item{Planetary Plasma Interaction (PPI) Node \url{ftp://pds-ppi.igpp.ucla.edu/}}
\item{Planetary Rings Node \url{ftp://pds-rings.seti.org/}}
    }
}
\examples{
data(fullMissionsList)
}
\keyword{datasets}
