\name{exploreMission}
\alias{exploreMission}
\title{
Explore missions datasets
}
\description{
This function parser the labels files extracting information about the asociated data.
}
\usage{
exploreMission(dataid, fullMissions=NULL, maxdep = 1)
}
\arguments{
  \item{dataid}{A character indicating the path of a .lbl object.}
  \item{fullMissions}{A character indicating the path of a .lbl object.}
  \item{maxdep}{Integer of the maximum number of recursions.}
}
\details{
This is a recursive function which use regex to perform the extraction of the information.
}
\value{
A nested list with the mission's corresponding data (folder) FTP tree.
}
% \references{}
\author{
George G. Vega
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
\examples{
\dontrun{
parseLbl("apollo12_sws_28s_19760325")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line