\name{getMissionURL}
\alias{getMissionURL}
\title{
What is this mission URL?
}
\description{
Given a dataid, returns the corresponding root FTP path of the mission
}
\usage{
getMissionURL(dataid, fullMissions=NULL)
}
\arguments{
  \item{dataid}{A character indicating the path of a .lbl object.}
  \item{fullMissions}{A character indicating the path of a .lbl object.}
}
\details{
Matches \code{dataid} with \code{fullMissions} looking for its 
}
\value{
A list with the mission's corresponding FTP URL.
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
getMissionURL("apollo12_sws_28s_19760325")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line