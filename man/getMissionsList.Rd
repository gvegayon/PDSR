\name{getMissionsList}
\alias{getMissionsList}
\title{
Listing all the missions URL
}
\description{
Retrieves NASA's PDS FTP structure 
}
\usage{
getMissionsList(maxdep=1)
}
\arguments{
  \item{maxdep}{Integer of the maximum number of recursions}
}
\details{
Matches \code{dataid} with \code{fullMissions} looking for its 
}
\value{
A nested list of PDS FTP structure.
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
getMissionsList()
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line