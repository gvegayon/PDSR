\name{readPDStable}
\alias{readPDStable}
\title{
Retrieve data into a PDS object.
}
\description{
Given a data id download the data from the ftp://pds-geosciences.wustl.edu/ and it convert into a PDS object
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readPDStable(dataid)
}
\arguments{
  \item{dataid}{String of PDS data id}
}
\details{
Given a header search for two files, a plain-text table (.tab) and plain text metadata file (.lbl) which describes table data.
}
\value{
A PDS object.
}
% \references{}
\author{
George G. Vega
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
% \examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line