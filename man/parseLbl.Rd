\name{parseLbl}
\alias{parseLbl}
\title{
Parser for .lbl files.
}
\description{
This function parser the labels files extracting information about the asociated data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parseLbl(x)
}
\arguments{
  \item{x}{A character indicating the path of a .lbl object.}
}
\details{
This is a recursive function which use regex to perform the extraction of the information.
}
\value{
A nested list with the information.
}
% \references{}
\author{
George Vega
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
\examples{
parseLbl("data/apollo12_sws_28s_19760325.lbl")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line