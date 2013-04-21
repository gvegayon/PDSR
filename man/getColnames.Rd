\name{getColnames}
\alias{getColnames}
\title{
Retrieve information of the variables from a .lbl file
}
\description{
Given a path to a .lbl file this function get the column names of the asociated .tab file. 
}
\usage{
getColnames(x)
}
\arguments{
  \item{x}{A string indicating the path of a .lbl file.}
}
\details{
this and that
}
\value{
A character vector of names.
}
% \references{}
\author{
George Vega
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
 \examples{
getColnames("data/apollo12_sws_28s_19760325.lbl")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line 