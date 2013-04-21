\name{plot.PDS}
\alias{plot.PDS}
\title{
Ploting a PDS object
}
\description{
This function make a plot of a PDS object.
}
\usage{
\method{plot}{PDS}(x, variables=sample(names(object$table), size =
                 min(c(4, ncol(object$table)))), type = c("ggpairs",
                 "plotmatrix", "time"), ...)
}
\arguments{
  \item{x}{A PDS Object.}
  \item{variables}{A character vector with names of variables in the object$table data.frame.}
  \item{type}{A string indicating the type of plot between "ggpairs", "plotmatrix" and "time".}
  \item{...}{Other arguments of plot (unused)}
}
\details{
ggpairs is 
}
\value{
Depending on the type of plot, this function can return a ggplot oject or ggpairs object (from ggplot2 and ggpairs packages respectively)
}
% \references{}
\author{
Joshua B. Kunst
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
\examples{
\dontrun{
object <- readPDStable("data/apollo12_sws_1hr_1976c9388")
plot.PDS(object, type= "time")
}
}

% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line