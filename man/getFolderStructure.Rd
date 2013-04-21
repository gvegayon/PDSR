\name{getFolderStructure}
\alias{getFolderStructure}
\title{
Get the Folder Structure of an ftp
}
\description{
This function can be used to get the folder structure given a the URL of an FTP in a list form.
}
\usage{
getFolderStructure(url)
}
\arguments{
  \item{url}{A character string naming the URL of a FTP.}
}
\details{
this and that
}
\value{
Nested list containing folder and their folder and files.
}
% \references{}
\author{
Joshua B. Kunst
}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{\code{\link{help}}}
\examples{
\dontrun{getFolderStructure("ftp://pds-geosciences.wustl.edu/earth/grsfe/")}
}

% R documentation directory.
\keyword{ IO }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line