\name{read.avida}
\alias{read.avida}
\title{Read a file in Avida's output format}
\usage{
read.avida(file, extract.colnames = TRUE)
}
\arguments{
  \item{file}{the name of the file which the data are to be
  read from}

  \item{extract.colnames}{Boolean indicating whether or not
  column names should be extracted from the comments in the
  data file (\code{TRUE}) or not (\code{FALSE}) (default:
  \code{TRUE}).}
}
\value{
A data frame (\code{\link{data.frame}}) containing a
representation of the data in the file
}
\description{
\code{read.avida} reads the given file in Avida's output
format and returns a data table. Column names are assigned
from comments within the data file unless the
\code{extract.colnames} parameter is set to \code{FALSE}.
Invalid column names will be replaced according to
\code{\link{make.names}}.
}
\examples{
data <- read.avida('resource.dat')
}
\seealso{
\code{\link{read.table}}
}

