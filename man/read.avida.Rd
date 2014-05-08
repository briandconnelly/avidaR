\name{read.avida}
\alias{read.avida}
\title{Read a file in Avida's output format}
\usage{
read.avida(file)
}
\arguments{
  \item{file}{the name of the file which the data are to be
  read from}
}
\value{
A data frame (\code{\link{data.frame}}) containing a
representation of the data in the file
}
\description{
\code{read.avida} reads the given file in Avida's output
format and returns a data table.
}
\examples{
data <- read.avida('resource.dat')
}
\seealso{
\code{\link{read.table}}
}

