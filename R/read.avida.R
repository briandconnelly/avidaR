library(stringr)

#' Read a file in Avida's output format
#'
#' \code{read.avida} reads the given file in Avida's output format and returns a
#' data table. Column names are assigned from comments within the data file
#' unless the \code{extract.colnames} parameter is set to \code{FALSE}.
#' Invalid column names will be replaced according to \code{\link{make.names}}.
#'
#' @export
#' @seealso \code{\link{read.table}}
#' @param file the name of the file which the data are to be read from
#' @param extract.colnames Boolean indicating whether or not column names should
#' be extracted from the comments in the data file (\code{TRUE}) or not 
#' (\code{FALSE}) (default: \code{TRUE}).
#' @return A data frame (\code{\link{data.frame}}) containing a representation of the data in the file
#' @examples
#' data <- read.avida('resource.dat')
#' 
read.avida <- function(file, extract.colnames=TRUE)
{    
	data <- read.table(file)
	
    if(extract.colnames)
    {
        raw <- readLines(file)
        comments <- grep('^\\s*#', raw, value=TRUE)
        
        col_info <- str_match(comments, '^\\s*#\\s*(\\d+):\\s+(.*)\\s*$')
        col_names <- col_info[,3]
        col_names <- col_names[!is.na(col_names)]
        
        # Make sure the column names are syntactically valid
        col_names <- make.names(col_names)
        
        names(data) <- col_names
    }
	
	return(data)
}
