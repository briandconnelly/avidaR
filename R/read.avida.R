library(stringr)

#' Read a file in Avida's output format
#'
#' \code{read.avida} reads the given file in Avida's output format and returns a
#' data table. Column names are assigned from comments within the data file
#' unless the \code{extract.colnames} parameter is set to \code{FALSE}.
#' Characters in column names that aren't letters, numbers, underscore, or
#' period are replaced by underscores.
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
	raw <- readLines(file)
	comments <- grep('^\\s*#', raw, value=TRUE)

	col_info <- str_match(comments, '^\\s*#\\s*(\\d+):\\s+(.*)\\s*$')
	col_names <- col_info[,3]
	col_names <- col_names[!is.na(col_names)]
    
    # Replace space characters and punctuation characters with underscores
    col_names <- str_replace_all(col_names, '[^a-zA-Z0-9_.\\s]', '_')
    
	data <- read.table(file)
	
    if(extract.colnames)
    {
        names(data) <- col_names
    }
	
	return(data)
}
