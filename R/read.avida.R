library(stringr)

#' Read a file in Avida's output format
#'
#' \code{read.avida} reads the given file in Avida's output format and returns a data table.
#'
#' @export
#' @seealso \code{\link{read.table}}
#' @param file the name of the file which the data are to be read from
#' @return A data frame (\code{\link{data.frame}}) containing a representation of the data in the file
#' @examples
#' data <- read.avida('resource.dat')
#' 
read.avida <- function(file)
{
	raw <- readLines(file)
	comments <- grep('^\\s*#', raw, value=TRUE)

	col_info <- str_match(comments, '^\\s*#\\s*(\\d+):\\s+(.*)\\s*$')
	col_names <- col_info[,3]
	col_names <- col_names[!is.na(col_names)]
    
    # Replace space characters and punctuation characters with underscores
    col_names <- str_replace_all(col_names, '[^a-zA-Z0-9_.\\s]', '_')
    
	data <- read.table(file)
	names(data) <- col_names
	
	return(data)
}
