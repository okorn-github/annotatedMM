#' Return 1:N comment lines from a Matrix Market file
#'
#' Return 1:N comment lines from a Matrix Market file
#'
#' @param readConn an open file connection
#' @param nLines optional number of lines to read, default 1
#'
#' @return A character vector of comment line/s starting from current file position
#'
getCommentLineMM <- function(readConn, nLines=1) {
  if (! isOpen(readConn)) {
    stop("File connection object must already be open")
  }
  retLines <- c()
  for (i in seq(1:nLines)) {
    val <- readLines(readConn, n=1)
    if (length(grep("^%", val, value=TRUE))) {
      retLines <- c(retLines, val)
    }
  }
  return(retLines)
}

