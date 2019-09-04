#' Return whether or not a line from file looks like a metadata line
#'
#' Return whether or not a line from file looks like a metadata line
#'
#' @param line Line from file
#'
#' @return Boolean TRUE or FALSE
#'
isMetadataLine <- function(line=NULL) {
  if (is.null(line)) {
    stop("Input line of text must be given")
  }
  return (length(grep("^%+\\w+\t\\w+", line, perl=TRUE, value=TRUE)) > 0)
}

