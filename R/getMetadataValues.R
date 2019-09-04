#' Return metadata values as vector of tokens
#'
#' Return metadata values as vector of tokens
#'
#' @param line Metadata line
#'
#' @return A string corresponding to the metadata key
#'
getMetadataValues <- function(line=NULL) {
  if (is.null(line)) {
    stop("Input line of text must be given")
  }
  return(strsplit(line, "\t")[[1]][-1])
}

