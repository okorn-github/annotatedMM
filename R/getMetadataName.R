#' Return metadata key name from input metadata line
#'
#' Return metadata key name from input metadata line
#'
#' @param line Metadata line
#'
#' @return A string corresponding to the metadata key
#'
getMetadataName <- function(line=NULL) {
  if (is.null(line)) {
    stop("Input line of text must be given")
  }
  return(gsub("^%+", "", strsplit(line, "\t")[[1]][1]))
}

