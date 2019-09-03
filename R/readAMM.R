#' Read an "Annotated" Matrix Market exchange file and return a sparse matrix
#'
#' TODO: Ensure documentation is accurate
#' Reads an "Annotated" Matrix Market file. If row and column names were 
#' recorded in the file, they will be attached to the returned sparse matrix
#' object.
#'
#' @param fileName name of file to read
#'
#' @return A sparse matrix object of type returned by Matrix::readMM
#'
#' @export
readAMM <- function(fileName=NULL) {
  if (is.null(fileName)) {
    stop("'fileName' argument is required")
  }
  ## TODO: This is just a placeholder!
  return(Matrix::readMM(fileName))
}
