#' Read an "Annotated" Matrix Market exchange file and return a sparseMatrix
#'
#' Reads an "Annotated" Matrix Market file and returns a corresponding
#' sparseMatrix (or descendant e.g. dgCMatrix). If row and column names are
#' found they will be attached to the returned sparse matrix object.
#' If reading a normal Matrix Market exchange file, this function should
#' return result identical to Matrix::readMM.
#'
#' @param fileName name of Matrix Market file to read
#'
#' @return A sparseMatrix (or descendant) object of type returned by Matrix::readMM
#'
#' @export
readAMM <- function(fileName=NULL) {
  if (is.null(fileName) || fileName == "") {
    stop("'fileName' argument is required")
  }
  if (! file.exists(fileName)) {
    stop(paste0("Matrix Market file '",fileName,"' not found"))
  }

  metadata <- list()
  ## Open MM file so we can first extract metadata from the header e.g. row
  ## and column names
  mmConReadOnly <- file(description=fileName, open="r", blocking=TRUE)
  line <- annotatedMM::getCommentLineMM(mmConReadOnly)
  ## We stop when the first non-comment line (line starting other than '%' is
  ## encountered).
  while (length(line)) {
    if (annotatedMM::isMetadataLine(line)) {
      key <- annotatedMM::getMetadataName(line)
      vals <- annotatedMM::getMetadataValues(line)
      metadata[[key]] <- vals
    }
    line <- annotatedMM::getCommentLineMM(mmConReadOnly)
  }
  close(mmConReadOnly)

  sparseMat <- Matrix::readMM(fileName)

  newDimNames <- c(NULL, NULL)
  if ("ROWNAMES" %in% names(metadata)) {
    newDimNames[1] <- metadata["ROWNAMES"]
  }
  if ("COLNAMES" %in% names(metadata)) {
    newDimNames[2] <- metadata["COLNAMES"]
  }
  dimnames(sparseMat) <- newDimNames
  return(sparseMat)
}
