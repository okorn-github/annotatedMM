#' Write an "Annotated" Matrix Market exchange file
#'
#' Writes a sparseMatrix (e.g. dgCMatrix object) to an "Annotated" Matrix
#' Market file ("AMM"). Row and column names are written to the output AMM
#' file. An AMM file is still a valid MM file so the output should inter-
#' operate with other tools albeit in a reduced capacity, for instance row
#' and column names would not be parsed as they are stored in the comments
#' section of the file. The numeric content of the file should always be
#' preserved, however. NOTE: The Matrix Market exchange format is variously
#' known as "MM", "MTX" or "MEX".
#'
#' @param object sparseMatrix object to write to file
#' @param fileName name of file to create
#'
#' @return TRUE on success, FALSE otherwise
#'
#' @export
writeAMM <- function(object=NULL, fileName=NULL) {
  if (is.null(object) || is.null(fileName)) {
    stop("'object' and 'fileName' arguments are both required")
  }
  ## Ensure input is sparseMatrix
  if (! is(object, "sparseMatrix")) {
    stop("Input must be a sparseMatrix or descendant type")
  }
  ## Don't accept empty matrices
  if (nrow(object) == 1 && ncol(object) == 1) {
    if (all(is.na(as.vector(object)))) {
      stop("Cannot write an empty matrix")
    }
  }

  pid <- Sys.getpid()
  mmbodyName <- paste0(fileName,".body.",pid)
  Matrix::writeMM(object, file=mmbodyName)

  mmheaderName <- paste0(fileName,".header.",pid)
  ## Open MM file so we can copy the header (comment lines) to our new file
  ## without changes. This should be efficient since the comment lines are at
  ## the start of the file.
  mmConReadOnly <- file(description=mmbodyName, open="r", blocking=TRUE)
  line <- getCommentLineMM(mmConReadOnly)
  ## We stop when the first non-comment line (line starting other than '%' is
  ## encountered). Any comment lines interspersed with data lines (if any)
  ## should be preserved because we don't actually remove any lines from the
  ## output of Matrix::writeMM call
  while (length(line)) {
    cat(line, file=mmheaderName, append=TRUE)
    line <- getCommentLineMM(mmConReadOnly)
  }
  cat("\n", file=mmheaderName, append=TRUE)
  close(mmConReadOnly)
  ## Insert custom metadata before the first data line. By default, we include
  ## the row names and column names that are defined for input "object"
  ## Future TODO: Args to allow override of row/col names
  rowNames <- dimnames(object)[[1]]
  if (! length(rowNames)) {
    message("Input object has no row names, none will be written")
  }
  colNames <- dimnames(object)[[2]]
  if (! length(colNames)) {
    message("Input object has no column names, none will be written")
  }
  if (length(rowNames) > 0) {
    rnames <- paste0(c("%ROWNAMES",rowNames), collapse='\t')
    cat(rnames, file=mmheaderName, append=T)
  }
  if (length(colNames) > 0) {
    cnames <- paste0(c("%COLNAMES",colNames), collapse='\t')
    cat(paste0("\n",cnames), file=mmheaderName, append=T)
  }
  ## Add a newline unless we didn't write anything
  if (length(rowNames) || length(colNames)) {
    cat("\n", file=mmheaderName, append=TRUE)
  }

  ## Work out how to "cat" on the Shell based on our OS (mainly about ensuring
  ## this works on Windows).
  ## Linux and MacOS will end up using "cat". Not sure about other *nix-like
  ## systems but 99% sure that "cat" is a reasonable expectation.
  catName <- "cat"
  if (.Platform["OS.type"] == "windows") {
    catName <- "type"
  }
  ## Merge new header and existing MM file body contents. Am unaware of a
  ## native R way to to do this, hence the system call.
  ## NOTE: The original %%MatrixMarket header (from writeMM output) will appear
  ## *again* AFTER our inserted header and metadata lines.
  ## There is no reasonable platform-independent, native "sed"-like tool and no
  ## R-based "sed" function, so that precludes our ability to simply remove
  ## that single line without scanning the entire MTX file and skipping one
  ## line.
  ## We assume that the superfluous "%%MatrixMarket" line will be ignored by
  ## any downstream consumer where our AMM is treated like a standard MM file.
  retval <- system(paste0(catName," ",mmheaderName," ",mmbodyName," > ",fileName))

  file.remove(c(mmheaderName, mmbodyName))
  return(retval == 0)
}

