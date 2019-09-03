#' Write an "Annotated" Matrix Market exchange file
#'
#' TODO: Ensure documentation is accurate
#' Writes a sparseMatrix (e.g. dgCMatrix object) to an "Annotated" Matrix Market
#' file. Row and column names are written to the output "AMTX" file. An AMTX file
#' is still a valid MTX file so the output should interroperate with other tools
#' albeit in a reduced capacity, for instance row and column names would not
#' be parsed as they are stored in the comments section of the file. The numeric
#' content of the file should always be preserved however. NOTE: The Matrix
#' Market exchange format is variously referred to as "MM", "MTX" or "MEX".
#'
#' @param object sparse matrix object to write to file, any type supported by Matrix::writeMM
#' @param fileName name of file to create (will add "amtx" extension if no extension given)
#'
#' @return TRUE on success, FALSE otherwise
#'
#' @export
writeAMM <- function(object=NULL, fileName=NULL) {
  if (is.null(object) || is.null(fileName)) {
    stop("'object' and 'fileName' arguments are both required")
  }
  ## Add extension if no extension given
  if (! length(grep(".", fileName, fixed=TRUE))) {
    warning("No file extension given, adding '.amtx' to output file name")
    fileName <- paste0(fileName,".amtx")
  }
  pid <- Sys.getpid()
  mtxbodyName <- paste0(file,".body.",pid)
  Matrix::writeMM(object, file=mtxbodyName)

  ## TODO: Move to own source file or otherwise package appropriately
  getCommentLineAMM <- function(readConn, nLines=1) {
    retLines <- c()
    for (i in seq(1:nLines)) {
      val <- readLines(readConn, n=1)
      if (length(grep("^%", val, value=TRUE))) {
        retLines <- c(retLines, val)
      }
    }
    return(retLines)
  }

  mtxheaderName <- paste0(file,".header.",pid)
  mtxheader <- file(mtxheaderName)
  ## Open MTX file so we can copy the header (comment lines) to our new file
  ## without changes
  mtxConReadOnly <- file(description=mtxbodyName, open="r", blocking=TRUE)
  line <- getCommentLineAMM(mtxConReadOnly)
  ## We stop when the first non-comment line (line starting other than '%' is
  ## encountered). Any comment lines interspersed with data lines (if any)
  ## should be preserved because we don't actually remove any lines from the
  ## output of Matrix::writeMM call
  while (length(line)) {
    cat(line, file=mtxheaderName, append=TRUE)
    line <- getCommentLineAMM(mtxConReadOnly)
  }
  close(mtxConReadOnly)
  ## Insert custom metadata before the first data line. By default, we include
  ## the row names and column names that are defined for input "object"
  ## TODO: Args to allow override of row/col names
  rowNames <- row.names(object)
  if (! length(rowNames)) {
    warning("Input object has no row names, none will be written")
  }
  colNames <- colnames(object)
  if (! length(colNames)) {
    warning("Input object has no column names, none will be written")
  }
  rnames <- paste0(c("%ROWNAMES",rowNames), collapse='\t')
  cnames <- paste0(c("%COLNAMES",colNames, collapse='\t'))
  cat(paste0("\n",rnames), file=mtxheaderName, append=T)
  cat(paste0("\n",cnames,"\n"), file=mtxheaderName, append=T)

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
  ## any downstream consumer where our AMTX is treated like a standard MTX.
  retval <- system(paste0(catName," ",mtxheaderName," ",mtxbodyName," > ",file))

  ## TODO: Reinstate temp file deletion after testing
  #file.remove(c(mtxheaderName,mtxbodyName))
  return(retval == 0)
}

