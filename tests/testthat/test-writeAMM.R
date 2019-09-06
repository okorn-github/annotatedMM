# Example 5x5 values from NIST example:
#
#	1    0      0       6      0 
#	0   10.5    0       0      0
#	0    0    .015      0      0
#	0  250.5    0     -280    33.32
#	0    0      0       0     12

test_that("Writing a valid matrix with row/col names should work", {
  outAMM <- tempfile("NIST_5x5", fileext=".amm")
  verysmallMat <- matrix(
    c(1,0,0,0,0,0,10.5,0,250.5,0,0,0,0.15,0,0,6,0,0,-280,0,0,0,0,33.32,12),
    nrow=5,
    ncol=5
  )
  row.names(verysmallMat) <- c("Gene1","Gene2","Gene3","Gene4","Gene5")
  colnames(verysmallMat) <- c("SampleA","SampleB","SampleC","SampleD","SampleE")

  retVal <- annotatedMM::writeAMM(as(verysmallMat,"sparseMatrix"), outAMM)
  #message("DEBUG: Got retVal=[",retVal,"]")
  expect_equal(TRUE, retVal)

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})


test_that("Writing a matrix without row/col names should work", {
  outAMM <- tempfile("NIST_5x5_nometa", fileext=".amm")
  verysmallMat <- matrix(
    c(1,0,0,0,0,0,10.5,0,250.5,0,0,0,0.15,0,0,6,0,0,-280,0,0,0,0,33.32,12),
    nrow=5,
    ncol=5
  )

  retVal <- annotatedMM::writeAMM(as(verysmallMat,"sparseMatrix"), outAMM)
  #message("DEBUG: Got retVal=[",retVal,"]")
  expect_equal(TRUE, retVal)

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})


test_that("Writing a matrix having ONLY row names should work", {
  outAMM <- tempfile("NIST_5x5_rnames", fileext=".amm")
  verysmallMat <- matrix(
    c(1,0,0,0,0,0,10.5,0,250.5,0,0,0,0.15,0,0,6,0,0,-280,0,0,0,0,33.32,12),
    nrow=5,
    ncol=5
  )
  row.names(verysmallMat) <- c("Gene1","Gene2","Gene3","Gene4","Gene5")

  retVal <- annotatedMM::writeAMM(as(verysmallMat,"sparseMatrix"), outAMM)
  #message("DEBUG: Got retVal=[",retVal,"]")
  expect_equal(TRUE, retVal)

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})


test_that("Writing a matrix having ONLY column names should work", {
  outAMM <- tempfile("NIST_5x5_cnames", fileext=".amm")
  verysmallMat <- matrix(
    c(1,0,0,0,0,0,10.5,0,250.5,0,0,0,0.15,0,0,6,0,0,-280,0,0,0,0,33.32,12),
    nrow=5,
    ncol=5
  )
  colnames(verysmallMat) <- c("SampleA","SampleB","SampleC","SampleD","SampleE")

  retVal <- annotatedMM::writeAMM(as(verysmallMat,"sparseMatrix"), outAMM)
  #message("DEBUG: Got retVal=[",retVal,"]")
  expect_equal(TRUE, retVal)

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})


test_that("Writing an empty matrix results in error", {
  outAMM <- tempfile("empty", fileext=".amm")

  emptyMat <- matrix()
  #> emptyMat
  #       [,1]
  #[1,]   NA

  ## There is no value in writing this out. Matrix::writeMM() produces output:
  ##
  ## %%MatrixMarket matrix coordinate real symmetric
  ## 1 1 1
  ## 1 1 1e308
  ##
  ## So the "NA" ends up being coerced to maximum double value for some reason
  ## which is probably not what we want for an *empty* matrix.

  expect_equal(TRUE, all(is.na(as.vector(emptyMat))))

  expect_error(annotatedMM::writeAMM(as(emptyMat,"sparseMatrix"), outAMM))

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})


test_that("Writing a zero matrix should work", {
  outAMM <- tempfile("all_zero", fileext=".amm")
  zeroMat <- matrix(
    rep(0,25),
    nrow=5,
    ncol=5
  )
  ## suppress warning related to lack of row/col names
  retVal <- annotatedMM::writeAMM(as(zeroMat,"sparseMatrix"), outAMM)
  #message("DEBUG: Got retVal=[",retVal,"]")
  expect_equal(TRUE, retVal)

  if (file.exists(outAMM)) {
    file.remove(outAMM)
  }
})

