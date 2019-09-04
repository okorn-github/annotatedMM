
test_that("We can read an AMM file with row/col names", {
  verysmallAMM <- system.file("extdata", "NIST_5x5.amm", package = "annotatedMM", mustWork = TRUE)
  sparseMat <- annotatedMM::readAMM(verysmallAMM)
  ## Descendant of Matrix
  expect_equal(TRUE, inherits(sparseMat,"Matrix"))
  ## Is a sparseMatrix
  expect_is(sparseMat, "sparseMatrix")
  ## Has row names, 5 of them
  expect_equal(5, length(row.names(sparseMat)))
  ## Has column names, 5 of them
  expect_equal(5, length(colnames(sparseMat)))
  ## Test row label identities
  expect_identical(c("Gene1","Gene2","Gene3","Gene4","Gene5"), row.names(sparseMat))
  ## Test column label identities
  expect_identical(c("SampleA","SampleB","SampleC","SampleD","SampleE"), colnames(sparseMat))
  ## Spot test some values
  expect_equal(c(0,33.32,-280,0), c(sparseMat[1,2],sparseMat[4,5],sparseMat[4,4],sparseMat[5,1]))
})


test_that("We can also read a normal MM file", {
  verysmallMM <- system.file("extdata", "NIST_5x5.mtx", package = "annotatedMM", mustWork = TRUE)
  sparseMat <- annotatedMM::readAMM(verysmallMM)
  ## Descendant of Matrix
  expect_equal(TRUE, inherits(sparseMat,"Matrix"))
  ## Is a sparseMatrix
  expect_is(sparseMat, "sparseMatrix")
  ## Has 5 rows of data
  expect_equal(5, nrow(sparseMat))
  ## Has 5 columns of data
  expect_equal(5, ncol(sparseMat))
  ## Test row label identities - should be NULL
  expect_identical(NULL, row.names(sparseMat))
  ## Test column label identities - should be NULL
  expect_identical(NULL, colnames(sparseMat))
  ## Spot test some values
  expect_equal(c(0,33.32,-280,0), c(sparseMat[1,2],sparseMat[4,5],sparseMat[4,4],sparseMat[5,1]))
})


test_that("We can read an all-zero MM file", {
  ## All zero means every matrix position is zero - there should be no data
  zeroMM <- system.file("extdata", "zero.mtx", package = "annotatedMM", mustWork = TRUE)
  sparseMat <- annotatedMM::readAMM(zeroMM)
  expect_equal(TRUE, all(as.vector(sparseMat) == 0))
})


test_that("We get an error when input file is not found", {
  notfound <- "thisfileisnotsupposedtoexist.mtx"
  expect_error(annotatedMM::readAMM(notfound))
})


test_that("We get an error when input file is missing header", {
  badMM <- system.file("extdata", "NIST_bad_noheader.mtx", package = "annotatedMM", mustWork = TRUE)
  ## File has no %%MatrixMarket header and is not valid
  ## Should be intercepted by Matrix::readMM()
  expect_error(annotatedMM::readAMM(badMM))
})


test_that("We get an error when input file header is invalid", {
  badMM <- system.file("extdata", "NIST_bad_invalid_header.mtx", package = "annotatedMM", mustWork = TRUE)
  ## File has invalid header: %%MatrixMarket matrix coordinate foo bar
  ## Should be intercepted by Matrix::readMM()
  expect_error(annotatedMM::readAMM(badMM))
})


test_that("We get an error when input file is corrupt", {
  badMM <- system.file("extdata", "corrupt.mtx", package = "annotatedMM", mustWork = TRUE)
  ## File is corrupt. Should be intercepted by Matrix::readMM()
  ## We suppress a warning which is most likely related to string locale
  ## not handling file contents (file contains simulated binary corruption)
  expect_error(suppressWarnings(annotatedMM::readAMM(badMM)))
})


test_that("We get an error when input file is empty", {
  badMM <- system.file("extdata", "empty.mtx", package = "annotatedMM", mustWork = TRUE)
  ## File is empty. Should be intercepted by Matrix::readMM()
  expect_error(annotatedMM::readAMM(badMM))
})

