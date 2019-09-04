---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# annotatedMM

<!-- badges: start -->
<!-- badges: end -->

The `annotatedMM` package provides an interface to work with our "annotated" Matrix Market exchange format which we call "AMM".

The Matrix Market exchange format (variously referred to as MM, MTX or MEX) are formats typically used to represent sparse matrices in an efficient manner (since zero matrix positions are not reported).

One major drawback of the format however is that it does not describe a way to store useful metadata such as row or column names.

Our "annotated" MM format (AMM) makes use of comment lines (those typically at the top of MM files starting with `'%'`) - to store this metadata.

Since comment lines are used, any software not understanding our metadata injection formats will simply ignore these lines which may or may not be desired, but at the very least, our "AMM" file is a valid "MM" file.

Metadata stored by default includes row and column names of sparse matrices (dGCMatrix, sparseMatrix etc - formats supported by `Matrix::writeMM()` and `Matrix::readMM()`).

It is planned but not currently supported by `annotatedMM` to allow users to inject custom metadata. Some careful design of a metadata format specification will be required before this is implemented.

Row and column metadata lines are formatted as follows:

```
%ROWNAMES<tab>Row1<tab>Row2<tab>...
%COLNAMES<tab>Sample1<tab>Sample2<tab>...
```

Note: `<tab>` in the above represents actual tabstops.


## Installation

You can install the released version of annotatedMM from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("annotatedMM")
```

## Example

```
  ## 1 0 4
  ## 2 0 5
  ## 3 0 6.66
  mat <- matrix(
    c(1,2,3,0,0,0,4,5,6.66),
    nrow=3,
    ncol=3
  )
  colnames(mat) <- c("A","B","C")
  row.names(mat) <- c("one","two","three")
  outFile <- tempfile("out.amm")
  annotatedMM::writeAMM(as(mat,"Matrix::sparseMatrix"), outFile)))
  # TRUE

  newMat <- annotatedMM::readAMM(outFile)
  colnames(newMat)
  # [1] "A" "B" "C"
  row.names(newMat)
  # [1] "one" "two" "three"
```
