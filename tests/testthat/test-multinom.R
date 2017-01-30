context("Multinomial density helpers")

test_that("softmax versions are equivalent", {
  
  # Comparison softmax
  comp_softmax <- function(x){
    exp(x)/sum(exp(x))
  }
  
  # Test cases
  small_case <- 0:5
  neg_case <- -5:4
  na_case <- c(NA, 1)
  
  # Tests
  expect_equal(softmax(small_case), comp_softmax(small_case))
  expect_equal(softmax(neg_case), comp_softmax(neg_case))
  expect_equal(softmax(na_case), c(NA, NA)*1)
  
})

test_that("column selection works", {
  
  # Comparison function
  select_col <- function(mat, vec){
    mat[cbind(1:nrow(mat), vec)]
  }
  
  # Test case
  mat <- matrix(1:20, 4, 5)
  vec <- c(1, 3, 4, 5)
  
  # Check equal
  expect_equal(select_col(mat, vec), dMultinom(vec, mat, log = FALSE))
  
})

