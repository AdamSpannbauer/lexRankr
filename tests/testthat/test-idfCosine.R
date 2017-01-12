context("lexRankr:::idfCosineSimil")

# test bad inputs ---------------------------------------
test_that("bad inputs to idf cosine", {
  expect_error(lexRankr:::idfCosineSimil(NULL))
  
  badMat <- matrix(c("a","b","c","d"), nrow=2)
  expect_error(lexRankr:::idfCosineSimil(badMat))
})

# test object out str and class ---------------------------------------
test_that("object out str and class", {
  testMat <- matrix(runif(9, min = .01, max = 1), nrow=3)
  testResult <- lexRankr:::idfCosineSimil(testMat)
  
  expect_equal(class(testResult), "numeric")
  expect_equal(length(testResult), 3)
})

# test object out value
test_that("object out value", {
  testMat <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3)
  expect_equal(lexRankr:::idfCosineSimil(testMat), c(0,0,0))
  
  testMat <- matrix(c(0,0,0,0,0,0,0,0,0), nrow=3)
  expect_equal(lexRankr:::idfCosineSimil(testMat), c(NaN,NaN,NaN))

  testMat <- matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
  expect_equal(lexRankr:::idfCosineSimil(testMat), c(1,1,1))
  
  testMat <- matrix(runif(9, min = .01, max = 1), nrow=3)
  rcppIdf <- round(lexRankr:::idfCosineSimil(testMat), 10)
  #pure r version comparison
  idfCosine <- function(x,y) {
    res <- sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
    return(round(res, 10))
  }
  
  elem1 <- idfCosine(testMat[1,], testMat[2,])
  elem2 <- idfCosine(testMat[1,], testMat[3,])
  elem3 <- idfCosine(testMat[2,], testMat[3,])
  
  expect_equal(rcppIdf, c(elem1, elem2, elem3))
})

