context("tokenize")

# test tokenize output classes ----------------------------------------
test_that("All clean options TRUE", {
  testDocs <- c("12345", "Testing 1, 2, 3.", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs)
  
  expect_equal(class(testResult), "list")
  
  expect_equal(unique(vapply(testResult, class, character(1))), "character")
})

# test bad input -------------------------------------------------------
test_that("test input checking", {
  expect_error(tokenize(NULL))
  expect_error(tokenize(data.frame(badInput="test")))
  
  expect_error(tokenize("test", removePunc=NULL))
  expect_error(tokenize("test", removeNum=NULL))
  expect_error(tokenize("test", toLower=NULL))
  expect_error(tokenize("test", stemWords=NULL))
  expect_error(tokenize("test", rmStopWords=NULL))
})

# test tokenize and arg option variations ------------------------------

test_that("All clean options TRUE", {
  testDocs <- c("Testing 1, 2, 3.", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=TRUE,
                         removeNum=TRUE,
                         toLower=TRUE,
                         stemWords=TRUE,
                         rmStopWords=TRUE)
  
  expectedResult <- list("test", c("work","expect","mr","wickham"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("All clean options FALSE", {
  testDocs <- c("Testing 1, 2, 3", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=FALSE,
                         removeNum=FALSE,
                         toLower=FALSE,
                         stemWords=FALSE,
                         rmStopWords=FALSE)
  
  expectedResult <- list(c("Testing", "1", ",", "2", ",", "3"), 
                         c("Is", "everything", "working", "as", "expected", "Mr", ".", "Wickham", "?"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("Single option tests: removePunc = FALSE", {
  testDocs <- c("Testing 1, 2, 3.", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=FALSE,
                         removeNum=TRUE,
                         toLower=TRUE,
                         stemWords=TRUE,
                         rmStopWords=TRUE)
  
  expectedResult <- list(c("test",",",",","." ), 
                         c("work","expect","mr",".","wickham","?" ))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("Single option tests: removeNum = FALSE", {
  testDocs <- c("Testing 1, 2, 3", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=TRUE,
                         removeNum=FALSE,
                         toLower=TRUE,
                         stemWords=TRUE,
                         rmStopWords=TRUE)
  
  expectedResult <- list(c("test","1","2","3"), 
                         c("work","expect","mr","wickham"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("Single option tests: toLower = FALSE", {
  testDocs <- c("Testing 1, 2, 3", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=TRUE,
                         removeNum=TRUE,
                         toLower=FALSE,
                         stemWords=TRUE,
                         rmStopWords=TRUE)
  
  expectedResult <- list(c("Test"), 
                         c("work","expect","Mr","Wickham"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("Single option tests: stemWords = FALSE", {
  testDocs <- c("Testing 1, 2, 3", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=TRUE,
                         removeNum=TRUE,
                         toLower=TRUE,
                         stemWords=FALSE,
                         rmStopWords=TRUE)
  
  expectedResult <- list(c("testing"), 
                         c("working","expected","mr","wickham"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

test_that("Single option tests: rmStopWords = FALSE", {
  testDocs <- c("Testing 1, 2, 3", "Is everything working as expected Mr. Wickham?")
  testResult <- tokenize(testDocs,
                         removePunc=TRUE,
                         removeNum=TRUE,
                         toLower=TRUE,
                         stemWords=TRUE,
                         rmStopWords=FALSE)
  
  expectedResult <- list(c("test"), 
                         c("i","everyth","work","a", "expect", "mr", "wickham"))
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "list")
})

