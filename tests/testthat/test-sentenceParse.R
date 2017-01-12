context("sentenceParse")

# test sentence object structure-----------------------------------------------

test_that("sentenceParse output class and structure check", {
  testDoc <- "Testing one, two, three. Is everything working as expected Mr. Wickham?"
  testResult <- sentenceParse(testDoc)
  
  expect_equal(class(testResult), "data.frame")
  
  expect_equal(names(testResult), c("docId", "sentenceId", "sentence"))
  
  expect_true(is.numeric(testResult$docId))
  expect_true(is.character(testResult$sentenceId))
  expect_true(is.character(testResult$sentence))
  
})

# test bad input -------------------------------------------------------
test_that("test input checking", {
  expect_error(sentenceParse(NULL))
  expect_error(sentenceParse(data.frame(badInput="test")))
  
  expect_error(sentenceParse("test", docId = c("fake","fake2")))
  expect_error(sentenceParse(c("test","test2"), docId = "fake"))
  expect_error(sentenceParse(c("test","test2"), docId = NULL))
})

# test sentence output value -----------------------------------------------

test_that("Example doc parses sentences as expected", {
  testDoc <- "Testing one, two, three. Is everything working as expected Mr. Wickham?"
  testResult <- sentenceParse(testDoc)
  
  expectedResult <- data.frame(docId = c(1L, 1L), 
                               sentenceId = c("1_1", "1_2"),
                               sentence = c("Testing one, two, three.", 
                                            "Is everything working as expected Mr. Wickham?"),
                               stringsAsFactors = FALSE)
  
  expect_equal(testResult, expectedResult)
  
  expect_equal(class(testResult), "data.frame")
  
  expect_equal
  
})