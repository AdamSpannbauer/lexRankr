context("lexRank")

# test object out str and class ---------------------------------------
test_that("object out str and class", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  testResult <- lexRank(testDocs, Verbose = FALSE)
  
  expect_equal(class(testResult), "data.frame")
  expect_equal(names(testResult), c("docId","sentenceId", "sentence","value"))
  expect_true(is.character(testResult$sentenceId))
  expect_true(is.character(testResult$sentence))
  expect_true(is.numeric(testResult$value))
})

# test bad inputs ---------------------------------------
test_that("bad inputs", {
  expect_error(lexRank(FALSE, Verbose = FALSE))
  expect_error(lexRank(NULL, Verbose = FALSE))
})

# test object out value
test_that("object out value", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  
  testResult <- lexRank(testDocs, Verbose = FALSE) 
  testResult$value = round(testResult$value, 5)
  
  expectedResult <- data.frame(docId = c(2L, 1L, 3L),
                               sentenceId = c("2_1", "1_1", "3_1"),
                               sentence = c("Is everything working as expected in my test?", 
                                            "Testing 1, 2, 3.", "Is it working?"),
                               value = c(0.48649, 0.25676, 0.25676),
                               stringsAsFactors = FALSE)
  
  expect_identical(testResult, expectedResult)
})
