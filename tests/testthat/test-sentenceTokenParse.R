context("sentenceTokenParse")

# test output classes ----------------------------------------
test_that("object class and structure check", {
  testDocs <- c("12345", "Testing 1, 2, 3.", "Is everything working as expected Mr. Wickham?")
  testResult <- sentenceTokenParse(testDocs)
  
  expect_equal(class(testResult), "list")
  
  expect_equal(unique(vapply(testResult, class, character(1))), "data.frame")
  expect_equal(names(testResult$tokens), c("docId","sentenceId","token"))
  
  expect_true(is.numeric(testResult$tokens$docId))
  expect_true(is.character(testResult$tokens$sentenceId))
  expect_true(is.character(testResult$tokens$sentence))
})

# test output value -------------------------------------------

test_that("All clean options TRUE", {
  testDocs <- c("Testing 1, 2, 3.", "Is everything working as expected Mr. Wickham?")
  testResult <- sentenceTokenParse(testDocs,
                                   docId = "create",
                                   removePunc=TRUE,
                                   removeNum=TRUE,
                                   toLower=TRUE,
                                   stemWords=TRUE,
                                   rmStopWords=TRUE)
  
  expectedResultSentences <- sentenceParse(testDocs)
  expectedResultTokens <- unlist(lexRankr::tokenize(testDocs))
  expectedResultTokens <- expectedResultTokens[which(!is.na(expectedResultTokens))]
  
  expect_equal(testResult$sentences, expectedResultSentences)
  expect_equal(testResult$tokens$token, expectedResultTokens)
  
  expect_equal(class(testResult), "list")
})

