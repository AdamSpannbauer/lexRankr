context("lexRankFromSimil")

# test object out str and class ---------------------------------------
test_that("object out str and class", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  similDf <- sentenceSimil(sentenceId = tokenDf$sentenceId,
                           token = tokenDf$token,
                           docId = tokenDf$docId)
  
  testResult <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal)
  
  expect_equal(class(testResult), "data.frame")
  expect_equal(names(testResult), c("sentenceId", "value"))
  expect_true(is.character(testResult$sentenceId))
  expect_true(is.numeric(testResult$value))
})

# test bad inputs ---------------------------------------
test_that("bad inputs", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  similDf <- sentenceSimil(sentenceId = tokenDf$sentenceId,
                           token = tokenDf$token,
                           docId = tokenDf$docId)
  
  expect_error(lexRankFromSimil(NULL, similDf$sent2, similDf$similVal))
  expect_error(lexRankFromSimil(c(1,2), similDf$sent2, similDf$similVal))
  expect_error(lexRankFromSimil(similDf$sent1, similDf$sent2, c("a","b","c")))
  expect_error(lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, threshold = NULL))
  expect_error(lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, damping = NULL))
  
})

# test object out value
test_that("object out value", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  similDf <- sentenceSimil(sentenceId = tokenDf$sentenceId,
                           token = tokenDf$token,
                           docId = tokenDf$docId)
  
  testResult <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal)
  testResult$value = round(testResult$value, 5)
  
  expectedResult <- data.frame(sentenceId = c("1_1", "2_1", "3_1"),
                               value = c(0.25676, 0.48649, 0.25676),
                               stringsAsFactors = FALSE)
  
  expect_identical(testResult, expectedResult)
  
  testResult <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, continuous = TRUE)
  testResult$value = round(testResult$value, 5)
  
  expectedResult <- data.frame(sentenceId = c("1_1", "2_1", "3_1"),
                               value = c(0.25676, 0.48649, 0.25676),
                               stringsAsFactors = FALSE)
  
  expect_identical(testResult, expectedResult)
  
  testResult <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, usePageRank = FALSE)
  testResult$value = round(testResult$value, 5)
  
  expectedResult <- data.frame(sentenceId = c("2_1", "1_1", "3_1"),
                               value = c(2, 1, 1),
                               stringsAsFactors = FALSE)
  
  expect_identical(testResult, expectedResult)
})
