context("sentenceSimil")

# test object out str and class ---------------------------------------
test_that("testing result str and class", {
  testDocs <- c("Testing 1, 2, 3.", 
                "Is everything working as expected in my test?",
                "Is it working?")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  testResult <- sentenceSimil(sentenceId = tokenDf$sentenceId,
                              token = tokenDf$token,
                              docId = tokenDf$docId)
  
  expect_equal(class(testResult), "data.frame")
  expect_equal(names(testResult), c("sent1","sent2","similVal"))
  
  expect_true(is.character(testResult$sent1))
  expect_true(is.character(testResult$sent2))
  expect_true(is.numeric(testResult$similVal))
})
  

test_that("bad input", {
  expect_error(sentenceSimil(sentenceId = c("1_1"),
                             token = c("word","word2"),
                             docId = c(1,2)))
  
  expect_error(sentenceSimil(sentenceId = c("1_1", "2_1"),
                             token = c(1,2),
                             docId = c(1,2)))
  
  testDocs <- c("test","test")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  expect_error(sentenceSimil(sentenceId = tokenDf$sentenceId,
                             token = tokenDf$token,
                             docId = tokenDf$docId))
  
  testDocs <- c("1","2")
  tokenDf <- sentenceTokenParse(testDocs)$tokens
  
  expect_error(sentenceSimil(sentenceId = tokenDf$sentenceId,
                             token = tokenDf$token,
                             docId = tokenDf$docId))
})





