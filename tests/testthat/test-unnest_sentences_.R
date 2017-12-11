context("unnest_sentences_")

# test output str --------------------------------------------------------
test_that("correct ouput class and str", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  
  test_result <- unnest_sentences_(df, "out", "text")
  
  expect_equal(dim(test_result), c(4,3))
  expect_true(is.data.frame(test_result))
  expect_equal(names(test_result), c("doc_id","sent_id","out"))
  
  test_result <- unnest_sentences_(df, "out", "text", drop=FALSE)
  
  expect_equal(dim(test_result), c(4,4))
  expect_equal(names(test_result), c("doc_id","text","sent_id","out"))
})

# test bad input -------------------------------------------------------
test_that("test input checking", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  
  expect_error(unnest_sentences_(df, "out", "fake"))
  expect_error(unnest_sentences_(NULL, "out", "text"))
  expect_error(unnest_sentences_(df, "out", "text", drop = NULL))
  expect_error(unnest_sentences(df, "out", "text", doc_id = "fake"))
  expect_warning(unnest_sentences_(df, "out", "text", output_id=c("test","test2")))
})

# test output val ------------------------------------------------------
test_that("output value", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  
  test_result     <- unnest_sentences_(df, "out", "text")
  expected_result <- data.frame(doc_id = c(1L, 1L, 2L, 3L), 
                                sent_id = c(1L, 2L, 1L, 1L), 
                                out = c("Testing the system.", 
                                        "Second sentence for you.", 
                                        "System testing the tidy documents df.", 
                                        "Documents will be parsed and lexranked."),
                                stringsAsFactors = FALSE)
  
  expect_equal(test_result, expected_result)
  
  df <- data.frame(doc_id = c(1,1,3), 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  
  test_result     <- unnest_sentences_(df, "out", "text", doc_id = "doc_id")
  expected_result <- data.frame(doc_id = c(1L, 1L, 1L, 3L), 
                                sent_id = c(1L, 2L, 3L, 1L), 
                                out = c("Testing the system.", 
                                        "Second sentence for you.", 
                                        "System testing the tidy documents df.", 
                                        "Documents will be parsed and lexranked."),
                                stringsAsFactors = FALSE)
  
  expect_equal(test_result, expected_result)
})

