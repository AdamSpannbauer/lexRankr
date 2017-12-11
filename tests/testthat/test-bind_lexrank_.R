context("bind_lexrank_")

# test output str --------------------------------------------------------
test_that("correct ouput class and str", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  
  test_result <- unnest_sentences(df, sents, text)
  test_result <- bind_lexrank_(test_result, "sents", "doc_id", level = 'sentences')
  
  expect_equal(dim(test_result), c(4,4))
  expect_true(is.data.frame(test_result))
  expect_equal(names(test_result), c("doc_id","sent_id","sents","lexrank"))
  
  test_result <- unnest_sentences(df, sents, text, drop=FALSE) 
  test_result <- bind_lexrank_(test_result, "sents", "doc_id", level = 'sentences')
  
  expect_equal(dim(test_result), c(4,5))
  expect_equal(names(test_result), c("doc_id","text","sent_id","sents","lexrank"))
  
  df <- data.frame(doc_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), 
                   sent_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                   sents = c("Testing the system.", "Testing the system.", "Testing the system.", 
                             "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked."),
                   tokens = c("testing", "the", "system", "second", "sentence", "for", "you", "system", "testing", "the", 
                              "tidy", "documents", "df", "documents", "will", "be", "parsed", "and", "lexranked"),
                   stringsAsFactors = FALSE)
  
  test_result <- bind_lexrank_(df, "tokens", "doc_id", "sent_id", "tokens")
  
  expect_equal(dim(test_result), c(19,5))
  expect_equal(names(test_result), c("doc_id","sent_id","sents","tokens","lexrank"))
})

# test bad input -------------------------------------------------------
test_that("test input checking", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  df <- unnest_sentences(df, sents, text)
  
  expect_error(bind_lexrank_(df, "sents", "fake"))
  expect_error(bind_lexrank_(NULL, "sents", "doc_id"))
  expect_error(bind_lexrank_(df, "sents", "doc_id", level="fake"))
  # expect_warning(bind_lexrank_(df, "sents", "doc_id", level=c("sentences","tokens")))
  
  df <- data.frame(doc_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), 
                   sent_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                   sents = c("Testing the system.", "Testing the system.", "Testing the system.", 
                             "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked."),
                   tokens = c("testing", "the", "system", "second", "sentence", "for", "you", "system", "testing", "the", 
                              "tidy", "documents", "df", "documents", "will", "be", "parsed", "and", "lexranked"),
                   stringsAsFactors = FALSE)
  
  expect_error(bind_lexrank_(df, "tokens", "doc_id", "fake", level="tokens"))
  expect_error(bind_lexrank_(df, "tokens", "doc_id", level="tokens"))
  # expect_warning(bind_lexrank_(df, "tokens", "doc_id", "sent_id", level=c("tokens","sentences")))
})

# test output val ------------------------------------------------------
test_that("output value", {
  df <- data.frame(doc_id = 1:3, 
                   text = c("Testing the system. Second sentence for you.", 
                            "System testing the tidy documents df.", 
                            "Documents will be parsed and lexranked."),
                   stringsAsFactors = FALSE)
  df <- unnest_sentences(df, sents, text)
  
  test_result     <- bind_lexrank_(df, "sents", "doc_id", level="sentences")
  expected_result <- data.frame(doc_id = c(1L, 1L, 2L, 3L), 
                                sent_id = c(1L, 2L, 1L, 1L), 
                                sents = c("Testing the system.", "Second sentence for you.", 
                                          "System testing the tidy documents df.", "Documents will be parsed and lexranked."), 
                                lexrank = c(0.5, NA, 0.5, NA),
                                stringsAsFactors = FALSE)
  
  expect_equal(test_result, expected_result)
  
  df <- data.frame(doc_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), 
                   sent_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                   sents = c("Testing the system.", "Testing the system.", "Testing the system.", 
                             "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", 
                             "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked."),
                   tokens = c("testing", "the", "system", "second", "sentence", "for", "you", "system", "testing", "the", 
                              "tidy", "documents", "df", "documents", "will", "be", "parsed", "and", "lexranked"),
                   stringsAsFactors = FALSE)
  
  test_result     <- bind_lexrank_(df, "tokens", "doc_id", "sent_id", level="sentences")
  test_result$lexrank <- round(test_result$lexrank, 5)
  expected_result <- data.frame(doc_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), 
                                sent_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                                sents = c("Testing the system.", "Testing the system.", "Testing the system.", 
                                          "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", "Second sentence for you.", 
                                          "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                                          "System testing the tidy documents df.", "System testing the tidy documents df.", "System testing the tidy documents df.", 
                                          "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", 
                                          "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked.", "Documents will be parsed and lexranked."),
                                tokens = c("testing", "the", "system", "second", "sentence", "for", "you", "system", "testing", "the", 
                                           "tidy", "documents", "df", "documents", "will", "be", "parsed", "and", "lexranked"),
                                lexrank = c(0.16667, NA, 0.16667, NA, NA, NA, NA, 0.16667, 0.16667, NA, NA, 0.16667, NA, 0.16667, NA, NA, NA, NA, NA),
                                stringsAsFactors = FALSE)
  
  expect_equal(test_result, expected_result)
})

