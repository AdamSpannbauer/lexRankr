# lexRankr 0.4.0

* added functions `unnest_sentences` and `unnest_sentences_` to parse sentences in a dataframe  following tidy data principles
* added functions `bind_lexrank` and `bind_lexrank_` to calculate lexrank scores for sentences in a dataframe following tidy data principles (`unnest_sentences` & `bind_lexrank` can be used on a df in a magrittr pipeline)
* added vignette for using lexrank to analyze tweets

# lexRankr 0.3.0

* sentence similarity from `sentenceSimil` now calculated using Rcpp.  Improves speed by ~25%-30% over old implementation using `proxy` package


# lexRankr 0.2.0

* Added logic to avoid naming conflicts in proxy::pr_DB in `sentenceSimil` (#1, @AdamSpannbauer)

* Added check and error for cases where no sentences above threshold in `lexRankFromSimil` (#2, @AdamSpannbauer)

* `tokenize` now has stricter punctuation removal.  Removes all non-alphnumeric characters as opposed to removing `[:punct:]`
