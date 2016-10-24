# lexRankr 0.3.0

* sentence similarity from `sentenceSimil` now calculated using Rcpp.  Improves speed by ~25%-30% over old implementation using `proxy` package


# lexRankr 0.2.0

* Added logic to avoid naming conflicts in proxy::pr_DB in `sentenceSimil` (#1, @AdamSpannbauer)

* Added check and error for cases where no sentences above threshold in `lexRankFromSimil` (#2, @AdamSpannbauer)

* `tokenize` now has stricter punctuation removal.  Removes all non-alphnumeric characters as opposed to removing `[:punct:]`
