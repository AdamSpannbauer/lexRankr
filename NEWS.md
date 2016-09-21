# lexRankr 0.2.0

* Added logic to avoid naming conflicts in proxy::pr_DB in `sentenceSimil` (#1, @AdamSpannbauer)

* Added check and error for cases where no sentences above threshold in `lexRankFromSimil` (#2, @AdamSpannbauer)

* `tokenize` now has stricter punctuation removal.  Removes all non-alphnumeric characters as opposed to removing `[:punct:]`
