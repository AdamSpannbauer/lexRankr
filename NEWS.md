
# lexRankr 0.5.2

* fix damping bug where damping parameter wasn't passed to  `igraph::pagerank`

# lexRankr 0.5.1

* changed `smart_stopwords` to be internal data so that package doesnt need to be explicitly loaded with `library` to be able to parse

# lexRankr 0.5.0

* bug fix in sentence parsing for parsing exclamatory sentences
* converted idf calculation from `idf(d, t) = log( n / df(d, t) )` to `idf(d, t) = log( n / df(d, t) ) + 1` to avoid zeroing out common word tfidf values
* removed dplyr, tidyr, stringr, magrittr, & tm as dependencies
* created option to bypass assumption that each row/vector-element are different documents in `lexRank` and `unnest_sentences`
* various bug fixes in token & sentence parsing

# lexRankr 0.4.1

* added bug report url: (https://github.com/AdamSpannbauer/lexRankr/issues/)
* formatting updates to README.md

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
