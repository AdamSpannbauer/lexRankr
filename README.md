# lexRankr: Extractive Text Summariztion in R

[![Build Status](https://travis-ci.org/AdamSpannbauer/lexRankr.svg?branch=master)](https://travis-ci.org/AdamSpannbauer/lexRankr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AdamSpannbauer/lexRankr?branch=master&svg=true)](https://ci.appveyor.com/project/AdamSpannbauer/lexRankr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/lexRankr)](https://CRAN.R-project.org/package=lexRankr) [![Coverage Status](https://img.shields.io/codecov/c/github/AdamSpannbauer/lexRankr/master.svg)](https://codecov.io/github/AdamSpannbauer/lexRankr?branch=master) 

##Installation
    
    ##install from CRAN
    install.packages("lexRankr")
    
    #install from this github repo
    devtools::install_github("AdamSpannbauer/lexRankr")
    


##Overview
lexRankr is an R implementation of the LexRank algorithm discussed by Güneş Erkan & Dragomir R. Radev in [LexRank: Graph-based Lexical Centrality as Salience in Text Summarization](http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html).  LexRank is designed to summarize a cluster of documents by proposing which sentences subsume the most information in that particular set of documents.  The algorithm may not perform well on a set of unclustered/unrelated set of documents.  As the white paper's title suggests, the sentences are ranked based on their centrality in a graph.  The graph is built upon the pairwise similarities of the sentences (where similarity is measured with a modified idf cosine similiarity function).  The paper describes multiple ways to calculate centrality and these options are available in the R package.  The sentences can be ranked according to their degree of centrality or by using the Page Rank algorithm (both of these methods require setting a minimum similarity threshold for a sentence pair to be included in the graph).  A third variation is Continuous LexRank which does not require a minimum similarity threshold, but rather uses a weighted graph of sentences as the input to Page Rank.

##Basic Usage
####lexRank applied to a charcter vector of documents
  ```
  library(lexRankr)
  library(dplyr)
  
  #note1: the lexrank algorithm is designed to work on a cluster of documents.
          #LexRank is built on the idea that a cluster of docs will focus on similar topics.
      
  #note2: pairwise sentence similiarity is calculated for the entire set of documents passed to the function.  
          #This can be a computationally instensive process (esp with a large set of documents)
          
  df <- tibble(doc_id = 1:3, 
               text = c("Testing the system. Second sentence for you.", 
                        "System testing the tidy documents df.", 
                        "Documents will be parsed and lexranked."))
  
  (topSentsDf <- lexRank(df$text))
  ```
####lexRank in a tidy framework
  ```
  library(lexRankr)
  library(dplyr)
      
  df <- tibble(doc_id = 1:3, 
               text = c("Testing the system. Second sentence for you.", 
                        "System testing the tidy documents df.", 
                        "Documents will be parsed and lexranked."))
  df %>% 
    unnest_sentences(sents, text) %>% 
    bind_lexrank(sents, doc_id, level = 'sentences') %>% 
    arrange(desc(lexrank))
  ```
  
##Example with Twitter (using lexRank helper functions)
    ```
    library(jsonlite)
    library(httr)
    
    library(lexRankr)
    
    #########################################################
    #FUNCTION TO GET TEXT OF USERS LAST N TWEETS ON TIMELINE#
    #########################################################
    getUserTweets <- function(user, n, consKey, consSecret, token, tokenSecret) {
    
      #SET UP FOR API
      auth <- oauth_app("twitter", key=consKey, secret=consSecret)
      sig  <- sign_oauth1.0(auth, token=token, token_secret=tokenSecret)
    
      #INITIALIZE COUNTERS AND STORAGE
      nLeft  <- n
      i <- 0
      tweetText <- character(0)
      #LOOP UNTIL n IS MET
      while (nLeft > 0) {
        nToGet <- min(200, nLeft)
        i <- i+1
    
        #SET MAX ID IF i > 1 (MAX ID WILL KEEP TWEETS FROM BEING DUPLICATED IN GET)
        if (i==1) {
          GETurl    <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=", user,"&count=", nToGet)
        } else {
          GETurl    <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=", user,"&count=",nToGet,"&max_id=",    max_id)
        }
    
        #GET TWEETS
        timelineRaw <- GET(GETurl, sig)
    
        #EXTRACT TWEET TEXT FROM GET
        timelineContent <- content(timelineRaw)
        jsonRaw <- toJSON(timelineContent)
        jsonConv <- iconv(jsonRaw, "UTF-8", "ASCII", sub = "") #clean out emoji and other special chars
        jsonConv <- gsub("\003", "", jsonConv) #special character (^C) not caught by above clean
        jsonClean <- fromJSON(jsonConv)
    
        #APPEND TWEET TEXT
        tweetText <- c(tweetText, unlist(jsonClean$text))
    
        #STORE MAX ID FOR USE IN NEXT GETurl
        suppressWarnings(max_id <- min(unlist(jsonClean$id)))
    
        #UPDATE NUMBER OF TWEETS LEFT TO MEET n PARAMETER
        nLeft <- nLeft-nToGet
      }
    
      return(unique(tweetText))
    }
    
    
    ##################################################################################
    #FUNCTION USING LEXRANKR TO FIND THE MOST REPRESENTATIVE TWEETS OF USERS TIMELINE#
    ##################################################################################
    tweetRankr <- function(tweetText, dropMentions=TRUE, dropHashtags=TRUE, n=5, returnTies=TRUE, printTweets = TRUE) {
    
      #store original tweet text
      tweetTextOg <- tweetText
    
      #remove instances of @username from text
      if (dropMentions) tweetText <- gsub("\\@\\w+","",tweetText)
      #remove insances of #topic from text
      if (dropHashtags) tweetText <- gsub("\\#\\w+","",tweetText)
      #clean up any multiple spaces introduced by modifying text
      tweetText <- trimws(gsub("\\s+", " ", tweetText))
    
      #parse text and create doc/sent Ids for each token
      sentenceTokenList <- lexRankr::sentenceTokenParse(tweetText)
      #store token data.frame from list output
        #token data.frame has columns for the token, document Id, and sentence Id
      tweetTokenDf <- sentenceTokenList$tokens
    
      #compute pairwise tweet similiarity
        #using document id from token df instead of sentence id
        #using docId will find most central tweets as opposed to most central sentences within tweets
      similDf <- lexRankr::sentenceSimil(tweetTokenDf$docId, tweetTokenDf$token, tweetTokenDf$docId)
    
      #apply lexRank algorithm to return top n tweet ids
      topTweetIdsDf <- lexRankr::lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, n = n, returnTies = returnTies)
    
      #subset tweet text variable to extract most central tweets according to lexrank
      topTweets <- tweetTextOg[as.numeric(topTweetIdsDf$sentenceId)]
    
      if (printTweets) cat(paste(paste0(1:length(topTweets), ") ",topTweets), collapse="\n\n"))
      invisible(return(topTweets))
    }
    
    consKey     <- 'consumer key'
    consSecret  <- 'consumer secret'
    token       <- 'access token'
    tokenSecret <- 'access token secret'
    
    ###################
    # Hillary Clinton #
    ###################
    tweetTextHill <- getUserTweets(user="HillaryClinton", n=5000, consKey, consSecret, token, tokenSecret)
    topTweetsHill <- tweetRankr(tweetTextHill)
    ```
    
1) "Hillary Clinton must become the next president of the United States. @BernieSanders #DemsInPhilly

2) "In this election, Im with her!" @FLOTUS on Hillary #DemsInPhilly

3) We are better than this.

4) "As your president, I will always have your back." Hillary

5) She knows that love trumps hate. @POTUS on Hillary

    ```
    ################
    # Donald Trump #
    ################
    tweetTextTrump <- getUserTweets(user="realDonaldTrump", n=5000, consKey, consSecret, token, tokenSecret)
    topTweetsTrump <- tweetRankr(tweetTextTrump)
    ```
1) "@tcloer11: @realDonaldTrump Great job! Make America Great Again!"

2) Wisconsin, we will MAKE AMERICA GREAT AGAIN!

3) MAKE AMERICA GREAT AGAIN!

4) MAKE AMERICA GREAT AGAIN! MAKE AMERICA SAFE AGAIN!

5) AMERICA FIRST!

    ```
    ##################
    # Bernie Sanders #
    ##################
    tweetTextBern <- getUserTweets(user="SenSanders", n=5000, consKey, consSecret, token, tokenSecret)
    topTweetsBern <- tweetRankr(tweetTextBern)
    ```
1) Unless Congress stands up for the middle class that's getting stepped on by the billionaire class, soon there won't be aiddle class left.
 
2) The current federal minimum wage of $7.25 an hour is a starvation wage and must be raised. The minimum wage must become aiving wage.

3) Weve got to stand up to the fossil fuel industry and fight for legislation that transforms our energy system away from fossiluels.

4) There is no justice when so few have so much and so many have so little.

5) Health care is a right, not a privilege. Everyone in America should be able to access the health care they need regardless of their income.

    ```
    #############
    # Rbloggers #
    #############
    tweetTextRblog <- getUserTweets(user="Rbloggers", n=5000, consKey, consSecret, token, tokenSecret)
    topTweetsRblog <- tweetRankr(tweetTextRblog)
    ```
1) New R job: Data Scientist  Machine Learning https://t.co/YiWwXkmxmc #rstats #DataScience #jobs

2) New R job: Principal Analysts x2, Senior Analyst, Analyst (@ Wellington ) http://t.co/5OLIDl51tw #rstats #jobs

3) A Few Days of Python: Using R in Python http://t.co/28j8CAYThn #rstats

4) Network visualization  part 4: 3D networks https://t.co/U6U53xG679 #rstats #DataScience

5) Network visualization  part 4: 3D networks https://t.co/Y625xNNr03 #rstats #DataScience

