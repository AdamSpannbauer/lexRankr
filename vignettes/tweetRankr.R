## ----warning=FALSE,message=FALSE-----------------------------------------
library(lexRankr)
library(tidyverse)
library(stringr)
library(httr)
library(jsonlite)

## ------------------------------------------------------------------------
# set api tokens/keys/secrets as environment vars
# Sys.setenv(cons_key     = 'my_cons_key')
# Sys.setenv(cons_secret  = 'my_cons_sec')
# Sys.setenv(token        = 'my_token')
# Sys.setenv(token_secret = 'my_token_sec')

#sign oauth
auth <- httr::oauth_app("twitter", key=Sys.getenv("cons_key"), secret=Sys.getenv("cons_secret"))
sig  <- httr::sign_oauth1.0(auth, token=Sys.getenv("token"), token_secret=Sys.getenv("token_secret"))

## ---- collapse=TRUE------------------------------------------------------
get_timeline_df <- function(user, n_tweets=200, oauth_sig) {
  i <- 0
  n_left <- n_tweets
  timeline_df <- NULL
  #loop until n_tweets are all got
  while (n_left > 0) {
    n_to_get <- min(200, n_left)
    i <- i+1
    #incorporae max id in get_url (so as not to download same 200 tweets repeatedly)
    if (i==1) {
      get_url <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
                       user,"&count=", n_to_get)
    } else {
      get_url <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
                       user,"&count=",n_to_get,"&max_id=", max_id)
    }
    #GET tweets
    response <- httr::GET(get_url, oauth_sig)
    #extract content and clean up
    response_content <- httr::content(response)
    json_content     <- jsonlite::toJSON(response_content)
    #clean out evil special chars
    json_conv <- iconv(json_content, "UTF-8", "ASCII", sub = "") %>%
      stringr::str_replace_all("\003", "") #special character (^C) not caught by above clean
    timeline_list <- jsonlite::fromJSON(json_conv)
    #extract desired fields
    fields_i_care_about <- c("id", "text", "favorite_count", "retweet_count", "created_at")
    timeline_df <- purrr::map(fields_i_care_about, ~unlist(timeline_list[[.x]])) %>% 
      purrr::set_names(fields_i_care_about) %>% 
      dplyr::as_data_frame() %>% 
      dplyr::bind_rows(timeline_df) %>% 
      dplyr::distinct()
    #store min id (oldest tweet) to set as max id for next GET
    max_id <- min(purrr::map_dbl(timeline_list$id, 1))
    #update number of tweets left
    n_left <- n_left-n_to_get
  }
  return(timeline_df)
}

## ------------------------------------------------------------------------
tweets_df <- get_timeline_df("realDonaldTrump", 600, sig) %>% 
    mutate(text = str_replace_all(text, "\n", " ")) #clean out newlines for display

tweets_df %>% 
  head(n=3) %>% 
  select(text, created_at) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
tweets_df %>% 
  bind_lexrank(text, id, level="sentences") %>% 
  arrange(desc(lexrank)) %>% 
  head(n=5) %>% 
  select(text, lexrank) %>% 
  knitr::kable(caption = "Most Representative @realDonaldTrump Tweets")

## ------------------------------------------------------------------------
get_timeline_df("dog_rates", 600, sig) %>% 
  mutate(text = str_replace_all(text, "\n", " ")) %>% 
  bind_lexrank(text, id, level="sentences") %>% 
  arrange(desc(lexrank)) %>% 
  head(n=5) %>% 
  select(text, lexrank) %>% 
  knitr::kable(caption = "Most Representative @dog_rates Tweets")

