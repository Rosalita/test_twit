# Twitter Scraping and sentiment analysis
# The twitteR package for R allows you to scrapte tweets from Twitter's API
# The ROAuth package provides an interface allowing users to authenticate via OAuth
# The httr package provides useful tools for working with HTTP, GET(), POST() etc.
# The stringr package provides useful string operations
# The readr package reads a file into a string

#install.packages("twitteR")
#install.packages("ROAuth", dependencies = TRUE)
#install.packages("httr")
#install.packages("stringr")
#install.packages("readr")

library(twitteR)
library(ROAuth)
library(httr)
library(stringr)
library(readr)

# Set working directory to project root
setwd("C:/Dev/git/test_twit")

# Make sure Twitter account has a phone number attached.
# Go to Twitter apps page (https://apps.twitter.com/) and create a new app
# Once app is created, this will give Keys and Access tokens


# Set API Keys - Note: for security, keep secrets secret! 
# API Secret and Access Token Secret should never be human readable, remember not to commit them!
api_key <- "aVXP1fw3fyxFFYfSDsAKje3vy"
api_secret <- "xxxxxxxxxxxxxxxxxxxx"
access_token <- "69366834-DdbmXBAxgxybC27MSBK3gaojj26Qcdr5Mi1rSzGpd"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Grab latest tweets
latest_tweets <- searchTwitter("#testbash", n=100)

# Loop over tweets and extract text
library(plyr)
# laply splits a list, applies function, then returns results in an array
tweet_text = laply(latest_tweets, function(x) x$getText())
tweet_text <- iconv(tweet_text, to='UTF-8') # convert all tweets to UTF8 to make emojis play nice


# As a starting point, use the Hu and Liu Opinion Lexicon 
# from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
# This is two lists, one of positive words and one of negative words

# Added new word 'new' to the positive list

# Import the good words and get them into vectors
good <- read_file("positive-words.txt", locale = default_locale())
good = gsub('[[:cntrl:]]', ' ',good)  # replace control characters, like \n or \r with a space 
good.list = str_split(good, '\\s+')   # split into a list of words
good_text = unlist(good.list)         # make sure words is a vector, not a list
good_text = good_text[1:(length(good_text) -1)] # the last item appears to be "", so just trim it off


bad <- read_file("negative-words.txt", locale = default_locale())
bad = gsub('[[:cntrl:]]', ' ',bad)  # replace control characters, like \n or \r with a space 
bad.list = str_split(bad, '\\s+')   # split into a list of words
bad_text = unlist(bad.list)         # make sure words is a vector, not a list
bad_text = bad_text[1:(length(bad_text) -1)] # the last item appears to be "", so just trim it off

# Now score the text of each tweet based on count of positive and negative words used.

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up each sentence with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)  # strips out punctuation
    sentence = gsub('[[:cntrl:]]', '', sentence)  # strips out control characters, like \n or \r 
    sentence = gsub('\\d+', '', sentence)         # strips out numbers
    sentence <- iconv(sentence, to='UTF-8')       # convert to UTF8
    sentence = tolower(sentence)                  # converts all text to lower case     
    word.list = str_split(sentence, '\\s+')       # split into a list of words
    words = unlist(word.list)                     # make sure words is a vector, not a list
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
  
    # identify the words which matched positivesly or negatively
    pos.words = good_text[pos.matches] # apply index of pos matches to get pos words
    pos.words = pos.words[!is.na(pos.words)] # remove any NA values
    neg.words = bad_text[neg.matches] # apply index of neg matches to get neg words
    neg.words = neg.words[!is.na(neg.words)] # remove any NA values   
    
    # if there are no positive or negative words set these values to NA
    if (length(pos.words) == 0){
      pos.words = NA
    }
    if (length(neg.words) == 0){
      neg.words = NA
    }
      
    # match() returns the position of the matched term or NA
    # convert matches to TRUE/FALSE instead
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE is treated as 1/0 by sum(), so add up the score
    score = sum(pos.matches) - sum(neg.matches)
    
   return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


# Call the score sentiment function and return a data frame
feelings <- score.sentiment(tweet_text, good_text, bad_text, .progress='text')

# To Do, collect positive words? some kind of plot?

