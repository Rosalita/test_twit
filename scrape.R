# Twitter Scraping and sentiment analysis
# The twitteR package for R allows you to scrapte tweets from Twitter's API
# The ROAuth package provides an interface allowing users to authenticate via OAuth
# The httr package provides useful tools for working with HTTP, GET(), POST() etc.
# The stringr package provides useful string operations
# The readr package reads a file into a string
# The tm package is a framework for text mining packages within R
# The wordcloud package lets you make pretty word clouds


#install.packages("twitteR")
#install.packages("ROAuth", dependencies = TRUE)
#install.packages("httr")
#install.packages("stringr")
#install.packages("readr")
#install.packages("tm", dependencies = TRUE)
#install.packages("wordcloud", dependencies = TRUE)
#install.packages("ggplot2")

library(twitteR)
library(ROAuth)
library(httr)
library(stringr)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)

# Set working directory to project root
setwd("C:/Dev/git/test_twit")
#setwd("C:/git/test_twit")

# Make sure Twitter account has a phone number attached.
# Go to Twitter apps page (https://apps.twitter.com/) and create a new app
# Once app is created, this will give Keys and Access tokens

# For security, to keep secrets secret they are stored in environment variables! 
# API Secret and Access Token Secret should never be human readable,

#TWITAPISECRET <- Sys.getenv("TWITAPISECRET") 
#TWITTOKENSECRET <- Sys.getenv("TWITTOKENSECRET")

# Set API Keys 
#api_key <- "aVXP1fw3fyxFFYfSDsAKje3vy"
#api_secret <- TWITAPISECRET
#access_token <- "69366834-DdbmXBAxgxybC27MSBK3gaojj26Qcdr5Mi1rSzGpd"
#access_token_secret <- TWITTOKENSECRET 
#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Collected 2840 tweets on 29-10-2016 with hashtag #testbash
# first tweet at 2016-10-19 17:25:17 last tweet at 2016-10-29 07:39:26
# latest_tweets <- searchTwitter("#testbash", n=2840)

# this had to be done as twitter api only stores a sample of tweets for a 
# around 10 daysweek so this information needs to be collected and saved

# converted tweets collected to a data frame.
# tweetdf <- twListToDF(latest_tweets)

# saved the dataframe object as a .Rda file
# saveRDS(tweetdf,file="testbashtweets.Rda")

# load dataframe of testbash tweets which intend to perform analysis on

testbashtweets <- readRDS("testbashtweets.Rda")


#clean up the tweets because we want to perform analysis on test bash Manchester

#create an index of tweets about testbash events in Philadelphia, Brighton and Netherlands
othersindex <- grep("Philadelphia|Philly|Philad|Brighton|Netherlands", testbashtweets[,1])

#407 is false positive that mentions testbash manchester and netherlands so remove it from index
othersindex <- othersindex[which(othersindex!=407)] 

#apply index to tweets to discard tweets about non-manchester events
testbashtweets  <- testbashtweets[-othersindex,]

#fix row names after removing some rows
row.names(testbashtweets) <- 1:nrow(testbashtweets)

# extract tweet text
tweet_text <- testbashtweets$text



#library(plyr)
# laply splits a list, applies function, then returns results in an array
#tweet_text = laply(latest_tweets, function(x) x$getText())
tweet_text <- iconv(tweet_text, to='UTF-8') # convert all tweets to UTF8 to make emojis play nice


# As a starting point, use the Hu and Liu Opinion Lexicon 
# from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
# This is two lists, one of positive words and one of negative words

# Note: Added some new words to the positive list like 'honour', as it only had US spelling 'honor'

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

# initialise some global variables
positivity <- NULL
negativity <- NULL

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
    
    # match() returns the position of the matched term or NA
    # convert matches to TRUE/FALSE instead
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE is treated as 1/0 by sum(), so add up the score
    score = sum(pos.matches) - sum(neg.matches)
    
    #if any positive matches
    if (any(pos.matches)){
      pos.matches = match(words, good_text)
      pos.words = good_text[pos.matches] # apply index of pos matches to get pos words
      pos.words = pos.words[!is.na(pos.words)] # remove any NA values
      # append positive words to global positivity variable
      positivity <<- append(positivity, pos.words)
    }
    
    # identify the words which matched positively or negatively
    # maybe use <<- to set pos.words and neg.words as global variables?
    if (any(neg.matches)){
      neg.matches = match(words, bad_text)
      neg.words = bad_text[neg.matches] # apply index of neg matches to get neg words
      neg.words = neg.words[!is.na(neg.words)] # remove any NA values 
      #append negative words to global negativity variable
      negativity <<- append(negativity, neg.words)
    }
    
   return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Call the score sentiment function and return a data frame
feelings <- score.sentiment(tweet_text, good_text, bad_text, .progress='text')

sentiment_score <- feelings$score

#bind the sentiment scores onto the tweet dataframe
testbashtweets <- cbind(testbashtweets,sentiment_score)


#tally up all the positive and negative words in a table.
ptable <- table(positivity)
ntable <- table(negativity)

# Word clouds

library(tm)
library(wordcloud)

# make a corpus for positive and negative words
pcorp = Corpus(VectorSource(positivity))
ncorp = Corpus(VectorSource(negativity))

pcorp <- tm_map(pcorp, PlainTextDocument)
ncorp <- tm_map(ncorp, PlainTextDocument)

# Basic Wordcloud
# wordcloud(pcorp, max.words = 100, random.order = FALSE)

# Start a new plot frame
plot.new()

# Set the display a 2 by 2 grid
par(mfrow=c(1,2))

# Outer Margins
par(oma=c(0.5,0.1,0.5,0.1))
# Margins, bottom, left, top, right (default is  c(5,4,4,2))
par(mar=c(0.1,3,0.1,3))

# par(mar=c(9.3,4.1,4.1,2.1))
# par(mfrow=c(2,2))
# par(cex.axis=1.3)
# par(cex.main=1.3)


# Positive Wordcloud
wordcloud(pcorp, 
          scale=c(3,0.8), 
          max.words=200,
          min.freq=-1,
          random.order=FALSE, 
          rot.per=0.2, 
          use.r.layout=FALSE, 
          # Nice custom blue to green sequential colours
          colors = c(#"#ACF8A5",
                     #"#8DE99B",
                     #"#77DB9D", 
                     "#63CDA4", 
                     "#50BFAE",
                     "#3FA7B1", 
                     "#307EA2", 
                     "#235594", 
                     "#172F86",
                     "#100E78",
                     "#200569"))

text(x=-0.03, y=0.5, "Positive Words", srt=90)

#mtext("This is my margin text", side=2, line =1)
         # colors=brewer.pal(5, "BuGn"))

# Negative Wordcloud
wordcloud(ncorp, 
          scale=c(3,0.8), 
          max.words=200, 
          min.freq=-1,
          random.order=FALSE, 
          rot.per=0.2, 
          use.r.layout=FALSE, 
          # Nice custom yellow to red colours
          colors = c(#"#FFDE6A",
                     #"#F4C55C",
                     "#E9AC4F", 
                     "#DF9343", 
                     "#D47A37",
                     "#CA612D", 
                     "#BF4A23", 
                     "#B4331A", 
                     "#AA1D11",
                     "#9F0A0C"))
                     #"#950312"))

         # colors=brewer.pal(5, "Reds"))

text(x=1.03, y=0.5, "Negative Words", srt=270)
#mtext("This is my margin text", side=4, line =0, adj=1)



#separate out the date from the tweet creation time stamp
justdate <- as.Date(testbashtweets$created)
#justtime <- format(testbashtweets$created,"%H:%M:%S")

#bind date onto the dataframe
testbashtweets <- cbind(testbashtweets, justdate)

# subset the data to identify tweets created on 21-10-16, the day of the conference. 
index <- which(testbashtweets[,18] == "2016-10-21")
confdaytweets <- testbashtweets[index,]

#correct row names for confdaytweets dataframe
row.names(confdaytweets) <- 1:nrow(confdaytweets)

#bind a column on to conference day tweets to hold dates of dividing lines on plot
lines <- NA
confdaytweets <- cbind(confdaytweets, lines)

#indicate first line is at 08:00 am
confdaytweets[1,19] = "2016-10-21 08:00:00 GMT" #start time
confdaytweets[2,19] = "2016-10-21 17:45:00 GMT" #end time

str(confdaytweets$lines)

# convert location of lines to dates to POSIXlt
confdaytweets$lines <- as.POSIXct(confdaytweets$lines, tz="GMT")

str(confdaytweets$lines)


# plot tweets on 21-10-16, the day of the conference by time and sentiment
plot <- ggplot(confdaytweets, aes(x = created, y = sentiment_score))+
  #geom_jitter()+
 # geom_point(aes(color = factor(sentiment_score))) +
  geom_jitter(aes(color = factor(sentiment_score)))+ 
  ggtitle("Test Bash Manchester Tweets on 21-10-31")+
  labs(x="Time", y="Sentiment Score")+
  #scale_colour_hue(guide=FALSE)+ #to remove legend 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")+ #use scale_*_datetime for POSIXct variables
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3,4,5,6))+
 
  # Colour the scatter plot by sentiment
  # Sentiment is a discrete scale from -3 to 6 
  # Define some custom colours for these 10 discrete values
  scale_colour_manual(name = "Positivity Index",
                      breaks = c("6", "5", "4", "3", "2", "1", "0", "-1", "-2", "-3"),
                      labels = c("6 : Very Positive", "5", "4", "3", "2", "1", "0 : Neutral", "-1", "-2", "-3 : Negative"),
                      values = c("#E70255", "#EA0495", "#EC07D5", 
                                 "#C809EF", "#8D0CF2", "#520EF4", 
                                 "#1711F7", "#144BF9", "#178BFC", "#19CCFF"))+
  scale_fill_manual(name = "Positivity Index",
                    breaks = c("6", "5", "4", "3", "2", "1", "0", "-1", "-2", "-3"),
                    labels = c("6 : Very Positive", "5", "4", "3", "2", "1", "0 : Neutral", "-1", "-2", "-3 : Negative"),
                    values = c("#E70255", "#EA0495", "#EC07D5", 
                               "#C809EF", "#8D0CF2", "#520EF4", 
                               "#1711F7", "#144BF9", "#178BFC", "#19CCFF"))+
  #geom_vline(xintercept = as.numeric(confdaytweets[which(confdaytweets[,19] == 1),5]))  
  geom_vline(xintercept = as.numeric(confdaytweets[1,19]))+
  geom_vline(xintercept = as.numeric(confdaytweets[2,19]))+ 
  #geom_rect(aes(xmin=confdaytweets[1,19], xmax=confdaytweets[2,19],ymin=-3, ymax=Inf), alpha=0.2, fill="red")
  annotate("rect", xmin=confdaytweets[1,19], xmax=confdaytweets[2,19],ymin=-3, ymax=Inf, alpha=0.2, fill="red")
  
plot
  
  

