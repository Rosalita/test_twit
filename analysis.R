# Twitter Scraping and sentiment analysis of Tweets 

#install.packages("tm")

library(twitteR)
library(ROAuth)
library(httr)
library(stringr)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)

# set working directory to project root
setwd("C:/Dev/git/test_twit")


testbashtweets <- readRDS("testbashtweets.Rda")

#clean up the tweets because we want to perform analysis on test bash Manchester

#create an index of tweets about testbash events in Philadelphia, Brighton and Netherlands
othersindex <- grep("Philadelphia|Philly|Philad|Brighton|Netherlands", testbashtweets[,1])

#407 is false positive picked up by grep that mentions both testbash manchester and netherlands
#so make sure tweet 407 is not the index of tweets to be removed
othersindex <- othersindex[which(othersindex!=407)] 

#apply index to tweets to discard tweets about non-manchester events
testbashtweets  <- testbashtweets[-othersindex,]

#fix row names after removing some rows
row.names(testbashtweets) <- 1:nrow(testbashtweets)

# subset the data to identify tweets created on 2016-10-21, the day of the conference. 
index <- which(as.Date(testbashtweets$created) == "2016-10-21")
confdaytweets <- testbashtweets[index,]


#fix row names after removing some rows
row.names(confdaytweets) <- 1:nrow(confdaytweets)


# for a starting point, used the Hu and Liu Opinion Lexicon 
# from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar

# import the good words and get them into a vector
good <- read_file("positive-words.txt", locale = default_locale())
good = gsub('[[:cntrl:]]', ' ',good)  # replace control characters, like \n or \r with a space 
good.list = str_split(good, '\\s+')   # split into a list of words
good_text = unlist(good.list)         # make sure words is a vector, not a list
good_text = good_text[1:(length(good_text) -1)] # the last item appears to be "", so just trim it off

# import the bad words and get them into a vector
bad <- read_file("negative-words.txt", locale = default_locale())
bad = gsub('[[:cntrl:]]', ' ',bad)  # replace control characters, like \n or \r with a space 
bad.list = str_split(bad, '\\s+')   # split into a list of words
bad_text = unlist(bad.list)         # make sure words is a vector, not a list
bad_text = bad_text[1:(length(bad_text) -1)] # the last item appears to be "", so just trim it off


# extract just the tweet text 
textdata <- confdaytweets$text


#check encoding
Encoding(textdata) # mixture of all kinds of encoding

#Apply Native encoding on the vector    
textdata <- enc2native(textdata)

#Apply UTF-8 encoding on the vector
textdata <- enc2utf8(textdata)

# This removes all weird characters
# some are changed to format \u0085
# and others in format <U+0096>


# Can start to do a bit tiny of cleaning at this stage
# Any '&' characters will now be '&amp;', simply replace them with the word 'and'
textdata = gsub("\\&amp;", "and", textdata)

# Any ">" or "<" characters will now be '&gt;' and '&lt;', so just remove these with gsub
textdata = gsub("\\&gt;", " ", textdata)
textdata = gsub("\\&lt;", " ", textdata)

# identify tweets containing an @username mention, they will contain a words that start with @
index <- grep("(^|[^@\\w])@(\\w{1,15})\\b",textdata)
mentions <- textdata[index]

#split each of the tweets with a mention down into individual words
mentions <- unlist(strsplit(mentions, " "))

regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @
users <- gsub(regex2, "", mentions[grep(regex1, mentions, perl = T)])


#TWitter usernames aren't case sensitive so convert them all to lowercase
users <- tolower(users)

unique(users)

# All users mentioned in tweets
total <- table(users)

sort(total)
unique(users)

length(unique(users))

# remove the usernames from the text data now they have been collected
textdata = gsub("(^|[^@\\w])@(\\w{1,15})\\b", " ", textdata)

# Can do a bit more cleaning now
textdata = gsub('[[:punct:]]', '', textdata)  # strips out punctuation
textdata = gsub('[[:cntrl:]]', ' ', textdata)  # strips out control characters, like \n or \r 
textdata = gsub('\\d+', '', textdata)         # strips out numbers

textdata = gsub("RT", " ", textdata) #remove any instances of "RT" as this isn't a real word

# there are some orphaned "s" on their own created previous cleanings
textdata = gsub("^s\\s", " ", textdata) #remove any single "s" followed by a space,

#strip out remains of links - commented out because fear this turned word 'delight' into delig'
# textdata = gsub('ht(\\w{1,60})\\b', '', textdata) #remove any words between 1 and 60 chars starting with "ht"

#strip out emoji residue, anything starting with edUAU
#This cleans up leftovers like edUAUBDedUBUA, edUAUBDedUBUU and edUAUBDedUBUDedUAUBCedUBFUBB
textdata = gsub('edUAU(\\w{1,130})\\b', '', textdata) #remove any words upto 130 chars long starting "edUAU"

#convert all text to lowercase
textdata <- tolower(textdata)

# convert textdata to dataframe so can transfer it to corpus later
textdataframe <- as.data.frame(textdata)

# extract the words by splitting up the text data
words <- unlist(strsplit(textdata, " "))

# remove blank ""'s in the vector of words
words <- words[words != ""]

#total up the words in a table
wordtable <- table(words)

#store totals in a data frame
worddf <- as.data.frame(wordtable, stringsAsFactors=FALSE)

#remove stop words
#read in a list of stop words
stopwords <- read_file("stop-words.txt", locale = default_locale())
#clean up control chars
stopwords = gsub('[[:cntrl:]]', ' ',stopwords)  # replace control characters, like \n or \r with a space 
# split the stopwords up into a vector of words
stopwords <- unlist(strsplit(stopwords, " "))
# remove blank ""'s
stopwords <- stopwords[stopwords != ""]

#make an index of stopwords which are in the words dataframe
index <- which(worddf[,1] %in% stopwords)

#remove the stopwords from the words dataframe
worddf <- worddf[-index,]

#re-index
row.names(worddf) <- 1:nrow(worddf)

# rows 684 - 1142 are all partial link text so discard these values
index <- 684:1142
worddf <- worddf[-index,]
#relabel rows
row.names(worddf) <- 1:nrow(worddf)

# Now score each word on whether it is positive or negative.
library(plyr)
# ddply() takes a dataframe, does stuff to it, returns a dataframe

# Score all the words and output as dataframe
scoredwords <- ddply(worddf, "words", function(x) {
  wordtocheck <- x$words
  # compare the word to check to the dictionaries of positive & negative terms
  pos.match = match(wordtocheck, good_text)
  neg.match = match(wordtocheck, bad_text)

  # match() returns the position of the matched term or NA
  # convert matches to TRUE/FALSE instead
  pos.match = !is.na(pos.match)
  neg.match = !is.na(neg.match)
  
  # TRUE/FALSE is treated as 1/0 by sum(), so add up the score
  score = sum(pos.match) - sum(neg.match)
  })


# bind the word frequencies onto this dataframe
scoredwords <- cbind(scoredwords, worddf$Freq)

# tidy up column names on this new dataframe to sentiment, -1 is negative, 0 is neutral, +1 is positive
colnames(scoredwords) <- c("words", "sentiment", "freq")

# sentiment is currently stored as an int which is continuous data type
# for plotting purposes, change it to char which is discrete 
scoredwords$sentiment <- as.character(scoredwords$sentiment)

#find the popular words
popularwordindex <- which(scoredwords$freq > 18)

popularwords <- scoredwords[popularwordindex,]

#plot word frequency
p <- ggplot(popularwords, aes(x=reorder(words, -freq),y=freq, fill=popularwords$sentiment )) +
  geom_bar(stat="identity")+ 
  coord_cartesian(xlim = NULL, ylim = c(0,200), expand = TRUE)+
  annotate("text", x = 20, y = 200, label = "'testbash' frequency = 1774 (extends off chart)")+
  labs(x="Popular Words (18+ mentions)", y="Frequency", fill="Sentiment")+
 # scale_x_discrete(trans ="reverse")+
  scale_fill_manual(breaks = c("-1", "0", "1"),
                    labels = c("Negative", "Neutral", "Positive"),
                    values = c("#FF0000", "#d3d3d3", "#00ff00"),
                    limits = c(-1, 0, 1))+   
  ggtitle("Tweeted Words by Frequency and Sentiment")
 # coord_flip()
#ggtitle("Strategies for Using Homework Solution and Mini-Lecture Screencasts")

p +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))

