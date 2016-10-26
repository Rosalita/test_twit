# test_twit

This code does the following:
* Searches Twitter API and collects a defined number of tweets on a certain topic. 
* Cleans them up by removing punctuation, control characters, numbers,
* Converts them to UTF-8 so emojiis don't break anything
* Converts them to lower case
* Breaks them down into individual words.
* Compares those words with a list of 6800+ positive and negative words compiled by Bing Liu and Minqing Hu of the University of Illinois at Chicago.
* Tabulates the frequency of each positive and negative word contained in the sample of tweets.

To Do:
* Generate word clouds for positive and negative words
