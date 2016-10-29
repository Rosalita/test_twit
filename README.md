# test_twit

This code does the following:
* Searched the Twitter API and collected 2840 tweets containing the hashtag #testbash 
* Tweets collected were dated from 19th October 2016 until 29th October 2016, co-inciding with the Test Bash Manchester event, a software testing conference held on 21st October 2016 and software testing openspace event held on 22nd October.
* Tweet data is converted to a dataframe and saved as testbashtweets.Rda, an object which can be loaded in RStudio.
* Tweet text is cleaned up by removing punctuation, control characters, numbers
* Tweet text is converted to UTF-8 so emojiis don't break anything, converted to lower case, broken down into individual words.
* A list of 6800+ positive and negative words compiled by Bing Liu and Minqing Hu of the University of Illinois at Chicago has been used as a starting point to categorise words into positive and negative. This list has been modified to take UK English into account e.g. 'honour' has been added as only US spelling 'honor' was present. Words like 'buzzing' have been moved from negative to positive as they were used in a positive context
* Tweet words are compared against the list of positive and negative words and word clouds generated.
