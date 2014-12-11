###############################################################################
## IT497 November 19, 2014 In-Class Exercise
##
## The R code that we will use today is taken directly from Christopher
## Claeys' Upstat 14 Presentation
## http://www.up-stat.org/files/presentations/UPSTAT2014_Claeys_twitterScrape.R
## 
## We are only exploring a portion of the original code. I encourage you to 
## visit the original page for more information and several interesting
## extensions. 
###############################################################################

## When you see:

###############################################################################
##
##   STOP
##
###############################################################################

## please stop and wait for further instructions

###############################################################################
## If you have not installed any of the following, 
## you will need to install the missing package before loading it.
library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("reshape2")

## These are my codes for an application I created for this example.
## These codes will not work for you

my.key <-"HoC8RjTPnq1UvVMD7QoYyTDra"
my.secret <-"c1e59HgLPQoD1xuijzM5LzogSJ0Khu79h3RTfEo2E8xKi4TwoO"

cred <- OAuthFactory$new(consumerKey=my.key,
                         consumerSecret=my.secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL= 'https://api.twitter.com/oauth/authorize')

## save the credentials to your local drive
## on future uses of the script you'll only need to load the .Rdata
## file and won't have to re-authorize your account.

save(cred, file="twitter_authentication.Rdata")


## load("twitter_authentication.Rdata")
cred$handshake()


###############################################################################
##
##   STOP
##
###############################################################################

## check that authorization was successful

registerTwitterOAuth(cred)

###############################################################################
##
##   STOP
##
###############################################################################

## tweets <- searchTwitter("@Boeing",n=500)

tweets1 <- searchTwitter("@eBay",n=200, lang="en", since='2014-12-3', until='2014-12-4')

###############################################################################
##
##   STOP
##
###############################################################################

length(tweets1)

tweets1.id <- sapply(tweets1, function(x) x$getText())
tweets1.text <- sapply(tweets1, function(x) x$getText())
tweets1.screenname <- sapply(tweets1, function(x) x$getScreenName())
tweets1.isretweet <- sapply(tweets1, function(x) x$getIsRetweet())
tweets1.retweeted <- sapply(tweets1, function(x) x$getRetweeted())
tweets1.created <- sapply(tweets1, function(x) x$getCreated())

head(tweets1.text)



df1 <- data.frame(tweets1.id, tweets1.text, tweets1.screenname, tweets1.isretweet, tweets1.retweeted, tweets1.created)
names(df1) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df1, file = "ebay_1.1.txt", append = TRUE)

# Day 2

tweets2 <- searchTwitter("@eBay",n=200, lang="en",since='2014-12-04', until='2014-12-05')

#Extracting the fields that we want
tweets2.id <- sapply(tweets2, function(x) x$getId())
tweets2.text <- sapply(tweets2, function(x) x$getText())
tweets2.screenname <- sapply(tweets2, function(x) x$getScreenName())
tweets2.isretweet <- sapply(tweets2, function(x) x$getIsRetweet())
tweets2.retweeted <- sapply(tweets2, function(x) x$getRetweeted())
tweets2.created <- sapply(tweets2, function(x) x$getCreated())

#Create a dataframe of the required tweet data 
#and write it into a text file for day 1
df2 <- data.frame(tweets2.id, tweets2.text, tweets2.screenname, 
                  tweets2.isretweet, tweets2.retweeted, tweets2.created)
names(df2) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df2, file = "ebay_2.txt", append = TRUE)

#Day 3 
tweets3 <- searchTwitter("@eBay",n=200, lang="en",
                        since='2014-12-05', until='2014-12-06')
#Extracting the fields that we want
tweets3.id <- sapply(tweets3, function(x) x$getId())
tweets3.text <- sapply(tweets3, function(x) x$getText())
tweets3.screenname <- sapply(tweets3, function(x) x$getScreenName())
tweets3.isretweet <- sapply(tweets3, function(x) x$getIsRetweet())
tweets3.retweeted <- sapply(tweets3, function(x) x$getRetweeted())
tweets3.created <- sapply(tweets3, function(x) x$getCreated())

#Create a dataframe of the required tweet data 
#and write it into a text file for day 3
df3 <- data.frame(tweets3.id, tweets3.text, tweets3.screenname, 
                  tweets3.isretweet, tweets3.retweeted, tweets3.created)
names(df3) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df3, file = "ebay_3.txt", append = TRUE)

#Day 4

tweets4 <- searchTwitter("@eBay",n=200, lang="en",since='2014-12-08', until='2014-12-09')
#Extracting the fields that we want
tweets4.id <- sapply(tweets4, function(x) x$getId())
tweets4.text <- sapply(tweets4, function(x) x$getText())
tweets4.screenname <- sapply(tweets4, function(x) x$getScreenName())
tweets4.isretweet <- sapply(tweets4, function(x) x$getIsRetweet())
tweets4.retweeted <- sapply(tweets4, function(x) x$getRetweeted())
tweets4.created <- sapply(tweets4, function(x) x$getCreated())

#Create a dataframe of the required tweet data 
#and write it into a text file for day 4
df4 <- data.frame(tweets4.id, tweets4.text, tweets4.screenname, 
                  tweets4.isretweet, tweets4.retweeted, tweets4.created)
names(df4) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df4, file = "ebay_4.txt", append = TRUE)

##Day 5 

tweets5 <- searchTwitter("@eBay",n=200, lang="en",
                        since='2014-12-09', until='2014-12-10')
#Extracting the fields that we want
tweets5.id <- sapply(tweets5, function(x) x$getId())
tweets5.text <- sapply(tweets5, function(x) x$getText())
tweets5.screenname <- sapply(tweets5, function(x) x$getScreenName())
tweets5.isretweet <- sapply(tweets5, function(x) x$getIsRetweet())
tweets5.retweeted <- sapply(tweets5, function(x) x$getRetweeted())
tweets5.created <- sapply(tweets5, function(x) x$getCreated())

#Create a dataframe of the required tweet data 
#and write it into a text file for day 5
df5 <- data.frame(tweets5.id, tweets5.text, tweets5.screenname, 
                  tweets5.isretweet, tweets5.retweeted, tweets5.created)
names(df5) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df5, file = "ebay_5.txt", append = TRUE)



