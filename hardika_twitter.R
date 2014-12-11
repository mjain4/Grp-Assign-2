
  ###############################################################################
## 1st Day Tweets

## If you have not installed any of the following, 
## you will need to install the missing package before loading it.
library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")

## These are my codes for an application I created for this example.
## These codes will not work for you

my.key <-"KbBDqQifBUi1HN5SKDABGt2VA"
my.secret <-"tnJUP5JTZQXQ9TFZP91JmKamad1sEBwR7NfuYcyAxnNbm8oW97"

cred <- OAuthFactory$new(consumerKey=my.key,
                         consumerSecret=my.secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL= 'https://api.twitter.com/oauth/authorize')

## save the credentials to your local drive
## on future uses of the script you'll only need to load the .Rdata
## file and won't have to re-authorize your account.

save(cred, file="twitter_authentication.Rdata")


load("twitter_authentication.Rdata")
cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


## check that authorization was successful

registerTwitterOAuth(cred)

## tweets <- searchTwitter("@Boeing",n=500)

tweets <- searchTwitter("@Amazon",n=200, lang="en", since='2014-12-3', until='2014-12-4')

length(tweets)

## Now, we will extract the fields that we want

tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

## Write data to file
df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df, file = "Amazon1.txt", append = TRUE)
## these are the files from ReggieNet
##load opinion lexicon
##from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
## the load path is relative to the working directory that is set above
pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))

## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})


## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment1 <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment1)

sentiment1<-sum(simple.sentiment1)

  ###############################################################################
## 2nd Day Tweets


## tweets <- searchTwitter("@Boeing",n=500)

tweets <- searchTwitter("@Amazon",n=200, lang="en", since='2014-12-4', until='2014-12-5')

length(tweets)

tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

## Write data to file
df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")

write.table(df, file = "Amazon2.txt", append = TRUE)

pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))

## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})


## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment2 <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment2)


sentiment2<-sum(simple.sentiment2)

  ###############################################################################
## 3rd Day Tweets

tweets <- searchTwitter("@Amazon",n=200, lang="en", since='2014-12-5', until='2014-12-6')

length(tweets)


## Now, we will extract the fields that we want


tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

## Write data to file
df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")

write.table(df, file = "Amazon3.txt", append = TRUE)

## these are the files from ReggieNet
##load opinion lexicon
##from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
## the load path is relative to the working directory that is set above
pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))

## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})

## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment3 <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment3)

sentiment3<-sum(simple.sentiment3)

  ###############################################################################
## 4th Day Tweets

tweets <- searchTwitter("@Amazon",n=200, lang="en", since='2014-12-8', until='2014-12-9')


length(tweets)

## Now, we will extract the fields that we want
tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

## Write data to file
df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")

write.table(df, file = "Amazon4.txt", append = TRUE)

## these are the files from ReggieNet
##load opinion lexicon
##from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
## the load path is relative to the working directory that is set above
pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))


## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})


## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment4 <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment4)
sentiment4<-sum(simple.sentiment4)

  ###############################################################################
## 5th Day Tweets

tweets <- searchTwitter("@Amazon",n=200, lang="en", since='2014-12-9', until='2014-12-10')


length(tweets)

## Now, we will extract the fields that we want

tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

## Write data to file
df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")

write.table(df, file = "Amazon5.txt", append = TRUE)

## these are the files from ReggieNet
##load opinion lexicon
##from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
## the load path is relative to the working directory that is set above
pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))

## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})


## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment5 <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment5)


sentiment5<-sum(simple.sentiment5)
  
#Graph showing stock prices

  AmazonData <- read.csv(file = "Amazon.csv")
  
  AmazonData$Start <- NULL
  AmazonData$End <- NULL
  
  AmazonData
  
  AmazonData$Stock.Price <- NULL
  AmazonData$Sentiment.Score <- NULL
  
  molten.AmazonData <- melt(AmazonData, id.vars = c("Day", "Tweets"), 
                            measure.vars = c("Sentiment.Prediction", "Actual"))
 
  
  ggplot(molten.AmazonData, aes(x=Day, y=value, fill = variable))+geom_bar(stat="identity", position=position_dodge())+xlab("Day")+ylab("Value")+ ggtitle("Amazon Stock Sentiment Analysis")


