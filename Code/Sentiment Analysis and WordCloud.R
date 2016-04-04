require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


infile <- "JCDataGreenEnergy_All_TweetsOnly"
TweetsGreen <- read.csv(paste0('./', infile, '.csv'), header =TRUE)
na.omit(TweetsGreen)
Tweet<- na.omit(TweetsGreen)

infile <- "JCDataGreenEnergy_All"
TweetsAll <- read.csv(paste0('./', infile, '.csv'), header =TRUE)
na.omit(TweetsAll)
TweetAll<- na.omit(TweetsAll)

head(TweetAll)
str(TweetAll)
summary(TweetAll)


##Data Converstion Clean up

TweetAll$ID= as.character(TweetAll$ID)
TweetAll$TweetText= as.character(TweetAll$TweetText)
TweetAll$UserID= as.character(TweetAll$UserID)
TweetAll$UserName= as.character(TweetAll$UserName)
TweetAll$Locacation= as.character(TweetAll$Locacation)
TweetAll$Date= as.Date(TweetAll$Date)

##Removing Coordianates columns as the data is not complete

Cols<-c(1,2,3,4,5,7)

TweetData<- TweetAll[Cols]



###Creating a function to clean data from special characters 

clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  
  some_txt = gsub("@\\w+", "", some_txt)
  
  some_txt = gsub("[[:punct:]]", "", some_txt)
  
  some_txt = gsub("[[:digit:]]", "", some_txt)
  
  some_txt = gsub("http\\w+", "", some_txt)
  
  some_txt = gsub("[ t]{2,}", "", some_txt)
  
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  
  try.tolower = function(x)
    
  {
    
    y = NA
    
    try_error = tryCatch(tolower(x), error=function(e) e)
    
    if (!inherits(try_error, "error"))
      
      y = tolower(x)
    
    return(y)
    
  }
  
  some_txt = sapply(some_txt, try.tolower)
  
  some_txt = some_txt[some_txt != ""]
  
  names(some_txt) = NULL
  
  return(some_txt)
  
}


clean_text = clean.text(Tweet$TweetText)


##########################
#SENTIMENT ANALYSYS
##########################

pos = scan('c:/Users/casal_000/OneDrive/Documents/CAPSTPONE/CAPSTONE/Words/Positive.txt', what='character', comment.char=';')

neg = scan('c:/Users/casal_000/OneDrive/Documents/CAPSTPONE/CAPSTONE/Words/Negative.txt', what='character', comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  require(plyr)
  require(stringr)
  
  # Written by Jeffrey Breen  modified by Julian Casallas
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

analysis = score.sentiment(clean_text, pos, neg)
table(analysis$score)

mean(analysis$score)
hist(analysis$score)



######################
####WORDCLOUD
#####################

install.packages(c("wordcloud","tm"),repos="http://cran.r-project.org")

library(wordcloud)

#memory.limit()
#memory.size()

#memory.limit(16000) 


library(plyr); library(dplyr)
require(stringr)


tweet_corpus = Corpus(VectorSource(clean_text))

tweet_corpus <- Corpus(VectorSource(nohandles))
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))
tweet_corpus <- tm_map(tweet_corpus, removeWords, c("amp", "2yo", "3yo", "4yo"))
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)

tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("machine", "learning", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))



require(plyr)

m = as.matrix(tdm) #we define tdm as matrix
word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #and we visualize our data

#####Use only records with high counts but excluding words in our search criteria
dmmain<-dm[10:500,]
wordcloud(dmmain$word, dmmain$freq, random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal) #and we visualize our data

pal <- brewer.pal(8,"Dark2")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = tweet_corpus, scale=c(5,0.5), min.freq = 50, max.words=300, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

