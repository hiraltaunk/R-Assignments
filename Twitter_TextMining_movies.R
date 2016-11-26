
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr
updateR()

install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")
install.packages("plyr")
install.packages("twitteR")
install.packages("stringr")
install.packages("rtools")
install.packages("tm")
install.packages("wordcloud")

library(wordcloud)
library(devtools)
library(plyr)
library(twitteR)
library(tm)

set.seed(200)

#Twitter session information

api_key = "TIP6JVETY6GYfWHjTY361L3Dq"
api_secret = "BQPgWZ9N4kqd8jw5yiISMLdoiC3R1N6cbJKuzY9TGzj5Xky2H5"
access_token = "313785357-p1W0CK6t6QME2xGuEqaMHoDSdFa3T0hhibp4CEwK"
access_token_secret = "soubgsFkAwbmlGDrsbwgVSKKopex12aTObFdT4VtkPiVf"

devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0",version="0.6.1")

#Twitter authentication
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##hashtags <- c("#dear zindagi","#force 2","#the jungle book")
##needle <- paste(hashtags, collapse = " OR ")



#Get the top 500 tweets for the movie Dear Zindagi,Arrival
hashtags <- c("#doctor strange")
needle <- paste(hashtags, collapse ="OR")
tweets=searchTwitter(needle,n=500,lang ="en")

#dear zindagi","#force 2","#shivaay","#zootopia"

?searchTwitter()
#converting into dataframe 
df = do.call("rbind", lapply(tweets, as.data.frame))

head(df)
text = df$text
corpus = Corpus(VectorSource(text))

#Remove puntuation,whitespace and stopwords also convert into lower

corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,removeWords,stopwords("english"))
corpus = tm_map(corpus,content_transformer(tolower))




dtm = DocumentTermMatrix(corpus)
dtm2 = as.matrix(dtm)


##?DocumentTermMatrix()

frequency =colSums(dtm2)
frequency = sort(frequency,decreasing=TRUE)
head(frequency,n=200)


trunc_freq <- frequency[2:100]
frequency[1]
words =names(frequency)

clr=brewer.pal(6, "Dark2")   
wordcloud(words[1:50],frequency[1:50],random.order = FALSE,colors =clr)

