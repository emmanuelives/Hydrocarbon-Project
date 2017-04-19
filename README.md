# Hydrocarbon-Project
Sentiment Analysis of the Tweets from Twitter on the recent Hydrocarbon Project in Tamil Nadu, India.

INSTALL THE REQUIRED PACKAGES
install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("data.table")
install.packages("plyr")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("RgoogleMaps")
install.packages("sentimentr")
install.packages("stringr")

LOAD THE PACKAGES INTO R
require(twitteR)
require(RCurl)
require(tm)
require(wordcloud)
require(wordcloud2)
require(data.table)
require(plyr)
require(ggplot2)
require(ggmap)
require(RgoogleMaps)
require(sentimentr)
require(stringr)

TWITTER API AUTHORIZATION PROCESS
1. Go to https://apps.twitter.com
2. Sign in
3. Create New App
NOTE: Make sure you attach your phone number to your twitter account.
4. Fill in the credentials 
Name: Apache Spark Data
Description: Sentiment Analysis of Data and Tweets 
Website: assign a website to it
5. Leave the rest of the text boxes empty
6. Go to the Keys and Access Tokens Tab
7. Scroll down and click on Create Access Tokens

THESE ARE THE CREDENTIALS YOU NEED TO ACCESS THE TWITTER API:
consumer_key <- 'G2ukqH7z5AvjhTZ5WM3uEMQ9a'
consumer_secret <- '0InaWrHLpIDjZPDUVdvqE6duwWxmyzi0iRjgePzUUvFMDxspoI'
access_token <- '431785833-yl21PGanAckxFN4RbV6nO6HuYx9h6hkV6EOVXhJk'
access_secret <- 'IreFzA9S5USbdrigy9zD2syXAnxJhxfV2S99kIfBZ653N'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
Type in these commands above in your RGUI console or copy and paste
- It will run for some time and ask you a question which you should respond with a YES or NO:
1. YES
2. NO
Type 1 
After that, run the following commands to access the tweets from your Twitter account.


SEARCH AND EXTRACT TWEETS FROM TWITTER
HydrocarbonProject <- searchTwitter("SaveNeduvasal", n=1500, lang="en", resultType = ”recent”)
HydrocarbonProject

CHECK THE STRUCTURE OF THE TWEETS
class(HydrocarbonProject)
str(HydrocarbonProject)

CONVERT LIST OF TWEETS TO TEXT
HydrocarbonProject_text <- sapply(HydrocarbonProject, function(x) x$getText())
class(HydrocarbonProject_text)
Amount_Of_Tweets <- length(HydrocarbonProject_text)
Amount_Of_Tweets

CREATE CORPUS FROM VECTOR TO TWEETS 
NOTE: Corpus - collection of text documents
HydrocarbonProject_corpus <- Corpus(VectorSource(HydrocarbonProject_text))
HydrocarbonProject_corpus
inspect(HydrocarbonProject_corpus[181]) 

REMOVE IRRELEVANT ATTACHMENTS TO THE TWEETS
HydrocarbonProject_clean <- tm_map(HydrocarbonProject_corpus, removePunctuation)
HydrocarbonProject_clean <- tm_map(HydrocarbonProject_clean, content_transformer(tolower))
HydrocarbonProject_clean <- tm_map(HydrocarbonProject_clean, removeWords, stopwords("english"))
HydrocarbonProject_clean <- tm_map(HydrocarbonProject_clean, removeNumbers)
HydrocarbonProject_clean <- tm_map(HydrocarbonProject_clean, stripWhitespace)
inspect(HydrocarbonProject_clean [181])

BUILD A TERM DOCUMENT MATRIX
tdm <- TermDocumentMatrix(HydrocarbonProject_clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
df <- data.frame(Words = names(v), Frequency=v, Sentiment_Number=m)
head(df, 10)

EXPLORE FREQUENT TERMS AND THEIR ASSOCIATIONS
findFreqTerms(tdm, lowfreq = 2)
findAssocs(tdm, terms = “neduvasal”, corlimit = 0.3)
head(df, 10)
barplot(df[1:20,]$Frequency, las = 2, names.arg = df[1:20,]$Words, col ="maroon", main ="Most Frequent Words", ylab = "Word Occurrencies")

CREATE AND PLOT A WORDCLOUD
wordcloud(HydrocarbonProject_clean, random.order=F, max.words=100, scale=c(3, 2), colors=rainbow(60))

CALCULATE THE SENTIMENT SCORE
positive_words <- c("SaveTamilnadu","save","SaveAgriculture","savefarmers","savetnfarmers","standwithfarmers","joinhandswithfarmers","saveneduvaasal")
negative_words <- c("stop","destroys","lose","marinaprotest","banhydrocarbon","neduvasalprotest","ban","StopHydrocarbonProject","banhydrocarbonproject","danger","fight","problems","no","Subjugation","farmersprotest")
positive_wordsDF <- data.frame(words=positive_words, value=1, stringsAsFactors=F)
negative_wordsDF <- data.frame(words=negative_words, value=-1, stringsAsFactors=F)
score.sentiment = function(words, positive_words, negative_words, .progress='none'){
require(plyr)
require(stringr)
scores = laply(words, function(HydrocarbonProject_clean, positive_words, negative_words){
pos.matches <- match(HydrocarbonProject_clean, positive_words)
neg.matches <- match(HydrocarbonProject_clean, negative_words)
pos.matches <- !is.na(pos.matches)
neg.matches <- !is.na(neg.matches) 
score <- sum(pos.matches)-sum(neg.matches)
return(score)
},positive_words, negative_words, .progress=.progress)
scores.df <- data.frame(score=scores, text=words)
return(scores.df)}

df2 <- score.sentiment(HydrocarbonProject_text, positive_words, negative_words, .progress='text')
df2$name <- (HydrocarbonProject)
HydrocarbonProject_analysis  <- sentimentr::extract_sentiment_terms(HydrocarbonProject_clean, polarity_dt = lexicon::hash_sentiment_jockers, valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "", amplifier.weight = 0.8, n.before = 5, n.after = 2, question.weight = 1, adversative.weight = 0.85, missing_value = 0)
HydrocarbonProject_analysis  
plotdata <- df2[c("name", "score")]
str(plotdata)
plotdata1 <- plotdata[!plotdata$score == 0,]
plotdata1 <- plotdata1[!plotdata1$score > 3, ]
plotdata1 <- plotdata1[!plotdata1$score < (-3), ]
qplot(factor(score), data=plotdata, geom="bar", fill="HydrocarbonProject", xlab = "Sentiment Score")

FIND THE LOCATION OF THE TWEETS
Tweet_Location <- get_map(location = "neduvasal", zoom = 6, color = "bw")
ggmap(Tweet_Location)
Tweet_Roadmap <- get_map(location = "neduvasal", maptype = "roadmap")
ggmap(Tweet_Roadmap)
ggmap(map, fullpage = TRUE)
