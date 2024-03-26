#unique(film$Title)

#1
film <- read.csv("IMDb_Feature Film_2022_review_data.csv")
film1=film[which(film$Title=="Thor: Love and Thunder"),]

library(tm)
x1 <- Corpus(VectorSource(film1$Review))#建語料庫
x1 <- tm_map(x1,tolower)
x1 <- tm_map(x1,content_transformer(tolower))
x1 <- tm_map(x1,removePunctuation)
x1StopWords <- c(stopwords(),"the","and","this","that","was","but","for")
x1 <- tm_map(x1,removeWords,x1StopWords)
library(SnowballC)#好像不用（但之前...
x1 <- tm_map(x1,stemDocument)

x1tdm <- TermDocumentMatrix(x1)
inspect(x1tdm)

x1review <- as.matrix(x1tdm)
x1freq <- rowSums(x1review)
x1freq <- sort(x1freq, decreasing=T)
x1freq[1:10]

barplot(x1freq[1:10],las=2,col="blue")
library(wordcloud2)
x1freqframe <- data.frame(word=names(x1freq),num=x1freq)
wordcloud2(x1freqframe,size=1)

library(tidytext)
library(tidyverse)
bing_word_counts1 <- x1freqframe %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE)
table(bing_word_counts1$sentiment)

#2
film2=film[which(film$Title=="Texas Chainsaw Massacre"),]

library(tm)
x2 <- Corpus(VectorSource(film2$Review))#建語料庫
x2 <- tm_map(x2,tolower)
x2 <- tm_map(x2,content_transformer(tolower))
x2 <- tm_map(x2,removePunctuation)
x2StopWords <- c(stopwords(),"the","and","this","that","was","but","for")
x2 <- tm_map(x2,removeWords,x2StopWords)
library(SnowballC)
x2 <- tm_map(x2,stemDocument)

x2tdm <- TermDocumentMatrix(x2)
inspect(x2tdm)

x2review <- as.matrix(x2tdm)
x2freq <- rowSums(x2review)
x2freq <- sort(x2freq, decreasing=T)
x2freq[1:10]

barplot(x2freq[1:10],las=2,col="blue")
library(wordcloud2)
x2freqframe <- data.frame(word=names(x2freq),num=x2freq)
wordcloud2(x2freqframe,size=1)

library(tidytext)
library(tidyverse)
bing_word_counts2 <- x2freqframe %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE)
table(bing_word_counts2$sentiment)
