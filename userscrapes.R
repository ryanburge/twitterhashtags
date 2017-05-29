user <- getUser('barackobama')
user$toDataFrame()


timeline <- userTimeline('edstetzer', n=5000)
timeline <- twListToDF(timeline)

timeline <- userTimeline('GovRauner', n=5000)
timeline <- twListToDF(timeline)

timeline$text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", timeline$text)
timeline$text <- gsub("/.*","",timeline$text)

nohandles <- str_replace_all(timeline$text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
library(RColorBrewer)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

