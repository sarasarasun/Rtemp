# Twitter

tweets.df <- read.csv("redsox_tweets.csv") #? latin encoder - file encoding will solve that problem later?

tweets.df <- read.csv("redsox_tweets.csv",fileEncoding = "latin1") # this is the simple solution 

# pre-clean: gsub is akin to replace in Excel
# we are removing special characters, links, etc.
# this process will change class to "character"; so at end will convert to DF
# I have added a few more
#
tweets_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets_clean <- gsub("@\\w+", "", tweets_clean)
tweets_clean <- gsub("[[:punct:]]", "", tweets_clean)
tweets_clean <- gsub("[[:digit:]]", "", tweets_clean)
tweets_clean <- gsub("http\\w+", "", tweets_clean)  
tweets_clean <- gsub("amp", "", tweets_clean)            # remove amp which mens &
tweets_clean <- tolower(tweets_clean)                    # convert to lower case
tweets_clean <- gsub("redsox", "", tweets_clean)         # remove redsox


class(tweets_clean)
tweets_clean <- as.data.frame(tweets_clean)
tweets_clean[1:10,]
tweets.df$text[[1]]

#create corpus
library(tm)
tweets_df <- DataframeSource(tweets_clean)
tweets_corpus <- Corpus(tweets_df)

# cleaning within tm package 
# several options, some of which were done earlier such as 
# punctution and remove numbers
# here we will remove stop words
getTransformations()
# stopwords
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords('english'))

#debug code
#tweets_corpus <- tm_map(tweets_corpus, function(x) removeWords(x, stopwords('english'))) # also not working

#tweets_corpus <- tm_map(tweets_corpus, PlainTextDocument) # fail later

#tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords('english'), PlainTextDocument)
#
# create matrices - they are transposes of each other
tweets_tdm <- TermDocumentMatrix(tweets_corpus) # bug note: mac has a problem with this line of code
tweets_dtm <- DocumentTermMatrix(tweets_corpus)
inspect(tweets_tdm[1:10,1:10])

# words occuring more than n times
tweets_freq <- findFreqTerms(tweets_tdm,50)
length(tweets_freq)
tweets_freq

# convert to regular R matrix
# count number of times a word occurs
tweets_matrix_tdm <- as.matrix(tweets_tdm)
tweets_rowsum <- rowSums(tweets_matrix_tdm)
tweets_rowsum <- sort(tweets_rowsum,decreasing=TRUE)
head(tweets_rowsum) 

# create a plot
plot(tweets_rowsum)
plot(tweets_rowsum, xlim = c(0,500))  # set range for x-axis

# Word Cloud:  d1 creats a DF with name then frequency                       
library(wordcloud)
d1 <- data.frame(word=names(tweets_rowsum), freq=tweets_rowsum)
head(tweets_rowsum, 25) 
d2 <- d1[1:25,] # select the first 25 rows
wordcloud(d2$word,d2$freq,  color= "blue")

###########################################################
#                                                         #
#  Associations / Correlation                             #
#                                                         #
###########################################################
#
# the 0.2 is the correlation threshold - this can be changed
assoc <- findAssocs(tweets_tdm, "koji", 0.2)
assoc
class(assoc) # note it is a list
assoc_df <- as.data.frame(assoc) # convert to dataframe
assoc_df_n  <- data.frame(rownames(assoc_df), freq=assoc_df)
wordcloud(assoc_df_n$rownames.assoc_df, assoc_df_n$koji, color= "red")

