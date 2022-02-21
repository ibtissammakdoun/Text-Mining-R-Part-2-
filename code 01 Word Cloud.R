
#**************************************************************************
#                  Chapter 1 : Create a wordcloud
#**************************************************************************

#install.packages("tm")
library(tm)

#--------------------------------------------------------------------------
#                  1.2.: Prepare data for word cloud
#--------------------------------------------------------------------------

#Load the corpus
course_corpus <- VCorpus(DirSource("/Users/ibtissammakdoun/Desktop/Ex_Files_Analyse_Text_R/Exercise Files/cours"))

#Conver to minuscle
course_corpus2 <- tm_map(course_corpus, content_transformer(tolower))

#Delete ponctuations
course_corpus3 <- tm_map(course_corpus2, removePunctuation)

#Delete stopwords
course_corpus4 <- tm_map(course_corpus3, removeWords, stopwords())

#Generate TF-IDF matrix
course_dtm <- DocumentTermMatrix(course_corpus4)

#Inspect to TF-IDF
inspect(course_dtm)

#Generer la data frame de frequence des mots
word_frequency <- sort(colSums(as.matrix(course_dtm)),
                       decreasing=TRUE)
df_frequency<- data.frame(word = names(word_frequency),
                          freq=word_frequency)

head(df_frequency)

#--------------------------------------------------------------------------
#                  1.3.: Display the word cloud
#--------------------------------------------------------------------------

#install.packages("wordcloud")
library(wordcloud)

#Simple wordcloud
wordcloud(df_frequency$word,
          df_frequency$freq)

#Top 10 words
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=10, min.freq = 1)

#--------------------------------------------------------------------------
#                  1.4.: Enhance the word cloud
#--------------------------------------------------------------------------

#Choose a specific font and order
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=10, min.freq = 1,
          random.order=FALSE,
          font = 3)

#Using a color palatte

library(RColorBrewer)

word_pal <- brewer.pal(10,"Dark2")

wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=20, min.freq = 1,
          random.order=FALSE,
          colors=word_pal, font = 3)

#--------------------------------------------------------------------------
