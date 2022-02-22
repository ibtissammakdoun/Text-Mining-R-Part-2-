#**************************************************************************
#                  Chapter 2 : Sentiment Analysis
#**************************************************************************

#install.packages(c("sentimentr","data.table","plyr"))
library(sentimentr)
library(data.table)
library(plyr)
#--------------------------------------------------------------------------
#                  2.2.: Finding Sentiment
#--------------------------------------------------------------------------

#Load the movie reviews file and convert it into sentences
movie_reviews <- readLines(file("reviews/The-Tinder-Swindler.txt"))

review_text <- get_sentences(movie_reviews)

#See sentiments for each line
sentiment(review_text)

#Sentiment by each review
sentiments <- sentiment_by(review_text)

#--------------------------------------------------------------------------
#                  2.3.: Summarizing Sentiment
#--------------------------------------------------------------------------

#Convert sentiment data.table to a data frame
sentiment_df <- setDF(sentiments)

#Function that generates a sentiment class based on sentiment score
get_sentiment_class <- function(sentiment_score) {
  
  sentiment_class = "Positive"
  
  if ( sentiment_score < -0.3) {
    sentiment_class = "Negative"
  } 
  
  else if (sentiment_score < 0.3) {
    sentiment_class = "Neutral"
  }
  
  sentiment_class
}

#add a sentiment_class attribute
sentiment_df$sentiment_class <- 
        sapply(sentiment_df$ave_sentiment,get_sentiment_class)

#Print resulting sentiment
sentiment_df[,4:5]

#Draw a pie chart
sentiment_summary <- count(sentiment_df, "sentiment_class")

pie(sentiment_summary$freq, 
    sentiment_summary$sentiment_class,
    col=c("Red","Blue","Green"))

#--------------------------------------------------------------------------
#                  2.4.: Analyzing Emotions
#--------------------------------------------------------------------------

#Create a dataframe for emotions by review
emotion_df <- setDF(emotion_by(review_text))
emotion_df

#aggregate by emotion types and remove 0 values
emotion_summary=subset(
                  aggregate(emotion_count  ~ emotion_type , 
                                 emotion_df, sum),
                   emotion_count > 0 )
library(RColorBrewer)
pal = palette("Dark2")
#Draw a pie chart for emotion summary
pie(emotion_summary$emotion_count, emotion_summary$emotion_type,
    col= c("Red","Green","Blue","Orange","Brown","Purple","Yellow","Grey","Pink","White","Maroon","Beige","Black") )
pie(emotion_summary$emotion_count, emotion_summary$emotion_type,
    col= pal )

#--------------------------------------------------------------------------


                              