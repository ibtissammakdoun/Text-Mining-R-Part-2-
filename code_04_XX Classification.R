#**************************************************************************
#                  Chapter 4 : Classification
#**************************************************************************

#install.packages("tm")
library(tm)

#--------------------------------------------------------------------------
#                  4.2.: Preparing data for Classification
#--------------------------------------------------------------------------

#Load up the corpus

course_raw = scan("classification/Course-Descriptions.txt",
                  what="", sep="\n")

course_corpus <- VCorpus(VectorSource(course_raw))
inspect(course_corpus[[1]])

#Convert to lower case
course_corpus2 <- tm_map(course_corpus, content_transformer(tolower))

#Remove punctuations
course_corpus3 <- tm_map(course_corpus2, removePunctuation)

#Remove stopwords
course_corpus4 <- tm_map(course_corpus3, removeWords, stopwords())

inspect(course_corpus4[[1]])

#Generate TF-IDF matrix
course_dtm <- DocumentTermMatrix(course_corpus4)
course_dtm

findFreqTerms(course_dtm,5)

#Remove terms not in 90% of the documents. Only have those that are there
#in atleast 2 documents
dense_course_dtm <- removeSparseTerms(course_dtm, .8)

#Inspect to TF-IDF
inspect(dense_course_dtm)

#Convert continuous values to classes = { Yes, No }
conv_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

class_dtm <- apply(dense_course_dtm, MARGIN = 2, conv_counts)
class_dtm

#--------------------------------------------------------------------------
#                  4.3.: Building the model
#--------------------------------------------------------------------------

#Load the classifications for the descriptions
course_classes = scan("classification/Course-Classification.txt",
                  what="", sep="\n")

#install.packages("caret")
library(caret)

#Random split of training and testing sets
train_set <- createDataPartition(y=course_classes, p=.7,list=FALSE)
.
#spliting the dtm
train_dtm <- class_dtm[train_set,]
test_dtm <-class_dtm[-train_set,]

#split the course_classes
train_classes <- course_classes[train_set]
test_classes <- course_classes[-train_set]

#train the model using naive bayes
course_model <- train( data.frame(train_dtm), train_classes, method="nb")
course_model

#--------------------------------------------------------------------------
#                  4.3.: Predictions for Text
#--------------------------------------------------------------------------

#Predict for the test data
course_predictions <- predict(course_model,test_dtm)

#Analyze prediction accuracy
confusionMatrix(table(course_predictions , test_classes))

#--------------------------------------------------------------------------

