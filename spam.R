library(dplyr)
sms_spam_df <- read.csv(file="C:/Users/taoya/OneDrive/Documents/textminingspam/spam.csv", stringsAsFactors=F)
str(sms_spam_df)
sms_spam_df<-sms_spam_df[，1：2]
names(sms_spam_df)[1]<-"type"
names(sms_spam_df)[2]<-"text"
library(tm)
sms_corpus <- Corpus(VectorSource(sms_spam_df$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
#translate all letters to lower case
clean_corpus <- tm_map(sms_corpus, tolower)
# remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
# remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
stopwords()[1:10]
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
inspect(clean_corpus[1:3])
sms_dtm <- DocumentTermMatrix(clean_corpus)
inspect(sms_dtm[1:4, 1:35])
spam_indices <- which(sms_spam_df$type == "spam")
spam_indices[1:3]
ham_indices <- which(sms_spam_df$type == "ham")
ham_indices[1:3]
library(wordcloud)
wordcloud(clean_corpus[ham_indices], min.freq=50)
wordcloud(clean_corpus[spam_indices], min.freq=20)
sms_raw_train <- sms_spam_df[1:4457,]
sms_raw_test <- sms_spam_df[4458:5572,]
sms_dtm_train <- sms_dtm[1:4457,]
sms_dtm_test <- sms_dtm[4458:5572,]
sms_corpus_train <- clean_corpus[1:4457]
sms_corpus_test <- clean_corpus[4458:5572]
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
five_times_words <- findFreqTerms(sms_dtm_train, 5)
length(five_times_words)
five_times_words[1:5]
sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))

sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


sms_train <- apply(sms_train, 2, convert_count)
sms_test <- apply(sms_test, 2, convert_count)

library(e1071)

sms_classifier <- naiveBayes(sms_train, factor(sms_raw_train$type))
class(sms_classifier)
sms_test_pred <- predict(sms_classifier, newdata=sms_test)

table(sms_test_pred, sms_raw_test$type)