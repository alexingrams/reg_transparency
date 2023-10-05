#Alex Ingrams
#LDA model for paper 'A machine learning approach to open public comments for policymaking'
#January 2019

#Update October 2023: Receiving new object error message with ggplot2. Seems to be something about masked features of package.

#Load text packages

library("tm")

library("rvest")

library("tidyverse")

library("magrittr")

library("httr")

library("stringr")

library("rjson")

library("jsonlite")

library("textstem")

library ("qdap")

library("dplyr")

library("tidytext")

library("ggplot2")

library("topicmodels")

library("bindrcpp")

library("ldatuning")


#Pull documents (public comments) from regulations.gov
#API allows max 1000 documents per request

webpage <- httr:: GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=0")

webpage_2 <- httr::GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=1000")

webpage_3 <-httr::GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=2000")

webpage_4 <-httr::GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=3000")

webpage_5 <-httr::GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=4000")

webpage_6 <-httr::GET("https://api.data.gov:443/regulations/v3/documents.json?api_key=iXKte74DWlCymlQdkLUSt601X2R4Vng2MXPMFYmI&encoded=1&dct=PS&dktid=TSA-2013-0004&rpp=1000&po=5000")


#Read the content of the documents as text

webpage_text <- httr:: content(webpage, "text")

webpage_text_2 <- httr:: content(webpage_2, "text")

webpage_text_3 <- httr:: content(webpage_3, "text")

webpage_text_4 <- httr:: content(webpage_4, "text")

webpage_text_5 <- httr:: content(webpage_5, "text")

webpage_text_6 <- httr::  content(webpage_6, "text")

#Convert document text into flat vector

webpage_json <- fromJSON(webpage_text, flatten = TRUE)

webpage_json_2 <- fromJSON(webpage_text_2, flatten = TRUE)

webpage_json_3 <- fromJSON(webpage_text_3, flatten = TRUE)

webpage_json_4 <- fromJSON(webpage_text_4, flatten = TRUE)

webpage_json_5 <- fromJSON(webpage_text_5, flatten = TRUE)

webpage_json_6 <- fromJSON(webpage_text_6, flatten = TRUE)

#Transform each of the corpuses into a data frame

webpage_df <- as.data.frame(webpage_json)

webpage_df_2 <- as.data.frame(webpage_json_2)

webpage_df_3 <- as.data.frame(webpage_json_3)

webpage_df_4 <- as.data.frame(webpage_json_4)

webpage_df_5 <- as.data.frame(webpage_json_5)

webpage_df_6 <- as.data.frame (webpage_json_6)

#Merge the six corpuses into one corpus

webpage_df_all <-rbind(webpage_df, webpage_df_2, webpage_df_3, webpage_df_4, webpage_df_5, webpage_df_6)

#Select only text from the column with the comment text

webpage_df_comments <- select(webpage_df, text = documents.commentText)


#Create corpus as a character only vector
corpus <- Corpus(VectorSource(as.vector(webpage_df_comments$text)))

#Transform all text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

#Remove punctuation
corpus <- tm_map(corpus, content_transformer(removePunctuation))

#Remove numbers
corpus <- tm_map(corpus, content_transformer(removeNumbers))

#Extra spaces between text reduced to single space
corpus <- tm_map(corpus, content_transformer(stripWhitespace))

#Remove common words such as conjunctions and (in)definite articles
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#Lematize words
corpus <- tm_map(corpus, lemmatize_strings)

#Create Document Term Matrix
comments_DTM <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))

#Convert DTM into a character matrix
comments_m <- as.matrix(comments_DTM)

#Remove empty rows in the matrix
rowSum <- apply(comments_m, 1, sum)
comments_m <- comments_m[rowSum>0,]

#Test for best number of topics (beta)
k_test <- FindTopicsNumber(comments_m, topics = seq(from = 2, to = 30, by = 2), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010") , mc.cores = 2L)
FindTopicsNumber_plot(k_test)

#Run the LDA model
text_lda <- LDA(comments_m, k=20, alpha=2.5, method = "VEM", control = NULL)

#Visualise the model
text_topics <- tidy(text_lda, matrix = "beta")
  text_topics

text_top_terms <- text_topics %>%
 group_by(topic) %>%
 top_n(10, beta) %>%
 ungroup() %>%
 arrange(topic, -beta)

text_top_terms %>%
 mutate(term = reorder(term, beta)) %>%
 ggplot(aes(term, beta, fill = factor(topic))) +
 geom_col(show.legend = FALSE) +
 facet_wrap(~ topic, scales = "free") +
 coord_flip()




