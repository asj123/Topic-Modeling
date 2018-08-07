setwd("C:/Users/Dell/Documents/R/Topic Modelling")
library(readr)
library(tidyr) # dataset manipulation (e.g. row/col transforms)
library(dplyr) # dataset cleaning & magrittr data pipelines.
library(lubridate) # date cleaning.
#library(stringr) # regex and string manipulation.
library(tidytext) # tidy sentiment analysis and tf-idf.
#library(tm) # to clean the DSIs.
#library(qdap) # to clean the DSIs. 
library(quanteda) # quant text data package. https://github.com/kbenoit/quanteda/issues/519
library(phrasemachine) # noun-phrase generator using POS tagging of openNLP.
library(stringi) # string manipulation.
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(textclean)
library(tibble) # rownames_to_column
library(topicmodels)
library(csv)
library(tm)
library(csv)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(textstem)
#library(qdap)
library(magrittr)

reviews<- read.csv("glassdoor-nfa-reviews (2-10-17).csv")

#stem <- read.csv("EE_wordlist.csv")
#corpus1 <- Corpus(VectorSource(stem$Growth[!is.na(stem$Growth)]))
#corpus1 <- tm_map(corpus1,stemDocument)
#as.character(corpus1)
#Replace all punctations with space
reviews$Candidate_Response <- gsub("[[:punct:]]", " ", reviews$Candidate_Response)
#put spaces between words
reviews$Candidate_Response <- gsub("([a-z])([A-Z])", "\\1 \\2", reviews$Candidate_Response)
pros_corpus<-Corpus(VectorSource(reviews))
pros_corpus[[1]][[1]]
#reviews.Corpus<-tm_map(reviews.Corpus, PlainTextDocument)
clean <- function(myCorpus){
  
  
  # manual replacement with spaces. removePunctuation() will not do this.
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  #myCorpus<-Corpus(VectorSource(txt))
  
  cleaned2 <- myCorpus %>%
    tm_map(stripWhitespace) %>% 
    tm_map(removeNumbers) %>% # I noticed numbers are messy to work with. 
    tm_map(content_transformer(replace_symbol)) %>% # qdap. e.g. % = 'percent'
    tm_map(removePunctuation) %>% # including curly {} and round () brackets.
    tm_map(content_transformer(replace_contraction)) %>% # qdap. e.g. shouldn't replaced by 'should not'
    # tm_map(content_transformer(replace_abbreviation)) %>% # qdap. data(abbreviations)
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) 
  
  cleaned3 <-lemmatize_words(cleaned2)
  return(cleaned3)
  
}
#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))




#tweetsDS.Clean<-tm_map(tweetsDS.Corpus, PlainTextDocument)
# pros_corpus<-tm_map(pros_corpus,tolower)
# pros_corpus<-tm_map(pros_corpus,removeNumbers)
# pros_corpus<-tm_map(pros_corpus,removeWords,stopwords("english"))
# pros_corpus<-tm_map(pros_corpus,removePunctuation)
# pros_corpus<-tm_map(pros_corpus,stripWhitespace)
# pros_corpus<- lemmatize_words(pros_corpus)

#pros_corpus_clean <- clean(pros_corpus)

review_clean <- vector("character", nrow(reviews))
for (text in 1:nrow(reviews)) {
  Cps <- Corpus(VectorSource(reviews$Candidate_Response[text]))
  review_clean[text] <- as.character(clean(myCorpus = pros_corpus))[[1]]
  #review_clean[text] <- pros_corpus_clean[[text]][[1]]
}

my_stopwords <- c(stopwords("english"), "NFA", "nfa", "national", "futures", "association", 
                  "work", "day")

# pros_unigrams <- tokenize(pros_corpus, what = "word",
#                           removePunct = TRUE, 
#                           removeNumbers = TRUE,
#                           removeSymbols = TRUE) %>%
#   removeFeatures(my_stopwords) 
# class(pros_unigrams)
# pros_unigrams


pros_phrases <- phrasemachine(review_clean,
                              maximum_ngram_length = 2,
                              minimum_ngram_length = 1,
                              return_phrase_vectors = FALSE,
                              return_tag_sequences = FALSE)
