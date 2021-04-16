library(syuzhet)
library(SnowballC)
library(dplyr)
library(tm)
library(wordcloud)
library(readr)

#' Get Sentiment Values for a String
#' @description
#' Iterates over a vector of strings and returns sentiment values based on user supplied method. The default method, "syuzhet" is a custom sentiment dictionary developed in the Nebraska Literary Lab.  
#' The default dictionary should be better tuned to fiction as the terms were extracted from a collection of 165,000 human coded sentences taken from a small corpus of contemporary novels.   
#' @param char_v A vector of strings for evaluation.
#' @param method A string indicating which sentiment method to use. Options include "syuzhet", "bing", "afinn", "nrc" and "stanford."  See references for more detail on methods.
#' @param language A string. Only works for "nrc" method
#' @param cl Optional, for parallel sentiment analysis.
#' @param path_to_tagger local path to location of Stanford CoreNLP package
#' @param lexicon a data frame with at least two columns labeled "word" and "value."

get_sentiment_rus <- function(char_v, method="custom", lexicon=NULL, 
                              path_to_tagger = NULL, cl = NULL, 
                              language = "english") {
        language <- tolower(language)
        russ.char.yes <- "[\u0401\u0410-\u044F\u0451]"
        russ.char.no <- "[^\u0401\u0410-\u044F\u0451]"
        
        if (is.na(pmatch(method, c("syuzhet", "afinn", "bing", "nrc", 
                                   "stanford", "custom")))) 
                stop("Invalid Method")
        if (!is.character(char_v)) 
                stop("Data must be a character vector.")
        if (!is.null(cl) && !inherits(cl, "cluster")) 
                stop("Invalid Cluster")
        if (method == "syuzhet") {
                char_v <- gsub("-", "", char_v)
        }
        if (method == "afinn" || method == "bing" || method == "syuzhet") {
                word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
                if (is.null(cl)) {
                        result <- unlist(lapply(word_l, get_sent_values, 
                                                method))
                }
                else {
                        result <- unlist(parallel::parLapply(cl = cl, word_l, 
                                                             get_sent_values, method))
                }
        }
        else if (method == "nrc") {
                #    word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
                word_l <- strsplit(tolower(char_v), paste0(russ.char.no, "+"), perl=T)
                lexicon <- dplyr::filter(syuzhet:::nrc, lang == tolower(language), 
                                         sentiment %in% c("positive", "negative"))
                lexicon[which(lexicon$sentiment == "negative"), "value"] <- -1
                result <- unlist(lapply(word_l, get_sent_values, method, 
                                        lexicon))
        }
        else if (method == "custom") {
                #    word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
                word_l <- strsplit(tolower(char_v), paste0(russ.char.no, "+"), perl=T)
                result <- unlist(lapply(word_l, get_sent_values, method, 
                                        lexicon))
        }
        else if (method == "stanford") {
                if (is.null(path_to_tagger)) 
                        stop("You must include a path to your installation of the coreNLP package.  See http://nlp.stanford.edu/software/corenlp.shtml")
                result <- get_stanford_sentiment(char_v, path_to_tagger)
        }
        return(result)
}

#' Get Emotions and Valence from NRC Dictionary
#' @description
#' Calls the NRC sentiment dictionary to calculate the presence of eight different 
#' emotions and their corresponding valence in a text file.
#' 
#' @param char_v A character vector
#' @param language A string
#' @param cl Optional, for parallel analysis
#' 
#' @return data frame where each row represents a sentence from the original file.  
#' The columns include one for each emotion type as well as a positive or negative valence.  
#' The ten columns are as follows: "anger", "anticipation", "disgust", "fear", "joy", 
#' "sadness", "surprise", "trust", "negative", "positive." 

get_nrc_sentiment_rus <- function(char_v, cl=NULL, language = "english"){
        language <- tolower(language)
        russ.char.yes <- "[\u0401\u0410-\u044F\u0451]"
        russ.char.no <- "[^\u0401\u0410-\u044F\u0451]"
        
        if (!is.character(char_v)) stop("Data must be a character vector.")
        if(!is.null(cl) && !inherits(cl, 'cluster')) stop("Invalid Cluster")
        lexicon <- dplyr::filter(syuzhet:::nrc, lang == language) # filter lexicon to language
        #word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
        word_l <- strsplit(tolower(char_v), paste0(russ.char.no, "+"), perl=T)
        
        if(is.null(cl)){
                nrc_data <- lapply(word_l, get_nrc_values, lexicon = lexicon)
        }
        else{
                nrc_data <- parallel::parLapply(cl=cl, word_l, lexicon = lexicon, get_nrc_values)
        }
        result_df <- as.data.frame(do.call(rbind, nrc_data), stringsAsFactors=F)
        # reorder the columns
        my_col_order <- c(
                "anger", 
                "anticipation", 
                "disgust", 
                "fear", 
                "joy", 
                "sadness", 
                "surprise", 
                "trust", 
                "negative", 
                "positive"
        )
        result_df[, my_col_order]
}

stopwords <- read.csv("gh-stopwords-json-ru.txt", header = FALSE, sep = "\t", 
                      encoding = "UTF-8")

lemma <- read_csv("S:/_STUDIES/Автофорумы/2. Exploratory Analysis/Rio_lemmatization.csv")

poa_word_v <- lemma$Message
nrc_data <- get_nrc_sentiment_rus(poa_word_v, language="russian")

# draw the histogram with the specified number of bins
barplot(
        sort(colSums(prop.table(nrc_data[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "KIA RIO", 
        sub = "Сентимент анализ сообщений о ходовой KIA RIO", 
        xlab="Percentage"
)

lemma <- read_csv("S:/_STUDIES/Автофорумы/2. Exploratory Analysis/Vesta_lemmatization.csv")

poa_word_v <- lemma$Message
nrc_data <- get_nrc_sentiment_rus(poa_word_v, language="russian")

# draw the histogram with the specified number of bins
barplot(
        sort(colSums(prop.table(nrc_data[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "LADA VESTA", 
        sub = "Сентимент анализ сообщений о ходовой LADA VESTA", 
        xlab="Percentage"
)

VestaMessage <- paste(poa_word_v, collapse = " ")

words.vec <- VectorSource(VestaMessage)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("russian"))
tdm <- TermDocumentMatrix(words.corpus)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
cloudFrame <- data.frame(word=names(wordCounts), freq=wordCounts)

wordcloud(cloudFrame$word, cloudFrame$freq, 
          min.freq = input$freq1, 
          max.words = input$maxw, 
          random.order = input$rand, 
          colors = brewer.pal(8, "Dark2"))
