#' A Text Excerpting Function
#'
#' This function extracts words and their surrounding text. Note that this only words with individual words (or multiple individual words).
#' @param text Character string to search.
#' @param words Character vector containing key search words.
#' @param n Return +/- n words around key search words.
#' @keywords 
#' @export
#' @examples
#' texcerpt(example, words = c("data", "statistic"), n = 10)

texcerpt <- function(text, words, n){
  require(tm)
  require(dplyr)
  word <- tolower(words) %>%
    paste(collapse = "|")
  text <-  text %>%
    stripWhitespace() %>% # Strip whitespace
    strsplit(" ") %>%
    unlist() %>%
    c()
  spots <- which(text %in% text[grep(word, text, ignore.case = T)])
  sapply(spots, 
         function(y) 
           paste(text[max(c(1, y-n)):min(c(length(text), y+n))], 
                 collapse = " ")) %>%
    unique()
}


#' A Function for Getting English Words
#'
#' Get list of English words.
#' @param 
#' @keywords 
#' @export
#' @examples
#' getEnglishWords()
getEnglishWords <- function(){
  read.table("http://tinyurl.com/bvapsn7") %>%
    select(V1) %>%
    c() %>%
    unlist() %>%
    as.character()
}



#' A Language Checking Function
#'
#' This function checks if given percentage of text is in a certain language where the user supplies the list of words. For English, can use getEnglishWords() function.
#' @param text Character string to search.
#' @param language_list Object containing list of words for a given language.
#' @param threshold Value in (0, 1] above which text is considered to be a match for the language. Set to 2/3 by default.
#' @keywords 
#' @export
#' @examples
#' languagecheck(text = some_text, language_list = eng_words, threshold = 2/3)
languagecheck <- function(text, language_list, threshold = 2/3){
  text <- gsub("[^A-Za-z0-9]", " ", text) %>%
    tolower() %>%
    stripWhitespace() %>%
    strsplit(" ") %>%
    unlist() %>%
    as.character()
  
  prop <- length(text[text %in% language_list])/length(text)
  
  ifelse(prop >= threshold, 1, 0)
  
}



#' A Function for Splitting Apart Character Separated Values
#'
#' This function splits character-separated values.
#' @param x Character string with character-separated values.
#' @param char Character separating values.
#' @keywords 
#' @export
#' @examples
#' untangle(c("400|500|600"), char = "|")

untangle <- function(x, char = ",") {
  
  x %>%
    unlist() %>%
    c() %>%
    .[!is.na(.)] %>%
    as.character() %>%
    strsplit(split = char, fixed = T) %>%
    unlist() %>%
    trimws() %>%
    unique()
  
}


