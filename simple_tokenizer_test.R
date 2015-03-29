#-----------------------------------------------------------------------
# Community TA's simple tokenizer
# https://github.com/zero323/r-snippets/blob/master/R/ngram_tokenizer.R
# Reference
# https://class.coursera.org/dsscapstone-003/forum/thread?thread_id=54
#-----------------------------------------------------------------------
#' Ngrams tokenizer
#' 
#' @param n integer
#' @param skip_word_none boolean see: ?stri_split_boundaries
#' @param skip_word_number boolean see: ?stri_split_boundaries
#' @return n-gram tokenizer function
#' @examples
#' trigram_tokenizer <- ngram_tokenizer(3)
#' trigram_tokenizer(as.character(citation()))
#' 
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE,filter_words=c()) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  
  #' To avoid :: calls
  stri_split_boundaries <- stringi::stri_split_boundaries
  stri_join <- stringi::stri_join
  
  options <- stringi::stri_opts_brkiter(
    type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
  )
  
  #' Tokenizer
  #' 
  #' @param x character
  #' @return character vector with n-grams
  function(x) {    
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
    
    # If filter words provided, clean up
    #if (length(filter_words)>0){
      tokens <- tokens[!tokens %in% filter_words]  
    #}
    
    
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}

# or tau package? a bit noise
#tokenize_ngrams <- function(x, n=3) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

# also take filter_words= now

#unigram<-ngram_tokenizer(1,filter_words=profanity)

unigram<-ngram_tokenizer(1)
bigram<-ngram_tokenizer(2)
trigram<-ngram_tokenizer(3)


test<-"I have a pen. This is something. This one is good. I do something. I have a cat. I have a cat x."
test_uni<-unigram(test)
test_bi<-bigram(test)
test_tri<-trigram(test)

###############
freq_uni<-head(sort(table(data.frame(test_uni)),decreasing=T),5)
freq_uni
freq_uni/length(test_uni)

freq_bi<-head(sort(table(data.frame(test_bi)),decreasing=T),5)
freq_bi
prob_bi<-freq_bi/length(test_bi)

a<-"I have a"
input_bi <- bigram(a)
matches<-rownames(prob_bi)[grepl("a ",rownames(prob_bi))]

# I have a cat -> 0.001690458  (occured twice)
matches[1] 
prob_bi[rownames(prob_bi) == input_bi[1]] * prob_bi[rownames(prob_bi) == input_bi[2]] * prob_bi[rownames(prob_bi) == matches[1]]
# I have a pen -> 0.001690458  (occured once)
matches[2]
prob_bi[rownames(prob_bi) == input_bi[1]] * prob_bi[rownames(prob_bi) == input_bi[2]] * prob_bi[rownames(prob_bi) == matches[2]]

freq_tri<-head(sort(table(data.frame(test_bi)),decreasing=T),5)
freq_tri
freq_tri/length(test_tri)

###################################
library(doParallel)
registerDoParallel(4)

#read all twitter
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f,100)
close(f)

tw_uni<-unigram(lines)
tw_bi<-bigram(lines)
tw_tri<-trigram(lines)
