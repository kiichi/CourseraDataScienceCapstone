library(tm)
library(RWeka)
data("crude")
#library(doParallel)

# Sets the default number of threads to use
options(mc.cores=1)

#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
plot(tdm, terms = findFreqTerms(tdm, lowfreq = 6)[1:25], corThreshold = 0.5)


#####################################
f<-file('final/en_US/en_US.twitter.txt','r')
#lines<-readLines(f)#,100)
lines<-readLines(f,100)
close(f)


#no memory
btokens<-BigramTokenizer(lines)
head(sort(table(data.frame(btokens)),decreasing=T),10)


