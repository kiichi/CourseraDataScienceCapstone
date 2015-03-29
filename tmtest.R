library(tm)
library(RWeka)
data("crude") #for testing
#library(doParallel)

# Sets the default number of threads to use
options(mc.cores=1)

path<-'data/blog_sm.csv'
cps <- Corpus(DataframeSource(read.csv(path,sep='\t'))) 
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, function(x) removeWords(x, stopwords("english")))

#Tokenizer for n-grams and passed on to the term-document matrix constructor
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
tdm <- TermDocumentMatrix(cps, control = list(tokenize = UnigramTokenizer))
tdm
tdm <- TermDocumentMatrix(cps, control = list(tokenize = BigramTokenizer))
tdm
tdm <- TermDocumentMatrix(cps, control = list(tokenize = TrigramTokenizer))
tdm

#####################################
lines<-readLines('data/blog_sm.csv')

tokens<-BigramTokenizer(lines)
head(sort(table(data.frame(tokens)),decreasing=T),20)

tokens<-TrigramTokenizer(lines)
head(sort(table(data.frame(tokens)),decreasing=T),20)

rm(lines)
########################################
#Install RGraphviz
# If you get an error from regular package installer, use following commands
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
# Try with crude's tdm
#plot(tdm, terms = findFreqTerms(tdm, lowfreq = 200)[1:20], corThreshold = 0.5)





