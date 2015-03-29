library(tm)
library(RWeka)
library(ggplot2)
library(gridExtra)
options(mc.cores=1)
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

path<-'data/blog.csv'
cps <- Corpus(DataframeSource(read.csv(path,sep='\t'))) 
cps <- tm_map(cps, removePunctuation)
#cps <- tm_map(cps, content_transformer(tolower)) 
cps <- tm_map(cps, function(x) removeWords(x, stopwords("english")))

tdm <- TermDocumentMatrix(cps, control = list(tokenize = UnigramTokenizer))
tdm
tdm <- TermDocumentMatrix(cps, control = list(tokenize = BigramTokenizer))
tdm
tdm <- TermDocumentMatrix(cps, control = list(tokenize = TrigramTokenizer))
tdm


lines<-readLines('data/blog_sm.csv')
tokens<-TrigramTokenizer(lines)
freq_tbl<-head(sort(table(data.frame(tokens)),decreasing=T),20)
freq_df<-data.frame(token=rownames(freq_tbl),freq=as.vector(freq_tbl))
freq_df$token<-factor(freq_df$token,levels=freq_df[order(freq_df$freq),"token"])
qplot(token,freq,data=freq_df,geom="bar",stat="identity",fill=freq) + coord_flip()



#p<-ggplot(freq_df,aes(token,freq,fill=factor(freq_df$freq))) 
#p<- p + geom_bar(stat="identity")
#p<- p + coord_flip()
#p


###########################
generateHist<-function(path,tokenizer){  
  tokens<-tokenizer(readLines(path))
  freq_tbl<-head(sort(table(data.frame(tokens)),decreasing=T),20)
  freq_df<-data.frame(token=rownames(freq_tbl),freq=as.vector(freq_tbl))
  freq_df$token<-factor(freq_df$token,levels=freq_df[order(freq_df$freq),"token"])
  qplot(token,freq,data=freq_df,geom="bar",stat="identity",fill=freq) + coord_flip() + theme(legend.position="none")
}

path<-'data/blog_sm.csv'
grid.arrange(generateHist(path,UnigramTokenizer),
             generateHist(path,BigramTokenizer),
             generateHist(path,TrigramTokenizer),
             ncol=3)


###########
showTDM<-function(path){
  cps <- Corpus(DataframeSource(read.csv(path,sep='\t'))) 
  cps <- tm_map(cps, removePunctuation)
  cps <- tm_map(cps, function(x) removeWords(x, stopwords("english")))
  TermDocumentMatrix(cps, control = list(tokenize = TrigramTokenizer))
}

showTDM(path)