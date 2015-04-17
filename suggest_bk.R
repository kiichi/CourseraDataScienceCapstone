library(tm)
library(RWeka)
#library(doParallel)
#registerDoParallel(2)
#stopImplicitCluster()
#detectCores()
# Sets the default number of threads to use

getCorpus<-function(path){
	options(mc.cores=1)
	profanity<-gsub(",","",read.csv("Terms-to-Block.csv")[-(1:3),2])
	cps <- Corpus(VectorSource(readLines(path,encoding="UTF-8")))
	#cps <- tm_map(cps, content_transformer(tolower))
	cps <- tm_map(cps, tolower)
	cps <- tm_map(cps, removePunctuation)
	cps <- tm_map(cps, removeNumbers)
	cps <- tm_map(cps, stripWhitespace)	
	cps <- tm_map(cps, function(x) removeWords(x, profanity))
	cps	
}

#somehow, passing corpus in tokenizer generates attr names in words; e.g. "list"
getTokens2<-function(corpus,num){
	tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = num, max = num))
	tokenizer(corpus)
}


getTokens<-function(path,num){		
	tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = num, max = num))
	tokenizer(readLines(path,encoding="UTF-8"))
}

getFrequency<-function(tokens){
	tbl<-table(data.frame(tokens))
	tbl
	freq<-data.frame(token=rownames(tbl),freq=as.vector(tbl))
	freq
}

#path<-'data/blog_sm.csv'
path<-'data/simple.csv'
#cps<-getCorpus(path)
#uniTokens<-getTokens(cps,1) 
#biTokens<-getTokens(cps,2)
#triTokens<-getTokens(cps,3)


uniTokens<-getTokens(path,1)
biTokens<-getTokens(path,2)
triTokens<-getTokens(path,3)

uniFreq<-getFrequency(uniTokens)
biFreq<-getFrequency(biTokens)
triFreq<-getFrequency(triTokens)

#uniFreq[uniFreq>1]
#names(uniFreq[uniFreq>1])

#freq_tbl<-head(sort(table(data.frame(tokens)),decreasing=T),20)
#freq_df<-data.frame(token=rownames(freq_tbl),freq=as.vector(freq_tbl))
#freq_df$token<-factor(freq_df$token,levels=freq_df[order(freq_df$freq),"token"])


