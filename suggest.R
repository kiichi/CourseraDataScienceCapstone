library(tm)
library(RWeka)
library(doParallel)
registerDoParallel(2)


getCorpus<-function(path){
	options(mc.cores=1)	
	cps <- Corpus(VectorSource(readLines(path,encoding="UTF-8")))
	#cps <- tm_map(cps, content_transformer(tolower))
	cps <- tm_map(cps, tolower)
	cps <- tm_map(cps, removePunctuation)
	cps <- tm_map(cps, removeNumbers)
	cps <- tm_map(cps, stripWhitespace)		
	cps
}

getCorpusV<-function(vec){
	options(mc.cores=1)	
	cps <- Corpus(VectorSource(vec))
	#cps <- tm_map(cps, content_transformer(tolower))
	cps <- tm_map(cps, tolower)
	cps <- tm_map(cps, removePunctuation)
	cps <- tm_map(cps, removeNumbers)
	cps <- tm_map(cps, stripWhitespace)		
	cps
}

getTokens<-function(corpus,num){
	tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = num, max = num))
	tokenizer(unlist(corpus))
}

getFrequency<-function(tokens){
	tbl<-table(data.frame(tokens))
	fdf<-data.frame(token=rownames(tbl),freq=as.vector(tbl))
	fdf[order(fdf$freq,decreasing=T),]
}

findWords<-function(freq,target,limit=10){
	#head(freq[grepl(paste('^',target,'.*$',sep=''),freq$token,ignore.case=T),],limit)
	head(freq[grepl(target,freq$token,ignore.case=T),],limit)
}


createTrainingSet<-function(src_path,target_path,ratio){
	ptm <- proc.time()
	filter_words<-gsub(",","",read.csv("Terms-to-Block.csv")[-(1:3),2])
	lines<-readLines(src_path, skipNul=TRUE, encoding = "UTF-8")
	total<-length(lines)
	filter_regex<-paste(filter_words,collapse=" | ")
	print('read done')
	print(proc.time() - ptm)
	lines<-sample(lines,total*ratio)
	lines<-gsub(filter_regex,"",lines)	
	print('sampling done')
	print(proc.time() - ptm)	
	uniFreq<-getFrequency(getTokens(getCorpusV(lines),1))
	saveRDS(uniFreq,file=paste(target_path,'1.Rda',sep='_'))
	rm(uniFreq)
	print('uni done')
	print(proc.time() - ptm)		
	biFreq<-getFrequency(getTokens(getCorpusV(lines),2))
	saveRDS(biFreq,file=paste(target_path,'2.Rda',sep='_'))
	rm(biFreq)
	print('bi done')
	print(proc.time() - ptm)		
	triFreq<-getFrequency(getTokens(getCorpusV(lines),3))
	saveRDS(triFreq,file=paste(target_path,'3.Rda',sep='_'))
	rm(triFreq)
	print('tri done')
	print(proc.time() - ptm)			
	rm(lines)
}

###################################################################


#src_path<-'final/en_US/en_US.blogs.txt'
#target_path<-'data/blog_freq'
#ratio<-0.05
#src_path<-'data/simple.csv'
#target_path<-'data/simple_freq'
#ratio<-1
createTrainingSet('data/simple.csv','data/simple_freq',1)
createTrainingSet('final/en_US/en_US.blogs.txt','data/blog_freq',0.05)
createTrainingSet('final/en_US/en_US.twitter.txt','data/twitter_freq',0.05)
createTrainingSet('final/en_US/en_US.news.txt','data/news_freq',0.05)


#path<-'data/blog_sm.csv'
path<-'data/simple.csv'
cps<-getCorpus(path)

uniTokens<-getTokens(cps,1) 
biTokens<-getTokens(cps,2)
triTokens<-getTokens(cps,3)

#uniFreq<-getFrequency(uniTokens)
#biFreq<-getFrequency(biTokens)
#triFreq<-getFrequency(triTokens)

uniFreq<-readRDS('data/simple_freq_1.Rda')
biFreq<-readRDS('data/simple_freq_2.Rda')
triFreq<-readRDS('data/simple_freq_3.Rda')

uniFreq<-readRDS('data/blog_freq_1.Rda')
biFreq<-readRDS('data/blog_freq_2.Rda')
triFreq<-readRDS('data/blog_freq_3.Rda')

findWords(uniFreq,'^i$')
findWords(biFreq,'^i have$')
findWords(triFreq,'^i have a$')

#step 1: give P('i <Unk>'|'i')
wd<-'^i$'
wd2<-'^i .*$'
tmp<-findWords(uniFreq,wd)
tmp2<-findWords(biFreq,wd2)
tmp2$prob<-tmp2$freq/tmp$freq[1]
tmp2

#step 2: give possible combo P('i <Unk1> <Unk2>'|'i <Unk1>')
wd3<-as.vector(tmp2$token)
wd3<-paste(as.vector(tmp2$token),collapse='.*$|^')
wd3<-paste('^',wd3,'.*$',sep='')
tmp3<-findWords(triFreq,wd3)
tmp3
tmp3$prob<-tmp3$freq/tmp2$freq
tmp3
#tmp3$prob<-tmp3$freq/tmp2
#stopImplicitCluster()

