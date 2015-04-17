library(tm)
library(RWeka)
library(plyr)
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

findWords<-function(freq,target,limit=1000){
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
	quadFreq<-getFrequency(getTokens(getCorpusV(lines),4))
	saveRDS(quadFreq,file=paste(target_path,'4.Rda',sep='_'))
	rm(quadFreq)
	print('quad done')	
	print(proc.time() - ptm)			
	rm(lines)
}

suggestWord<-function(inputWord,uniFreq,biFreq,triFreq){
	#lambda for interpolation - do deep training for this too
	
	len<-length(inputWord)
	
	#unigram
	inwd<-inputWord[length(inputWord)]
	unilen<-nrow(uniFreq)
	unitmp<-uniFreq
	unitmp$prob<-uniFreq$freq/unilen
	unitmp$target<-uniFreq$token
	#should put this in findWord()
	unitmp<-head(unitmp[order(unitmp$freq,decreasing=T),],1000)
	
	#bigram
	inwd<-inputWord[len:length(inputWord)]
	wd<-paste(c('^',inwd,'$'),collapse='')
	wd2<-paste(c('^',inwd,' .*$'),collapse='')
	tmp<-findWords(uniFreq,wd)
	tmp2<-findWords(biFreq,wd2)
	tmp2$prob<-tmp2$freq/tmp$freq[1]
	tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)	
	bitmp<-tmp2
	#print(tmp2)
		
	
	#trigram
	inwd<-inputWord[(len-1):length(inputWord)]
	inwd<-paste(inwd,collapse=' ')
	wd<-paste('^',inwd,'$',sep='')
	wd2<-paste('^',inwd,' .*$',sep='')
	tmp<-findWords(biFreq,wd)
	tmp2<-findWords(triFreq,wd2)
	tmp2$prob<-tmp2$freq/tmp$freq
	tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)
	tritmp<-tmp2
	#print(tmp2)
	
	#quadgram
	#inwd<-inputWord[(len-2):length(inputWord)]
	#inwd<-paste(inwd,collapse=' ')
	#wd<-paste('^',inwd,'$',sep='')
	#wd2<-paste('^',inwd,' .*$',sep='')
	#tmp<-findWords(triFreq,wd)
	#tmp2<-findWords(quadFreq,wd2)
	#tmp2$prob<-tmp2$freq/tmp$freq
	#tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)
	#quadtmp<-tmp2
	#print(tmp2)
	
	tbl<-merge(x=unitmp,y=bitmp,by="target",all.x=T)
	tbl<-merge(x=tbl,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=tbl,y=quadtmp,by="target",all.x=T)
	tbl<-mdply(tbl[,c('target','prob.x','prob.y','prob')],function(target,token.x,prob.x,prob.y,prob){ 
		target					
		#prob.x
		#abs(log(prob.y)) * 1/3 + abs(log(prob)) * 1/3
		abs(log(prob.x)) * 1/3 + abs(log(prob.y)) * 1/3 + abs(log(prob)) * 1/3
	})
	
	print(head(tbl[order(tbl$V1,decreasing=T),]))
	
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl$score<-rowSums(tbl[,c(4,7)])
	#tbl<-tbl[order(tbl$score,decreasing=T),]
	#print(head(tbl))
}



###################################################################


#src_path<-'final/en_US/en_US.blogs.txt'
#target_path<-'data/blog_freq'
#ratio<-0.05
#src_path<-'data/simple.csv'
#target_path<-'data/simple_freq'
#ratio<-1
createTrainingSet('data/simple.csv','data/simple_freq',1)
 


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
quadFreq<-readRDS('data/blog_freq_4.Rda')

findWords(uniFreq,'^i$')
findWords(biFreq,'^i have$')
findWords(triFreq,'^i have a$')


####### Problem ################
# Let's predict what is expected in <Unk> "i have a <Unk>"

#e.g. 0: unigram test P('<Unk>') - not useful
#inwd<-c('.*')
#wd<-paste(c('^',inwd,'$'),collapse='')
#tmp2<-findWords(uniFreq,wd)
#tmp2$prob<-tmp2$freq/sum(uniFreq$freq)
#tmp2

#e.g 1: bigram test. give P('today <Unk>'|'today')
inwd<-c('a')
wd<-paste(c('^',inwd,'$'),collapse='')
wd2<-paste(c('^',inwd,' .*$'),collapse='')
tmp<-findWords(uniFreq,wd)
tmp2<-findWords(biFreq,wd2)
tmp2$prob<-tmp2$freq/tmp$freq[1]
tmp2


#e.g 2: trigram test. try P('today is <Unk>'|'today is')
inwd<-c('have','a')
inwd<-paste(inwd,collapse=' ')
wd<-paste('^',inwd,'$',sep='')
wd2<-paste('^',inwd,' .*$',sep='')
tmp<-findWords(biFreq,wd)
tmp2<-findWords(triFreq,wd2)
tmp2$prob<-tmp2$freq/tmp$freq
tmp2

#e.g 3: quadgram test. try P('today is the <Unk>'|'today is the')
inwd<-c('i','have','a')
inwd<-paste(inwd,collapse=' ')
wd<-paste('^',inwd,'$',sep='')
wd2<-paste('^',inwd,' .*$',sep='')
tmp<-findWords(triFreq,wd)
tmp2<-findWords(quadFreq,wd2)
tmp2$prob<-tmp2$freq/tmp$freq
tmp2




#e.g. ? some other long distance estimate?: give possible combo P('i <Unk1> <Unk2>'|'i <Unk1>')
#wd3<-as.vector(tmp2$token)
#wd3<-paste(as.vector(tmp2$token),collapse='.*$|^')
#wd3<-paste('^',wd3,'.*$',sep='')
#tmp3<-findWords(triFreq,wd3)
#tmp3
#tmp3$prob<-tmp3$freq/tmp2$freq
#tmp3

#tmp3$prob<-tmp3$freq/tmp2


#stopImplicitCluster()

#load grams first
inputWord<-c('i','have','a')
#inputWord<-c('to','the')
suggestWord(inputWord,uniFreq,biFreq,triFreq)



