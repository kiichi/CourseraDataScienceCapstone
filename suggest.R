library(tm)
library(RWeka)
library(plyr)
library(doParallel)
registerDoParallel(2)

# For Input Text
simpleTokenizer<-function(line){  
	strsplit(tolower(line),"[ \t\\.\\?!\\$,\"]+") [[1]]	
}

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

####################################################################################################
# test for Kneser-Ney Smoothing
# searchText<-'san francisco'

#estimateKN('san francisco',uniFreq,biFreq,triFreq)      # expected most results
#estimateKN('eduardo francisco',uniFreq,biFreq,triFreq)  # there is only 1 case
#estimateKN('new francisco',uniFreq,biFreq,triFreq)      # totally nothing. you mean, new york?


#estimateKN('new york',uniFreq,biFreq,triFreq)           # expected most results
#estimateKN('andrew york',uniFreq,biFreq,triFreq)        # looks like a guiterlist called andrew... never heard of 
#estimateKN('old york',uniFreq,biFreq,triFreq)           # just random guess of another city name.
####################################################################################################
estimateKN<-function(searchText,uniFreq,biFreq,triFreq){	
	inputWord <- simpleTokenizer(searchText)
	len<-length(inputWord)
	
	d<-0.75
	# how many bigram 'san francisco'?
	b1<-biFreq[biFreq$token==searchText,]$freq
	# how many preceeding word, 'san' by itself ?
	u1<-uniFreq[uniFreq$token==inputWord[len-1],]$freq
	#u2<-uniFreq[uniFreq$token=='francisco',]$freq
	
	#Novel Continousity
	#how many # of word "type" precieding like	'.* francisco'
	prec<-biFreq[grepl(paste(paste('.*',inputWord[len]),'$',sep=''),biFreq$token),]
	types<-nrow(prec)
	#sum of # of word "type" precieding	to normalize
	sumtypes<-sum(prec$freq)
	
	pkn<-max(b1-d,0)/u1 + d/u1 * types/sumtypes
	pkn
}



#searchText <- 'been three months'
#searchText<-'Please let us'
#limit<-3000
suggestWord<-function(searchText,uniFreq,biFreq,triFreq,limit=1000){
	inputWord <- simpleTokenizer(searchText)
	
	len<-length(inputWord)
	
	#unigram
	inwd<-inputWord[length(inputWord)]
	unilen<-nrow(uniFreq)
	unitmp<-uniFreq
	unitmp$prob<-uniFreq$freq/unilen
	unitmp$target<-uniFreq$token
	#should put this in findWord()
	unitmp<-unitmp[!(unitmp$target %in% inputWord),]
	unitmp<-head(unitmp[order(unitmp$freq,decreasing=T),],limit)
	print(head(unitmp,10))
	
	#bigram
	inwd<-inputWord[len:length(inputWord)]
	wd<-paste(c('^',inwd,'$'),collapse='')
	wd2<-paste(c('^',inwd,' .*$'),collapse='')
	tmp<-findWords(uniFreq,wd,limit)
	tmp2<-findWords(biFreq,wd2,limit)
	tmp2$prob<-tmp2$freq/tmp$freq[1] #only one word
	tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)		
	bitmp<-tmp2
	bitmp<-bitmp[!(bitmp$target %in% inputWord),]
	print(head(bitmp,10))
		
	
	#trigram
	inwd<-inputWord[(len-1):length(inputWord)]
	inwd<-paste(inwd,collapse=' ')
	wd<-paste('^',inwd,'$',sep='')
	wd2<-paste('^',inwd,' .*$',sep='')
	tmp<-findWords(biFreq,wd,limit)
	tmp2<-findWords(triFreq,wd2,limit)
	tmp2$prob<-tmp2$freq/tmp$freq
	tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)
	tritmp<-tmp2
	tritmp<-tritmp[!(tritmp$target %in% inputWord),]
	print(head(tritmp,10))
	
	#quadgram
	#inwd<-inputWord[(len-2):length(inputWord)]
	#inwd<-paste(inwd,collapse=' ')
	#wd<-paste('^',inwd,'$',sep='')
	#wd2<-paste('^',inwd,' .*$',sep='')
	#tmp<-findWords(triFreq,wd,limit)
	#tmp2<-findWords(quadFreq,wd2,limit)
	#tmp2$prob<-tmp2$freq/tmp$freq
	#tmp2$target<-gsub(paste('^',inwd,' ',sep=''),'',tmp2$token)
	#quadtmp<-tmp2
	#print(tmp2)
	
	#try l1, l2, l3
	ttl<-nrow(unitmp) + nrow(bitmp) + nrow(tritmp)
	l1<-ttl/nrow(unitmp)
	l2<-ttl/nrow(bitmp)
	l3<-ttl/nrow(tritmp)
	ttl<-l1+l2+l3
	l1<-l1/ttl
	l2<-l2/ttl
	l3<-l3/ttl
	
	
	
	tbl<-merge(x=unitmp,y=bitmp,by="target",all.x=T)
	tbl<-merge(x=tbl,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=tbl,y=quadtmp,by="target",all.x=T)
	
	#head(biFreq[biFreq$token=='new york',])
	
	#show the table
	#head(tbl[order(tbl$freq,decreasing=T),])
	
	tbl<-mdply(tbl[,c('target','prob.x','prob.y','prob')],function(target,token.x,prob.x,prob.y,prob){ 
		target					
		uniProb<-0
		if (!is.na(prob.x)){			
			uniProb <- prob.x
		}
		biProb<-0
		if (!is.na(prob.y)){			
			biProb <- prob.y
		}
		triProb<-0
		if (!is.na(prob)){			
			triProb <- prob
		}	
		uniProb*l1 + biProb*l2 + triProb*l3
		#exp(log(uniProb * l1) + log(biProb * l2) + log(triProb * l3))		
	})
	names(tbl) <- c('target','uni','bi','tri','score')	
	head(tbl[order(tbl$score,decreasing=T),],20)
	
	
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl$score<-rowSums(tbl[,c(4,7)])
	#tbl<-tbl[order(tbl$score,decreasing=T),]
	#print(head(tbl))
	
	
	
	###################
	# simple KN test
# 	d<-0.75
# 	b1<-biFreq[biFreq$token=='san francisco',]$freq
# 	u1<-uniFreq[uniFreq$token=='san',]$freq
# 	#u2<-uniFreq[uniFreq$token=='francisco',]$freq
# 	
# 	#Novel Continousity
# 	#how many # of word "type" precieding	
# 	prec<-biFreq[grepl('.* francisco$',biFreq$token),]
# 	types<-nrow(prec)
# 	#sum of # of word "type" precieding	to normalize
# 	sumtypes<-sum(prec$freq)
# 	
# 	pkn<-max(b1-d,0)/u1 + d/u1 * types/sumtypes
	
	

}



###################################################################


#src_path<-'final/en_US/en_US.blogs.txt'
#target_path<-'data/blog_freq'
#ratio<-0.05
#src_path<-'data/simple.csv'
#target_path<-'data/simple_freq'
#ratio<-1
createTrainingSet('data/simple.csv','data/simple_freq',1)
 


path<-'data/blog_sm.csv'
#path<-'data/simple.csv'
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

write.table(uniFreq,'data/blog_freq_1.csv',row.names=F,quote=F, sep = ",")
write.table(biFreq,'data/blog_freq_2.csv',row.names=F,quote=F, sep = ",")
write.table(triFreq,'data/blog_freq_3.csv',row.names=F,quote=F, sep = ",")

uniFreq<-readRDS('data/news_freq_1.Rda')
biFreq<-readRDS('data/news_freq_2.Rda')
triFreq<-readRDS('data/news_freq_3.Rda')

write.table(uniFreq,'data/news_freq_1.csv',row.names=F,quote=F, sep = ",")
write.table(biFreq,'data/news_freq_2.csv',row.names=F,quote=F, sep = ",")
write.table(triFreq,'data/news_freq_3.csv',row.names=F,quote=F, sep = ",")

uniFreq<-readRDS('data/twitter_freq_1.Rda')
biFreq<-readRDS('data/twitter_freq_2.Rda')
triFreq<-readRDS('data/twitter_freq_3.Rda')

write.table(uniFreq,'data/twitter_freq_1.csv',row.names=F,quote=F, sep = ",")
write.table(biFreq,'data/twitter_freq_2.csv',row.names=F,quote=F, sep = ",")
write.table(triFreq,'data/twitter_freq_3.csv',row.names=F,quote=F, sep = ",")


li<-list.files('data/','.csv$')
for (i in 1:length(li)){
	
}



uniFreq<-read.csv(unz('data/twitter_freq_1.csv.zip','twitter_freq_1.csv'))
biFreq<-read.csv(unz('data/twitter_freq_2.csv.zip','twitter_freq_2.csv'))
triFreq<-read.csv(unz('data/twitter_freq_3.csv.zip','twitter_freq_3.csv'))



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

suggestWord('I have a',uniFreq,biFreq,triFreq)
#http://www.redditblog.com/
suggestWord('been three months',uniFreq,biFreq,triFreq,1000) #since - soso 10 th position
suggestWord('please let us',uniFreq,biFreq,triFreq,1000) # know - good
suggestWord('nice cup of',uniFreq,biFreq,triFreq,1000) #coffee - good
suggestWord('you may not',uniFreq,biFreq,triFreq,1000) #have - close! 2nd pos



