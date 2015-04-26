library(tm)
library(RWeka)
library(plyr)
library(doParallel)
library(stringr)
library(stringi)
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
	colnames(uniFreq)<-c('w1','freq')
	saveRDS(uniFreq,file=paste(target_path,'1.Rda',sep='_'))
	rm(uniFreq)
	print('uni done')
	print(proc.time() - ptm)		
	biFreq<-getFrequency(getTokens(getCorpusV(lines),2))
	#split token into colums
	biFreq<-cbind(as.data.frame(str_match(biFreq$token, "^(.*) (.*)$")[,-1]),biFreq[,c('freq')])
	colnames(biFreq)<-c('w1','w2','freq')
	saveRDS(biFreq,file=paste(target_path,'2.Rda',sep='_'))
	rm(biFreq)
	print('bi done')
	print(proc.time() - ptm)		
	triFreq<-getFrequency(getTokens(getCorpusV(lines),3))
	#split token into colums
	triFreq<-cbind(as.data.frame(str_match(triFreq$token, "^(.*) (.*) (.*)$")[,-1]),triFreq[,c('freq')])
	colnames(triFreq)<-c('w1','w2','w3','freq')
	saveRDS(triFreq,file=paste(target_path,'3.Rda',sep='_'))
	rm(triFreq)
	print('tri done')
	quadFreq<-getFrequency(getTokens(getCorpusV(lines),4))
	#split token into colums
	quadFreq<-cbind(as.data.frame(str_match(quadFreq$token, "^(.*) (.*) (.*) (.*)$")[,-1]),quadFreq[,c('freq')])
	colnames(quadFreq)<-c('w1','w2','w3','w4','freq')
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


kntest<-function(searchText,uniFreq,biFreq,triFreq){	
	inputWord <- simpleTokenizer(searchText)
	len<-length(inputWord)
	
	inwd<-inputWord[len:length(inputWord)]  
	
	d<-0.75
	# how many bigram 'san francisco'?
	bidf<-biFreq[biFreq$w1==inwd[1],]
	# how many preceeding word, 'san' by itself ?
	u1<-uniFreq[uniFreq$w1==inwd[1],]$freq
	#u2<-uniFreq[uniFreq$token=='francisco',]$freq
	
	#Novel Continousity
	#how many # of word "type" precieding like	'.* francisco'	
	types<-nrow(bidf)
	#sum of # of word "type" precieding	to normalize
	sumtypes<-sum(bidf$freq)
	
	b1<-bidf$freq
	
	#pkn<-max(b1-d,0)/u1 + d/u1 * types/sumtypes
	bidf$pkn<-(b1-d)/u1 + d/u1 * types/sumtypes
	head(bidf,10)
}


########################################################################################
#searchText <- 'been three months'
#searchText<-'Please let us'
#searchText<-'a nice cup of'
#limit<-1000
#uniFreq<-readRDS('data1/blog_freq_1.Rda')
#biFreq<-readRDS('data1/blog_freq_2.Rda')
#triFreq<-readRDS('data1/blog_freq_3.Rda')
#quadFreq<-readRDS('data1/blog_freq_4.Rda')
#suggestWord('please let us know',uniFreq,biFreq,triFreq,quadFreq,1000)
suggestWord<-function(searchText,uniFreq,biFreq,triFreq,quadFreq,limit=1000){
	inputWord <- simpleTokenizer(searchText)	
	len<-length(inputWord)
	
	#unigram
	inwd<-inputWord[length(inputWord)]
	unilen<-nrow(uniFreq)
	unitmp<-head(uniFreq,limit*5)#already sorted
	unitmp$prob<-unitmp$freq/unilen
	unitmp$target<-unitmp$w1
	#should put this in findWord()
	# no limit
	#unitmp<-unitmp[!(unitmp$target %in% inputWord),]
	# need to sort?
	#unitmp<-unitmp[order(unitmp$freq,decreasing=T),]
	colnames(unitmp)<-c('w1','freq1','prob1','target')
	#print(head(unitmp[order(unitmp$prob1,decreasing=T),],5))
	
	#bigram
	inwd<-inputWord[len:length(inputWord)]	
	tmp<-uniFreq[uniFreq$w1==inwd,]
	tmp2<-biFreq[biFreq$w1==inwd,]
	tmp2<-head(tmp2,limit*3) #cut
	tmp2$prob<-tmp2$freq/tmp$freq[1] #only one word
	tmp2$target<-tmp2$w2
	bitmp<-tmp2
	#bitmp<-bitmp[!(bitmp$target %in% inputWord),]	
	colnames(bitmp)<-c('bw1','bw2','freq2','prob2','target')
	#print(head(bitmp[order(bitmp$prob2,decreasing=T),],5))
	
	############################################################
	#Also calculate Knesian-Ney
	d<-0.75
	# how many bigram 'san francisco'?
	bidf<-tmp2
	# how many preceeding word, 'san' by itself ?
	u1<-tmp$freq	
	
	#Novel Continousity
	#how many # of word "type" precieding like	'.* francisco'	
	types<-nrow(tmp2)
	#sum of # of word "type" precieding	to normalize
	sumtypes<-sum(tmp2$freq)	
	b1<-tmp2$freq	
	#bitmp$probkn<-(b1-d)/u1 + d/u1 * types/sumtypes
	# Replae BiGram Probability Itself?
	bitmp$prob2<-(b1-d)/u1 + d/u1 * types/sumtypes
	############################################################
		
	
	#trigram
	inwd<-inputWord[max((len-1),0):length(inputWord)]	
	tmp<-biFreq[biFreq$w1==inwd[1] & biFreq$w2==inwd[2],]
	tmp2<-triFreq[triFreq$w1==inwd[1] & triFreq$w2==inwd[2],]
	tmp2<-head(tmp2,limit*2) #cut
	tmp2$prob<-tmp2$freq/tmp$freq[1]
	tmp2$target<-tmp2$w3
	tritmp<-tmp2	
	#tritmp<-tritmp[!(tritmp$target %in% inputWord),]	
	colnames(tritmp)<-c('tw1','tw2','tw3','freq3','prob3','target')
	#print(head(tritmp[order(tritmp$prob3,decreasing=T),],5))
	
	#quadgram
	inwd<-inputWord[max((len-2),0):length(inputWord)]	
	tmp<-triFreq[triFreq$w1==inwd[1] & triFreq$w2==inwd[2] & triFreq$w3==inwd[3],]
	tmp2<-quadFreq[quadFreq$w1==inwd[1] & quadFreq$w2==inwd[2] & quadFreq$w3==inwd[3],]	
	tmp2<-head(tmp2,limit) #cut
	tmp2$prob4<-tmp2$freq/tmp$freq[1]
	tmp2$target<-tmp2$w4
	quadtmp<-tmp2
	colnames(quadtmp)<-c('qw1','qw2','qw3','qw4','freq4','prob4','target')
	#print(head(quadtmp[order(quadtmp$prob4,decreasing=T),],5))
	
	rm(tmp)
	rm(tmp2)
	
	tbl<-merge(x=unitmp,y=bitmp,by="target",all.x=T)
	tbl<-merge(x=tbl,y=tritmp,by="target",all.x=T)	
	tbl<-merge(x=tbl,y=quadtmp,by="target",all.x=T)
	
	
	#head(biFreq[biFreq$token=='new york',])
	
	#show the table
	#head(tbl[order(tbl$freq4,decreasing=T),])
	#head(tbl[order(tbl$freq3,decreasing=T),])
	
	#################################################################
	ttl<-nrow(unitmp) + nrow(bitmp) + nrow(tritmp) + nrow(quadtmp)	
	l1<-ttl/max(nrow(unitmp),1)
	l2<-ttl/max(nrow(bitmp),1)
	l3<-ttl/max(nrow(tritmp),1)
	l4<-ttl/max(nrow(quadtmp),1)
	ttl<-l1+l2+l3+l4
	l1<-l1/ttl#not used
	l2<-l2/ttl
	l3<-l3/ttl
	l4<-l4/ttl
	
	#tbl<-head(tbl[order(tbl$prob3,decreasing=T),],8)
	
	tbl<-mdply(tbl[,c('target','prob1','prob2','prob3','prob4')],function(target,prob1,prob2,prob3,prob4){ 
		target					
		#uniProb<-0
		#if (is.numeric(prob1) && !is.na(prob1)){			
		#	uniProb <- prob1
		#}
		biProb<-0
		if (is.numeric(prob2) && !is.na(prob2)){			
			biProb <- prob2
		}
		triProb<-0
		if (is.numeric(prob3) && !is.na(prob3)){			
			triProb <- prob3
		}	
		quadProb<-0
		if (is.numeric(prob4) && !is.na(prob4)){			
			quadProb <- prob4
		}	
		
		#uniProb*l1 + 
		biProb*l2 + triProb*l3 + quadProb * l4
		#exp(log(uniProb * l1) + log(biProb * l2) + log(triProb * l3))		
	})
	names(tbl) <- c('WORD','P(UNI)','P(BI)','P(TRI)','P(QUAD)','SCORE')	
	final<-head(tbl[order(tbl$SCORE,decreasing=T),],5)
	rownames(final)<-NULL
	final	
}



###################################################################


#src_path<-'final/en_US/en_US.blogs.txt'
#target_path<-'data/blog_freq'
#ratio<-0.05
#src_path<-'data/simple.csv'
#target_path<-'data/simple_freq'
#ratio<-1
createTrainingSet('data/simple.csv','data/simple_freq',1)
createTrainingSet('final/en_US/en_US.blogs.txt','data1/blog_freq',0.01);
createTrainingSet('final/en_US/en_US.news.txt','data1/news_freq',0.01);
createTrainingSet('final/en_US/en_US.twitter.txt','data1/twitter_freq',0.01);
createTrainingSet('final/en_US/en_US.blogs.txt','data2/blog_freq',0.02);
createTrainingSet('final/en_US/en_US.news.txt','data2/news_freq',0.02);
createTrainingSet('final/en_US/en_US.twitter.txt','data2/twitter_freq',0.02);
createTrainingSet('final/en_US/en_US.blogs.txt','data3/blog_freq',0.03);
createTrainingSet('final/en_US/en_US.news.txt','data3/news_freq',0.03);
createTrainingSet('final/en_US/en_US.twitter.txt','data3/twitter_freq',0.03);

createTrainingSet('final/en_US/en_US.blogs.txt','data5/blog_freq',0.05);
createTrainingSet('final/en_US/en_US.news.txt','data5/news_freq',0.05);
createTrainingSet('final/en_US/en_US.twitter.txt','data5/twitter_freq',0.05);

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
quadFreq<-readRDS('data/simple_freq_4.Rda')


uniFreq<-readRDS('data/blog_freq_1.Rda')
biFreq<-readRDS('data/blog_freq_2.Rda')
triFreq<-readRDS('data/blog_freq_3.Rda')

uniFreq<-readRDS('data1/blog_freq_1.Rda')
biFreq<-readRDS('data1/blog_freq_2.Rda')
triFreq<-readRDS('data1/blog_freq_3.Rda')
quadFreq<-readRDS('data1/blog_freq_4.Rda')

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



uniFreq<-readRDS('data5/blog_freq_1.Rda')
biFreq<-readRDS('data5/blog_freq_2.Rda')
triFreq<-readRDS('data5/blog_freq_3.Rda')
quadFreq<-readRDS('data5/blog_freq_4.Rda')


# Add Splitted columns dynamically
#biFreq<-mdply(biFreq[,c('token','freq')],function(token,freq){
#	unlist(strsplit(as.character(token),' '))
#})

#triFreq<-mdply(triFreq[,c('token','freq')],function(token,freq){
#	unlist(strsplit(as.character(token),' '))
#})



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
suggestWord('please let us',uniFreq,biFreq,triFreq,1000) # know - good? or 2nd pos based on data
suggestWord('nice cup of',uniFreq,biFreq,triFreq,1000) #coffee - good
suggestWord('you may not',uniFreq,biFreq,triFreq,1000) #have - close! 2nd pos

# not in quad but in tri?
suggestWord('a nice cup of',uniFreq,biFreq,triFreq,quadFreq,1000)
# in quad gram
suggestWord('a the end of',uniFreq,biFreq,triFreq,quadFreq,1000)
suggestWord('please let us know',uniFreq,biFreq,triFreq,quadFreq,1000)

test<-suggestWord('please let us',uniFreq,biFreq,triFreq,1000)


test$score2<-c(
	estimateKN('us ago',uniFreq,biFreq,triFreq)+test$score[1],
	estimateKN('us and',uniFreq,biFreq,triFreq)+testhe$score[2],
	estimateKN('us well',uniFreq,biFreq,triFreq)+test$score[3],
	estimateKN('us after',uniFreq,biFreq,triFreq)+test$score[4],
	estimateKN('us old',uniFreq,biFreq,triFreq)+test$score[5]
)




