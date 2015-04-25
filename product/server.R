library(shiny)
library(tm)
library(RWeka)
library(plyr)
library(doParallel)
registerDoParallel(2)

simpleTokenizer<-function(line){  
	strsplit(tolower(line),"[ \t\\.\\?!\\$,\"]+") [[1]]	
}

findWords<-function(freq,target,limit=1000){
	head(freq[grepl(target,freq$token,ignore.case=T),],limit)
}

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
	# no limit
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
	final<-head(tbl[order(tbl$score,decreasing=T),],5)
	final
	
	
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


loaded<-F

function(input, output,session) {
	
#	#dataset <- reactive({
#		diamonds[sample(nrow(diamonds), input$sampleSize),]
#	})
	
	#output$result <- renderPrint({ paste(input$search,input$ds)})
	
	#output$result <- renderPrint({ input$search })
	
	output$message <- renderText({
		if (loaded){			
			'Ready to search now.'
		}
		else {
			invalidateLater(1000, session)
			paste("Loading ... Please wait .... ", Sys.time())
		}
	})
	
	#output$message<-renderText('Loading ... please wait')
	print('loading')
	
	
	uniFreq<-readRDS('data/blog_freq_1.Rda')
	biFreq<-readRDS('data/blog_freq_2.Rda')
	triFreq<-readRDS('data/blog_freq_3.Rda')
	loaded<-T
	print('done!')
	
	
	#output$result<-renderTable({suggestWord(input$search,uniFreq,biFreq,triFreq)})
	
	
	doSearch <- eventReactive(input$predictButton, {
		print('searching...')
		suggestWord(input$search,uniFreq,biFreq,triFreq)
	})
	
	output$result <- renderTable({
		doSearch()
	})
	
	
}