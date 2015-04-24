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
	#stillTyping<-!grepl(' $',searchText)
	
	#if still typing, filter by last word
	
	#e.g. "have a nice d" -> take filter everything start with "d"	
	
	#lambda for interpolation - do deep training for this too
	
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
	
	
	
	tbl<-merge(x=unitmp,y=bitmp,by="target",all.x=T)
	tbl<-merge(x=tbl,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl<-merge(x=tbl,y=quadtmp,by="target",all.x=T)
	
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
		uniProb * l1 + biProb * l2 + triProb * l3		
	})
	names(tbl) <- c('target','uni','bi','tri','score')	
	head(tbl[order(tbl$score,decreasing=T),],20)
	#print(head(tbl[order(tbl$V1,decreasing=T),]))
	
	#tbl<-merge(x=bitmp,y=tritmp,by="target",all.x=T)
	#tbl$score<-rowSums(tbl[,c(4,7)])
	#tbl<-tbl[order(tbl$score,decreasing=T),]
	#print(head(tbl))
}



function(input, output) {
	
#	#dataset <- reactive({
#		diamonds[sample(nrow(diamonds), input$sampleSize),]
#	})
	
	#output$result <- renderPrint({ paste(input$search,input$ds)})
	
	#output$result <- renderPrint({ input$search })
	
	output$message<-renderText('Loading ... please wait')
	print('loading')
	uniFreq<-read.csv(unz('data/twitter_freq_1.csv.zip','twitter_freq_1.csv'))
	biFreq<-read.csv(unz('data/twitter_freq_2.csv.zip','twitter_freq_2.csv'))
	triFreq<-read.csv(unz('data/twitter_freq_3.csv.zip','twitter_freq_3.csv'))
	output$message<-renderText('Ready to search now.')	
	print('done!')
	#output$result<-renderTable({suggestWord(input$search,uniFreq,biFreq,triFreq)})
	
	
	doSearch <- eventReactive(input$predictButton, {
		suggestWord(input$search,uniFreq,biFreq,triFreq)
	})
	
	output$result <- renderTable({
		doSearch()
	})
	
	
}