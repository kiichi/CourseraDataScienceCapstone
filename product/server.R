library(shiny)
library(tm)
library(RWeka)
library(plyr)
library(doParallel)
registerDoParallel(2)

simpleTokenizer<-function(line){  
	strsplit(tolower(line),"[ \t\\.\\?!\\$,\"]+") [[1]]	
}

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
	l1<-0
	l2<-l2/ttl
	l3<-l3/ttl
	l4<-l4/ttl
	
	#tbl<-head(tbl[order(tbl$prob3,decreasing=T),],8)
	
	tbl<-mdply(tbl[,c('target','prob1','prob2','prob3','prob4')],function(target,prob1,prob2,prob3,prob4){ 
		target					
		uniProb<-0
		if (is.numeric(prob1) && !is.na(prob1)){			
			uniProb <- prob1
		}
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
		
		uniProb*l1 + biProb*l2 + triProb*l3 + quadProb * l4
		#exp(log(uniProb * l1) + log(biProb * l2) + log(triProb * l3))		
	})
	names(tbl) <- c('target','uni','bi','tri','quad','score')	
	final<-head(tbl[order(tbl$score,decreasing=T),],5)
	rownames(final)<-NULL
	final	
}


loaded<-F
uniFreq<-NULL#readRDS('data/blog_freq_1.Rda')
biFreq<-NULL#readRDS('data/blog_freq_2.Rda')
triFreq<-NULL#readRDS('data/blog_freq_3.Rda')
quadFreq<-NULL#readRDS('data/blog_freq_4.Rda')

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
	
	
	
	loaded<-T
	print('done!')
	
	
	#output$result<-renderTable({suggestWord(input$search,uniFreq,biFreq,triFreq)})
	
	
	doSearch <- eventReactive(input$predictButton, {		
		if (input$search == ''){
			data.frame(results='Not Found. Please enter some words')
		}
		else {
			suggestWord(input$search,uniFreq,biFreq,triFreq,quadFreq)		
		}
	})
	
	output$result <- renderTable({		
		doSearch()
	})	
	
	doSelectDS <- eventReactive(input$datasource, {	
		ds<-tolower(input$datasource)
		uniFreq<<-readRDS(sprintf('data/%s_freq_1.Rda',ds))
		biFreq<<-readRDS(sprintf('data/%s_freq_2.Rda',ds))
		triFreq<<-readRDS(sprintf('data/%s_freq_3.Rda',ds))
		quadFreq<<-readRDS(sprintf('data/%s_freq_4.Rda',ds))		
		paste(input$datasource,'data has been loaded.')		
	})
	
	output$message <- renderText({
		doSelectDS()
	})
}