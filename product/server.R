library(shiny)
library(tm)
library(RWeka)
library(plyr)
library(doParallel)
registerDoParallel(2)

simpleTokenizer<-function(line){  
	strsplit(tolower(line),"[ \\t\\.\\?!\\$,\"]+") [[1]]	
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
	head(tbl[order(tbl$V1,decreasing=T),])
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
	
	uniFreq<-readRDS('data/blog_freq_1.Rda')
	biFreq<-readRDS('data/blog_freq_2.Rda')
	triFreq<-readRDS('data/blog_freq_3.Rda')
	output$result<-renderTable({suggestWord(simpleTokenizer(input$search),uniFreq,biFreq,triFreq)})
}