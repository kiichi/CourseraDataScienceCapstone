library(tm)
library(wordcloud)

generateWordCloud<-function(path){
  #cps <- Corpus(VectorSource(as.vector(read.csv(path,sep='\t'))))  
  cps <- Corpus(DataframeSource(read.csv(path,sep='\t'))) 
  cps <- tm_map(cps, removePunctuation)
  cps <- tm_map(cps, function(x) removeWords(x, stopwords("english")))
  #png(paste(path,".png",sep=""), width=400,height=300)
  options(mc.cores=1)
  #BuGn or Dark2
  wordcloud(cps, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
  #dev.off()
}

generateWordCloud('data/blog_sm.csv')
generateWordCloud('data/news_sm.csv')
generateWordCloud('data/twitter_sm.csv')