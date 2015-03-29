# sentensify<-function(line){
#   strsplit(line,"[\\.\\?!]")
# }

profanity<-c(t(read.csv("terms_to_block_clean.csv")))
profanity<-c(profanity,"")
tokenize<-function(line){  
  #strsplit(line,"[ +\\.\\?!]")
  tokens<-strsplit(tolower(line),"[ \\t\\.\\?!\\$,\"]+") [[1]]
  tokens[!tokens %in% profanity]
}

tokenizeLines<-function(lines){  
  unlist(sapply(lines,tokenize,USE.NAMES=F))
}



############## MAIN ################
#setwd("~/work/r/class/CourseraDataScienceCapstone")

total_lines<-100
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f,total_lines)
#70% training sets
#training_lines<-lines[rbinom(total_lines,1,0.7) == 1]
training<-tokenizeLines(lines)
close(f)

############## Write Training Set ################
#write(training,file="training.csv",sep="\t")

############## Visualizing Frequency ################

# Count Appearance of Tokens
# Cut top 50
training_tbl<-table(data.frame(fac = factor(training)))
training_tbl<-head(sort(training_tbl,decreasing=T),50)
#names
#training_count[training_tbl > 10]
#hist(training_tbl, xlab = "Frequency of Level Occurrence", main = "")

require(ggplot2)
#p <- ggplot(data.frame(Freq = training_tbl, fac = names(training_tbl)), aes(fac, Freq)) + geom_point()
#p

#fill=training_tbl
p<-ggplot(data.frame(freq = training_tbl, token = names(training_tbl)),aes(token,freq,fill=training_tbl)) 
p<- p + geom_bar(stat="identity")
p<- p + coord_flip()
#stat="bin" is default to countig. however, we alread counted frequecy,
#so use identity
p


