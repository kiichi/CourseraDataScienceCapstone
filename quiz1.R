#Q1 Calculate Size
f<-file('final/en_US/en_US.blogs.txt','r')
lines<-readLines(f)
print(object.size(lines))
close(f)
#260,564,320 bytes and this looks too big. It might contain other object's data when you 
#loaded it in vector. The actual file size is 210MB.

#Q2 How many lines?
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f)
print(length(lines))
close(f)
#2,360,148 lines


#Q3 The length of the longest line
f<-file('final/en_US/en_US.blogs.txt','r')
lines<-readLines(f)
close(f)
max(nchar(lines))
#which(nchar(lines) == max(nchar(lines)))
#40,833

f<-file('final/en_US/en_US.news.txt','r')
lines<-readLines(f)
close(f)
max(nchar(lines))
#11,384

#Q4 number of lines contains love / hate
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f)
close(f)
sum(grepl("love",lines))/sum(grepl("hate",lines))
#4.108592

#Q5 find line contains "biostats"
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f)
close(f)
lines[(grepl("biostats",lines))]
# "i know how you feel.. i have biostats on tuesday and i have yet to study =/"


#Q6 How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing"
f<-file('final/en_US/en_US.twitter.txt','r')
lines<-readLines(f)
close(f)
length(lines[(grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$",lines))])


