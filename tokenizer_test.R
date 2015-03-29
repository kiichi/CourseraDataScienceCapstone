#http://cran.r-project.org/web/packages/NLP/NLP.pdf
library(NLP)
str<-String("I have a pen. You are calling me from my office. Are you serious? Yes, I am! no kidding.")
#reg<-Regexp_Tokenizer("[ \\.\\?!]")
str[wordpunct_tokenizer(str)]

#str[whitespace_tokenizer(str)]
