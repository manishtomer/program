

setwd("C:/Users/Manish/Documents/Analytics/Coursera/Assignment/Capstone/dataset/final/en_US")
library(dplyr)
library(tm)
library(NLP)
library(RWeka)
library(stringi)
library(ggplot2)
library(RColorBrewer)



## read the files
# con<- file("en_US.twitter.txt", "r")
# tfile<-readLines(con)
# close(con)
# stri_stats_general(tfile)
# 
# con<- file("en_US.blogs.txt", "r")
# bfile<-readLines(con)
# close(con)
# stri_stats_general(bfile)
# 
# con<- file("en_US.news.txt", "r")
# nfile<-readLines(con)
# close(con)
# stri_stats_general(nfile)

## read the files
con<- file("en_US.twitter.txt", "r")
twf<-readLines(con,500000)
save(twf, file = "./sub/twf.txt")
close(con)

con<- file("en_US.blogs.txt", "r")
bwf<-readLines(con,300000)
save(bwf, file = "./sub/bwf.txt")
close(con)


con<- file("en_US.news.txt", "r")
nwf<-readLines(con,100000)
save(nwf, file = "./sub/nwf.txt")
close(con)


## create a Corpus of 3 files - Twitter, Blogs and News

filecr <- Corpus(DirSource("C:/Users/Manish/Documents/Analytics/Coursera/Assignment/Capstone/dataset/final/en_US/sub"), 
               readerControl = list(language="en_US"))

names(filecr)
summary(filecr)

## clean up the files - convert to lowecase

filecr<-tm_map(filecr, content_transformer(tolower))

## remove punctuations, numbers, whitespaces and stopwords
## For stopwords - English language stopwords have been used
## for Profanity, manual list was created and the words were removed
filecr<-tm_map(filecr, FUN = removePunctuation)

filecr<-tm_map(filecr, FUN = removeNumbers)

filecr<-tm_map(filecr, FUN = stripWhitespace)

# mystopwords<-stopwords("english")
# filecr<-tm_map(filecr, removeWords, mystopwords)

prowords<-c("fuck", "ass", "asshole", "shit", "crap", "bitch", "cock", "cunt")
filecr<-tm_map(filecr, removeWords, prowords)

filecr <- tm_map(filecr, PlainTextDocument)

## termdoc matix was created for count based evaluation

filedtm<-DocumentTermMatrix(filecr)
filedtm

frqt<-findFreqTerms(filedtm, lowfreq = 100, highfreq=Inf)
frqt

#assoc<-findAssocs(filedtm, c("one"), c(.95))
#assoc

freq <- sort(colSums(as.matrix(filedtm)), decreasing=TRUE)
wordcount <- data.frame(word=names(freq), freq=freq)
# Plot Histogram
# subset(wordcount, freq>75)    %>%
#   ggplot(aes(word, freq)) +
#   geom_bar(stat="identity", fill="lightblue", colour="black") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))


# library(wordcloud)
# set.seed(111)
# wordcloud(names(freq), freq, min.freq=50, colors=brewer.pal(8, "Dark2"))


BigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 2, max = 2))
dtmtoken <- DocumentTermMatrix(filecr, control = list(tokenize = BigramTokenizer))
freq <- sort(colSums(as.matrix(dtmtoken)), decreasing=TRUE)
wof <- data.frame(word=names(freq), freq=freq)

## break prefixes in words
bivar<-as.character(wof$word)
freq2<-wof$freq
bivar2<-as.data.frame(do.call(rbind,strsplit(bivar, " ")))
bivar2final<-cbind(bivar2,freq2)

bivar<-as.character(wof$word)
bivar<-strsplit(bivar, " ")
bivar<-unlist(bivar)
# bivart<-c(bivar[[1]][1],bivar[[1]][2]))
# bivar <- lapply(bivar, function(x) c(paste(x[[1]][1], x[[1]][2])))
bivar1<-tolower(bivar[seq(1, length(bivar), 2)])
bivar2<-bivar[seq(2, length(bivar), 2)]
bivar <- unlist(bivar)

pl <- ggplot(subset(wof, freq >10) ,aes(word, freq))
pl <- pl + geom_bar(stat="identity", fill="lightblue", colour="black")
pl + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle("2 - Gram Frequency") 


TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 3, max = 3))
dtmtoken3 <- DocumentTermMatrix(filecr, control = list(tokenize = TrigramTokenizer))
freq3 <- sort(colSums(as.matrix(dtmtoken3)), decreasing=TRUE)
wof3 <- data.frame(word3=names(freq3), freq3=freq3)

pl3 <- ggplot(subset(wof3, freq3 >10) ,aes(word3, freq3))
pl3 <- pl3 + geom_bar(stat="identity", fill="lightblue", colour="black")
pl3 + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle("3 - Gram Frequency") 

trivar<-as.character(wof3$word3)
freq3<-wof3$freq3
trivar2<-as.data.frame(do.call(rbind,strsplit(trivar, " ")))
trivar2m<-as.data.frame(paste(trivar2$V1,trivar2$V2,sep=" "))
trivar2final<-cbind(trivar2m, trivar2$V3, freq3)





FourgramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 4, max = 4))
dtmtoken4 <- DocumentTermMatrix(filecr, control = list(tokenize = FourgramTokenizer))
freq4 <- sort(colSums(as.matrix(dtmtoken4)), decreasing=TRUE)
wof4 <- data.frame(word4=names(freq4), freq4=freq4)

pl3 <- ggplot(subset(wof3, freq3 >10) ,aes(word3, freq3))
pl3 <- pl3 + geom_bar(stat="identity", fill="lightblue", colour="black")
pl3 + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle("3 - Gram Frequency") 


fourvar<-as.character(wof4$word4)
freq4<-wof4$freq4
fourvar2<-as.data.frame(do.call(rbind,strsplit(fourvar, " ")))
fourvar2m<-as.data.frame(paste(fourvar2$V1,fourvar2$V2,fourvar2$V3,sep=" "))
fourvar2final<-cbind(fourvar2m, fourvar2$V4, freq4)
names(fourvar2final)<-c("prefix", "word", "frequency")

## Cleaning of input data

inputd<-c("cable tool nicd")
inputdspl<-as.data.frame(strsplit(inputd, " "))

inputspl<-as.data.frame(do.call(rbind,strsplit(inputd, " ")))
inputunigram<-as.data.frame(inputspl[,length(inputspl)])
inputbigram<-as.data.frame(paste(inputspl[,length(inputspl)-1], inputspl[,length(inputspl)],sep=" "))
inputtrigram<-as.data.frame(paste(inputspl[,length(inputspl)-2], inputspl[,length(inputspl)-1], inputspl[,length(inputspl)],sep=" "))
as.character(inputtrigram)

## compare with word grams and predict the next word

## start with 4-grams 

patternm<-as.character(inputtrigram[1,])
match4g<-grep(patternm,fourvar2final$prefix)
nextword<-as.character(fourvar2final[match4g,2])
nextword


match4g<-fourvar2final[which(fourvar2final==inputtrigram),]
