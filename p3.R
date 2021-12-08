install.packages("tm")
install.packages("wordcloud")
install.packages("zipfR")
install.packages("tokenizers")
install.packages("tidyverse")
install.packages("qdapTools")
install.packages("qdapRegex")
library(tm)
library(wordcloud)
library(zipfR)
library(tokenizers)
library(tidyverse)
library(qdapTools)
library(qdapRegex)

bohemia<-VCorpus(DirSource(".", ignore.case=FALSE, mode="text"))
str(bohemia)
btext1<-bohemia[[1]]
btext2<-bohemia[[2]]
btext3<-bohemia[[3]]

bohemiaDTM<-DocumentTermMatrix(bohemia)
inspect(bohemiaDTM)
str(bohemiaDTM)

bohemiaTDM<-tm::TermDocumentMatrix(bohemia)
bohemiaTDM

bohemiadf1<-data.frame(btext1[1])
bohemiadf2<-data.frame(btext2[1])
bohemiadf3<-data.frame(btext3[1])

# freqTerms<-tm::findFreqTerms(bohemiaTDM, lowfreq=5)
# 
# for (i in freqTerms) {
#       wordLen<-nchar(freqTerms[i])
#     }

## Chapter 1 -- 10 longest words
bohemiaCh1<-read_file("AScandalInBohemiaI.txt")
bohemiaCh1Nonl<-str_replace_all(bohemiaCh1, "\n", " ")
bohemiaCh1Cl<-str_remove_all(bohemiaCh1Nonl, "\r")
bohemiaCh1Cl<-gsub("\\\\", "", bohemiaCh1Cl)

w<-tokenize_words(bohemiaCh1Cl)
bohemiaCh1.words<-data.frame(c("word"=w,"length"=0), stringsAsFactors = FALSE)

for(i in 1:nrow(bohemiaCh1.words)) {
  this.row<-bohemiaCh1.words[i,]
  len<-nchar(this.row['word'])
  this.row['length']<-len
  bohemiaCh1.words[i,]<-this.row
}

bohemia1.ordered<-bohemiaCh1.words[order(-bohemiaCh1.words$length),]
bohemia1.ordered.distinct<-distinct(bohemia1.ordered)
head(bohemia1.ordered.distinct, 10)

##Chapter 1 -- longest sentences
tsent = tokenize_sentences(bohemiaCh1Cl)
sentences = data.frame(c("sent" = tsent,"length"=0),stringsAsFactors = FALSE)
head(sentences)

for(i in 1:nrow(sentences)){
  this.row = sentences[i,]
  len = str_count(this.row['sent']," ")
  this.row['length']=len+1
  sentences[i,]=this.row
}
head(sentences)
orderedSent = sentences[order(-sentences$length),]
head(orderedSent,10)

## Chapter 2 -- 10 longest words
bohemiaCh2<-read_file("AScandalInBohemiaII.txt")
bohemiaCh2Nonl<-str_replace_all(bohemiaCh2, "\n", " ")
bohemiaCh2Cl<-str_remove_all(bohemiaCh2Nonl, "\r")
bohemiaCh2Cl<-gsub("\\\\", "", bohemiaCh2Cl)

w<-tokenize_words(bohemiaCh2Cl)
bohemiaCh2.words<-data.frame(c("word"=w,"length"=0), stringsAsFactors = FALSE)

for(i in 1:nrow(bohemiaCh2.words)) {
  s<-bohemiaCh2.words[i,]
  l<-nchar(s['word'])
  s['length']<-l
  bohemiaCh2.words[i,]<-s
}

bohemia2.ordered<-bohemiaCh2.words[order(-bohemiaCh2.words$length),]
bohemia2.ordered.distinct<-distinct(bohemia2.ordered)
head(bohemia2.ordered.distinct, 10)

##Chapter 2 -- longest sentences
s = tokenize_sentences(bohemiaCh2Cl)
sentences = data.frame(c("sent" = s,"length"=0),stringsAsFactors = FALSE)
head(sentences)

for(i in 1:nrow(sentences)){
  s = sentences[i,]
  l = str_count(s['sent']," ")
  s['length']=l+1
  sentences[i,]=s
}
head(sentences)
orderedSent = sentences[order(-sentences$length),]
head(orderedSent,10)

## Chapter 3 -- 10 longest words
bohemiaCh3<-read_file("AScandalInBohemiaIII.txt")
bohemiaCh3Nonl<-str_replace_all(bohemiaCh3, "\n", " ")
bohemiaCh3Cl<-str_remove_all(bohemiaCh3Nonl, "\r")
bohemiaCh3Cl<-gsub("\\\\", "", bohemiaCh3Cl)

w<-tokenize_words(bohemiaCh3Cl)
bohemiaCh3.words<-data.frame(c("word"=w,"length"=0), stringsAsFactors = FALSE)

for(i in 1:nrow(bohemiaCh3.words)) {
  s<-bohemiaCh3.words[i,]
  l<-nchar(s['word'])
  s['length']<-l
  bohemiaCh3.words[i,]<-s
}

bohemia3.ordered<-bohemiaCh3.words[order(-bohemiaCh3.words$length),]
bohemia3.ordered.distinct<-distinct(bohemia3.ordered)
head(bohemia3.ordered.distinct, 10)

##Chapter 3 -- longest sentences
s = tokenize_sentences(bohemiaCh3Cl)
sentences = data.frame(c("sent" = s,"length"=0),stringsAsFactors = FALSE)
head(sentences)

for(i in 1:nrow(sentences)){
  s = sentences[i,]
  l = str_count(s['sent']," ")
  s['length']=l+1
  sentences[i,]=s
}
head(sentences)
orderedSent = sentences[order(-sentences$length),]
head(orderedSent,10)

## remove numbers and punctuation
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeHTML<-function(x) gsub("<.*?>", "", x)

# create a clean corpus
bohemiaCl0<-tm::tm_map(bohemia, content_transformer(removeHTML))
bohemiaCl1<-tm::tm_map(bohemiaCl0, content_transformer(removeNumPunct))
str(bohemiaCl1)
inspect(bohemiaCl1)

# convert to lowercase
bohemiaLow<-tm_map(bohemiaCl1, tm::content_transformer(tolower))
str(bohemiaLow)
inspect(bohemiaLow)

bohemiaDTM<-DocumentTermMatrix(bohemiaLow)
str(bohemiaDTM)
inspect(bohemiaDTM)
as.matrix(bohemiaDTM)

myStopWords<-c(tm::stopwords("english"))
bohemiaStop<-tm::tm_map(bohemiaLow,tm::removeWords,myStopWords)
tm::inspect(bohemiaStop[[1]])
tm::inspect(bohemiaStop[[2]])
tm::inspect(bohemiaStop[[3]])

bohemiaStopTDM<-tm::TermDocumentMatrix(bohemiaStop)

freqTerms<-tm::findFreqTerms(bohemiaStopTDM, lowfreq=5)
freqTerms

bohemiaTF1<-tm::termFreq(bohemiaStop[[1]])
bohemiaTF1
bohemiaTF2<-tm::termFreq(bohemiaStop[[2]])
bohemiaTF2
bohemiaTF3<-tm::termFreq(bohemiaStop[[3]])
bohemiaTF3

tm::inspect(bohemiaStopTDM)

bohemiaDF1<-as.data.frame(bohemiaStopTDM[[1]])
bohemiaDist1<-dist(bohemiaDF1)
bohemiaDG1<-hclust(bohemiaDist1,method="ward.D2")
str(bohemiaDG1)
plot(bohemiaDG1, labels=FALSE)

bohemiaDF2<-as.data.frame(bohemiaStopTDM[[2]])
bohemiaDist2<-dist(bohemiaDF2)
bohemiaDG2<-hclust(bohemiaDist2,method="ward.D2")
str(bohemiaDG2)
plot(bohemiaDG2, labels=FALSE)

bohemiaDF3<-as.data.frame(bohemiaStopTDM[[3]])
bohemiaDist3<-dist(bohemiaDF3)
bohemiaDG3<-hclust(bohemiaDist3,method="ward.D2")
str(bohemiaDG3)
plot(bohemiaDG3, labels=FALSE)
# 
# bohemiaStopNew<-tm::tm_map(bohemiaStop, tm::removeWords, c("even", "will", "may", "soon", "can", "as", "one", "much", "just", "now", "quite", "merely", "shall", "take", "certain", "well"))
# str(bohemiaStopNew)
# 
# bohemiaNewTDM<-tm::TermDocumentMatrix(bohemiaStopNew)
# 
# bohemiaNewDF1<-as.data.frame(bohemiaNewTDM[[1]])
# bohemiaNewDist1<-dist(bohemiaNewDF1)
# bohemiaNewDG1<-hclust(bohemiaNewDist1,method="ward.D2")
# str(bohemiaNewDG1)
# plot(bohemiaNewDG1)
# 
# bohemiaNewDF2<-as.data.frame(bohemiaNewTDM[[2]])
# bohemiaNewDist2<-dist(bohemiaNewDF2)
# bohemiaNewDG2<-hclust(bohemiaNewDist2,method="ward.D2")
# str(bohemiaNewDG2)
# plot(bohemiaNewDG2)
# 
# bohemiaNewDF3<-as.data.frame(bohemiaNewTDM[[3]])
# bohemiaNewDist3<-dist(bohemiaNewDF3)
# bohemiaNewDG3<-hclust(bohemiaNewDist3,method="ward.D2")
# str(bohemiaNewDG3)
# plot(bohemiaNewDG3, labels=FALSE)

# remove words with length <= 10
removeWords<-function(x) gsub("\\b\\w{1,10}\\b", "", x)
bohemiaLowNew<-tm_map(bohemiaLow, tm::content_transformer(removeWords))
bohemiaNewTDM1<-tm::TermDocumentMatrix(bohemiaLowNew)

bohemiaNewDF1.1<-as.data.frame(bohemiaNewTDM1[[1]])
bohemiaNewDist1.1<-dist(bohemiaNewDF1.1)
bohemiaNewDG1.1<-hclust(bohemiaNewDist1.1,method="ward.D2")
str(bohemiaNewDG1.1)
plot(bohemiaNewDG1.1, labels=FALSE)

bohemiaNewDF2.1<-as.data.frame(bohemiaNewTDM1[[2]])
bohemiaNewDist2.1<-dist(bohemiaNewDF2.1)
bohemiaNewDG2.1<-hclust(bohemiaNewDist2.1,method="ward.D2")
str(bohemiaNewDG2.1)
plot(bohemiaNewDG2.1, labels=FALSE)

bohemiaNewDF3.1<-as.data.frame(bohemiaNewTDM1[[3]])
bohemiaNewDist3.1<-dist(bohemiaNewDF3.1)
bohemiaNewDG3.1<-hclust(bohemiaNewDist3.1,method="ward.D2")
str(bohemiaNewDG3.1)
plot(bohemiaNewDG3.1, labels=FALSE)

## zipfR package

# Chapter 1
bohemia1.spc<-spc(bohemiaTF1)
summary(bohemia1.spc)
Vm(bohemia1.spc,1) / N(bohemia1.spc)
plot(bohemia1.spc)
plot(bohemia1.spc, log="x")
with(bohemia1.spc, plot(m, Vm, main="Chapter 1 Frequency Spectrum"))
# vocabulary growth curve
bohemia1.lnre<-lnre("zm", bohemia1.spc)
bohemia1.emp.vgc<-lnre.vgc(bohemia1.lnre, 1:807448)
head(bohemia1.emp.vgc)
summary(bohemia1.emp.vgc)
plot(bohemia1.emp.vgc)
# interpolated growth curve
bohemia1.bin.vgc<-vgc.interp(bohemia1.spc, N(bohemia1.emp.vgc, m.max=1))
head(bohemia1.bin.vgc)
plot(bohemia1.emp.vgc, bohemia1.bin.vgc, legend=c("observed", "interpolated"))

bohemia1.fzm<-lnre("fzm", bohemia1.spc, exact=FALSE)
summary(bohemia1.fzm)
bohemia1.fzm.spc<-lnre.spc(bohemia1.fzm, N(bohemia1.fzm))
plot(bohemia1.spc, bohemia1.fzm.spc, legend=c("observed", "fZM"))
bohemia1.fzm.vgc<-lnre.vgc(bohemia1.fzm, (1:100)*N(bohemia1.spc)*2)
plot(bohemia1.emp.vgc, bohemia1.fzm.vgc, legend=c("observed", "fZM"))
# evaluating extrapolation quality
set.seed(42)
bohemia1.sub.spc<-sample.spc(bohemia1.spc, N=40000)
bohemia1.sub.fzm<-lnre("fzm", bohemia1.sub.spc, exact=FALSE)
bohemia1.sub.fzm

bohemia1.sub.fzm.vgc<-lnre.vgc(bohemia1.sub.fzm, N=N(bohemia1.emp.vgc))
plot(bohemia1.bin.vgc, bohemia1.sub.fzm.vgc, N0=N(bohemia1.sub.fzm), legend=c("interpolated","fZM"))

# Chapter 2
bohemia2.spc<-spc(bohemiaTF2)
summary(bohemia2.spc)
Vm(bohemia2.spc,1) / N(bohemia2.spc)
plot(bohemia2.spc)
plot(bohemia2.spc, log="x")
with(bohemia2.spc, plot(m, Vm, main="Chapter 2 Frequency Spectrum"))

# Chapter 3
bohemia3.spc<-spc(bohemiaTF3)
summary(bohemia3.spc)
Vm(bohemia3.spc,1) / N(bohemia3.spc)
plot(bohemia3.spc)
plot(bohemia3.spc, log="x")
with(bohemia3.spc, plot(m, Vm, main="Chapter 3 Frequency Spectrum"))

## word clouds
pal<-brewer.pal(9,"BuGn")
pal2<-brewer.pal(9, "Spectral")
str(pal)

words1<-names(bohemiaTF1)
bohemiaWC1<-wordcloud(words1, bohemiaTF1, colors=pal2)

words2<-names(bohemiaTF2)
bohemiaWC2<-wordcloud(words2,bohemiaTF2, scale=c(3,.5), colors=pal2)

words3<-names(bohemiaTF3)
bohemiaWC3<-wordcloud(words3,bohemiaTF3, colors=pal2)
