install.packages("tm")
install.packages("wordcloud")
install.packages("zipfR")
install.packages("tokenizers")
install.packages("tidyverse")
library(tm)
library(wordcloud)
library(zipfR)
library(tokenizers)
library(tidyverse)

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
  s<-bohemiaCh1.words[i,]
  l<-nchar(s['word'])
  s['length']<-l
  bohemiaCh1.words[i,]<-s
}

bohemia1.ordered<-bohemiaCh1.words[order(-bohemiaCh1.words$length),]
bohemia1.ordered.distinct<-distinct(bohemia1.ordered)
head(bohemia1.ordered.distinct, 10)

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
plot(bohemiaDG1)

bohemiaDF2<-as.data.frame(bohemiaStopTDM[[2]])
bohemiaDist2<-dist(bohemiaDF2)
bohemiaDG2<-hclust(bohemiaDist2,method="ward.D2")
str(bohemiaDG2)
plot(bohemiaDG2)

bohemiaDF3<-as.data.frame(bohemiaStopTDM[[3]])
bohemiaDist3<-dist(bohemiaDF3)
bohemiaDG3<-hclust(bohemiaDist3,method="ward.D2")
str(bohemiaDG3)
plot(bohemiaDG3)

bohemiaStopNew<-tm::tm_map(bohemiaStop, tm::removeWords, c("even", "will", "may", "soon", "can", "as", "one", "much", "just", "now", "quite", "merely", "shall", "take", "certain", "well"))
str(bohemiaStopNew)

bohemiaNewTDM<-tm::TermDocumentMatrix(bohemiaStopNew)

bohemiaNewDF1<-as.data.frame(bohemiaNewTDM[[1]])
bohemiaNewDist1<-dist(bohemiaNewDF1)
bohemiaNewDG1<-hclust(bohemiaNewDist1,method="ward.D2")
str(bohemiaNewDG1)
plot(bohemiaNewDG1)

bohemiaNewDF2<-as.data.frame(bohemiaNewTDM[[2]])
bohemiaNewDist2<-dist(bohemiaNewDF2)
bohemiaNewDG2<-hclust(bohemiaNewDist2,method="ward.D2")
str(bohemiaNewDG2)
plot(bohemiaNewDG2)

bohemiaNewDF3<-as.data.frame(bohemiaNewTDM[[3]])
bohemiaNewDist3<-dist(bohemiaNewDF3)
bohemiaNewDG3<-hclust(bohemiaNewDist3,method="ward.D2")
str(bohemiaNewDG3)
plot(bohemiaNewDG3)

## zipfR package
bohemia1.spc<-spc(bohemiaTF1)
summary(bohemia1.spc)
Vm(bohemia1.spc,1) / N(bohemia1.spc)

bohemia2.spc<-spc(bohemiaTF2)
summary(bohemia2.spc)
Vm(bohemia2.spc,1) / N(bohemia2.spc)

bohemia3.spc<-spc(bohemiaTF3)
summary(bohemia3.spc)
Vm(bohemia3.spc,1) / N(bohemia3.spc)

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



