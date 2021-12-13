install.packages("tm")
install.packages("wordcloud")
install.packages("zipfR")
install.packages("tokenizers")
install.packages("tidyverse")
install.packages("qdapTools")
install.packages("qdapRegex")
install.packages("syuzhet")
install.packages("quanteda")
install.packages("stringdist")
install.packages("stringi")
install.packages("ngram")
library(tm)
library(wordcloud)
library(zipfR)
library(tokenizers)
library(tidyverse)
library(qdapTools)
library(qdapRegex)
library(syuzhet)
library(quanteda)
library(stringdist)
library(stringi)
library(ngram)

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
longestSent1<-head(orderedSent,10)
longestSent1

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
longestSent2<-head(orderedSent,10)
longestSent2

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
longestSent3<-head(orderedSent,10)
longestSent3

## Chapter 1 word extraction
count<-1
count2<-1
zList1<-c()

for (i in 1:nrow(longestSent1)) {
  counter<-paste("count: ", count, sep="")
  v <- gsub("[[:punct:]]", " ", as.character(longestSent1[[1]][i]))
  a=(strsplit(v, " "))
  count<-count+1
  for (j in 1:length(a[[1]])) {
      if (count2 > length(a[[1]])) {
        count2<-0
      }
      x<-nchar(a[[1]])
      y<-a[[1]] [x %in% 7:15]
      z<-paste(y, collapse=" ")
      zList1[[i]]<-z
      count2<-count2+1
    }
}

## Chapter 2 word extraction
count<-1
count2<-1
zList2<-c()
rm(i)
rm(j)

for (i in 1:nrow(longestSent2)) {
  counter<-paste("count: ", count, sep="")
  v <- gsub("[[:punct:]]", " ", as.character(longestSent2[[1]][i]))
  a=(strsplit(v, " "))
  count<-count+1
  for (j in 1:length(a[[1]])) {
    if (count2 > length(a[[1]])) {
      count2<-0
    }
    x<-nchar(a[[1]])
    y<-a[[1]] [x %in% 7:15]
    z<-paste(y, collapse=" ")
    zList2[[i]]<-z
    count2<-count2+1
  }
}

## Chapter 3 word extraction
count<-1
count2<-1
zList3<-c()
rm(i)
rm(j)

for (i in 1:nrow(longestSent3)) {
  counter<-paste("count: ", count, sep="")
  v <- gsub("[[:punct:]]", " ", as.character(longestSent3[[1]][i]))
  a=(strsplit(v, " "))
  count<-count+1
  for (j in 1:length(a[[1]])) {
    if (count2 > length(a[[1]])) {
      count2<-0
    }
    x<-nchar(a[[1]])
    y<-a[[1]] [x %in% 7:15]
    z<-paste(y, collapse=" ")
    zList3[[i]]<-z
    count2<-count2+1
  }
}

## bigrams
# Chapter 1
for (k in 1:length(zList1)) {
  print("-----start-----")
  print(ngram(zList1[[k]], n=2), output="full")
  print("------end------")
}

# Chapter 2
for (k in 1:length(zList2)) {
  print("-----start-----")
  print(ngram(zList2[[k]], n=2), output="full")
  print("------end------")
}

# Chapter 3
for (k in 1:length(zList3)) {
  print("-----start-----")
  print(ngram(zList3[[k]], n=2), output="full")
  print("------end------")
}

## trigrams
# Chapter 1
for (k in 1:length(zList1)) {
  print("-----start-----")
  print(ngram(zList1[[k]], n=3), output="full")
  print("------end------")
}

# Chapter 2
for (k in 1:length(zList2)) {
  print("-----start-----")
  print(ngram(zList2[[k]], n=3), output="full")
  print("------end------")
}

# Chapter 3
for (k in 1:length(zList3)) {
  if(k == 2 | k == 6 | k == 8) {
    print("trigram not possible")
  }
  else {
    print("-----start-----")
    print(ngram(zList3[[k]], n=3), output="full")
    print("------end------")
  }
}

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
bohemiaStopTDM

freqTerms<-tm::findFreqTerms(bohemiaStopTDM, lowfreq=5)
freqTerms

bohemiaTF1<-tm::termFreq(bohemiaStop[[1]])
bohemiaTF1[1:30]
bohemiaTF2<-tm::termFreq(bohemiaStop[[2]])
bohemiaTF2[1:30]
bohemiaTF3<-tm::termFreq(bohemiaStop[[3]])
bohemiaTF3[1:30]

tm::inspect(bohemiaStopTDM)

## dendrograms
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

## new dendrograms
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

## sentiment analysis

# Chapter 1 #

# get text as string and then get sentences
bohemiaAsString1<-get_text_as_string("AScandalInBohemiaI.txt")
bohemiaSentences1<-get_sentences(bohemiaAsString1)
# sentiment analysis
bohemiaSentiment1<-get_sentiment(bohemiaSentences1, "syuzhet")
bohemiaSentiment1
bohemiaSentimentBing1<-get_sentiment(bohemiaSentences1, "bing")
bohemiaSentimentBing1
# bohemiaDictionary1<-get_sentiment_dictionary()
# sum sentiment values
bohemiaSentimentSum1<-sum(bohemiaSentiment1)
bohemiaSentimentSum1
bohemiaSentimentBingSum1<-sum(bohemiaSentimentBing1)
bohemiaSentimentBingSum1
# means of sentiment values
bohemiaSentimentMean1<-mean(bohemiaSentiment1)
bohemiaSentimentMean1
bohemiaSentimentBingMean1<-mean(bohemiaSentimentBing1)
bohemiaSentimentBingMean1
# NRC
bohemiaNRC1<-get_nrc_sentiment(bohemiaSentences1)
bohemiaNRC1[1:10,]
summary(bohemiaSentiment1)
summary(bohemiaSentimentBing1)
summary(bohemiaNRC1)

# plots
plot(bohemiaSentiment1, main="A Scandal In Bohemia I Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")
plot(bohemiaSentimentBing1, main="A Scandal In Bohemia I Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

# percentage values
bohemiaSentimentPctVal1.1<-get_percentage_values(bohemiaSentiment1, bins=10)
structure(bohemiaSentimentPctVal1.1)
plot(bohemiaSentimentPctVal1.1, main="A Scandal In Bohemia I PCTValue 10 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

bohemiaSentimentPctVal1.2<-get_percentage_values(bohemiaSentiment1, bins=20)
structure(bohemiaSentimentPctVal1.2)
plot(bohemiaSentimentPctVal1.2, main="A Scandal In Bohemia I PCTValue 20 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

# Chapter 2 #

# get text as string and then get sentences
bohemiaAsString2<-get_text_as_string("AScandalInBohemiaII.txt")
bohemiaSentences2<-get_sentences(bohemiaAsString2)
# sentiment analysis
bohemiaSentiment2<-get_sentiment(bohemiaSentences2, "syuzhet")
bohemiaSentiment2
bohemiaSentimentBing2<-get_sentiment(bohemiaSentences2, "bing")
bohemiaSentimentBing2
# bohemiaDictionary2<-get_sentiment_dictionary()
# sum sentiment values
bohemiaSentimentSum2<-sum(bohemiaSentiment2)
bohemiaSentimentSum2
bohemiaSentimentBingSum2<-sum(bohemiaSentimentBing2)
bohemiaSentimentBingSum2
# means of sentiment values
bohemiaSentimentMean2<-mean(bohemiaSentiment2)
bohemiaSentimentMean2
bohemiaSentimentBingMean2<-mean(bohemiaSentimentBing2)
bohemiaSentimentBingMean2
# NRC
bohemiaNRC2<-get_nrc_sentiment(bohemiaSentences2)
bohemiaNRC2[1:10,]
summary(bohemiaSentiment2)
summary(bohemiaSentimentBing2)
summary(bohemiaNRC2)

# plots
plot(bohemiaSentiment2, main="A Scandal In Bohemia II Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")
plot(bohemiaSentimentBing2, main="A Scandal In Bohemia II Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

# percentage values
bohemiaSentimentPctVal2.1<-get_percentage_values(bohemiaSentiment2, bins=10)
structure(bohemiaSentimentPctVal2.1)
plot(bohemiaSentimentPctVal2.1, main="A Scandal In Bohemia II PCTValue 10 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

bohemiaSentimentPctVal2.2<-get_percentage_values(bohemiaSentiment2, bins=20)
structure(bohemiaSentimentPctVal2.2)
plot(bohemiaSentimentPctVal2.2, main="A Scandal In Bohemia II PCTValue 20 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

# Chapter 3 #

# get text as string and then get sentences
bohemiaAsString3<-get_text_as_string("AScandalInBohemiaIII.txt")
bohemiaSentences3<-get_sentences(bohemiaAsString3)
# sentiment analysis
bohemiaSentiment3<-get_sentiment(bohemiaSentences3, "syuzhet")
bohemiaSentiment3
bohemiaSentimentBing3<-get_sentiment(bohemiaSentences3, "bing")
bohemiaSentimentBing3
# bohemiaDictionary3<-get_sentiment_dictionary()
# sum sentiment values
bohemiaSentimentSum3<-sum(bohemiaSentiment3)
bohemiaSentimentSum3
bohemiaSentimentBingSum3<-sum(bohemiaSentimentBing3)
bohemiaSentimentBingSum3
# means of sentiment values
bohemiaSentimentMean3<-mean(bohemiaSentiment3)
bohemiaSentimentMean3
bohemiaSentimentBingMean3<-mean(bohemiaSentimentBing3)
bohemiaSentimentBingMean3
# NRC
bohemiaNRC3<-get_nrc_sentiment(bohemiaSentences3)
bohemiaNRC3[1:10,]
summary(bohemiaSentiment3)
summary(bohemiaSentimentBing3)
summary(bohemiaNRC3)

# plots
plot(bohemiaSentiment3, main="A Scandal In Bohemia III Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")
plot(bohemiaSentimentBing3, main="A Scandal In Bohemia III Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

# percentage values
bohemiaSentimentPctVal3.1<-get_percentage_values(bohemiaSentiment3, bins=10)
structure(bohemiaSentimentPctVal3.1)
plot(bohemiaSentimentPctVal3.1, main="A Scandal In Bohemia III PCTValue 10 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

bohemiaSentimentPctVal3.2<-get_percentage_values(bohemiaSentiment3, bins=20)
structure(bohemiaSentimentPctVal3.2)
plot(bohemiaSentimentPctVal3.2, main="A Scandal In Bohemia III PCTValue 20 Bins", xlab="Narrative", ylab="Emotional Valence", col="red")

## quanteda package

# Chapter 1 #
bohemiaText1<-bohemiaStop[[1]]
# apply tokenization
bohemiaTokens1<-quanteda::tokens(bohemiaText1$content[1:10])
str(bohemiaTokens1)
# sparse document-feature matrix
bohemiaDFM1<-quanteda::dfm(bohemiaTokens1)
str(bohemiaDFM1)
# frequency
bohemiaDocFreq1<-quanteda::docfreq(bohemiaDFM1)
str(bohemiaDocFreq1)
bohemiaDocFreq1
# tf-idf score
bohemiaTFIDF1<-quanteda::dfm_tfidf(bohemiaDFM1, scheme_tf="count", scheme_df="inverse")
str(bohemiaTFIDF1)

# Chapter 2 #
bohemiaText2<-bohemiaStop[[2]]
# apply tokenization
bohemiaTokens2<-quanteda::tokens(bohemiaText2$content[1:10])
str(bohemiaTokens2)
# sparse document-feature matrix
bohemiaDFM2<-quanteda::dfm(bohemiaTokens2)
str(bohemiaDFM2)
# frequency
bohemiaDocFreq2<-quanteda::docfreq(bohemiaDFM2)
str(bohemiaDocFreq2)
bohemiaDocFreq2
# tf-idf score
bohemiaTFIDF2<-quanteda::dfm_tfidf(bohemiaDFM2, scheme_tf="count", scheme_df="inverse")
str(bohemiaTFIDF2)

# Chapter 3 #
bohemiaText3<-bohemiaStop[[3]]
# apply tokenization
bohemiaTokens3<-quanteda::tokens(bohemiaText3$content[1:10])
str(bohemiaTokens3)
# sparse document-feature matrix
bohemiaDFM3<-quanteda::dfm(bohemiaTokens3)
str(bohemiaDFM3)
# frequency
bohemiaDocFreq3<-quanteda::docfreq(bohemiaDFM3)
str(bohemiaDocFreq3)
bohemiaDocFreq3
# tf-idf score
bohemiaTFIDF3<-quanteda::dfm_tfidf(bohemiaDFM3, scheme_tf="count", scheme_df="inverse")
str(bohemiaTFIDF3)

## stringdist 

stringdist(as.character(btext1[1]), as.character(btext2[1]))
stringdist(as.character(btext1[1]), as.character(btext3[1]))
stringdist(as.character(btext2[1]), as.character(btext3[1]))

