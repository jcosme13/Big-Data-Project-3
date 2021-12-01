install.packages("tm")
library(tm)

bohemia<-VCorpus(DirSource(".", ignore.case=FALSE, mode="text"))
str(bohemia)
btext1<-bohemia[[1]]
btext2<-bohemia[[2]]
btext3<-bohemia[[3]]

bohemiaDTM<-DocumentTermMatrix(bohemia)

bohemiaTDM<-tm::TermDocumentMatrix(bohemia)
bohemiaTDM

freqTerms<-tm::findFreqTerms(bohemiaTDM, lowfreq=5)

for (i in freqTerms) {
      wordLen<-nchar(freqTerms[i])
    }


myStopWords<-c(tm::stopwords("english"))
bohemiaStop<-tm::tm_map(bohemia,tm::removeWords,myStopWords)
