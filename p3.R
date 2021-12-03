install.packages("tm")
library(tm)

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

#dendrogram for chapter 1
> bohemiadf<-as.data.frame(bohemiaStopTDM[[1]])
> bohemiaDist<-dist(bohemiadf)
> bohemiaDG<-hclust(bohemiaDist,method="ward.D2")
> str(bohemiaDG)
> plot(bohemiaDG)

#dendrogram for chapter 2
> bohemiadf2<-as.data.frame(bohemiaStopTDM[[2]])
> bohemiaDist<-dist(bohemiadf2)
> bohemiaDG<-hclust(bohemiaDist,method="ward.D2")
> str(bohemiaDG)
> plot(bohemiaDG)

#dendrogram for chapter 3
> bohemiadf3<-as.data.frame(bohemiaStopTDM[[3]])
> bohemiaDist<-dist(bohemiadf2)
> bohemiaDG<-hclust(bohemiaDist,method="ward.D2")
> str(bohemiaDG)
> plot(bohemiaDG)
