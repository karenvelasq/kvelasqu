library(dplyr)
library(stringi)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(ggplot2)
library(NLP)
library(tm)
library(slam)
library(RColorBrewer)
library(RWeka)
library(wordcloud)
library(data.table)
library(dplyr)


library(stringr)


blogs <- readLines("C:/Users/Familia Zapata/Documents/Karen/Especializacion/Ciencia de Datos/Final/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)

news <- readLines("C:/Users/Familia Zapata/Documents/Karen/Especializacion/Ciencia de Datos/Final/Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)

twitter <- readLines("C:/Users/Familia Zapata/Documents/Karen/Especializacion/Ciencia de Datos/Final/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

Sdata <- data.frame('F_data' = c("BLOGS","NEWS","TWITTER"),
                    'F_Size' =   sapply(list(blogs,news,twitter),function(x){format(object.size(x),"MB")}),
                    'F_Lines'= sapply(list(blogs,news,twitter),function(x){length(x)}),
                    'F_TCharacteres'= sapply(list(blogs,news,twitter), function(x){sum(nchar(x))}),
                    'MaxCharacters' = sapply(list(blogs, news, twitter), function(x){max(unlist(lapply(x, function(y) nchar(y))))}),
                    'MinCharacters' = sapply(list(blogs, news, twitter), function(x){min(unlist(lapply(x, function(y) nchar(y))))})                    
                    
)

blogs <- iconv(blogs,"latin1","ASCII", sub = "")
news <- iconv(news,"latin1","ASCII", sub = "")
twitter <- iconv(twitter,"latin1","ASCII", sub = "")

size<- 0.03
s_data <- c(sample(blogs,length(blogs)*size),
            sample(news,length(news)*size),
            sample(twitter,length(twitter)*size))

corpus <- VCorpus(VectorSource(s_data))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, content_transformer(tolower))


library(rJava)
library(RWeka)


#Unigram
uni_tk <- function(x) NGramTokenizer(x,Weka_control(min=1,max=1))

#Bigram
bi_tk <- function(x) NGramTokenizer(x,Weka_control(min=2,max=2))

#Trigram
tri_tk <- function(x) NGramTokenizer(x,Weka_control(min=3,max=3))


#Create matrix
uni_mtx <- TermDocumentMatrix(corpus, control = list(tokenize = uni_tk))

bi_mtx <- TermDocumentMatrix(corpus, control = list(tokenize = bi_tk))

tri_mtx <- TermDocumentMatrix(corpus, control = list(tokenize = tri_tk))


#Frecuency for matrix

uni_cp <- findFreqTerms(uni_mtx,lowfreq = 5000)
bi_cp <- findFreqTerms(bi_mtx,lowfreq = 1400)
tri_cp <- findFreqTerms(tri_mtx,lowfreq = 200)


#Unigram
uni_cp_fq <- rowSums(as.matrix(uni_mtx[uni_cp,]))
uni_cp_fq <- data.frame(word=names(uni_cp_fq), frequency=uni_cp_fq)
termFreq1 <- uni_cp_fq[order(-uni_cp_fq$frequency),]
unigramlist <- setDT(termFreq1)
save(unigramlist,file="unigram.Rda")



#Bigram
bi_cp_fq <- rowSums(as.matrix(bi_mtx[bi_cp,]))
bi_cp_fq <- data.frame(word=names(bi_cp_fq), frequency=bi_cp_fq)
termFreq2 <- bi_cp_fq[order(-bi_cp_fq$frequency),]
bigramlist <- setDT(termFreq2)
save(bigramlist,file="bigram.Rda")

#Trigram
tri_cp_fq <- rowSums(as.matrix(tri_mtx[tri_cp,]))
tri_cp_fq <- data.frame(word=names(tri_cp_fq), frequency=tri_cp_fq)
termFreq3 <- tri_cp_fq[order(-tri_cp_fq$frequency),]
trigramlist <- setDT(termFreq3)
save(trigramlist,file="trigram.Rda")



bigramlist2 <- bigramlist
bigramlist2$primera <- as.character(sapply(bigramlist2$word,word))


bigramlist3 <- trigramlist
bigramlist3$primera <- as.character(sapply(bigramlist3$word,word,1,2))






wordproc <- function(x){
  
  sen = unlist(strsplit(x,' '))
  
  if(length(sen)==1){
    
    kq <- filter(bigramlist2,bigramlist2$primera == x)
    k2 <- top_n(kq,1,frequency)
    print(k2$word)
   
  }
  
  if(length(sen)==2){
    
    kq <- filter(bigramlist3,bigramlist3$primera == x)
    k2 <- top_n(kq,1,frequency)
    print(k2$word)
    
  }
  
  if(length(sen)>=3){
    
    k2 <- 'to'
    print(k2)
    
  }
  
  if(is.null(k2)){
    
    k2 = "the"
    
    print(k2)
    
    #        k<-unigramlist$unigram
    
    #        value = as.String(k[1])
    
  }
  
  
}



