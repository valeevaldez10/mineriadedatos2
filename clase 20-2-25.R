rm(list=ls())

library(dplyr)
library(tm) #minería de texto 
library(udpipe) #etiquetado #########INVESTIGAR
library(hunspell) #ortografía
library(syuzhet) #análisis de sentimiento
library(stringr)
library(pdftools)
library(ggplot2)
library(wordcloud2)
library(ggworldcloud)
library(igraph)
library(visNetwork)

options(stringAsFactors=FALSE)
Sys.setlocale(category="LC_ALL",locale="es_ES.UTF-8")
#############################
load("datasets/larazon.RData")
#############################
topico<-bd %>% filter(str_detect(tolower(titular),"economía"))
#############################
bd_corpus<-VCorpus(VectorSource(topico$titular))
# Corpus en carpeta
getReaders() #ver tipos de formato
dirpdf<-"datasets/pdfs"
pdf_corpus<-Corpus(DirSource(dirpdf,pattern = ".pdf"),readerControl=list(reader=readPDF))

#Limpieza del corpus
getTransformations()
bd_corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(str_replace),"la paz","lapaz") %>% 
  tm_map(removeWords,stopwords("es")) %>% 
  tm_map(removeWords,c("ejemplo"))
#funcion de limpieza
limpieza<-function(cp,extra=c(""),cambio=c("la paz"="lapaz")){
  cp %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(str_replace_all),cambio) %>% 
    tm_map(removeWords,stopwords("es")) %>% 
    tm_map(removeWords,extra) %>% 
    tm_map(stripWhitespace) %>% 
    return()
  }
bd_corpusd<-limpieza(bd_corpus,
                     extra=c("hola"),
                     cambio=c("‘"="","’"=""))

bd_corpus[[1]]$content
bd_corpusd[[1]]$content

pdf_corpusd<-limpieza(pdf_corpus)
pdf_corpus[[1]]$content
pdf_corpus[[1]]$content

#armar el TDM o DTM
dtm_V<-DocumentTermMatrix(bd_corpusd)
tdm_V<-TermDocumentMatrix(bd_corpusd)
dtm_pdf<-DocumentTermMatrix(pdf_corpusd)
tdm_pdf<-TermDocumentMatrix(pdf_corpusd)
dim(dtm_pdf)
dim(tdm_pdf)

#analisis descriptivo de frecuencias

#para los titulares
aux<-tdm_V %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux$words<-rownames(aux)
ggplot(aux %>% filter(freq>2),aes(freq,word))+geom_point()
wordcloud2(aux[,c(2,1)]) #el orden es la palabra y la frecuencia para que wordcloud pueda leer el documento
ggplot(aux,aes(label=words,size)


       
#para mis pdfs
aux1<-tdm_pdf %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux1$words<-rownames(aux1)
wordcloud2(aux1[,c(2,1)])
