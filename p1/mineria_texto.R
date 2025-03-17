###Ejercicio 1
rm(list=ls())
library(dplyr)
library(tm) #minería de texto 
library(udpipe) #etiquetado
library(hunspell) #ortografía
library(syuzhet) #análisis de sentimiento
library(stringr)
library(pdftools)
library(ggplot2)
library(wordcloud2)
library(ggwordcloud)
library(igraph)
library(visNetwork)

options(stringAsFactors=FALSE)
Sys.setlocale(category="LC_ALL",locale="es_ES.UTF-8")


#función de limpieza
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

eco %>% str_detect("pobreza") %>% table
eco %>% str_detect("Pobreza") %>% table
eco %>% str_detect("pobre") %>% table
eco %>% str_detect("empobrecido") %>% table

#PREGUNTAR SI SE NECESITA POBRE TAMBIÉN
eco<-eco %>% str_to_lower() %>% removePunctuation()
pob<-eco %>% str_detect("pobre")
nro_doc<-length(eco)
nro_pob<-sum(pob)
(nro_pob/nro_doc)*100



