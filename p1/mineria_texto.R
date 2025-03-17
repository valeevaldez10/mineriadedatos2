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


###Ejercicio 3
rm(list=ls())
load(url("https://github.com/AlvaroLimber/md1_2024/raw/refs/heads/main/_data/eh22.RData"))
pre<-eh22p$s03b_09e
p<-data.frame(pre)
p<-p %>% filter(pre!="") #los verdaderos
p
bd_corpus<-VCorpus(VectorSource(p$pre))
bd_corpus[[1]]$content
## preguntar
bd_corpusd<-limpieza(bd_corpus,extra=c("realiza","falta","busca"))
bd_corpusd<-limpieza(bd_corpusd,cambio=c("cuidado miembros hogar"="hogar"))

##armar dtm y tdm
dtm_V<-DocumentTermMatrix(bd_corpusd)
tdm_V<-TermDocumentMatrix(bd_corpusd)
dim(dtm_V)
dim(tdm_V)

##nube de palabras
aux<-tdm_V %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux$words<-rownames(aux)
wordcloud2(aux[,c(2,1)])

###normal
bd1<-limpieza(bd_corpus)
tdm_v<-TermDocumentMatrix(bd1)
aux1<-tdm_v %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux1$words<-rownames(aux1)
wordcloud2(aux1[,c(2,1)])

##analisis de sentimiento
auxs<-get_nrc_sentiment(aux$words,language = "spanish")
rownames(auxs)<-aux$words
#gráfico de barras con las frecuencias de cada sentimiento
barplot(apply(auxs,2,sum),horiz=T,las=1)
auxsc<-auxs %>% mutate(words=aux$words,freq=aux$freq)
ggplot(auxsc,aes(label=words,size=freq,col=joy))+
  geom_text_wordcloud()+
  theme_minimal()

wordcloud2(auxsc %>% select(words,freq),
           color =ifelse(auxsc$joy==1,"red","black"))


##EJERCICIO 3 (EH2021)
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
eh21p$s03a_05e %>% table
pre<-as.character(eh21p$s03a_05e)
p<-data.frame(pre)
View(p)
p<-p %>% filter(pre!="") #los verdaderos
length(p$pre)



bd_corpus<-VCorpus(VectorSource(p$pre))
bd_corpusd<-bd_corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords("es"))
bd_corpusd<-bd_corpusd %>% 
  tm_map(content_transformer(str_replace_all),"covid 19","covid19") %>% 
  tm_map(stripWhitespace)

##armar dtm y tdm
dtm_V<-DocumentTermMatrix(bd_corpusd)
tdm_V<-TermDocumentMatrix(bd_corpusd)
dim(dtm_V)
dim(tdm_V)

##nube de palabras
aux<-tdm_V %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux$words<-rownames(aux)
wordcloud2(aux[,c(2,1)] %>% filter(freq>2))

ggplot(aux %>% filter(freq>2),aes(label=words,size=freq))+
  geom_text_wordcloud(col="darkblue")+
  scale_size_area(max_size = 15)+
  theme_minimal()



