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
library(ggwordcloud)
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

#para mis pdfs
aux1<-tdm_pdf %>% as.matrix() %>% rowSums() %>% 
  data.frame(freq=.)
aux1$words<-rownames(aux1)
wordcloud2(aux1[,c(2,1)])

##########CLASE 25/02/2024
ggplot(aux,aes(label=words,size=freq))+
  geom_text_wordcloud(col="darkblue")+
  scale_size_area(max_size = 15)+
  theme_minimal()

### Análisis descriptivo: sentimiento
auxs<-get_nrc_sentiment(aux$words,language = "spanish")
rownames(auxs)<-aux$words

#gráfico de barras con las frecuencias de cada sentimiento
barplot(apply(auxs,2,sum),horiz=T,las=1)
# creamos una BD pero incluimos las frecuencias como
# palabras de nuestro dataframe original
auxsc<-auxs %>% mutate(words=aux$words,freq=aux$freq)
ggplot(auxsc,aes(label=words,size=freq,col=fear))+
  geom_text_wordcloud()+
  theme_minimal()
#Debido a que es una revista con noticias de economía
auxsc<-auxsc %>% filter(words!="economía")
wordcloud2(auxsc %>% select(words,freq),
           color =ifelse(auxsc$fear==1,"red","black"))

### Análisis diagnóstico: asociación 
#palabras que participan con la palabra arce, tienen correlación y un valor de corr min
findAssocs(tdm_V,"arce",0.2) 
findAssocs(tdm_V,"ministro",0.1) 
#se puede hacer un análisis de sentimiento con las palabras que se correlacionan
#con ministro en mínimo un 10%

#Análisis diagnóstico: redes
mxd<-as.matrix(dtm_V) %*% t(as.matrix(dtm_V))
mxt<-as.matrix(tdm_V) %*% t(as.matrix(tdm_V))
dim(mxd)
dim(mxt)

dim(tdm_V)
#se lo reduce porque ocupa mucha memoria
tdm_reducido <- removeSparseTerms(tdm_V,sparse = 0.99) #remueve palabras que no tienen mucha participación en el 99%
dim(tdm_reducido)
mxt<-as.matrix(tdm_reducido) %*% t(as.matrix(tdm_reducido))

#Recomendaciones: reducir la cantidad de terminos con base a las frecuencias
g <-graph.adjacency(mxt,weighted = T,mode = "undirected")
g<-simplify(g)
V(g)$label<-V(g)$name
V(g)$degree<-degree(g)
data<-toVisNetworkData(g)
nodes = data$nodes;edges=data$edges
set.seed(12345)
grupos<-cluster_label_prop(g)
nodes$value<-prop.table(nodes$degree)*1000
nodes$group<-grupos$membership
visNetwork(nodes,edges)
visNetwork(nodes,edges) %>% visPhysics(enabled = FALSE)
### Análisis diagnóstico: agrupamiento
#según esto podemos ver que tan distintos son los documentos entre ellos
#esta clasificación se da dadas las palabras que se están usando
kmeans(as.matrix(dtm_pdf),3)
res<-kmeans(as.matrix(dtm_pdf),3)
data.frame(res$cluster)


