rm(list=ls())
library(dplyr)
library(tm) #minería de texto 
library(udpipe) #etiquetado
library(hunspell) #ortografía
library(syuzhet) #análisis de sentimiento

#configuración para caracteres e idioma
options(stringsAsFactors=FALSE)
Sys.setlocale(category = "LC_ALL",locale="es_ES.UTF-8")
##############
load("datasets/larazon.RData")
##############
#Ortografía
list_dictionaries()
texto<-"hola pequeña vandida"
aux<-hunspell(texto,dict = "es_ES")
hunspell_suggest(aux[[1]],dict = "es_ES") #nos dice cuales pueden ser posibles correcciones
#caso archivo la razon
bd$titular
hunspell(bd$titular[1:5],dict = "es_ES")

clarazon <- VCorpus(VectorSource(bd$titular))
clarazon[[20]] #toma en cuenta a cada titular como un documento
clarazon[[20]]$content


