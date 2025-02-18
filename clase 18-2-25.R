rm(list=ls())
library(dplyr)
library(tm) #minería de texto 
library(udpipe) #etiquetado
library(hunspell) #ortografía
library(syuzhet) #análisis de sentimiento

load("datasets/larazon.RData")
