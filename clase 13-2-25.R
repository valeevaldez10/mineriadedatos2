rm(list=ls())
library(stringr)
library(stringi)
library(tm)
library(dplyr)
# Cargar los datos
load("larazon.RData")
##############exploración y funciones de texto
# conocer el tamaño de los documentos
bd$titular %>% nchar() %>% hist()
bd$titular %>% tolower()
bd$titular %>% toupper()
bd$titular %>% tolower() %>% 
  str_detect("potosi") %>% table()

bd$titular %>% tolower() %>% 
  str_detect("potosí") %>% table()

titular<-bd$titular %>% tolower()
# correcciones / etiquetado
titular<-titular %>% 
  str_replace("potosi","potosí")

titular %>% tolower() %>% 
  str_detect("potosi") %>% table()
# Cambios en ortografía y etiquetado
pp<-c("la paz", 
      "santa cruz", 
      "santacruz de la sierra",
      "avaroa",
      "evo morales"
)
cc<-c("lapaz", 
      "santacruz",
      "santacruzdelasierra",
      "abaroa",
      "evo"
)
for(i in 1:length(pp)){
  titular<-titular %>% 
    str_replace(pp[i],cc[i])
}