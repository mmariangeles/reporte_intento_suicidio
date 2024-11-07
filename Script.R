library(readr)
library(dplyr)
library(tidyverse)
library(data.table)



base <- fread("NEUQUEN_NOMINAL.csv", sep=";",na.strings=c("NA","sin dato","","NULL", "SIN DATO (SIN DATO)"),encoding="UTF-8")

#DATAFRAME
base <- as.data.frame(base)
#class(base)

#str(base)



#filtros evento provincia residencia, prov atencion, invalidados)
base <- base %>% 
  filter(PROVINCIA_RESIDENCIA == "Neuquén")

base <- base %>% 
  filter(PROV_CLINICA == "Neuquén")

base <- base %>% 
  filter(EVENTO == "Intento de Suicidio")


base <- base %>% 
  filter(CLASIFICACION_MANUAL!="Caso invalidado por epidemiología")









