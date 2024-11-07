#librerias----
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(highcharter)
library(knitr)
library(kableExtra)


#Carga de dataset----
base <- read_excel("base.xlsx")
base <- as.data.frame(base)

#Objeto SE MAX----
tabla_SEANIO <- base%>%
  group_by(SEPI_SINTOMA, ANIO_EPI_SINTOMA) %>%  
  summarise(cantidad = n(), .groups = "drop") %>%  # Conteo 
  arrange(desc(ANIO_EPI_SINTOMA), desc(SEPI_SINTOMA))  # Ordenado

# año máximo en la tabla
anio_maximo <- max(tabla_SEANIO$ANIO_EPI_SINTOMA, na.rm = TRUE)

# Filto la tabla para el año máximo y poder obtener la semana máxima
SE_MAX<- tabla_SEANIO %>%
  filter(ANIO_EPI_SINTOMA == anio_maximo) %>%  # Filtrar año max
  summarise(tabla_SEANIO = max(SEPI_SINTOMA, na.rm = TRUE))


#Objeto conteo notificaciones----
cant_notif <- base %>% count()

#Objeto distribucion sx----
SX <- base %>% count(SEXO) %>%             
  mutate(perc = round(n / sum(n) * 100))  

# porcentaje max en la tabla
porc_max <- max(SX$perc, na.rm = TRUE)

#n max de la tabla
sx_max <- max(SX$n, na.rm = TRUE)
