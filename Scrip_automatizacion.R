#librerias----
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(highcharter)
library(knitr)
library(kableExtra)
library(gt)
library(glue)


#Carga de dataset----
base <- read_excel("base.xlsx")

base <- as.data.frame(base)
#Objeto fecha máx----


class(base$FECHA_APERTURA)
base$FECHA_APERTURA <- as.Date(base$FECHA_APERTURA, format = "%dd/%mm/%YY")
table(base$FECHA_APERTURA)

fecha_max <- max(base$FECHA_APERTURA, na.rm = TRUE) 
View(fecha_max)



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

#tabla clasificacion manual----
tabla_clasifmanual <- base %>%
  group_by(CLASIFICACION_MANUAL) %>%
  summarize(total = n(), .groups = "drop") %>%
  mutate(porcentaje = total / sum(total, na.rm = TRUE)) %>% 
  arrange(desc(total))


#Objeto distribucion sx----
SX <- base %>% count(SEXO) %>%             
  mutate(perc = round(n / sum(n) * 100))  

# porcentaje max en la tabla
porc_max <- max(SX$perc, na.rm = TRUE)

#n max de la tabla
sx_max <- max(SX$n, na.rm = TRUE)


#objeto filtro defunciones----
defunciones <- base %>% 
  filter(CLASIFICACION_MANUAL == "Intento de Suicidio con resultado mortal")

cant_defunciones <- count(defunciones)

#tabla por sexo y edad
tabla_defunc_SX <- defunciones %>%
  count(SEXO) %>%
  mutate(
    SEXO = if_else(SEXO == "M", "masculino", if_else(SEXO == "F", "femenino", SEXO)),  # Reemplazar M por Masculino y F por Femenino
    porcentaje = round(n / sum(n) * 100, 0))

#objeto porcentaje máx, n y sx
porcentaje_maximo <- max(tabla_defunc_SX$porcentaje)

n_max_def <- max(tabla_defunc_SX$n)

sx_max_def <- max(tabla_defunc_SX$SEXO)

#objeto min y max edad de defunciones
min_edad_defunc <- min(defunciones$EDAD_ACTUAL)
max_edad_defunc <- max(defunciones$EDAD_ACTUAL)



#tabla frencuencia por SE----
tabla_conteo <- base %>% count(ANIO_EPI_SINTOMA, SEPI_SINTOMA, SEXO)

#tabla para gráfico doble eje: notificaciones y tasas de notificacion----
poblacion <- read_csv2("Poblacion.csv")
poblacion <- as.data.frame(poblacion)


#armado tabla frecuencia por grupo edad y SX
cant_edadSX<- base %>% count(GRUPO_ETARIO)  #para mis barras


cant_edadSX2<- base %>% count(GRUPO_ETARIO, SEXO)

#full join
tasas_edadSX <- full_join(cant_edadSX2, poblacion, by = c("GRUPO_ETARIO" = "GRUPO_ETARIO", "SEXO" = "SEXO"))

#contruccion de tasa
tasas_edadSX <- tasas_edadSX %>%
  mutate(tasa_notificacion = round((n / POBLACION) * 10000, 1))


#tabla % por grupo de edad----
#esto lo hago para chequear que siga igual esa parte del texto
cant_edadSX <- cant_edadSX %>% 
mutate(perc = round(n / sum(n) * 100), 
frec_acumulada <- cumsum(perc))

#objeto tasa max, sexo y grupo de edad----
tabla_tasa_max <- tasas_edadSX %>%
  filter(tasa_notificacion == max(tasa_notificacion, na.rm = TRUE)) %>%
  select(SEXO, GRUPO_ETARIO, tasa_notificacion)#esta tabla la hago porque no me salio de otra forma

tabla_tasa_max <- tabla_tasa_max %>%
  mutate(SEXO = case_when(
    SEXO == "F" ~ "mujeres",
    SEXO == "M" ~ "varones"
  ))

#objeto sexo
sexo_maximo <- max(tabla_tasa_max$SEXO)

#objeto grupo de edad
gedad_maximo <- tolower(max(tabla_tasa_max$GRUPO_ETARIO)) #tolower es para usar minusculas

#objeto valor de tasa
tasa_max<- round(max(tabla_tasa_max$tasa_notificacion))




#tabla grupos de edad y tasa general----
poblacion_agrupada <- read_csv2("Poblacion_agrupada.csv")
poblacion_agrupada <- as.data.frame(poblacion_agrupada)
tabla_grupoetario<- base %>% count(GRUPO_ETARIO)

tabla_grupoetario <- full_join(tabla_grupoetario, poblacion_agrupada, by = "GRUPO_ETARIO")

#tasa grupo etario agrupado
tabla_grupoetario <- tabla_grupoetario %>%
  mutate(tasa_notificacion = round((n / POBLACION) * 10000, 1))

#filtro solo a los varones
tasas_varones <- tasas_edadSX %>% 
  filter(SEXO=="M")

#filtro solo a las mujeres
tasas_mujeres <- tasas_edadSX %>% 
  filter(SEXO=="F")

#gráfico tasas de IS (este gráfico finalmente no lo utilicé)----
tasas_edadSX %>% 
  ggplot(aes(x = GRUPO_ETARIO)) +   
  # Barras para las notificaciones
  geom_bar(aes(y = n), stat = "identity", fill = "#8FBC8F", alpha = 0.7) + 
  
  # Puntos para la tasa de notificación, con colores diferentes según el sexo
  geom_point(aes(y = tasa_notificacion * 5, color = SEXO), shape= 18 ,size = 3) +  
  
  # Ajuste de la escala del eje Y y el eje secundario
  scale_y_continuous(
    name = "Notificaciones", 
    sec.axis = sec_axis(~ . / 5, name = "Tasa de notificación x 10000") # Ajuste del eje secundario para la tasa
  ) + 
  
  # Personalizar la paleta de colores para SEXO y las etiquetas
  scale_color_manual(values = c("M" = "#feb24c", "F" = "#756bb1"), 
                     labels = c("M" = "Varones", "F" = "Mujeres")) + 
  theme_classic() +
  labs(title = "Grupo de edad: Notificaciones y Tasa de Notificación",
       subtitle = "Elaboración propia en base a datos de SNVS 2.0",
       x = "Grupo de edad") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))




#mecanismos----
# tabla totales y % 
tabla_mecanismo <- base %>%
  group_by(MECANISMO, SEXO) %>%
  summarize(total = n(), .groups = "drop") %>%
  group_by(SEXO) %>%
  mutate(
    porcentaje = total / sum(total,na.rm = T)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = SEXO, values_from = c(total, porcentaje)) %>%
  mutate(
    total_general = rowSums(select(., starts_with("total_")),na.rm=T),
    porcentaje_total_general = total_general / sum(total_general,na.rm = T))


tabla_mecanismo <- tabla_mecanismo %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  arrange(desc(total_general)) 


#mutate para mecanismos
tabla_mecanismo <- tabla_mecanismo %>% 
  mutate(MECANISMO=case_when(
    MECANISMO=="Mecanismo: Ahogamiento por sumersión"~"Ahogamiento por sumersión",
    MECANISMO=="Mecanismo: Ahorcamiento, estrangulamiento o sofocación"~"Ahorcamiento, estrangulamiento o sofocación",
    MECANISMO=="Mecanismo: Arrojarse o colocarse delante delante de objeto en mov."~"Arrojarse o colocarse delante delante de objeto en movimiento",
    MECANISMO=="Mecanismo: Disparo de armas de fuego"~"Disparo de armas de fuego",
    MECANISMO=="Mecanismo: Objeto cortante"~"Objeto cortante",
    MECANISMO=="Mecanismo: Saltar desde un lugar elevado"~"Saltar desde un lugar elevado",
    MECANISMO=="Mecanismo: Siniestro vial"~"Siniestro vial",
    MECANISMO=="Mecanismo: Sobreingesta de medicamentos"~"Sobreingesta de medicamentos",
    MECANISMO=="Otro mecanismo o modalidad"~"Otro mecanismo o modalidad",
    MECANISMO=="Sin datos"~"Sin datos",
    TRUE ~ MECANISMO
  ))





#lugar de ocurrencia----
#acomodo nombres
base <- base %>% 
  mutate(LUGAR_OCURRENCIA=case_when(
    LUGAR_OCURRENCIA=="Establecimiento de salud"~"Establecimiento de salud",
    LUGAR_OCURRENCIA=="Institución residencial"~"Institución residencial",
    LUGAR_OCURRENCIA=="Lugar de ocurrencia: Vivienda"~"Vivienda",
    LUGAR_OCURRENCIA=="Lugar de ocurrencia: Vía pública"~"Vía pública",
    LUGAR_OCURRENCIA=="Lugar de trabajo"~"Lugar de trabajo",
    LUGAR_OCURRENCIA=="Otro lugar de ocurrencia del evento"~"Otro lugar",
    LUGAR_OCURRENCIA=="Sin datos"~"Sin datos",
    TRUE ~ LUGAR_OCURRENCIA
  ))


#tabla 
TABLA_OCURRENCIA <- base %>% 
  count(LUGAR_OCURRENCIA) %>%
  mutate(
    porc = n / sum(n, na.rm = TRUE)) %>%
  arrange(desc(n))


#objeto lugar ocurrencia maximo
ocurrencia_maximo <- max(tolower(TABLA_OCURRENCIA$LUGAR_OCURRENCIA))

#tipo cuidado requerido----


#tabla 
CUIDADO<- base%>% count(INTERNADO)
CUIDADO <- CUIDADO %>%
  mutate(porc= round(n / sum(n) * 100))

por_internacion_maximo <- max(CUIDADO$porc)
n_internacion <- max(CUIDADO$n)

#tabla con internado SI
base_internados <- base %>%
  filter(INTERNADO == "SI")

#tabla tratamiento
tratamiento <- base_internados %>%
  count(TRATAMIENTO_2) %>%
  rename(Tratamiento = TRATAMIENTO_2, N = n)
tratamiento$Tratamiento[is.na(tratamiento$Tratamiento)] <- "Sin datos"


# Preparar los datos en el formato adecuado para la serie de datos
data_tratamiento <- tratamiento %>%
  mutate(y = round(N / sum(N) * 100, 0),  
    name = Tratamiento) %>%
  select(name, y) %>%
  list_parse()






#comorbilidades----
#agrupamiento y renombre
base <- base %>% 
  mutate(COMORBILIDAD=case_when(
    COMORBILIDAD=="Sin información"~"Sin información",
    COMORBILIDAD=="Sin comorbilidades"~"Sin comorbilidades",
    COMORBILIDAD=="Antecedente de consumo problemático de alcohol"~"Consumo prob. de sustancia psicoactiva",
    COMORBILIDAD=="Antecedente de consumo problemático de cocaína"~"Consumo prob. de sustancia psicoactiva",
    COMORBILIDAD=="Antecedente de consumo problemático de marihuana"~"Consumo prob. de sustancia psicoactiva",
    COMORBILIDAD=="Otro antecedente de consumo problemático previo"~"Consumo prob. de sustancia psicoactiva",
    COMORBILIDAD=="Antecedente de consumo problemático de drogas de diseño"~"Consumo prob. de sustancia psicoactiva",
    COMORBILIDAD=="Diagnóstico de salud mental"~"Diagnóstico de salud mental",
    COMORBILIDAD=="Enfermedad crónica"~"Enf. crónica",
    COMORBILIDAD=="Con antecedentes de intentos previos de suicidio"~"IS previos",
    COMORBILIDAD=="Antec. consumo problemático de psicofármacos no recetados"~"Consumo prob. de psicofármacos no recetados",
    COMORBILIDAD=="Condición discapacitante"~"Condición discapacitante",
    COMORBILIDAD=="Enfermedad grave o terminal de referente vincular"~"Enf.grave/terminal de referente vincular",
    COMORBILIDAD=="Otra situación clínica relevante"~"Otra situación clínica relevante",
    COMORBILIDAD=="Situaciones clínicas relevantes: enfermedad crónica"~"Enf. crónica",
    TRUE ~ COMORBILIDAD)
  )


#tabla comorbilidades
comorbilidades<- base%>% count(COMORBILIDAD) %>% 
 mutate(porc= round(n / sum(n) * 100)) %>% 
 arrange(desc(n))

#objeto porcentaje de sin info 
porc_max_sdcomorb <- max(comorbilidades$porc, na.rm = TRUE)

#objeto n de sin info
n_max_sdcomorb <- max(comorbilidades$n, na.rm = TRUE)

#objeto % psicoactivas

porcentaje_consumo <- comorbilidades %>%
  filter(COMORBILIDAD == "Consumo problemático de sustancia psicoactiva") %>%
  pull(porc)
  
 
#objeto n psicoactivas
n_consumo <- comorbilidades %>%
  filter(COMORBILIDAD == "Consumo problemático de sustancia psicoactiva") %>%
  pull(n)



#notificacion por regiones----
regiones <- read_csv2("regiones.csv")
regiones <- as.data.frame(regiones)

#tabla
tabla_establecimientos <- base %>%
  group_by(ESTABLECIMIENTO_CARGA) %>%
  summarise(N = n()) %>%
  ungroup() 


#n tabla
sum(tabla_establecimientos$N) #hasta acá esta ok el N

#join
tabla_regiones <- tabla_establecimientos %>% 

  left_join(regiones, by = "ESTABLECIMIENTO_CARGA")

#n tabla regiones

sum(tabla_regiones$N) #hasta acá esta ok el N

#agrupo regiones

tabla_regiones <- tabla_regiones %>%
  group_by(REGION) %>%  
  summarise(
    N = sum(N, na.rm = TRUE),   
    porc = N / sum(tabla_regiones$N)   
  ) %>%
  ungroup() %>%
  arrange(desc(N))   






