########### COMIENZO CON APUNTES BORRADOR A FEBRERO 2024#########################
# qué hacer?

# modelo de ausencias: 
# tomar paises: CON debates y CON encuestas. 
# VD: proba de asistir a debate
# UdeA: candidatos. Cantu carrera tienen nombres?
#  VI: 
# --a nivel individual: 
#        incumbencia. 
#        puntaje promedio en encuestas total. lineal, exponencial, log?
#        ideo. 
#        populista o outsider?
# --para considerar si hacer multinivel: 
#      alguna medida de puntaje relativo (ej diferencia con el siguiente, ej competitividad), 
#      alguna medida de volatilidad (indiv / gral: ej indecisos)
#      alguna medida de tradicion (indiv cantidad de debates debatidos / gral )
# --efectos fijos país o elección?
# --alguna cualidad del debate? cantidad de invitados, si el organizador es un medio / el estado / otro?

# modelo a nivel pais
# tomar paises: todos de los que se cuente con datos de encuestas. eventualmente datos PE
# VD: proba de que haya al menos un debate
# VDs alternativas para robustez teo / metod:
#   - proba de que haya un debate "importante" con primer candidato / alguno de los primeros dos
#   - proba de que haya un debate organizado por X (medios / estado)?
#   - cantidad de debates (log? con 0 incluido, recordar que hay problemas para su transformacion)
#   - otras?
# VIs: 
# - baselines: penetracuiion de TV, 
# - alguna medida de rutinizacion informal: 
#      dummy lagged elecc pasada  
#      dummy alguna o varias elecciones anteriores c debate
#      cdad de debates total anteriores (log?)
#      prop de elecciones previas con y/o sin debates
# - alguna medida de competitividad entre partidos:
#      dif primero / segundo 
# - fragmentacion:
#       NEP (pero para candidatos)
#       cantidad de candidatos (log?) (quizas esta es más efectiva para mi H)
# - polarizacion: 
#       dummy hubo outsider
#       alguna medida de polarizacion ideo
# - incertidumbre: 
#       volatilidad en eleccion (propias encuestas)
#       volatilidad medida convencionalmente
#       cdad de indecisos (medidos en encuestas)


######### CARGA DE LIBRERIAS ################

library(tidyverse)


################# CARGA DE DATA, por referencia a data guardada en distintos directorios ##############

# data propia ############


# datos
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")
base <- readxl::read_xlsx("./datav2023/base_final3v2023.xlsx")
# elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
# base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
# base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
# base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
# base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
# base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")

}
# data cantu carreras #########

# ellos usan una "masterdatabase.csv" para todos sus analisis, 
# que se desprende de los pasos hechos más abajo + algunos más en "Code_1.1"
# usan data de paper previo + tienen data de encuestas por pais que sacaron de wikipedia, que usan para complementar de autores anteriores

# LINKS PARA DATA CANTU CARRERAS. ABAJO ESTA LA CARGA
# /home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/masterdatabase.csv
# /home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Working data/scrapping de wikipedia/all_countries/final_colombia_2014.csv # ejemplo # todos tienen la misma forma  #final_pais_año.csv"
# ademas tienen un dataset "candidates.xslx" con los nombres de los candidatos y sus partidos

##### LINKS PARA DATA JENNINGS AND WLEZIEN EN CANTU CARRERAS ####

# Importing Jennings and Wlezien (2018) dataset
# en esta data NO hay nombres de candidatos pero si un candidate id
Dataset.20180111 <- read.delim("~/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Jennings and Wlezien (2018)/Dataset-20180111.tab")

# Selecting cases by country and only considering those observations within the last 100 days before the election
paises <- c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Mexico", "Paraguay", "Peru", "Venezuela")
Lat_data <- Dataset.20180111 %>%
  filter(country %in% paises & daysbeforeED<100)

# We add variables that exist in our dataset (this allows us to do the merging)
Lat_data$source<-"Jennings and Wlezien (2018)"
Lat_data$c_names<-NA
Lat_data$reliable<-1
Lat_data$url<-NA

# We eliminate interpolations # importante
Lat_data <- Lat_data[is.na(Lat_data$poll_)==FALSE,]

rm(Dataset.20180111 ) # limpiamos escritorio

# IMPORT DE SU PROPIA DATA, LUEGO HACEN MERGE. ####
# SU PROPIA DATA SI INCLUYE NOMBRES DE CANDIDATOS
# es un compilado de todos los paises indiv
# notese que para el dataset anterior, c_names es NA 

Lat_update <- read.csv("~/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Working data/polls_corrected.csv",  fileEncoding="latin1")

# Keep the reliable data
Lat_update <- Lat_update[Lat_update$reliable==1,]
Lat_update$npolls<-1

# Fixing the date format
Lat_update$polldate<-as.Date(Lat_update$polldate, "%m/%d/%y")
Lat_update$elecdate<-as.Date(Lat_update$elecdate, "%m/%d/%y")

# Estimating the days before the election
Lat_update$daysbeforeED <- Lat_update$elecdate-Lat_update$polldate
Lat_update <- Lat_update[Lat_update$daysbeforeED>0,]

# Eliminating missing values
Lat_update <- Lat_update[is.na(Lat_update$poll_)==FALSE,]

# Modify the date format before merging
Lat_update$polldate<-as.character(Lat_update$polldate)
Lat_update$elecdate<-as.character(Lat_update$elecdate)
Lat_data$polldate<-as.character(Lat_data$polldate)
Lat_data$elecdate<-as.character(Lat_data$elecdate)

# Merging the databases
data<-rbind(Lat_data, Lat_update)

# Modify the date format after merging
data$polldate<-as.Date(data$polldate)
data$elecdate<-as.Date(data$elecdate)

# Renaming candidateid for partyyid
data <- rename(data, candidateid=partyid)

# Recoding electionid
data$electionid<-paste(data$country,data$elecdate)

# Omit missing values
data$poll_<-as.numeric(data$poll_)
data <- data[is.na(data$poll_)==FALSE,]

# Estimating the days before the election
data <- data %>% mutate(daysbeforeED = elecdate-polldate)

# Consider the last 100 days before the election
data <- data[data$daysbeforeED<=100,]

