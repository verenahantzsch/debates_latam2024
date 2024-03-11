# Little script breve creado temporalmente para homogenizar nombres de candidatos de las bases de datos
# parte de los archivos con los que aqui se trabaja fueron trabajados parcialmente en otros scripts.
# "exploracion cuanti febrero 2024"
# "segunda_limpieza_datos"
# Los datos aquí limpios son utilizados luego en "creacion_base_all_candidates.R" 

library(tidyverse)
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023/")


# PRIMERA PARTE: EXTRACCION DE CANDIDATOS DE BASE CANTU CARRERAS #################

# Importo y formateo data original de los autores.###########

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

# IMPORT DE SU PROPIA DATA, LUEGO HACEN MERGE. 
# SU PROPIA DATA SI INCLUYE NOMBRES DE CANDIDATOS
# es un compilado de todos los paises indiv
# notese que para el dataset anterior, c_names es NA 
# en este archivo algunos nombres fueron corregidos respecto del original,
# para poder matchearlos a los nombres en la base de datos propia 
Lat_update <- read.csv("~/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Working data/polls_corrected.csv")#,  fileEncoding="latin1")

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

# Creo df con nombres unicos de candidatos para unificar formatos ###########

unique_candidates <- data %>% 
  select(electionyr, country, candidateid, c_names) %>% 
  arrange(country, electionyr) %>% 
  unique()

# creo dos bases para completear y chequear datos. SON ARCHIVOS TEMPORARIOS creados a marzo 2024

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023/")

unique_candidates_complete <- unique_candidates %>% 
  subset(!is.na(c_names)) %>% 
  arrange(country, electionyr, c_names) %>% 
  mutate(source = "CantuCarreras")

unique_candidates_complete %>% writexl::write_xlsx("unique_candidates_tomerge_completos_chequear.xlsx")

unique_candidates_incomplete <- unique_candidates %>% 
  subset(is.na(c_names)) 

unique_candidates_incomplete %>% writexl::write_xlsx("unique_candidates_tomerge_incompletos_chequear.xlsx")

rm(list = ls())

# SEGUNDA PARTE: MERGES CON DATA PROPIA ###############################

# base propia 
datos_completos_propios <- readxl::read_xlsx("base_uniquecandidates_paracompletar.xlsx")

datos_completos_propios <- datos_completos_propios %>% 
  mutate(source = "Carolina")

# base cantu con datos
datos_completos_cantu <- readxl::read_xlsx("unique_candidates_tomerge_completos_chequear.xlsx")

datos_completos_cantu <- datos_completos_cantu %>% 
  dplyr::rename("cat_pais" = country, "ncat_eleccion" = electionyr, "nombres_candidatos" = c_names)

# limpiamos nombres base cantú

datos_completos_cantu <- datos_completos_cantu %>% 
  mutate(nombres_candidatos = str_trim(nombres_candidatos, "both"),
         nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT"))


# retengo solo elecciones en común y uno

min_election <- min(datos_completos_cantu$ncat_eleccion)
max_election <- max(datos_completos_cantu$ncat_eleccion)
countries_cantu <- datos_completos_cantu$cat_pais %>% unique()

data_merged_cantu_carolina <- datos_completos_propios %>% 
  mutate(cat_pais = str_replace(cat_pais, "Brasil", "Brazil")) %>% 
  subset(ncat_eleccion>=min_election&ncat_eleccion<=max_election) %>% 
  subset(cat_pais%in%countries_cantu) %>%
  mutate(candidateid = "NA") %>% 
  rbind(datos_completos_cantu)
 
# ordeno y guardo para comparar manualmente
#data_merged_cantu_carolina %>% 
 # arrange(cat_pais, ncat_eleccion, nombres_candidatos, source) %>% 
  #writexl::write_xlsx("unique_candidates_cantucarolina_paracompletar.xlsx")
# Creeria que esta data quedó homogenizada tras cambios en ambas bases, marzo 2024
# quizás puede efectuarse un ultimo check o a la hora de merge toda la data

# base cantu completada manualmente 

# esta base, que guardamos más arriba, la completamos manualmente 
# utilizando el Codebook de Jennings and Wlezien (2018) 
# y ajustando escritura de nombres a formas implementadas en nuestra base de datos propia,
# cuando corresponda
# data completa está en " unique_candidates_tomerge_incompletos_completado.xlsx"

