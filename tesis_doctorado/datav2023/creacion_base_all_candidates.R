# Este script crea una base con todos los candidatos, su desempeño en encuestas y su participación o no en debates
# se ejecuta luego de asegurarse de que los nombres de los candidatos son compatibles, 
# tarea que está contenida en "limpieza_unique_candidates.R"


# LIBRERIAS #####

library(tidyverse)

# Importación de datos #####

# propios

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_candidatos_propios <- readxl::read_xlsx("all_candidates.xlsx") # esta base fue creada en segunda_limpieza_datos

# Cantu Carreras 2024. Esto requiere más trabajo. 

# PRIMERA PARTE: EXTRACCION DE CANDIDATOS DE BASE CANTU CARRERAS #################

# Importo y formateo data original de los autores.###########

# Importing Jennings and Wlezien (2018) dataset
# en esta data NO hay nombres de candidatos pero si un candidate id
Dataset.20180111 <- read.delim("~/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Jennings and Wlezien (2018)/Dataset-20180111.tab")

# Selecting cases by country and only considering those observations within the last 100 days before the election
paises <- c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Mexico", "Paraguay", "Peru", "Venezuela")
Lat_data <- Dataset.20180111 %>%
  filter(country %in% paises & daysbeforeED<120)

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
data <- data[data$daysbeforeED<=120,]

# A partir de aqui script propio. 
# Agregamos nombres buscados manualmente. 

# cargo data
# esta base fue creada en limpieza_unique_candidates y luego completada manualmente

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
missing_names <-  readxl::read_xlsx("unique_candidates_tomerge_incompletos_completado.xlsx")

# Uno
data_joined <- data %>% 
  dplyr::rename("nombres_candidatos" = c_names) %>% # hago este paso ahora para no pisar data justamente
  left_join(missing_names)

data_joined <- data_joined %>% 
  mutate(nombres_candidatos = ifelse(is.na(nombres_candidatos),
                                     c_names,
                                     nombres_candidatos))

# Pruebo unir con base propia # HAY QUE VER COMO HACER CON FECHAS!!!!!!!!! #####

base_candidatos_cantu <- data_joined %>% 
  dplyr::rename("cat_pais" = country, "ncat_eleccion" = electionyr)

SELECT
SUBSET

base_all_candidates <- base_candidatos_cantu %>% 
  left_join(base_candidatos_propios)
