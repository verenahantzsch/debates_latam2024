# Este script crea base de datos cuya U de A son las encuestas, a partir de datos prestados por cantu carreras + propios actualizados en "new_polls"
# originalmente trabajamos en creacion_base_elecciones_cantu y versiones, pero decidimos separar esfuerzos para mayor prolijidad.
# marzo 2024
# CUANTO MAS ALTO EL NUMERO, POSTERIOR LA VERSION DEL CODIGO, EL MAS ALTO VIGENTE

library(tidyverse)

########################## CREO BASE DE ENCUESTAS UNIFICADA #############################

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
data <- dplyr::rename(data, candidateid=partyid)

# Recoding electionid
data$electionid<-paste(data$country,data$elecdate)

# Omit missing values
data$poll_<-as.numeric(data$poll_)
data <- data[is.na(data$poll_)==FALSE,]

# Estimating the days before the election
data <- data %>% mutate(daysbeforeED = elecdate-polldate)

# Consider the last 100 days before the election
data <- data[data$daysbeforeED<=120,]

rm(Lat_data, Lat_update)
reserva <- data 
#data <- reserva
# algunas limpiezas necesarias

data <- data %>% 
  subset(electionid!="Argentina 2011-08-14") %>%  # eliminamos elecciones primarias argentina
  mutate(round = ifelse(country=="Argentina"&round(electionyr)==2011,1,round)) %>% 
  mutate(turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==1, 80.61, turnout),
         turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==2, 78.90, turnout)) %>% # habia problemas con decimales en el reporte de este numero para distintos items de esta eleccion
  subset(!(country=="Brazil"&round(electionyr)==2010&turnout==81.90)) ## eliminamos base mas incompleta dupilcada de brasil para esta eleccion

reserva <- data 
# cargo data con nombres de candidatos ##############
# esta base fue creada en limpieza_unique_candidates y luego completada manualmente

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
missing_names <- readxl::read_xlsx("unique_candidates_tomerge_incompletos_completado.xlsx")

# Uno


data_con_nombres <- data %>% 
  dplyr::rename("nombres_candidatos" = c_names) %>% # hago este paso ahora para no pisar data justamente
  left_join(missing_names) 

data_con_nombres <- data_con_nombres %>% 
  mutate(nombres_candidatos = ifelse(is.na(nombres_candidatos),
                                     c_names,
                                     nombres_candidatos)) %>% 
  mutate(nombres_candidatos = str_trim(nombres_candidatos, "both"),
         nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT"))  %>% 
  mutate(country = ifelse(country=="Brazil", "Brasil", country))  %>% # para hacer luego compatible con base carolina
  mutate(electionyr = ifelse(country=="Chile"&round(electionyr)==2009, 2010, electionyr)) %>%  # para hacer luego compatible con base carolina
  mutate(vote_ = ifelse(electionid=="Brazil 2014-10-26"&nombres_candidatos=="Dilma Rousseff",51.64,vote_)) # correccion de incompatibilidad menor entre la data, decimales

rm(missing_names)

# chequeo. deberiamos tener al menos 32 elecciones (con debates) + otras sin debates

u_elect <- data$electionid %>% unique() # por ahora tenemos 63 elecciones, generales y ballotage

# ahora agrego update de elecciones y nuevas encuestas ##################
data_update_march2024 <- read.csv('/home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/Working data/scrapping de wikipedia/new_countries/new_polls.csv') %>% select(-X)


# algunos ajustes para hacer data compatible

# ajuste para poner todo en español, ya lo hicimos arriba para la base de cantu
data_update_march2024 <- data_update_march2024 %>% 
  mutate(country = ifelse(country=="Brazil", "Brasil", country))

differing_colnames <- setdiff(names(data_con_nombres), names(data_update_march2024))

data_update_march2024 <- data_update_march2024 %>% 
  subset(!is.na(poll_)) %>% 
 # select(-X) %>% 
  mutate(inc_ = "NA", 
         enpp = "NA",
         reliable = "NA",
         pollcycle = "NA",
         npolls = "NA",
         comentarios_nombres_candidatos = "NA",
         reliable = "NA", 
         ipoll_ = "NA" ,
         candidateid = "NA") %>% 
  mutate(electionid = paste0(country, " ", elecdate)) %>% 
  dplyr::rename("url" = source_url) 

u_elect <- data_update_march2024$electionid %>% unique() # sumamos 35 elecciones
u_elect <- data_con_nombres$electionid %>% unique() # por ahora tenemos 63 elecciones, generales y ballotage

# reordenamos columnas
data_update_march2024 <- data_update_march2024[, match(names(data_con_nombres), names(data_update_march2024))]

# unimos
full_dataset <- data_con_nombres %>% 
  rbind(data_update_march2024)

### ultimas cuestiones de prolijidad

full_dataset <- full_dataset %>% 
  subset(!(electionid=="Colombia 2014-06-15"&!is.na(reliable))) # COLOMBIA 2014 esta dos veces, qitamos data de cantu. Una pista de a que se refiere con reliable

full_dataset <- full_dataset %>% 
  mutate(nombres_candidatos = str_trim(nombres_candidatos, "both"),
         nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT")) 

full_dataset <- full_dataset %>% 
  mutate(poll_ = str_trim(poll_, "both")) %>% 
  mutate(poll_ = str_replace(poll_, "\u00A0", "")) %>% 
  mutate(poll_ = as.numeric(poll_)) %>% 
  subset(!is.na(poll_) | poll_ == "NA")  

# agregamos id de encuesta
full_dataset <- full_dataset %>% 
  dplyr::mutate(id_polldatapoint = row_number()) 

u_elect <- full_dataset$electionid %>% unique() # tenemos 97 elecciones cuando deberiamos tener 998

# guardamos esta data  #############

full_dataset %>% 
  #write.csv("/home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv")
  
  
  