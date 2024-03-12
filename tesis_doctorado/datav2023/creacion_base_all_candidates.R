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
base_candidatos_cantu <- data %>% 
  dplyr::rename("nombres_candidatos" = c_names) %>% # hago este paso ahora para no pisar data justamente
  left_join(missing_names)

base_candidatos_cantu <- base_candidatos_cantu %>% 
  mutate(nombres_candidatos = ifelse(is.na(nombres_candidatos),
                                     c_names,
                                     nombres_candidatos)) %>% 
  mutate(id_polldatapoint = row_number())

# borro data que no voy a usar
rm(Lat_data, Lat_update, data, missing_names)


# Pruebo unir con base propia 

# creo variable de período por el cual filtrar datos

base_candidatos_propios <- base_candidatos_propios %>% 
  mutate(debate_date = as.Date(t_fecha),
         two_previous_weeks = debate_date - 14)

# 1era estrategia: intentamos matchear todas las encuestas disponibles en la semana previa, 
# primero limpiamos un poco los datos

unique_candidates_cantu <- base_candidatos_cantu$nombres_candidatos %>%  unique()
min_year_cantu <- base_candidatos_cantu$electionyr %>% round %>% min()
max_year_cantu <- base_candidatos_cantu$electionyr %>% round %>% max()

base_candidatos_propios_filtered <- base_candidatos_propios %>% 
  subset(nombres_candidatos%in%unique_candidates_cantu) %>% 
  subset(ncat_eleccion >= min_year_cantu & ncat_eleccion <= max_year_cantu)

# mediante un loop, busco coincidencias en nombre de candidato y periodo
# por ahora, nos interesan las encuestas obtenidas entre la fecha del debate y las dos semanas previas a su realizacion

# en este vector vamos a colocar las id matcheables. 

base_candidatos_propios_filtered$matching_polls <- ""

for (i in 1:nrow(base_candidatos_propios_filtered)){
  for (j in 1:nrow(base_candidatos_cantu)){
  #  print( paste0( "iteracion entre ", i, " y ", j ))
    
    if (base_candidatos_propios_filtered$nombres_candidatos[i]==base_candidatos_cantu$nombres_candidatos[j] &
        base_candidatos_propios_filtered$ncat_ronda[i]==base_candidatos_cantu$round[j] &
        base_candidatos_propios_filtered$debate_date[i] > base_candidatos_cantu$polldate[j] &
        base_candidatos_propios_filtered$two_previous_weeks[i] < base_candidatos_cantu$polldate[j]) {
      
      base_candidatos_propios_filtered$matching_polls[i] <- paste0(base_candidatos_propios_filtered$matching_polls[i],";", base_candidatos_cantu$id_polldatapoint[j])
    } else {
     # print( paste0( "no hay match entre ", i, " y ", j ))
    }
  }
}

# filtro si alguno quedo sin data

base_candidatos_matcheados <- base_candidatos_propios_filtered %>% 
  subset(matching_polls!="")

# CHEQUEOS de la cantidad de matches respecto de las bases originales
# con este abordaje tenemos 450 matches

paises <- base_candidatos_cantu$country %>%  unique()
yearelecciones <- base_candidatos_cantu$yr %>% round() %>% unique()
all_elections <- base_candidatos_cantu$electionid %>% unique()
paises <- base_candidatos_matcheados$cat_pais %>%  unique()
yearelecciones <- base_candidatos_matcheados$ncat_eleccion %>% round() %>% unique()
base_candidatos_matcheados <- base_candidatos_matcheados %>% 
  mutate(electid = paste(cat_pais, ncat_eleccion))
all_elections <- base_candidatos_matcheados$electid %>% unique()

# parece que el match se hizo correctamente, dado que el paper original tenia 32 elecciones

# ahora hay que buscar la manera de efectivamente hacer el match
# nos interesa la variable "poll_" Raw poll data (poll-of-polls)
# mediante otro loop extraemos estos datos

# vector para colocar la data de los matches

base_candidatos_matcheados$matching_data <- ""

# paso necesario para encontrar matches exactos 

base_candidatos_matcheados$matching_polls <- paste(base_candidatos_matcheados$matching_polls, ";", sep = "")

for (i in 1:nrow(base_candidatos_matcheados)){
  for (j in 1:nrow(base_candidatos_cantu)){
    if(str_detect(base_candidatos_matcheados$matching_polls[i], paste0(";",as.character(base_candidatos_cantu$id_polldatapoint[j]),";", sep=""))){
      base_candidatos_matcheados$matching_data[i]<-paste0(base_candidatos_matcheados$matching_data[i],";", base_candidatos_cantu$poll_[j])
    } 
  }
}

# ahora trato de extraer data relevante de esta lista de polls
#reserva <- base_candidatos_matcheados # para mientras pienso codigo
base_candidatos_matcheados <- reserva

# manualmente observamos que el maximo de puntos de dato por candidato son 16 encuestas
base_candidatos_matcheados <- base_candidatos_matcheados %>% 
  separate(matching_data, sep = ";" , into = paste0("intencion_voto", 1:16)) %>% 
  select(-intencion_voto1) %>% 
  # quiero contar la cantidad de puntos de datos
  mutate(count_encuestas = rowSums(!is.na(select(., paste0("intencion_voto", 2:16))))) %>% 
 # ahora quiero sumar y sacar promedios de los valores de los puntos de datos disponibles
  mutate(across(starts_with("intencion_voto"), as.numeric)) %>% 
  mutate(sum_encuestas = rowSums(select(., paste0("intencion_voto", 2:16)), na.rm=TRUE))  %>% 
  mutate(mean_encuestas = rowMeans(select(., paste0("intencion_voto", 2:16)), na.rm=TRUE)) 

# GUARDO ESTE DATASET 
#base_candidatos_matcheados %>% write_csv("base_candidatos_matcheados2023.csv")
#test <- read.csv("base_candidatos_matcheados2023.csv")
# obvio que despues puedo querer otros datos de la base original, como sea el margen de error u otros
# parece que tengo data para 150 debates, 450 candidatos
 
  