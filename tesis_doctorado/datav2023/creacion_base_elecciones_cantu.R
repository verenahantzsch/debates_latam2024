# Este script crea base de datos cuya U de A son las elecciones, a partir de datos prestados por cantu carreras
# marzo 2024

library(tidyverse)

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

rm(Lat_data, Lat_update)

# algunas limpiezas necesarias

data <- data %>% 
  subset(electionid!="Argentina 2011-08-14") %>%  # eliminamos elecciones primarias argentina
  mutate(round = ifelse(country=="Argentina"&round(electionyr)==2011,1,round)) %>% 
  mutate(turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==1, 80.61, turnout),
         turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==2, 78.90, turnout)) %>% # habia problemas con decimales en el reporte de este numero para distintos items de esta eleccion
  subset(!(country=="Brazil"&round(electionyr)==2010&turnout==81.90)) %>%  # eliminamos base mas incompleta dupilcada de brasil para esta eleccion  
  mutate(id_polldatapoint = row_number()) 

reserva <- data 
# cargo data con nombres de candidatos
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
  mutate(electionyr = ifelse(country=="Chile"&round(electionyr)==2009, 2010, electionyr))  # para hacer luego compatible con base carolina
  

rm(missing_names)

# chequeo. deberiamos tener al menos 32 elecciones (con debates) + otras sin debates

u_elect <- data$electionid %>% unique() # por ahora tenemos 63 elecciones, generales y ballotage


# PRIMERA MITAD DE LA BASE: LADO CANTU ###########

data_candidatos <- data_con_nombres %>% 
  # primero agrupamos por todas las variables que correspondan al nivel "candidatos", perdiendo el nivel de "polls"
  # tengo ademas que seleccionar todas las variables que quiero retener del nivel electoral 
  group_by(electionid, electionyr, country, round, enpp, turnout, espv, regime, nombres_candidatos, gov_, inc_) %>% 
  summarise(mean_encuestas_candidato = mean(poll_),
         sd_encuestas_candidato = sd(poll_),
         vote_ = mean(vote_)) %>% # esta es porque habia unas inconsistencias menores en data de brasil (0.01 diferencia entre filas)
  ungroup() 
  
u_elect <- data_candidatos$electionid %>% unique() # por ahora tenemos 63 elecciones, generales y ballotage

# ahora agrupamos por todas las variables que corresponden al nivel "elecciones"
# tenemos que usar algunos indicadores del nivel "candidatos" para construir indicadores de nivel superior, de "elecciones"
data_competencia <- data_candidatos %>% 
  select(electionid, electionyr, country, round, enpp, turnout, espv, regime, mean_encuestas_candidato, vote_, gov_) %>%
  unique() %>% 
  group_by(electionid, country, round) %>% 
  mutate(rank_polls = rank(100-mean_encuestas_candidato),
         rank_elections = rank(100-vote_)) 

data_candidatos <- data_candidatos %>% 
  left_join(data_competencia) %>% 
  mutate(frontrunner = ifelse(rank_polls==1,1,0),
         challenger = ifelse(rank_polls==2,1,0),
         winner = ifelse(rank_elections==1,1,0),
         incumbent = ifelse(gov_==1,1,0))

data_incumbents <- data_candidatos %>% 
  subset(incumbent==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("incumbente" = nombres_candidatos) %>% 
  unique()

data_frontrunner <- data_candidatos %>% 
  subset(frontrunner==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("frontrunner" = nombres_candidatos)

data_winner <- data_candidatos %>% 
  subset(winner==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("winner" = nombres_candidatos)

data_challenger <- data_candidatos %>% 
  subset(challenger==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("challenger" = nombres_candidatos)
  
data_elecciones <- data_candidatos %>% 
  group_by(electionid, electionyr, country, round, enpp, turnout, espv, regime) %>% 
  summarise(sd_mean_encuestas_candidato = sd(mean_encuestas_candidato),
            sd_vote_ = sd(vote_)) %>% 
 left_join(data_incumbents) %>% 
  left_join(data_frontrunner) %>% 
  left_join(data_winner) %>% 
  left_join(data_challenger) %>% 
  ungroup()


# SEGUNDA MITAD DE LA BASE: LADO CAROLINA #######################
# Nos interesa indicador de: elecciones con debate Y elecciones con debate con winner presente
# eventualmente, elecciones con dos ppales, (facil, crear variable challenger como arriba)
# eventualmente, elecciones con debates org por medios. 
# (no se si vale la pena. a HOY 13 MARZO 2024 este proceso esta en pausa, falta seguir segunda_limpieza_datos completando por los tipos de organizadores, no deberia ser mucho trabajo)


base <- readxl::read_xlsx("base_final1v2023.xlsx") # ATENTI base con datos por debate. REEMPLAZAR POR ULTIMA VERSION LIMPIA QUE CORRESPONDA
data_elecciones_carolina <-  readxl::read_xlsx("base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país # IDEM

# check de compatibilidad de años / nombres de pais SE PUEDE SALTEAR ######

# data_elecciones_cantu <- data_elecciones %>% 
#   select(electionyr, country, round) %>% 
#   unique() %>% 
#   mutate(electionyr = round(electionyr)) %>% 
#   group_by(electionyr, country) %>% 
#   summarise(cat_ballotage = n()) %>% 
#   ungroup() %>% 
#   dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country) %>% 
#   mutate( cat_pais = ifelse(cat_pais=="Brazil", "Brasil", cat_pais))
# 
# data_elecciones_carolina2 <- data_elecciones_carolina %>% 
#   select(-n_candidaturas)
# 
# # Check for matching rows
# matching_rows <- data_elecciones_cantu %>%
#   semi_join(data_elecciones_carolina2)
# 
# # Check for non-matching rows
# nonmatching_rows_df1 <- data_elecciones_cantu %>%
#   anti_join(data_elecciones_carolina2)
# 
# # Check for non-matching rows
# nonmatching_rows_df2 <- data_elecciones_carolina2 %>%
#   anti_join(data_elecciones_cantu)

# REVISO MANUALMENTE
# Cantu tiene 3 entradas para Brasil 2010. Me viene generando problemas en varios lugares. ESTA DUPLICDO
# OJO parece que está contando primarias como ronda? eso explicaria diferencias tb para Argentina 2011
# Chile problema de nomenclatura. Carolina pone 2010, Cantu 2009 , ballotage y primera vuelta respectivamente
# Chile 2013: Cantu no tiene datos para alguna de las vueltas , pero esta bien
# Idem para Colombia 2014
# Idem para Guatemala 2015
# Idem para Ecuador 2017
# Carolina tiene error en cat_ballotage Colombia 2018. Deberia decir "2", dice "1"
# PASAMOS A CORREGIR MANUALMENTE LOS QUE SE PUEDEN y CHEQUEAR si no hay problemas en los "missing"
# ELIMINAMOS UNA DE Brazil 2010-10-03 # DUPLICADO CON MISSING, OJO ELIMINAR TB DE BASE DE CANDIDATOS! previo a los joins. 
# ELIMINAMOS Argentina 2011-08-14 PRIMARIAS y MODIFICAMOS ANOTACIONES
# MODIFICAR CHILE 2009 por CHILE 2010 en base elecciones cantu
# MODIFICAR COLOMBIA 2018 en base elecciones carolina
# algunos de estos pasos fueron hechos manualmente sobre base original,
# otros mas arriba en este script
# chequeos sobre data de arriba, cambios automaticos agregados arriba
# data_missing_chile <- data  %>%   
#   subset((country=="Chile"&round(electionyr)==2013)) # efectivamente falta segunda vuelta, 15 de diciembre de 2013
# data_missing_Colombia <- data  %>%   
#   subset((country=="Colombia"&round(electionyr)==2014)) # efectivamente falta primera vuelta, 	Domingo 25 de mayo de 2014 hay encuestas en: https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2014
# data_missing_Guatemala <- data  %>%   
#   subset((country=="Guatemala"&round(electionyr)==2015)) # efectivamente falta segunda vuelta, Domingo 25 de octubre de 2015 (2.ª vuelta) https://es.wikipedia.org/wiki/Elecciones_generales_de_Guatemala_de_2015
# data_missing_Ecuador <- data  %>%   
#   subset((country=="Ecuador"&round(electionyr)==2017)) # efectivamente falta segunda vuelta, Domingo 2 de abril de 2017 (Segunda vuelta) https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Ecuador_de_2017
#data_duplicated_Brasil <- data  %>%   
#     subset((country=="Brazil"&round(electionyr)==2014))  


# AHORA SI HACEMOS JOIN ########

# primero creamos base anual relevante
# tengo que separar ronda 1 de 2.  

# creamos loop para agregar rondas faltantes cuando corresponda
for (i in 1:nrow(data_elecciones_carolina)) {
  new_rows <- data_elecciones_carolina[i, ]
  new_rows$cat_ballotage <- 1
  if (data_elecciones_carolina$cat_ballotage[i]>1){
    data_elecciones_carolina <- rbind(data_elecciones_carolina, new_rows)
  }
}

# uno data sobre incumbentes etc a base de debates para poder identificar debates con frontrunner presente

# primero tenemos que ajustar estas bases para el join

data_incumbents <- data_incumbents %>% 
  mutate(ncat_eleccion = round(electionyr)) %>% 
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

data_frontrunner <- data_frontrunner %>% 
  mutate(ncat_eleccion = round(electionyr)) %>% 
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

data_winner <- data_winner %>% 
  mutate(ncat_eleccion = round(electionyr)) %>% 
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

data_challenger <- data_challenger %>% 
  mutate(ncat_eleccion = round(electionyr)) %>% 
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

# hacemos el join
base2 <- base %>% 
  mutate(str_presentes = str_trim(str_presentes, "both"),
         str_presentes = iconv(str_presentes, to = "ASCII//TRANSLIT")) %>% 
  left_join(data_incumbents) %>% 
  left_join(data_frontrunner) %>% 
  left_join(data_winner) %>% 
  left_join(data_challenger) %>% 
  mutate(frontrunner_presente = ifelse(str_detect(str_presentes, frontrunner),1,0),
         incumbente_presente = ifelse(str_detect(str_presentes, incumbente),1,0),
         winner_presente = ifelse(str_detect(str_presentes, winner),1,0),
         challenger_presente = ifelse(str_detect(str_presentes, challenger),1,0),
         frontycha_presentes = ifelse(str_detect(str_presentes, frontrunner)&str_detect(str_presentes, challenger),1,0))

# identifico elecciones con debates
base_elecciones_con_debates <- base2 %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(n_debates_eleccion = n(),
            n_debates_frontrunner_presente = sum(frontrunner_presente, na.rm=T),
            n_debates_frontycha_presentes = sum(frontycha_presentes, na.rm=T)) %>% 
  mutate(dico_debates_eleccion = ifelse(n_debates_eleccion>=1,1,0),
         dico_frontrunner_presente = ifelse(n_debates_frontrunner_presente>=1,1,0),
         dico_frontycha_presentes = ifelse(n_debates_frontycha_presentes>=1,1,0))

data_elecciones_carolina2 <- data_elecciones_carolina %>% 
  dplyr::rename("ncat_ronda" = cat_ballotage) %>% 
  left_join(base_elecciones_con_debates ) %>% 
  mutate(n_debates_eleccion = ifelse(is.na(n_debates_eleccion), 0, n_debates_eleccion),
         n_debates_frontrunner_presente = ifelse(is.na(n_debates_frontrunner_presente), 0, n_debates_frontrunner_presente),
         dico_debates_eleccion = ifelse(is.na(dico_debates_eleccion), 0, dico_debates_eleccion),
         dico_frontrunner_presente = ifelse(is.na(dico_frontrunner_presente), 0, dico_frontrunner_presente),
         dico_frontycha_presentes = ifelse(is.na(dico_frontycha_presentes), 0, dico_frontycha_presentes))

data_agregada <- data_elecciones %>% 
  mutate(electionyr = round(electionyr)) %>% 
  dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country, "ncat_ronda" = round) %>% 
  left_join(data_elecciones_carolina2)

u_elec <- data_elecciones$electionid %>% unique()


# PENDIENTE  !!!!! ######
# problemas con esta base: otra vez tengo desbalance en 1/ 0 
# PENDIENTE: CREAR VARIABLE C DICO DEBATE C/ LEAD Y C CHALLENGER. VA A HABER QUE HACER VARIOS JOINS CRUZADOS
# TB variable de debates en años previos
# Tb pensar indicadores adecuados para competitividad
# PENDIENTES 13 MARZO 2024 ####
# todavía hay algunas inconsistencias a resolver luego:
# data brasil 2014 o alguna otra parece estar otra vez replicada.
# varios missing en incumbencia. tiene sentido, dado que tenemos 399 incumbentes y 64 elecciones. Esta variable me deja dudando
# aunque conociendo inestabilidad latam, tb puede ser
# ademas pendiente pensar indicadores estado de competencia. Revisar variable enpp  Effective number of political parties at election
# abajo dejo apuntes Celeste

# sugerencias de indices de celeste Ratto

# En cuanto a las dimensiones del voto o del sistema de partidos: 
#   • Indices de fragmentación electoral y parlamentaria de Rae. 
# 
# • Indices del número efectivo de partidos, electorales y parlamentarios: 
#   ◦ Indices (electoral y parlamentario) sugeridos por Laakso y Taagepera y Taagepera y Shugart. 
# ◦ Indices de hiperfraccionamiento (electoral y parlamentario) de Kesselman y Wildgen. 
# ◦ Indices (electoral y parlamentario) de Molinar. 
# 
# • Indices de concentración electoral y parlamentaria. 
# 
# • Indices de competitividad electoral y parlamentaria. 
# 
# • Indice de polarización de Sartori. 
# • Indices de polarización ponderada (electoral y parlamentaria). 
# 
# • Indices de volatilidad (electoral y parlamentaria) total, entre bloques e intrabloques, propuestos por Pedersen y Bartolini y Mair (caben distintas aplicaciones, según la dimensión que resulte relevante: izquierda-derecha, centro- periferia, etc.). 
# 
# • Indice de voto dual, de Arian y Weiss. 
# 
# • Indice de voto regionalista. 
# • Indice de voto regionalista diferenciado. 
# • Indice de voto regional diferenciado, de Lee. 



# CHECK DE DUPLICADOS ################## 
check_agrupamiento <- data_agregada %>%
  # mutate(electionyr = ifelse(country=="Chile"&round(electionyr)==2009, 2010, electionyr)) %>% # para hacer luego compatible con base carolina
  # subset(electionid!="Argentina 2011-08-14") %>%  # eliminamos elecciones primarias argentina
  # mutate(round = ifelse(country=="Argentina"&round(electionyr)==2011,1,round)) %>%
  # mutate(turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==1, 80.61, turnout),
  #        turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==2, 78.90, turnout)) %>% # habia problemas con decimales en el reporte de este numero para distintos items de esta eleccion
  # subset(!(country=="Brazil"&round(electionyr)==2010&turnout==81.90)) %>%  # eliminamos base mas incompleta dupilcada de brasil para esta eleccion
  # mutate(id_polldatapoint = row_number())  %>%
  #select(electionid, electionyr, country, round, enpp, turnout, espv, regime) %>%
  select(electionid, ncat_eleccion, cat_pais, ncat_ronda, enpp, turnout, espv, regime) %>%
  unique()
u_elect <- check_agrupamiento$electionid %>% unique()

#Find duplicated rows in column A
duplicated_rows <- data_incumbents[duplicated(data_incumbents$electionid) | duplicated(data_incumbents$electionid, fromLast = TRUE), ]

# Print the duplicated rows
print(duplicated_rows)
# Function to identify columns where two rows differ
identify_differing_columns <- function(df) {
  # Initialize an empty vector to store column indices
  differing_columns <- c()
  
  # Loop through each column of the dataframe
  for (col in 1:ncol(df)) {
    # Check if the values in the column are different between the two rows
    if (!identical(df[1, col], df[2, col])) {
      differing_columns <- c(differing_columns, col)
    }
  }
  
  return(differing_columns)
}

# Identify columns where two rows differ
differing_columns <- identify_differing_columns(duplicated_rows)

# Print the column indices
print("Columns where two rows differ:")
print(differing_columns)

# Print the column names
print("Column names where two rows differ:")
print(colnames(duplicated_rows)[differing_columns])