# Este script crea una base con todos los candidatos, su desempeño en encuestas y su participación o no en debates
# se ejecuta luego de asegurarse de que los nombres de los candidatos son compatibles, 
# tarea que está contenida en "limpieza_unique_candidates.R"
# CUANTO MAS ALTO EL NUMERO AL FINAL DEL NOMBRE DEL SCRIPT, MAS ACTUAL LA VERSION 


# LIBRERIAS #####

library(tidyverse)

# Importación de datos #####

#### DATOS SOBRE CANDIDATOS Y SU PARTICIPACION EN DEBATES 
#### la creación de la base a continuación se hace en "limpieza_unique_candidates.R" con base en la data sobre debates actualizada

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_candidatos_debates <- readxl::read_xlsx("all_candidates.xlsx") # esta base fue creada en segunda_limpieza_datos
 
# DATOS DE ENCUESTAS, 
# actualizada con: original + cantu + propia. La creacion de esta base se hace en "creacion_base_encuestas.R"

base_encuestas <- read.csv("/home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv")

# extraigo data de candidatos y encuestas 

# Pruebo unir con base propia 
# creo variable de período por el cual filtrar datos

base_candidatos_debates <- base_candidatos_debates %>% 
  mutate(debate_date = as.Date(t_fecha),
         two_previous_weeks = debate_date - 21)  # pos ampliar periodo

# 1era estrategia: intentamos matchear todas las encuestas disponibles en la semana previa, 
# primero limpiamos un poco los datos

unique_candidates_encuestas <- base_encuestas$nombres_candidatos %>%  unique()
min_year_encuestas <- base_encuestas$electionyr %>% round %>% min()
max_year_encuestas <- base_encuestas$electionyr %>% round %>% max()

base_candidatos_debates_filtered <- base_candidatos_debates %>% 
  subset(nombres_candidatos%in%unique_candidates_encuestas) %>% 
  subset(ncat_eleccion >= min_year_encuestas & ncat_eleccion <= max_year_encuestas)

unique_candidates_debates <- base_candidatos_debates$nombres_candidatos %>%  unique()

base_encuestas_filtered <- base_encuestas %>% 
  subset(nombres_candidatos%in%unique_candidates_debates) 

base_encuestas_filtered <- base_encuestas_filtered %>% 
  subset(!is.na(polldate)) #  EVENTUALMENTE PENDIENTE CORREGIR; ES UNA SOLA ENCUESTA, CHILE 89 CERC17 A LA QUE SE LE PARSEO MAL LA FECHA

check1 <- base_candidatos_debates_filtered$nombres_candidatos %>%  unique()
check2 <- base_encuestas_filtered$nombres_candidatos %>%  unique()


# mediante un loop, busco coincidencias en nombre de candidato y periodo
# por ahora, nos interesan las encuestas obtenidas entre la fecha del debate y las dos semanas previas a su realizacion

# en este vector vamos a colocar las id matcheables. 
#### ACA ME QUEDE ######
base_candidatos_debates_filtered$matching_polls <- ""

# checks
# i <- 145
# j <- 4519

# loop
for (i in 1:nrow(base_candidatos_debates_filtered)){
  for (j in 1:nrow(base_encuestas_filtered)){
    print( paste0( "iteracion entre ", i, " y ", j ))
    
    if (base_candidatos_debates_filtered$nombres_candidatos[i]==base_encuestas_filtered$nombres_candidatos[j] &
        base_candidatos_debates_filtered$ncat_ronda[i]==base_encuestas_filtered$round[j] &
        base_candidatos_debates_filtered$debate_date[i] > base_encuestas_filtered$polldate[j] & # debate posterior a encuestas
        base_candidatos_debates_filtered$two_previous_weeks[i] < base_encuestas_filtered$polldate[j]) {  # encuestas posteriores a x tiempo anterior a debate
 
      print( paste0( "eureka entre ", i, " y ", j ))
      base_candidatos_debates_filtered$matching_polls[i] <- paste0(base_candidatos_debates_filtered$matching_polls[i],";", base_encuestas_filtered$id_polldatapoint[j])
    } else {
      print( paste0( "no hay match entre ", i, " y ", j ))
    }
  }
}

# filtro si alguno quedo sin data

base_candidatos_matcheados <- base_candidatos_debates_filtered %>% 
  subset(matching_polls!="")

# CHEQUEOS de la cantidad de matches respecto de las bases originales
# con este abordaje tenemos 450 matches

paises <- base_encuestas$country %>%  unique()
yearelecciones <- base_encuestas$yr %>% round() %>% unique()
all_elections <- base_encuestas$electionid %>% unique()
paises <- base_candidatos_matcheados$cat_pais %>%  unique()
yearelecciones <- base_candidatos_matcheados$ncat_eleccion %>% round() %>% unique()
base_candidatos_matcheados <- base_candidatos_matcheados %>% 
  mutate(electid = paste(cat_pais, ncat_eleccion))
all_elections <- base_candidatos_matcheados$electid %>% unique()

# parece que el match se hizo correctamente, dado que el paper original tenia 32 elecciones + agregamos varias

# ahora hay que buscar la manera de efectivamente hacer el match
# nos interesa la variable "poll_" Raw poll data (poll-of-polls)
# mediante otro loop extraemos estos datos

# vector para colocar la data de los matches

base_candidatos_matcheados$matching_data <- ""

# paso necesario para encontrar matches exactos 

base_candidatos_matcheados$matching_polls <- paste(base_candidatos_matcheados$matching_polls, ";", sep = "")

for (i in 1:nrow(base_candidatos_matcheados)){
  for (j in 1:nrow(base_encuestas_filtered)){
    if(str_detect(base_candidatos_matcheados$matching_polls[i], paste0(";",as.character(base_encuestas_filtered$id_polldatapoint[j]),";", sep=""))){
      base_candidatos_matcheados$matching_data[i]<-paste0(base_candidatos_matcheados$matching_data[i],";", base_encuestas_filtered$poll_[j])
    } 
  }
}

# ahora trato de extraer data relevante de esta lista de polls
#reserva <- base_candidatos_matcheados # para mientras pienso codigo
#base_candidatos_matcheados <- reserva

# manualmente observamos que el maximo de puntos de dato por candidato son 16 encuestas
base_candidatos_matcheados <- base_candidatos_matcheados %>% 
  mutate(cuenta_matches = str_count(matching_data, ";"))

n_cols <- base_candidatos_matcheados$cuenta_matches %>% max() + 1

base_candidatos_matcheados <- base_candidatos_matcheados %>% 
  separate(matching_data, sep = ";" , into = paste0("intencion_voto", 1:n_cols)) %>% 
  select(-intencion_voto1) %>% 
  # quiero contar la cantidad de puntos de datos
  mutate(count_encuestas = rowSums(!is.na(select(., paste0("intencion_voto", 2:n_cols))))) %>% 
 # ahora quiero sumar y sacar promedios de los valores de los puntos de datos disponibles
  mutate(across(starts_with("intencion_voto"), as.numeric)) %>% 
  mutate(sum_encuestas = rowSums(select(., paste0("intencion_voto", 2:n_cols)), na.rm=TRUE))  %>% 
  mutate(mean_encuestas = rowMeans(select(., paste0("intencion_voto", 2:n_cols)), na.rm=TRUE)) 

# GUARDO ESTE DATASET 
#base_candidatos_matcheados %>% write_csv("base_candidatos_matcheados2023.csv")
#test <- read.csv("base_candidatos_matcheados2023.csv")
# obvio que despues puedo querer otros datos de la base original, como sea el margen de error u otros
# parece que tengo data para 150 debates, 450 candidatos
 
# Agregamos variables pendientes de interés por candidato al dataset guardado
# pendiente matchear variable gov_ Party in government/governing coalition y vote_, vote share
rm(base_candidatos_debates)
# abro data
base_candidatos_matcheados <- read.csv("base_candidatos_matcheados2023.csv")

# extraigo data de interes por candidato / eleccion
base_candidatos_encuestas_agrupada <- base_encuestas %>% 
  mutate(vote_ = vote_ %>% round(1)) %>% 
  select(electionid, electionyr, country, round, c_names, gov_, vote_) %>% 
  unique()

base_candidatos_encuestas_agrupada <- base_candidatos_encuestas_agrupada  %>% 
  mutate(electionyr = electionyr %>% round()) %>% 
  dplyr::rename("cat_pais" = country, "ncat_eleccion" = electionyr, "ncat_ronda" = round, "nombres_candidatos" = c_names) %>% 
  mutate(cat_pais = ifelse(cat_pais=="Brazil", "Brasil", cat_pais))

base_candidatos_matcheados_expandida <- base_candidatos_matcheados %>% 
  left_join(base_candidatos_encuestas_agrupada)

# Guardo nuevamente
#base_candidatos_matcheados_expandida %>% write_csv("base_candidatos_matcheados2023.csv")
#test <- read.csv("base_candidatos_matcheados2023.csv")
