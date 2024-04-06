# Este script crea base de datos cuya U de A son las elecciones, a partir de datos prestados por cantu carreras + propios de encuestas actualizados + propios de debates 
# marzo 2024
# CUANTO MAS ALTO EL NUMERO, POSTERIOR LA VERSION DEL CODIGO, EL MAS ALTO VIGENTE

library(tidyverse)


########################## CREO BASE DE ELECCIONES UNO BASE ENCUESTAS CON BASE DEBATES  
########################## PUNTO DE PARTIDA ES BASE CON DATOS DE ENCUESTAS  #########

# DATA DE PRIMERA PARTE  
# Originalmente esta data se elaboraba en el script creacion_base_elecciones_cantu,
# pero para mayor prolijidad se trabaja ahora en creacion_base_encuestas
# parto de cargar base ya completa, con una fila por encuesta/candidato/eleccion
# ajustar PATH segun corresponda
path <- "/home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv"
#path <- "/home/carolina/Documents/R PROJECTS/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv"
data_encuestas <- read.csv(path)


#### AGREGO NIVEL DE BASE ENCUESTAS:  BASE DE DATOS 1 CANDIDATO POR FILA ##########################

data_candidatos <- data_encuestas %>% 
  # primero agrupamos por todas las variables que correspondan al nivel "candidatos", perdiendo el nivel de "polls"
  # tengo ademas que seleccionar todas las variables que quiero retener del nivel electoral 
  group_by(electionid, electionyr, country, round, enpp, turnout, espv, regime, nombres_candidatos, gov_) %>% 
  summarise(mean_encuestas_candidato = mean(poll_),
         sd_encuestas_candidato = sd(poll_),
         vote_ = mean(vote_)) %>% # esta es porque habia unas inconsistencias menores en data de brasil (0.01 diferencia entre filas)
  ungroup() 

#### CAMINO A CREAR UNA BASE DE DATOS 1 ELECCION POR FILA 
# tenemos que usar algunos indicadores del nivel "candidatos" para construir indicadores de nivel superior, de "elecciones"

competencia <- data_candidatos %>% 
  select(electionid, electionyr,country, round, enpp, turnout, espv, regime, mean_encuestas_candidato, vote_, gov_) %>%
  unique() %>% 
  group_by(electionid, country, round) %>% 
  mutate(rank_polls = rank(100-mean_encuestas_candidato),
         rank_elections = rank(100-vote_)) 

data_candidatos <- data_candidatos %>% 
  left_join(competencia) %>% 
  mutate(frontrunner = ifelse(rank_polls==1,1,0),
         challenger = ifelse(rank_polls==2,1,0),
         winner = ifelse(rank_elections==1,1,0),
         incumbent = ifelse(gov_==1,1,0))

incumbents <- data_candidatos %>% 
  subset(incumbent==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("incumbente" = nombres_candidatos) %>% 
  unique()

frontrunner <- data_candidatos %>% 
  subset(frontrunner==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("frontrunner" = nombres_candidatos)

winner <- data_candidatos %>% 
  subset(winner==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("winner" = nombres_candidatos)

challenger <- data_candidatos %>% 
  subset(challenger==1) %>% 
  select(electionid, electionyr, country, round, nombres_candidatos) %>% 
  dplyr::rename("challenger" = nombres_candidatos)


# AHORA SI CREO UNA BASE CON UNA ELECCION POR FILA #################  
# AQUI FALTA PENDIENTE CONSTRUIR ALGUNOS DE LOS INDICADORES DE COMPETENCIA  ##
# APUNTES
# concentracion: el % de voto que suman los dos primeros partidos
# competitividad: la proximidad de los resultados entre los dos ppales partidos
# NO tengo data para: volatilidad, fragmentacion, polarizacion en los sentidos tradicionales
# pos de calcular volatilidad dentro de la campaña? tendria que homogenizar la cantidad de mediciones para calcular la variacion en el voto experimentada durante la campaña. 

primeros_dos_partidos <- data_candidatos %>% 
  subset(frontrunner==1|challenger==1) %>% 
  group_by(electionid, electionyr, country, round) %>% 
  summarise(concentracion = sum(mean_encuestas_candidato),
            competitividad = abs(diff(mean_encuestas_candidato)))

data_elecciones <- data_candidatos %>% 
  group_by(electionid, electionyr, country, round, enpp, turnout, espv, regime) %>% 
  summarise(sd_mean_encuestas_candidato = sd(mean_encuestas_candidato),
            sd_vote_ = sd(vote_)) %>% 
  left_join(primeros_dos_partidos) %>% 
 left_join(incumbents) %>% 
  left_join(frontrunner) %>% 
  left_join(winner) %>% 
  left_join(challenger) %>% 
  ungroup() 


#rm(list=setdiff(ls(), c( "data_candidatos", "data_encuestas", "data_elecciones"))) # voy a usar de vuelta bases parciales sobre data_debates

# SEGUNDA MITAD DE LA BASE: LADO DEBATES CAROLINA #######################

# Nos interesa indicador de: elecciones con debate Y elecciones con debate con winner presente
# eventualmente, elecciones con dos ppales, (facil, crear variable challenger como arriba)
# eventualmente, elecciones con debates org por medios. 
# (no se si vale la pena. a HOY 13 MARZO 2024 este proceso esta en pausa, falta seguir segunda_limpieza_datos completando por los tipos de organizadores, no deberia ser mucho trabajo)

# CARGO DATA #
# ATENTI REEMPLAZAR CON BASE FINAL LIMPIA #####################
#path <- "./tesis_doctorado/datav2023/base_final1v2023.xlsx"
path <- "base_final1v2023.xlsx"
data_debates_carolina <- readxl::read_xlsx(path) # ATENTI base con datos por debate. REEMPLAZAR POR ULTIMA VERSION LIMPIA QUE CORRESPONDA
#path <- "./tesis_doctorado/datav2023/base_eleccionesv2023.xlsx"
path <- "base_eleccionesv2023.xlsx"
data_elecciones_carolina <-  readxl::read_xlsx(path) # base auxiliar: años que hubo elecciones por país # IDEM

# CHECK MANUA DE COMPATIBILIDAD, PUEDE SALTEARSE ################### 
# check de compatibilidad de años / nombres de pais SE PUEDE SALTEAR 

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
# 
# OTRO CHECK DESPUES DE AMPLIAR BASE DE DATOS CON SCRAPPING DE WIKIPEDIA
# VAMOS A TENER ALGUN PROBLEMA CON LOS AÑOS , ya que en una me base con variable electionyr, con la otra con variable yr
# corrijo ahora arriba si puedo

# data_elecciones_cantu <- data_elecciones %>%
#      select(electionyr, country, round) %>%
#      unique() %>%
#      mutate(electionyr = round(electionyr)) %>%
#      group_by(electionyr, country) %>%
#      summarise(cat_ballotage = n()) %>%
#      ungroup() %>%
#      dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country) %>%
#      mutate( cat_pais = ifelse(cat_pais=="Brazil", "Brasil", cat_pais))
# 
# data_elecciones_carolina2 <- data_elecciones_carolina %>%
#      select(-n_candidaturas)
# 
# # Check for matching rows
# matching_rows <- data_elecciones_cantu %>%
#     semi_join(data_elecciones_carolina2)
# 
# # Check for non-matching rows
# nonmatching_rows_df1 <- data_elecciones_cantu %>%
#      anti_join(data_elecciones_carolina2)
# 
# # Check for non-matching rows
# nonmatching_rows_df2 <- data_elecciones_carolina2 %>%
#   anti_join(data_elecciones_cantu)
# 
# rm(data_elecciones_cantu,data_elecciones_carolina2,matching_rows,nonmatching_rows_df1,nonmatching_rows_df2)

# EL "PROBLEMA" EN TODOS LOS CASOS SALVO GUATEMALA 1991/1990 EN EL QUE EL AÑO DIFIERE, OJOTA; HAY QUE RERUN EL COD USANDO YR EN LUGAR DE ELECYR PENDIENTE, ANOTADO ARRIBA ######
# SON MISSING BALLOTAGES 

# AHORA SI HACEMOS JOIN ########

# primero creamos base anual relevante
# tengo que separar ronda 1 de 2.  

# creamos loop para agregar *rondas* faltantes cuando corresponda
for (i in 1:nrow(data_elecciones_carolina)) {
  new_rows <- data_elecciones_carolina[i, ]
  new_rows$cat_ballotage <- 1
  if (data_elecciones_carolina$cat_ballotage[i]>1){
    data_elecciones_carolina <- rbind(data_elecciones_carolina, new_rows)
  }
}

# uno data sobre incumbentes etc a base de debates para poder identificar debates con frontrunner presente

# primero tenemos que ajustar estas bases para el join

incumbents <- incumbents %>% 
  mutate(ncat_eleccion = round(electionyr)) %>%  
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

frontrunner <- frontrunner %>% 
  mutate(ncat_eleccion = round(electionyr)) %>%  
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

winner <- winner %>% 
  mutate(ncat_eleccion = round(electionyr)) %>%   
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

challenger <- challenger %>% 
  mutate(ncat_eleccion = round(electionyr)) %>%  
  dplyr::rename("cat_pais" = country, "ncat_ronda" = round) %>% 
  select(-electionid, -electionyr)

# hacemos el join  
data_debates_carolina2 <- data_debates_carolina %>% 
  mutate(str_presentes = str_trim(str_presentes, "both"),
         str_presentes = iconv(str_presentes, to = "ASCII//TRANSLIT")) %>% 
  left_join(incumbents) %>% 
  left_join(frontrunner) %>% 
  left_join(winner) %>% 
  left_join(challenger) %>% 
  mutate(frontrunner_presente = ifelse(str_detect(str_presentes, frontrunner),1,0),
         incumbente_presente = ifelse(str_detect(str_presentes, incumbente),1,0),
         winner_presente = ifelse(str_detect(str_presentes, winner),1,0),
         challenger_presente = ifelse(str_detect(str_presentes, challenger),1,0),
         frontycha_presentes = ifelse(str_detect(str_presentes, frontrunner)&str_detect(str_presentes, challenger),1,0),
         frontORcha_presentes = ifelse(str_detect(str_presentes, frontrunner)|str_detect(str_presentes, challenger),1,0))
 
# identifico elecciones con debates
base_elecciones_con_debates <- data_debates_carolina2 %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(n_debates_eleccion = n(),
            n_debates_frontrunner_presente = sum(frontrunner_presente, na.rm=T),
            n_debates_frontycha_presentes = sum(frontycha_presentes, na.rm=T),
            n_debates_frontORcha_presentes = sum(frontORcha_presentes, na.rm=T)) %>% 
  mutate(dico_debates_eleccion = ifelse(n_debates_eleccion>=1,1,0),
         dico_frontrunner_presente = ifelse(n_debates_frontrunner_presente>=1,1,0),
         dico_frontycha_presentes = ifelse(n_debates_frontycha_presentes>=1,1,0),
         dico_frontORcha_presentes = ifelse(n_debates_frontORcha_presentes>=1,1,0)) %>% 
  ungroup()  
 

# uno base PROPIA de todas las elecciones , con datos sobre debates por eleccion
data_elecciones_carolina2 <- data_elecciones_carolina %>% 
  dplyr::rename("ncat_ronda" = cat_ballotage) %>% 
  left_join(base_elecciones_con_debates ) %>% 
  mutate(n_debates_eleccion = ifelse(is.na(n_debates_eleccion), 0, n_debates_eleccion),
         n_debates_frontrunner_presente = ifelse(is.na(n_debates_frontrunner_presente), 0, n_debates_frontrunner_presente),
         n_debates_frontycha_presentes = ifelse(is.na(n_debates_frontycha_presentes), 0, n_debates_frontycha_presentes),
         n_debates_frontORcha_presentes = ifelse(is.na(n_debates_frontORcha_presentes), 0, n_debates_frontORcha_presentes),
         dico_debates_eleccion = ifelse(is.na(dico_debates_eleccion), 0, dico_debates_eleccion),
         dico_frontrunner_presente = ifelse(is.na(dico_frontrunner_presente), 0, dico_frontrunner_presente),
         dico_frontycha_presentes = ifelse(is.na(dico_frontycha_presentes), 0, dico_frontycha_presentes),
         dico_frontORcha_presentes = ifelse(is.na(dico_frontORcha_presentes), 0, dico_frontORcha_presentes))   

# creo variable lagged sobre debates 
data_elecciones_lagged <- data_elecciones_carolina2 %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_debates_eleccion = sum(n_debates_eleccion),
            n_debates_frontrunner_presente = sum(n_debates_frontrunner_presente),
            n_debates_frontycha_presentes = sum(n_debates_frontycha_presentes),
            n_debates_frontORcha_presentes = sum(n_debates_frontORcha_presentes),
            dico_debates_eleccion =  max(dico_debates_eleccion),
            dico_frontrunner_presente =  max(dico_frontrunner_presente),
            dico_frontycha_presentes =  max(dico_frontycha_presentes),
            dico_frontORcha_presentes =  max(dico_frontORcha_presentes) ) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(lagged_n_debates_eleccion = lag(n_debates_eleccion),
         lagged_n_debates_frontrunner_presente = lag(n_debates_frontrunner_presente),
         lagged_n_debates_frontycha_presentes = lag(n_debates_frontycha_presentes),
         lagged_n_debates_frontORcha_presentes = lag(n_debates_frontORcha_presentes),
         lagged_dico_debates_eleccion = lag(dico_debates_eleccion),
         lagged_dico_frontrunner_presente =  lag(dico_frontrunner_presente),
         lagged_dico_frontycha_presentes = lag(dico_frontycha_presentes),
         lagged_dico_frontORcha_presentes = lag(dico_frontORcha_presentes)) %>% 
  ungroup() %>% 
  select(c(cat_pais, ncat_eleccion, starts_with("lagged_")))


# uno est data lagged a la base

data_elecciones_carolina3 <- data_elecciones_carolina2 %>% 
  left_join(data_elecciones_lagged)

# FINALMENTE UNO DATA ENCUESTAS CON DATA DEBATES
data_agregada <- data_elecciones %>% 
  mutate(electionyr = round(electionyr)) %>% 
  dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country, "ncat_ronda" = round) %>% 
  left_join(data_elecciones_carolina3)


# GUARDAMOS FINALMENTE ###################

#path <- "./tesis_doctorado/datav2023/all_elections.csv"
path <- "all_elections.csv"
data_agregada %>%
  #write.csv(path)




# CHEQUEOS RANDOM, NO HACE FALTA CORRER, ESPACIO BORRADOR  !!!!! ######

table(data_agregada$dico_debates_eleccion)
result <- t.test( competitividad ~ dico_debates_eleccion, data = data_agregada) # las elecciones son mas estrechamente competidas cuando hay debates
result <- t.test( concentracion ~ dico_debates_eleccion, data = data_agregada) # las elecciones estan ligeramente mas concentradas cuando hay debates que cuando no hay debates # esto deberia interacturse con RONDA supongo
table(data_agregada$dico_frontrunner_presente) # KII ESTO MEJORO UN MONTON 
# table(data_agregada$dico_debates_eleccion)
# 
# elecs_sin_frontrunner <- data_agregada  %>% 
#   subset(dico_frontrunner_presente==0&dico_debates_eleccion==1)

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