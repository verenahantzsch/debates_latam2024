# Este script crea base de datos cuya U de A son las elecciones, a partir de datos prestados por cantu carreras + propios de encuestas actualizados + propios de debates 
# marzo 2024
# CUANTO MAS ALTO EL NUMERO, POSTERIOR LA VERSION DEL CODIGO, EL MAS ALTO VIGENTE

library(tidyverse)
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

########################## CREO BASE DE ELECCIONES UNO BASE ENCUESTAS CON BASE DEBATES  
########################## PUNTO DE PARTIDA ES BASE CON DATOS DE ENCUESTAS  #########

# DATA DE PRIMERA PARTE  
# Originalmente esta data se elaboraba en el script creacion_base_elecciones_cantu,
# pero para mayor prolijidad se trabaja ahora en creacion_base_encuestas
# parto de cargar base ya completa, con una fila por encuesta/candidato/eleccion
# ajustar PATH segun corresponda
path <- "/home/carolina/Documents/dataexterna/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv"
#path <- "/home/carolina/Documents/R PROJECTS/Carrera_Cantu_datos_debates_completo/polls_completo_marzo2024_cantuYcarolina.csv"
data_encuestas <- read.csv(path) %>% select(-X)


# PASO PARA FILTRAR CALIDAD DE ENCUESTAS, agregado a mayo 2024
data_encuestas <- data_encuestas %>% 
  subset(reliable==1|reference_validity==T)

u_elect <- data_encuestas$electionid %>% unique() # sigo teniendo 95 elecciones  


#### AGREGO NIVEL DE BASE ENCUESTAS:  BASE DE DATOS 1 CANDIDATO POR FILA ##########################

data_candidatos <- data_encuestas %>% 
  # primero agrupamos por todas las variables que correspondan al nivel "candidatos", perdiendo el nivel de "polls"
  # tengo ademas que seleccionar todas las variables que quiero retener del nivel electoral 
  group_by(electionid, electionyr, country, round, #enpp, espv, regime, 
           turnout, nombres_candidatos, gov_) %>% 
  dplyr::summarize(mean_encuestas_candidato = mean(poll_),
         sd_encuestas_candidato = sd(poll_),
         vote_ = mean(vote_)) %>% # esta es porque habia unas inconsistencias menores en data de brasil (0.01 diferencia entre filas)
  ungroup() 

# a mayo 2024 agregue data sobre votos en blanco e indecisos, cuando habia.
# tengo que pasarlas a formato columna para sacarmelas de encima en las cuentas que siguen

data_blancos_indecisos <- data_candidatos %>% 
  subset(nombres_candidatos=="Blanco"|nombres_candidatos=="Indeciso")

# paso esta data a wider para poder agregarla mas adelante a data sobre elecciones
data_blancos_indecisos <- data_blancos_indecisos %>% 
  pivot_wider(names_from = "nombres_candidatos",
              names_prefix = "expected_",
              values_from = "mean_encuestas_candidato")

# ahora necesito quedarme con una sola fila por eleccion
data_blancos_indecisos <- data_blancos_indecisos %>% 
  group_by(electionid, electionyr, country, round, #enpp, espv, regime, 
           turnout) %>%
  dplyr::summarise(expected_blanco = mean(expected_Blanco, na.rm=T),
                   expected_indeciso = mean(expected_Indeciso, na.rm=T)) %>% 
  ungroup()
  
data_candidatos <- data_candidatos %>% 
  subset(!gov_==999)

#### CAMINO A CREAR UNA BASE DE DATOS 1 ELECCION POR FILA 
# tenemos que usar algunos indicadores del nivel "candidatos" para construir indicadores de nivel superior, de "elecciones"

competencia <- data_candidatos %>% 
  # select(electionid, electionyr, country, round, turnout, mean_encuestas_candidato, vote_, gov_, nombres_candidatos) %>% # deje fuera por ahora  espv, enpp,regime,
  # unique() %>% 
  dplyr::group_by(electionid, electionyr, country, round, turnout  ) %>% 
  dplyr::mutate(rank_polls = rank(100 - mean_encuestas_candidato),
         rank_elections = rank(100 - vote_)) 

# en Guatemala 2019 primera ronda aparece un empate en el 2do puesto dadas las encuestas
# pero si tomamos una muestra de encuestas mas amplia sabemos que Giammatei estaba mejor posicionado que su competidor ,
# además asi lo avalan los resultados posteriores,
# corregimos esto manualmente
competencia <- competencia %>% 
   mutate(rank_polls = ifelse(country=="Guatemala"&electionyr=="2019"&round==1&nombres_candidatos=="Alejandro Giammattei", 2, rank_polls))

# ahora cargamos la data de competencia a la data sobre candidatos 

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
  dplyr::summarise(concentracion = sum(mean_encuestas_candidato),
            competitividad = abs(diff(mean_encuestas_candidato)))

data_elecciones <- data_candidatos %>% 
  group_by(electionid, electionyr, country, round, turnout,) %>%  #  enpp, espv, regime
  dplyr::summarise(sd_mean_encuestas_candidato = sd(mean_encuestas_candidato),
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
#path <- "./tesis_doctorado/datav2023/base_final1v2023.xlsx"
path <- "base_final3v2023.csv"
data_debates_carolina <- read_csv(path) # ATENTI base con datos por debate. REEMPLAZAR POR ULTIMA VERSION LIMPIA QUE CORRESPONDA, ESTOY REEMPLAZANDO A ABRIL 2024
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

# parentesis: chequeo numeros #
# 
# # 253 años electorales cubiertos en total
# u_electoinid <- data_candidatos$electionid %>% unique() # 95 elecciones con datos de encuestas
# u_electoinid <- data_elecciones$electionid %>% unique() # 95 elecciones con datos de encuestas
# data_debates_carolina <- data_debates_carolina %>% mutate(elecid = paste(ncat_eleccion, cat_pais, ncat_ronda)) 
# u_elec <- data_debates_carolina$elecid %>% unique() # 140 elecciones con debates 
# 
# # chequeamos ahora de cuantos años CON debates TENEMOS datos de encuestas
# u_match <- data_debates_carolina %>% 
#   select(ncat_eleccion, cat_pais, ncat_ronda) %>% 
#   left_join(data_elecciones %>% 
#               dplyr::rename("cat_pais"="country", "ncat_ronda"="round", "ncat_eleccion"="electionyr") %>% 
#               mutate(ncat_eleccion = round(ncat_eleccion)))
# u_elec_ambas <- u_match$electionid %>% unique() # TENGO 71 ELECCIONES CON DEBATES Y DATOS 
# # POR ENDE, 24 ELECCIONES CON DATOS Y SIN DEBATES

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
 
# identifico elecciones con debates.
# hacemos pequeña distincion 
# para poder diferenciar entre ausencia de debates y "na", 
# ya que el ultimo ("na") solo aplica en el caso en el que los debates se identifican con base en datos de encuestas
base_elecciones_con_debates_dico <- data_debates_carolina2 %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  dplyr::summarise(n_debates_eleccion = n()) %>% 
  mutate(dico_debates_eleccion = ifelse(n_debates_eleccion>=1,1,0)) %>% 
  ungroup() 

base_elecciones_con_debates_otros <- data_debates_carolina2 %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  dplyr::summarise(n_debates_frontrunner_presente = sum(frontrunner_presente),
            n_debates_frontycha_presentes = sum(frontycha_presentes ),
            n_debates_frontORcha_presentes = sum(frontORcha_presentes )) %>% 
  mutate(dico_frontrunner_presente = ifelse(n_debates_frontrunner_presente>=1,1,0),
         dico_frontycha_presentes = ifelse(n_debates_frontycha_presentes>=1,1,0),
         dico_frontORcha_presentes = ifelse(n_debates_frontORcha_presentes>=1,1,0)) %>% 
  ungroup()  
 

# uno base PROPIA de todas las elecciones , con datos sobre debates por eleccion
data_elecciones_carolina2 <- data_elecciones_carolina %>% 
  dplyr::rename("ncat_ronda" = cat_ballotage) %>% 
  left_join(base_elecciones_con_debates_dico ) %>% 
  left_join(base_elecciones_con_debates_otros ) %>% 
  mutate(n_debates_eleccion = ifelse(is.na(n_debates_eleccion), 0, n_debates_eleccion),
         n_debates_frontrunner_presente = ifelse(n_debates_eleccion==0, 0, n_debates_frontrunner_presente), #ESTA BIEN COMPLETAR ASI, PORQUE SI NO HUBO NINGUN DEBATE POR DEFINICION NO HUBO DEBATES CON FRONTRUNNER PRESENTE
         n_debates_frontycha_presentes = ifelse(n_debates_eleccion==0, 0, n_debates_frontycha_presentes),
         n_debates_frontORcha_presentes = ifelse(n_debates_eleccion==0, 0, n_debates_frontORcha_presentes),
         dico_debates_eleccion = ifelse(is.na(dico_debates_eleccion), 0, dico_debates_eleccion),
         dico_frontrunner_presente = ifelse(dico_debates_eleccion==0, 0, dico_frontrunner_presente),
         dico_frontycha_presentes = ifelse(dico_debates_eleccion==0, 0, dico_frontycha_presentes),
         dico_frontORcha_presentes = ifelse(dico_debates_eleccion==0, 0, dico_frontORcha_presentes))   

# chequeo de control 
# check <- data_elecciones_carolina2 %>% 
#      subset(!is.na(n_debates_frontrunner_presente))
# check <- check %>% 
#   subset(n_debates_frontrunner_presente!=0)  # 196 observaciones con datos, 62 positivos
# check <-base_elecciones_con_debates_otros %>% 
#   subset(n_debates_frontrunner_presente!=0) # coincide ! , 71 con datos, 62 positivos. 

######## creo variable lagged sobre debates   ##

data_elecciones_lagged <- data_elecciones_carolina2 %>% 
  group_by(cat_pais, ncat_eleccion) %>% # estas variables lagged las creo para la eleccion tomada como un todo.  
  dplyr::summarise(n_debates_eleccion = sum(n_debates_eleccion),
            n_debates_frontrunner_presente = sum(n_debates_frontrunner_presente),  
            n_debates_frontORcha_presentes = sum(n_debates_frontORcha_presentes)) %>% 
  ungroup() %>% 
  # la correccion de abajo sucede porque en algunas elecciones de dos rondas tenemos datos positivos solo para una de ellas, 
  #y en este caso estamos operando con el criterio de "al menos un debate"
  dplyr::mutate(n_debates_frontrunner_presente = ifelse(cat_pais=="Guatemala"&ncat_eleccion>2010&ncat_eleccion<2016, 1,n_debates_frontrunner_presente)) %>% 
  # siguiente correccion es porque por definicion si no hubo ningun debate, no hubo ninguno con el frontrunner presente 
  dplyr::mutate(n_debates_frontrunner_presente = ifelse(n_debates_eleccion==0,0,n_debates_frontrunner_presente)) %>% 
  dplyr::mutate(dico_debates_eleccion =  ifelse(n_debates_eleccion>0,1,0),
            dico_frontrunner_presente =  ifelse(n_debates_frontrunner_presente>0,1,0),
            #dico_frontycha_presentes =  ifelse(n_debates_frontycha_presentes>0,1,0),
            dico_frontORcha_presentes =  ifelse(n_debates_frontORcha_presentes>0,1,0) ) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  dplyr::mutate(lagged_n_debates_eleccion = lag(n_debates_eleccion),
         lagged_n_debates_frontrunner_presente = lag(n_debates_frontrunner_presente),
         #lagged_n_debates_frontycha_presentes = lag(n_debates_frontycha_presentes),
         lagged_n_debates_frontORcha_presentes = lag(n_debates_frontORcha_presentes),
         lagged_dico_debates_eleccion = lag(dico_debates_eleccion),
         lagged_dico_frontrunner_presente =  lag(dico_frontrunner_presente),
         #lagged_dico_frontycha_presentes = lag(dico_frontycha_presentes),
         lagged_dico_frontORcha_presentes = lag(dico_frontORcha_presentes)) %>% 
  # completamos primeras elecciones en la base de datos (que ahora figuran como "nas") con "0"
  dplyr::mutate(lagged_n_debates_frontrunner_presente = ifelse(is.na(lagged_n_debates_eleccion), 0, lagged_n_debates_frontrunner_presente),
                lagged_n_debates_frontORcha_presentes = ifelse(is.na(lagged_n_debates_eleccion), 0, lagged_n_debates_frontORcha_presentes),
                lagged_dico_frontrunner_presente = ifelse(is.na(lagged_n_debates_eleccion), 0, lagged_dico_frontrunner_presente),
                lagged_dico_frontORcha_presentes = ifelse(is.na(lagged_n_debates_eleccion), 0, lagged_dico_frontORcha_presentes)  ) %>% 
  dplyr::mutate(lagged_n_debates_eleccion = ifelse(is.na(lagged_n_debates_eleccion), 0, lagged_n_debates_eleccion),
                lagged_dico_debates_eleccion = ifelse(is.na(lagged_dico_debates_eleccion), 0, lagged_dico_debates_eleccion) ) %>%
  dplyr::mutate(lagged_all_previous_elec = cumsum(dico_debates_eleccion) - dico_debates_eleccion, # ACA 
         lagged_all_previous_elec_frontrunner_presente = cumsum(dico_frontrunner_presente) - dico_frontrunner_presente,
         lagged_all_previous_debates = cumsum(n_debates_eleccion) - n_debates_eleccion,
         lagged_all_previous_debates_frontrunner_presente = cumsum(n_debates_frontrunner_presente) - n_debates_frontrunner_presente) %>%   
  dplyr::mutate(lagged_dico_any_previous_elec = ifelse(lagged_all_previous_elec>0,1,0),
         lagged_dico_any_previous_elec_frontrunner_presente = ifelse(lagged_all_previous_elec_frontrunner_presente>0,1,0)) %>% 
  ungroup()  %>% 
  select(c(cat_pais, ncat_eleccion, starts_with("lagged_")))
  

# PARENTESIS TEMPORARIO REVISION MANUAL DE NAS RESPECTO DE DEBATES CON FRONTRUNNER PRESENTE
# Y RESPECTO DE PRIMERAS ELECCIONES
# EN CUANTO A frontrunner presente, 
# el problema es que estoy combinando dos rondas y en pocos casos tengo datos para una de ellas y para la otra no
# hay un problema de criterios asimetricos ademas:
# si se que en "al menos una" ronda hubo debate, la variable dicotomica que agrega ambas rondas deberia valer 1,
# en cambio, si tengo el dato de que en una ronda NO hubo debate con el frontrunner presente, no puedo aseverar que en la otra no haya habido
# por lo que debo dejar el NA y luego reemplazar con base en la variable dicotomica de si hubo debates mas en general
# (como hice arriba, dado que por definicion si NO hubo NINGUN debate, tampoco lo hubo con frontrunner presente)
# en cuanto al caso en los que tengo una ronda con 1 y otra con NA,
# se trata de dos años electorales, los identifico manulamente para poner la solucion manual arriba
# 
# 
 # check1 <- data_elecciones_lagged %>% 
 #    subset(!is.na(n_debates_frontrunner_presente)) # 125 observaciones de 191. deberia tener 151 (-18 = 133), probablemente me estoy perdiendo datos por los NAs 
 # table(check1$dico_frontrunner_presente)
# check2 <- data_elecciones_carolina2  %>% 
#   #subset(!is.na(dico_frontrunner_presente)) %>% 
#   group_by(cat_pais, ncat_eleccion) %>% 
#   dplyr::summarise(n = n(),
#                   sum = sum(dico_frontrunner_presente, na.rm =T)) #%>% 
#  # subset(!is.na(sum))
# table(check2$sum)
# # 18*2+26 bien! deberia tener 62 datos positivos
# cuando_evito_nas <-  data_elecciones_carolina2  %>% 
#   subset(!is.na(dico_frontrunner_presente)) %>% 
#   group_by(cat_pais, ncat_eleccion) %>% 
#   dplyr::summarise(n = n(),
#                    sum = sum(dico_frontrunner_presente, na.rm =T)) %>% 
#   subset(sum==1)
# 
# cuando_no_evito_nas <-  data_elecciones_carolina2  %>% 
#   #subset(!is.na(dico_frontrunner_presente)) %>% 
#   group_by(cat_pais, ncat_eleccion) %>% 
#   dplyr::summarise(n = n(),
#                    sum = sum(dico_frontrunner_presente)) %>% 
#   subset(sum==1)
# 
# combined_df <- full_join(cuando_evito_nas, cuando_no_evito_nas, by = c("cat_pais", "ncat_eleccion"), suffix = c("_df1", "_df2"))

# problema son Guatemala 2011 y 2015

#chequeamos manualmente que hay NAs SOLAMENTE en primeras elecciones en nos base de datos
# por definicion, estas elecciones no estuvieron precedidas por elecciones por debate, por lo que reemplazamos NAs por 0
#replace(is.na(.),0) # OJO NO, PORQUE HAY AUTENTICOS MISSING DATA. 

######### uno est data lagged a la base ###

data_elecciones_carolina3 <- data_elecciones_carolina2 %>% 
  left_join(data_elecciones_lagged)

###### ultimos ajustes para unir base de datos

data_elecciones_carolina3 <- data_elecciones_carolina3 %>% 
  mutate(ncat_eleccion = ifelse(cat_pais=="Guatemala"&ncat_eleccion==1990,1991,ncat_eleccion))

data_elecciones2 <-  data_elecciones %>% 
     mutate(electionyr = round(electionyr)) %>% 
     dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country, "ncat_ronda" = round)

# FINALMENTE UNO DATA ENCUESTAS CON DATA DEBATES
# data_agregada <- data_elecciones %>% 
#   mutate(electionyr = round(electionyr)) %>% 
#   dplyr::rename("ncat_eleccion" = electionyr, "cat_pais" = country, "ncat_ronda" = round) %>% 
#   left_join(data_elecciones_carolina3)

# hacemos el join al reves, es mas prolijjo 

data_agregada <- data_elecciones_carolina3 %>% 
  left_join(data_elecciones2)

# check
#u_elec_data <- data_agregada$electionid %>% unique()

# GUARDAMOS FINALMENTE ###################

#path <- "./tesis_doctorado/datav2023/all_elections.csv"
path <- "all_elections.csv"
#data_agregada %>%
  #write.csv(path)
#test <- read.csv("all_elections.csv")

############ CREO VARIABLE " REGION " ####################
## SE PUEDE PARTIR DE ACA O SEGUIR DE LARGO
path <- "all_elections.csv" # base creada previamente en este mismo script
data_agregada <- read.csv(path)  %>% select(-X) 
#reserva <- data_agregada

# EN MAING Y PEREZ LIÑAN : 
# In addition to global trends and hegemonic powers, changes in neighboring countries may affect the preferences and resources of domestic coalitions. We estimate the presence of a favorable regional environment using the average score in our democracy scale for the whole region (but excluding the country in question) during the previous year. The coding for this indicator is based on our trichotomous measure of democracy. The value of this variable can theoretically range from 0, if none of the other nineteen countries in the region were democratic in a given year, to 1 if all nineteen countries were democratic in that year. To compute this average, we gave semi-democratic countries a score of 0.5. We exclude the country in question and lag the variable to minimize problems of endogeneity. Therefore, the variable reflecting the regional environment is defined for any country i at time t as: (3.1) where Rit is the value of the regional indicator, Dt–1 is the proportion ofdemocracies in the region during the previous year, St–1 is the proportion of semi-democracies in the region during the previous year, and γit−1 is a correction term that acquires a value of 1/N if the country was democratic, and 1/(2N) if the country was semi-democratic during the previous year (i.e., excludes the country’s score if the regime was competitive during the past year). The second term (N / (N−1)) reweights the proportions to reflect the fact that the specific country was excluded from the denominator.
# FORMULA ES
# REGION = ( PROP ELECCIONES CON DEBATES AÑO ANTERIOR  - (DICO_DEBATES EN PAIS/N) ) * ( N / (N-1))

# necesito primero crear base anual para la region. para esta base anual, contar : cantidad de elecciones. cantidad de elecciones con debates. prop elecciones con debates

base_anual <- data_agregada %>% 
  group_by(ncat_eleccion) %>% 
  dplyr::summarise(n_elecciones_en_año_region = n(),
                   n_elecciones_en_año_region_con_debates = sum(dico_debates_eleccion),
                   n_elecciones_en_año_region_con_frontrunner = sum(dico_frontrunner_presente, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::mutate(prop_elecciones_en_año_region_con_debates = n_elecciones_en_año_region_con_debates/n_elecciones_en_año_region,
                prop_elecciones_en_año_region_con_frontrunner = n_elecciones_en_año_region_con_frontrunner/n_elecciones_en_año_region)

base_elecciones_reducida <- data_agregada %>% 
  select(cat_pais, ncat_eleccion) %>% 
  unique() %>% 
  mutate(previous_year = ncat_eleccion -1 ) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(start_year = lag(ncat_eleccion)) %>% 
  ungroup() %>% 
  mutate(start_year = ifelse(is.na(start_year), ncat_eleccion - 4, start_year)) 

base_elecciones_reducida_año_anterior <- base_elecciones_reducida %>% 
  left_join(base_anual %>% dplyr::rename("previous_year" = "ncat_eleccion", # debe haber forma más automatizada de hacer esto pero no se me ocurre ahora 
                                         "n_elecciones_año_anterior_region" = "n_elecciones_en_año_region",
                                         "n_elecciones_año_anterior_region_con_debates" =  "n_elecciones_en_año_region_con_debates",
                                         "n_elecciones_año_anterior_region_con_frontrunner" = "n_elecciones_en_año_region_con_frontrunner",
                                         "prop_elecciones_año_anterior_region_con_debates" = "prop_elecciones_en_año_region_con_debates",
                                         "prop_elecciones_año_anterior_region_con_frontrunner" = "prop_elecciones_en_año_region_con_frontrunner")) %>% 
  # dplyr::rename(~ str_replace(., "en_año", "año_anterior"), .cols = starts_with("n_elecciones_en_año_region"))
  mutate(n_elecciones_año_anterior_region = ifelse(is.na(n_elecciones_año_anterior_region),0, n_elecciones_año_anterior_region)) %>% 
  mutate(n_elecciones_año_anterior_region_con_debates = ifelse( n_elecciones_año_anterior_region==0, 0, n_elecciones_año_anterior_region_con_debates),
         n_elecciones_año_anterior_region_con_frontrunner = ifelse( n_elecciones_año_anterior_region==0, 0, n_elecciones_año_anterior_region_con_frontrunner),
         prop_elecciones_año_anterior_region_con_debates  = ifelse( n_elecciones_año_anterior_region==0, 0, prop_elecciones_año_anterior_region_con_debates),
        prop_elecciones_año_anterior_region_con_frontrunner = ifelse( n_elecciones_año_anterior_region==0, 0, prop_elecciones_año_anterior_region_con_frontrunner))

# Calculate average values for periods between elections # CODIGO DE CHATGPT NO FUNCA
df_avg_values <- base_elecciones_reducida %>%
  arrange(cat_pais, ncat_eleccion) %>%
  #group_by(cat_pais) %>%
  #filter(row_number() > 1) %>%  # Remove the first row for each country
  rowwise() %>%
  mutate(
    avg_prop = base_anual %>%
      filter(ncat_eleccion >= start_year & ncat_eleccion <= previous_year) %>%
      summarize(avg_prop = mean(prop_elecciones_en_año_region_con_debates, na.rm = TRUE)) %>%
      pull(avg_prop)
  ) %>%
  select(cat_pais, ncat_eleccion, avg_prop) %>%
  ungroup()

# testeo de codigo, saltear 
# start_year <- 2019
# previous_year <- 2023
# 
# test_avg_prop = base_anual %>%
#   filter(ncat_eleccion >= start_year & ncat_eleccion <= previous_year) %>%
#   summarize(avg_prop = mean(prop_elecciones_en_año_region_con_debates, na.rm = TRUE)) %>%
#   pull(avg_prop)
# 
# mean(test_avg_prop$prop_elecciones_en_año_region_con_debates)

base_elecciones_reducida2 <- base_elecciones_reducida %>%
  left_join(df_avg_values, by = c("cat_pais", "ncat_eleccion")) %>% 
  left_join(base_elecciones_reducida_año_anterior) 

base_elecciones_reducida2 <- base_elecciones_reducida2 %>% 
  dplyr::rename("mean_prop_elecciones_con_debates_periodo_previo" = "avg_prop") 

data_agregada <- data_agregada %>% 
  left_join(base_elecciones_reducida2)

path <- "all_elections.csv"
#data_agregada %>%
#write.csv(path)

# EVENTUALMENTE, PENDIENTE: PONDERAR REGION POR DISTANCIA DE CAPITALES. ##
# n model 4.3.1 the indicators of regional and extra-regional diffusion are replaced by a spatial lag that weights the influence of Polity scores in all other countries in the world (including the Latin American neighbors), with similarresults. This spatial lags index was measured as Zit = (dij−1 / dij−1)*Pjt−1 where Zit is the value of the index for country i at time t, dij is the distance between the capital of country i and any other country j, and Pjt−1 is the Polity score for country j during the previous year. The expression (dij−1 / dij−1) weights Polity scores according to the inverse of the distance between the two countries.

############## AGREGAMOS DATA NORMATIVA #####################
# IDEM ANTERIOR, SE PUEDE PARTIR DE ACA O SEGUIR DE LARGO
#path <- "all_elections.csv" # base creada previamente en este mismo script
#data_unida <- read.csv(path)  %>% select(-X) 
path <- "base_anual_fullv2023.xlsx" # base creada en "tercera_limpieza_datos.R"
data_normativa <- readxl::read_xlsx(path) 

# descartamos variables que pueden generar problemas
data_normativa <- data_normativa %>% 
  select(-dico_hubo_debates)

# cambio menor para hacer data compatible 
data_normativa <- data_normativa %>% 
  mutate(ncat_eleccion = ifelse(cat_pais=="Guatemala"&ncat_eleccion==1990,1991,ncat_eleccion))

# unimos data

data_agregada <- data_agregada %>% 
  left_join(data_normativa)

# guardamos

path <- "all_elections.csv"
data_agregada %>% 
  write.csv(path) # YA FUE GUARDADA VERSION CORREGIDA AL 27 MAYO

#################################################################################################################
#################################################################################################################
# CHEQUEOS RANDOM, NO HACE FALTA CORRER, ESPACIO BORRADOR  !!!!! ######
# 
# table(data_agregada$dico_debates_eleccion)
# result <- t.test( competitividad ~ dico_debates_eleccion, data = data_agregada) # las elecciones son mas estrechamente competidas cuando hay debates
# result <- t.test( concentracion ~ dico_debates_eleccion, data = data_agregada) # las elecciones estan ligeramente mas concentradas cuando hay debates que cuando no hay debates # esto deberia interacturse con RONDA supongo
# table(data_agregada$dico_frontrunner_presente) # KII ESTO MEJORO UN MONTON 
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
# # • Indice de voto regional diferenciado, de Lee. 
# 
# 
# # CHECK DE DUPLICADOS ################## 
# check_agrupamiento <- data_agregada %>%
#   # mutate(electionyr = ifelse(country=="Chile"&round(electionyr)==2009, 2010, electionyr)) %>% # para hacer luego compatible con base carolina
#   # subset(electionid!="Argentina 2011-08-14") %>%  # eliminamos elecciones primarias argentina
#   # mutate(round = ifelse(country=="Argentina"&round(electionyr)==2011,1,round)) %>%
#   # mutate(turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==1, 80.61, turnout),
#   #        turnout = ifelse(country=="Brazil"&round(electionyr)==2014&round==2, 78.90, turnout)) %>% # habia problemas con decimales en el reporte de este numero para distintos items de esta eleccion
#   # subset(!(country=="Brazil"&round(electionyr)==2010&turnout==81.90)) %>%  # eliminamos base mas incompleta dupilcada de brasil para esta eleccion
#   # mutate(id_polldatapoint = row_number())  %>%
#   #select(electionid, electionyr, country, round, enpp, turnout, espv, regime) %>%
#   select(electionid, ncat_eleccion, cat_pais, ncat_ronda, enpp, turnout, espv, regime) %>%
#   unique()
# u_elect <- check_agrupamiento$electionid %>% unique()
# 
# #Find duplicated rows in column A
# duplicated_rows <- data_incumbents[duplicated(data_incumbents$electionid) | duplicated(data_incumbents$electionid, fromLast = TRUE), ]
# 
# # Print the duplicated rows
# print(duplicated_rows)
# # Function to identify columns where two rows differ
# identify_differing_columns <- function(df) {
#   # Initialize an empty vector to store column indices
#   differing_columns <- c()
#   
#   # Loop through each column of the dataframe
#   for (col in 1:ncol(df)) {
#     # Check if the values in the column are different between the two rows
#     if (!identical(df[1, col], df[2, col])) {
#       differing_columns <- c(differing_columns, col)
#     }
#   }
#   
#   return(differing_columns)
# }
# 
# # Identify columns where two rows differ
# differing_columns <- identify_differing_columns(duplicated_rows)
# 
# # Print the column indices
# print("Columns where two rows differ:")
# print(differing_columns)
# 
# # Print the column names
# print("Column names where two rows differ:")
# print(colnames(duplicated_rows)[differing_columns])