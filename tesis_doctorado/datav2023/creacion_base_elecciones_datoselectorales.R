
# librereias ######################

library(tidyverse)
library(googlesheets4)

# wd ##################
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")


# carga data ##############

# data basica propia
data_base_elecciones <- read.csv("base_base_elecs.csv")  # creada en creacion_base_base.R
data_base_candidatos <- readxl::read_xlsx("all_candidates.xlsx") # esta base fue creada en segunda_limpieza_datos y es usada en creacion_base_candidatos1

# data auxiliar

countrynames <- read.csv("countrynames.csv") %>% 
  mutate(cat_pais =  iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim()) %>% 
  mutate(mayus_eng_cat_pais = toupper(eng_cat_pais)) %>% 
  mutate(mayus_eng_cat_pais = str_replace(mayus_eng_cat_pais, 
                                          "DOMINICAN REPUB", 
                                          "DOMINICAN REPUBLIC"))
         
electyears <- read.csv("electyears.csv")

# data desagregada a nivel de candidatos
setwd("/home/carolina/Documents/dataexterna")
data_fried_seaw_caro_candidatos <- read.csv("combined_candidates_presdata_friedenberg_seawright_caro2.csv")
data_fried_seaw_caro_candidatos <- data_fried_seaw_caro_candidatos %>% 
  left_join(countrynames)  

data_fried_seaw_caro_candidatos <- data_fried_seaw_caro_candidatos %>% 
  mutate(vote_share = as.numeric(gsub(",", ".", vote_share)))

# data a nivel de eleccciones
# versiones originales y backups de esta data en dataexterna
setwd("/home/carolina/Documents/dataexterna")
data_dlujan_elecciones <- read.csv("Base_elecciones_presidenciales_AL_Luján.csv")
data_mainw_elecciones <- haven::read_dta("VOLATILITY/LAEVD_presidential_dataset.dta")
data_incumbentes <-  read.csv("base_incumbentes_oficialistas.csv")
data_idpart <- read.csv("idpart_lapop/idpart_lapop.csv")  
data_tecno_uit <- read.csv("UIT/df_tecno_uit.csv")  
data_tv_lapop <- read.csv("tv_lapop/proptv_lapop.csv")
data_tv_latinobarometro <- read.csv("latinobarometro/df_proptv_latinobarometro.csv")
data_descontento_latinobarometro <- read.csv("latinobarometro/df_satisfaccion_latinobarometro.csv")
data_descontento_lapop <- read.csv("lapop/data_satisfaccionlapop.csv")

# datos encuestas
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

data_encuestas_elecciones <-read.csv("all_elections_full_dataset.csv") %>% select(-X)
data_encuestas_candidatos <- read.csv("base_candidatos_matcheados2023.csv")


#### INDICADORES NIVEL ELECCION #####
# vamos paso por paso. primero vamos a ver que datos de los importantes tenemos para el modelo 1 OBS POR ELECCION

# join de bases disponibles # 

# creo variables para unir  
data_dlujan_elecciones_tojoin <- data_dlujan_elecciones %>% 
  mutate(ncat_ronda = 1,
         cat_pais =  iconv(pais, to = "ASCII//TRANSLIT") %>%  str_trim() %>% str_replace("R. Dominicana", "Republica Dominicana"),
         ncat_eleccion = id_eleccion)  
 
# descarto variables menos relevantes de momento
data_dlujan_elecciones_tojoin <- data_dlujan_elecciones_tojoin %>% 
  select(-c(pol_coppedge, pol_handlin,	pol_singer, federalismo_Gerring, reglaelectoral, anio, id_eleccion, nepl_golder))

# unimos
check_elecciones <- data_base_elecciones %>% 
  left_join(data_dlujan_elecciones_tojoin)

check_elecciones <- check_elecciones %>%
  mutate(ntc = ifelse(ncat_ronda==2,2,ntc))


# # vemos si hay data de la base original que por algun motivo NO se unio y eventualmente corregimos manualmente
# # queremos ver si hay tantos puntos de datos como filas en base original 
# # por lo menos en los que respecta a las variables de id de la base original
# # corregimos problema usual de elecciones chile y guatemala. y corregimos tema de acentos para unir
table(data_dlujan_elecciones$pais)
table(check_elecciones$pais %>% ifelse(is.na(.), "MISSING", .))
table(check_elecciones$cat_pais)
nrow(check_elecciones) - 120 == nrow(data_dlujan_elecciones)
desunidos_dlujan <- data_dlujan_elecciones_tojoin %>%
  anti_join(data_base_elecciones)
summary(check_elecciones)


# vamos a ir viendo en indicador por indicador que tenemos 

###### fragmentacion #######################

data_nefcandidatos <- check_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nec )

data_nefcandidatos <- data_nefcandidatos %>% 
  mutate(source_nec = ifelse(is.na(nec), NA, "Lujan (2020)"))

# . The formula consists on dividing one over the sum of the squares of the proportions (votes or seats) that the parties obtain in an electoral instance.
calculo_nec_propio <- data_fried_seaw_caro_candidatos %>% 
  subset(ncat_ronda==2|dico_debates_eleccion==1) %>%  # solo para estas recogimos datos completos por ahora
  mutate(proportions = vote_share/100) %>% 
  mutate(squared_proportions = proportions*proportions) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(nec_caro = 1 / sum(squared_proportions, na.rm=T)) %>% 
  ungroup()

# unimos calculo propio a base con calculo lujan
data_nefcandidatos <- data_nefcandidatos %>% 
  left_join(calculo_nec_propio) #%>% 
  #mutate(check_diff = format(nec - nec_caro , scientific = FALSE) )

data_nefcandidatos <- data_nefcandidatos %>% 
  mutate(nec_lujan = nec) %>% 
  mutate(nec = nec_caro) %>% 
  mutate(source_nec = ifelse(!is.na(nec_caro), 
                             "Calculo propio con base en datos de candidatos", 
                             source_nec)) %>% 
  mutate(nec = ifelse(is.na(nec), nec_lujan,nec)) 

indicador_nec <- data_nefcandidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nec, source_nec)


# sepramos missing para seguir buscando data

missing_nefcandidatos <- data_nefcandidatos %>% 
  subset(is.na(nec))  

calculo_nec_propio_missings <- data_fried_seaw_caro_candidatos %>% 
  mutate(proportions = vote_share/100) %>% 
  mutate(squared_proportions = proportions*proportions) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(nec_caro = 1 / sum(squared_proportions)) %>% 
  ungroup()

missing_nefcandidatos <- missing_nefcandidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  left_join(calculo_nec_propio_missings)

indicador_nec <-indicador_nec %>% 
  left_join(missing_nefcandidatos %>% 
              dplyr::rename("nec_caro_missing" = "nec_caro"))

indicador_nec <- indicador_nec %>% 
  mutate(nec = ifelse(is.na(nec), nec_caro_missing, nec)) %>% 
  mutate(source_nec = ifelse(!is.na(nec_caro_missing), 
                             "Calculo propio con base en datos de candidatos",
                             source_nec))

indicador_nec <- indicador_nec %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nec, source_nec)
 

###### n total de candidatos ####################

# habria que repetir procedimiento parecido al de recien
data_ntcandidatos <- check_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, ntc )

data_ntcandidatos <- data_ntcandidatos %>% 
  mutate(source_ntc = ifelse(is.na(ntc), NA, "Lujan (2020)"))

# calculo propio
calculo_all_ntc_propio <- data_fried_seaw_caro_candidatos %>% 
  subset(ncat_ronda==2|dico_debates_eleccion==1) %>%  # comenzamos por esta, mas abajo agregamos los missings
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(all_ntc_caro = n()) %>% # ACA INCLUIMOS CANDIDATOS QUE LUEGO SE RETIRARON, unos pocos que registramos participando en debates
  ungroup()

calculo_ntc_propio <- data_fried_seaw_caro_candidatos %>% 
  subset(ncat_ronda==2|dico_debates_eleccion==1) %>%  # comenzamos por esta, mas abajo agregamos los missings
  mutate(vote_share = as.numeric(vote_share)) %>% 
  subset(!is.na(vote_share)) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(ntc_caro = n()) %>% # ACA CONTAMOS SOLO LA LISTA FINAL DE CANDIDATOS
  ungroup()

calculo_ntc_propio <- calculo_ntc_propio %>% 
  left_join(calculo_all_ntc_propio)

# unimos calculo propio a base con calculo lujan
data_ntcandidatos <- data_ntcandidatos %>% 
  left_join(calculo_ntc_propio) #%>% 
  #mutate(check_diff = format(ntc - ntc_caro , scientific = FALSE) )

data_ntcandidatos <- data_ntcandidatos %>% 
  mutate(ntc_lujan = ntc) %>% 
  mutate(ntc = ntc_caro) %>% 
  mutate(source_ntc = ifelse(!is.na(ntc_caro), 
                             "Calculo propio con base en datos de candidatos", 
                             source_ntc)) %>% 
  mutate(ntc = ifelse(is.na(ntc), ntc_lujan, ntc)) 

indicador_ntc <- data_ntcandidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, ntc, all_ntc_caro, source_ntc)


# sepramos missing para seguir buscando data

missing_ntcandidatos <- data_ntcandidatos %>% 
  subset(is.na(ntc))  

calculo_all_ntc_propio_missings <- data_fried_seaw_caro_candidatos %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(all_ntc_caro = n()) %>% # ACA INCLUIMOS CANDIDATOS QUE LUEGO SE RETIRARON, unos pocos que registramos participando en debates
  ungroup()

calculo_ntc_propio_missings <- data_fried_seaw_caro_candidatos %>% 
  mutate(vote_share = as.numeric(vote_share)) %>% 
  subset(!is.na(vote_share)) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(ntc_caro = n()) %>% # ACA CONTAMOS SOLO LA LISTA FINAL DE CANDIDATOS
  ungroup()

calculo_ntc_propio_missings <- calculo_ntc_propio_missings %>% 
  left_join(calculo_all_ntc_propio_missings)

# unimos a la lista de missings # ALGO ESTA FUNCIONANDO MAL ACA Y NO SE QUE ES
missing_ntcandidatos <- missing_ntcandidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  left_join(calculo_ntc_propio_missings)

# uno al indicador
indicador_ntc <-indicador_ntc %>% 
  left_join(missing_ntcandidatos %>% 
              dplyr::rename("ntc_caro_missing" = "ntc_caro",
                            "all_ntc_caro_missing" = "all_ntc_caro"))

indicador_ntc <- indicador_ntc %>% 
  mutate(ntc = ifelse(is.na(ntc), ntc_caro_missing, ntc)) %>% 
  mutate(all_ntc_caro = ifelse(is.na(all_ntc_caro), all_ntc_caro_missing, all_ntc_caro)) %>%
  mutate(source_ntc = ifelse(!is.na(ntc_caro_missing), 
                             "Calculo propio con base en datos de candidatos",
                             source_ntc))

indicador_ntc <- indicador_ntc %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, ntc, source_ntc, all_ntc_caro)


##### volatilidad #####################

# chequeamos primero como se une la data de mainw. 

data_mainw_elecciones_tojoin <- data_mainw_elecciones %>% 
  dplyr::rename( "mayus_eng_cat_pais" = "country", 
                 "ncat_election_year" = "election_year" ) #%>% 
  # left_join(countrynames) %>% 
  # select(-c(mayus_eng_cat_pais))

data_base_elecciones_tojoin <- data_base_elecciones %>% 
  left_join(electyears) %>% 
  select(-eng_cat_pais) %>% 
  mutate(cat_pais = str_replace(cat_pais, "Republica Dominicana","Rep. Dominicana")) %>% 
  left_join(countrynames) 

data_mainw_elecciones_tojoin$mayus_eng_cat_pais %>% unique()
data_base_elecciones_tojoin$mayus_eng_cat_pais %>% unique()

check_elecciones3 <- data_base_elecciones_tojoin %>% 
  subset(ncat_ronda ==1) %>% 
  left_join(data_mainw_elecciones_tojoin)

desunidos_mainw <- data_mainw_elecciones_tojoin %>%
  anti_join(data_base_elecciones_tojoin) %>% 
  subset(ncat_election_year > 1963)

summary(check_elecciones3)

indicador_volatility <- check_elecciones3 %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, volatility, newparties, withinsv) %>% 
  mutate(source_volatility = ifelse(!is.na(volatility),"Mainwaring & Su (2021)",NA))
  

# unir d lujan para pispear

## NO APORTA NINGUNA DATA EXTRA
# data_volatility_lujan <- check_elecciones %>% 
#   select(cat_pais, ncat_eleccion, volatilidad)
# 
# data_volatility <- data_volatility %>% 
#   left_join(data_volatility_lujan)
# 
# summary(data_volatility)
# 
# data_volatility <- data_volatility %>% 
#   mutate(volatility = ifelse(is.na(volatility), volatilidad, volatility)) %>% 
#   mutate(source_volatility = ifelse(is.na(source_volatility)&!is.na(volatility), 
#                                     "Luján (2020)", 
#                                     source_volatility))
# 
# summary(data_volatility)

##### indicador partyid  o alineamiento #####################

indicador_alineamiento <- data_idpart %>% 
  select(-X) %>% 
  left_join(check_elecciones3 %>%  select(-ncat_ronda)) %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, party_id, idpart_lapop, source_idpart) %>% 
  mutate(alineamiento = ifelse(is.na(idpart_lapop), party_id, idpart_lapop)) %>% 
  mutate(source_alineamiento = ifelse(!is.na(idpart_lapop), source_idpart, 
                                      ifelse(!is.na(party_id), 
                                             "Mainwaring & Su (2021)",
                                             NA)))

indicador_alineamiento <- indicador_alineamiento %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, alineamiento, source_alineamiento)


##### competitividad o margen de victoria  ######################

# con base en data fried_seaw_caro de candidatos, hago el calculo

data_margen_victoria <- data_fried_seaw_caro_candidatos %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  arrange(desc(vote_share)) %>% 
  slice(1:2) %>% 
  summarise(marginvic = diff(vote_share)) %>% 
  ungroup() %>% 
  mutate(cat_pais = iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim() %>% str_replace("Republica Dominicana","Rep. Dominicana")) %>% 
  left_join(countrynames)  


# join
check_elecciones2 <- data_base_elecciones %>% 
  select(-eng_cat_pais) %>% 
  left_join(data_margen_victoria  %>% 
              mutate(cat_pais = iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  
                       str_trim() %>% 
                       str_replace("Rep. Dominicana","Republica Dominicana")), 
            by = join_by(cat_pais, ncat_eleccion, ncat_ronda))

# desunidos_margen <- data_margen_victoria %>% 
#   mutate(cat_pais = iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  
#            str_trim() %>% 
#            str_replace("Rep. Dominicana","Republica Dominicana")) %>% 
#   select(-eng_cat_pais) %>% 
#   anti_join(data_base_elecciones, by = join_by(cat_pais, ncat_eleccion, ncat_ronda))
summary(check_elecciones2)

indicador_competitividad <- check_elecciones2 %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, marginvic) %>% 
  mutate(source_marginvic = "Caluclo propio con base en datos desagregados por candidato")

###### incumbencias ####

data_incumbentes_firstround_tojoin <- data_incumbentes %>% 
  subset(ncat_ronda==1) %>% 
  #select(-ncat_ronda) %>% 
  select(-dico_debates_eleccion) %>% 
  select(-eng_cat_pais) #%>% 
  #select(-X)

indicador_incumbentes_firstround <- data_base_elecciones %>% 
  subset(ncat_ronda==1) %>% 
  left_join(data_incumbentes_firstround_tojoin) # hacemos esto porque en la base de incumbentes solo registramos las primeras rondas

indicador_incumbentes_firstround <- indicador_incumbentes_firstround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, nombre_presidente, nombre_oficialista)) %>% 
  mutate(dico_reeleccion = ifelse(nombre_presidente==nombre_oficialista, 1,0)) %>% 
  mutate(dico_oficialista = ifelse(nombre_oficialista=="No hay un claro oficialista",0,1))

indicador_incumbentes_firstround <- indicador_incumbentes_firstround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, dico_reeleccion, dico_oficialista))

# ahora agregamos segundas rondas
data_incumbentes_secondround_tojoin <- data_incumbentes %>% 
  subset(!(cat_pais=="Chile"&ncat_eleccion==2017&ncat_ronda==1)) %>% 
  select(-ncat_ronda) %>% 
  select(-dico_debates_eleccion) %>% 
  select(-eng_cat_pais)# %>% 
  #select(-X)

data_incumbentes_secondround <- data_fried_seaw_caro_candidatos %>% 
  subset(ncat_ronda==2) %>% 
  left_join(data_incumbentes_secondround_tojoin)  

indicador_incumbentes_secondround <- data_incumbentes_secondround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, candidate_name, nombre_presidente, nombre_oficialista)) %>% 
  mutate(is_reeleccion = ifelse(nombre_presidente==candidate_name, 1,0)) %>% 
  mutate(is_oficialista = ifelse(nombre_oficialista==candidate_name,1,0)) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(dico_reeleccion = sum(is_reeleccion, na.rm=T),
            dico_oficialista = sum(is_oficialista, na.rm=T))
  
indicador_incumbentes <- indicador_incumbentes_firstround %>% 
  rbind(indicador_incumbentes_secondround)
  
  # vamos a tener que ajustar manualmente el unico caso para el que contamos info sobre segunda ronda
# VIEJO
# data_incumbentes_excepcion <- data_incumbentes %>% 
#   subset(ncat_ronda==2) %>% 
#   select(-X)

# indicador_incumbentes <- indicador_incumbentes %>% 
#   mutate(filtrar = ifelse(cat_pais=="Chile"&ncat_eleccion==2017&ncat_ronda==2, 1,0)) %>%
#   subset(filtrar==0) %>% 
#   select(-filtrar) %>% 
#   rbind(data_incumbentes_excepcion)


##### TV tecnologia ######


indicador_proptv <- data_base_elecciones %>% 
  left_join(data_tv_lapop %>%  select(-X)) %>% 
  left_join(data_tv_latinobarometro %>%  select(-X)) 

indicador_proptv <- indicador_proptv %>% 
  dplyr::rename("source_proptvlapop" = source_proptv ) # para evitar confusiones

# latinobarometro mide la variable hasta 2010
# para ser mas correctos vamos a forzar a NA los valores proyectados despues de este año
# y vamos a promediar valores lapop y latinobarometro alli donde los tengamos. 
# independientemente de si se trata de valores medidos o proyectados 
# los numeros son muy parecidos entre las dos encuestas, a simple vista.
# por si acaso creamos variable de control de diferencias 
# en cambio los numeros de UIT divergen mas y en algunos casos como en Chile encontramos valores sospechosos. 

indicador_proptv <- indicador_proptv %>%
  mutate(proptv_latinobarometro = ifelse(ncat_eleccion>2010, NA, proptv_latinobarometro)) %>% 
  mutate(source_proptvlatinobarometro = ifelse(ncat_eleccion>2010, NA, source_proptvlatinobarometro))

indicador_proptv <- indicador_proptv %>%
  mutate(proptv_latinobarometro = proptv_latinobarometro*100) # estaban en distinta escala

check <- indicador_proptv %>% 
  mutate(check_diff = proptv_latinobarometro - proptv_lapop)

indicador_proptv <- indicador_proptv %>% 
  left_join(data_tecno_uit %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, proptv, source_proptv) %>% 
              dplyr::rename("proptv_uit" = proptv,
                            "source_proptvuit" = source_proptv))

check <- indicador_proptv %>% 
  mutate(check_diff = proptv_latinobarometro - proptv_lapop,
         check_diff2 = proptv_latinobarometro - proptv_uit,
         check_diff3 = proptv_lapop - proptv_uit)

indicador_proptv <- indicador_proptv %>% 
  mutate(check_diff3 = proptv_lapop - proptv_uit) %>% # nos deshacemos de valores inusualmente extremos
  mutate(proptv_uit = ifelse(check_diff3>5, NA, proptv_uit)) %>% 
  mutate(source_proptvuit = ifelse(is.na(proptv_uit), NA, source_proptvuit))

# indicador_proptv <- indicador_proptv %>% 
#   # mutate(new_proptv = ifelse(is.na(proptv_lapop),
#   #                            proptv,
#   #                            proptv_lapop)) %>% 
#   # mutate(new_source_proptv = ifelse(is.na(proptv_lapop),
#   #                                   source_proptv,
#   #                                   source_proptv_lapop))  

indicador_proptv <- indicador_proptv %>% 
  mutate(new_proptv = rowMeans(select(., proptv_uit, proptv_latinobarometro, proptv_lapop), na.rm = TRUE)) %>% 
  mutate(new_source_proptv = ifelse(!is.na(new_proptv),
    paste0("Promedio de valores provenientes de:", 
           source_proptvuit, 
           ";", 
           source_proptvlatinobarometro,
           ";",
           source_proptvlapop),
    NA))

check <- indicador_proptv %>% subset(!is.na(new_proptv)) %>% subset(ncat_ronda==2) 
check$dico_debates_eleccion %>% table()

indicador_proptv <- indicador_proptv %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, new_proptv, new_source_proptv) %>% 
  dplyr::rename("proptv" = "new_proptv",
                "source_proptv" = "new_source_proptv") %>% 
  mutate(source_proptv = str_replace(source_proptv, "NA;","")) %>% 
  mutate(source_proptv = str_replace(source_proptv, "Proyectado. Ultimo","Valor proyectado segun ultimo"))



##### satisfaccion con democracia ######

indicador_satisfaccion <- data_descontento_lapop %>% 
  select(-X) %>% 
  left_join(data_descontento_latinobarometro %>% select(-X))

indicador_satisfaccion <- indicador_satisfaccion %>% 
  mutate(satisfaccion = rowMeans(select(., satisfaccion_lapop, satisfaccion_latinobarometro), na.rm = TRUE)) %>% 
  mutate(source_satisfaccion = ifelse(!is.na(satisfaccion),
                                      paste0("Promedio de valores provenientes de:", 
                                             source_satisfaccionlapop, 
                                             ";", 
                                             source_satisfaccionlatinobarometro),
                                      NA)) %>% 
  mutate(source_satisfaccion = str_replace(source_satisfaccion, "NA;","")) %>% 
  mutate(source_satisfaccion = str_replace(source_satisfaccion, "Proyectado. Ultimo","Valor proyectado segun ultimo"))

indicador_satisfaccion <- indicador_satisfaccion %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, satisfaccion, source_satisfaccion)


### GURADADO chequeo y guardo lo disponible hasta ahora  #########################

summary(indicador_nec)
indicador_nec %>% write.csv("indicador_nec.csv")

summary(indicador_volatility)
indicador_nec %>% write.csv("indicador_volatility.csv")

summary(indicador_alineamiento)
indicador_alineamiento %>% write.csv("indicador_alineamiento.csv")

summary(indicador_competitividad)
indicador_nec %>% write.csv("indicador_competitividad.csv")

summary(indicador_ntc)
indicador_ntc %>% write.csv("indicador_ntc.csv")

summary(indicador_incumbentes)
indicador_incumbentes %>% write.csv("indicador_incumbentes.csv")

summary(indicador_proptv)
indicador_proptv %>% write.csv("indicador_proptv.csv")

summary(indicador_satisfaccion)
indicador_satisfaccion %>% write.csv("indicador_satisfaccion.csv")

########################################################################
##########################################################################
############# auxiliares para llenar incumbencia ##############

WINNERS <- data_fried_seaw_caro_candidatos %>%
  mutate(vote_share= as.numeric(vote_share)) %>% 
  group_by(cat_pais,ncat_eleccion,ncat_ronda) %>%
  filter(vote_share == max(vote_share, na.rm=T)) %>%
  ungroup() %>% 
  select(cat_pais,ncat_eleccion,ncat_ronda,candidate_name,party_name)

INCUMBENTS <- data_fried_seaw_caro_candidatos %>%
  filter(status == "Incumbent") %>% 
  select(cat_pais,ncat_eleccion,ncat_ronda,candidate_name,party_name)


#############################################################################
##### VIEJO ################################################ 
##### CHEQUEO DE MISSINGS ########################################

# vemos qué data esta missing y tendremos que buscar por nuestra cuenta


df_with_missing_check_elecciones <- check_elecciones %>%
  filter(if_any(everything(), is.na))

df_with_missing_check_elecciones <- check_elecciones2 %>%
  filter(if_any(everything(), is.na))

missing_nec <- check_elecciones %>%
  filter(is.na(nec))

missing_incumbentereeleccion <- check_elecciones %>%
  filter(is.na(incumbentreeleccion))

missing_ntc  <- check_elecciones %>%
  filter(is.na(ntc))

missing_polariz <- check_elecciones %>%
  filter(is.na(polarizdiffideo))

missing_volatilidad <- check_elecciones %>%
  filter(is.na(volatilidad))

missing_margin <- check_elecciones2 %>%
  filter(is.na(marginvic))

# vamos a crear listas de excel para rellenar manualmente   #
# vamos a tratar de buscar la data ANUAL solamente para los casos SIN debates,
# para los casos CON debates, vamosa reconstruir data ANUAL sobre la base de data POR CANDIDATO, si es posible

to_fill <- missing_margin %>% 
  rbind(missing_margin) %>% 
  arrange(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  mutate(vote_share = NA)

to_fill %>% write.csv("missing_margin_to_fill.csv")

to_fill <- check_elecciones %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, dico_debates_eleccion, nec)) %>% 
  arrange(cat_pais, ncat_eleccion, ncat_ronda ) %>%
  filter(is.na(nec)) # solo vamos a llenar manualmente los años que No haya habido debates y No sean ballotage
# el resto los vamos a calcular con base en datos electorales, en pppio 

to_fill %>% write.csv("missing_nec_to_fill.csv")

####### PASO A EXPLORAR DATA DE CANDIDATOS ####################################

# Me quedo con nombres unicos por eleccion / pais
data_base_candidatos_unicos <- data_base_candidatos %>% 
  select( cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos ) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  distinct(nombres_candidatos, .keep_all = TRUE) %>% 
  subset(nombres_candidatos!="sin datos"&nombres_candidatos!="sin ausencias conocidas")

# Preparo base con datos para unir
data_fried_seaw_caro_candidatos_tojoin <- data_fried_seaw_caro_candidatos %>% 
  mutate(nombres_candidatos = iconv(candidate_name, to = "ASCII//TRANSLIT")) %>% 
  select(-c(candidate_name, eng_cat_pais))

# uno
check_candidatos <- data_base_candidatos_unicos %>% 
  left_join(data_fried_seaw_caro_candidatos_tojoin) 

# veo si algunas no se unieron
desunidos_freid_seaw <- data_fried_seaw_caro_candidatos_tojoin %>% 
  anti_join(data_base_candidatos_unicos) %>% 
  left_join(data_base_elecciones) %>% 
  subset(dico_debates_eleccion==1)

# creo bases para compeltar y corregir nombres manualmente
#primero vamos a compatibilizar nombres manualmente. Luego vamos a completar datos faltantes (tb manualmente)
# check_candidatos %>% write.csv("check_candidatos_ago_2024.csv")

# creamos base para completar manualmente, seleccionando elecciones en las que hubieron debates 

# cargo data creada previamente en la que tenemos 1er y segundo candidato para elecciones Missing y sin debate
base_margin_filled <- read.csv("missing_margin_to_fill_filled.csv")
setdiff(colnames(data_fried_seaw_caro_candidatos2), colnames(base_margin_filled)) 

data_fried_seaw_caro_candidatos2 <- data_fried_seaw_caro_candidatos %>% 
  mutate(party_name = paste(party_name, vs_party_name, sep = " - ")) %>% 
  select(-c("eng_cat_pais", "anio", "vs_vote_share", "vs_party_name", "vs_status")) %>% 
  mutate(compilador_vote_share = NA)

# ajuste para unir, luego vamos a reagregar
base_margin_filled <- base_margin_filled %>% 
  select(-dico_debates_eleccion)

# Reorder df2 to match df1's column order
data_fried_seaw_caro_candidatos2 <- data_fried_seaw_caro_candidatos2[, colnames(base_margin_filled)]

# Combine the data frames
data_fried_seaw_caro_candidatos2 <- rbind(data_fried_seaw_caro_candidatos2, base_margin_filled)

data_fried_seaw_caro_candidatos2 <- data_base_elecciones %>% 
  left_join(data_fried_seaw_caro_candidatos2)

# check <- data_fried_seaw_caro_candidatos2 %>% 
#   group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
#   summarise(n = n()) # ESTAN TODAS LAS ELECCIONES :D

data_fried_seaw_caro_candidatos2 %>%  write.csv("combined_candidates_presdata_friedenberg_seawright_caro2.csv")


# data_base_candidatos2 <- data_base_elecciones %>% 
#   left_join(data_fried_seaw_caro_candidatos %>% 
#               left_join(countrynames) %>% 
#               select(-eng_cat_pais))
# 
# desunidos_freid_seaw2 <- data_fried_seaw_caro_candidatos %>% 
#   left_join(countrynames) %>% 
#   select(-eng_cat_pais) %>% 
#   anti_join(data_base_elecciones) 


####################################### borradorcito de data electoral mientras completo manual ########

# # Specify the path to your .txt file
# file_path <- "/home/carolina/Documents/dataexterna/resultados_electorales/DETALHE_VOTACAO_UF_1989.txt"
# 
# # Read the file
# data <- read.delim(file_path, sep = ";", header = FALSE)
# colnames(data) <- c("DATA_GERACAO","HORA_GERACAO","ANO_ELEICAO","NUM_TURNO","DESCRICAO_ELEICAO","SIGLA_UF",
#                     "SIGLA_UE","CODIGO_CARGO","DESCRICAO_CARGO","QTD_APTOS","QTD_COMPARECIMENTO","QTD_ABSTENCOES",
#                     "QTD_VOTOS_NOMINAIS","QTD_VOTOS_BRANCOS","QTD_VOTOS_NULOS","QTD_VOTOS_LEGENDA","QTD_VOTOS_ANULADOS_APU_SEP",
#                     "QTD_SECOES_TOT","QTD_SECOES_ANULADAS","QTD_SECOES_SEM_FUNCION","QTD_ZONAS_ELEITORAIS","QTD_JUNTAS_APURADORAS")


