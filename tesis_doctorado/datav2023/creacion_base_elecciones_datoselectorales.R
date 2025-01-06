
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
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
data_normativa <- readxl::read_xlsx("base_anual_fullv2023.xlsx") 

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
data_debates_USA <- read.csv("USA_all_debates - debates.csv")
data_elecs_USA <- read.csv("USA_all_debates - electyears.csv")
data_accesomedios <- read.csv("OBSREF/data_accesomedios.csv")  # creada en abriendo_data_friedenberg
data_exapproval <- read.csv("Carlin/data_exapproval.csv")

# carga data controles #
data_gdp <- read.csv("WDI/df_gdp.csv")  
qof_controles_ohtersregime <- read.csv("QofG/indicadores_ohtersregime_qof.csv")
qof_controles_mediaquality <- read.csv("QofG/indicadores_mediaquality_qof.csv")
qof_controles_democracia <- read.csv("QofG/indicadores_democracia_qof.csv")
qof_controles_desarrollo <- read.csv("QofG/indicadores_desarrollo_qof.csv")
qof_controles_turnout <- read.csv("QofG/indicadores_turnout_qof.csv")
data_controles_media_vdem <- read.csv("VDEM/V-Dem-CY-FullOthers-v14_csv_YyKfizl/data_media_vdem.csv")

# datos encuestas
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

data_encuestas_elecciones <-read.csv("all_elections_full_dataset.csv") %>% select(-X)
data_encuestas_candidatos <- read.csv("base_candidatos_matcheados2023.csv")

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

# INDICADORES NIVEL ELECCION #####
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

# sobre el siguiente breve paso:
# hacemos este agregado por separado por tratarse del sistema de doble voto simulaneo, que Lujan calcula de manera diferente a la propia. Lo mismo aplica a los casos uruguayos pero aquí el cálculo es automático porque ocurrieron debates
data_honduras <- data.frame(cat_pais = "Honduras",
                             ncat_eleccion = 1985,
                             ncat_ronda = 1,
                             nec = NA,
                            source_nec = NA,
                            nec_caro = NA,
                            nec_lujan = NA)

missing_nefcandidatos <- missing_nefcandidatos %>% 
  rbind(data_honduras)

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
  mutate(nec = ifelse(cat_pais=="Honduras"&ncat_eleccion==1985, nec_caro_missing, nec)) %>% 
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

# sobre el siguiente breve paso:
# hacemos este agregado por separado por tratarse del sistema de doble voto simulaneo, que Lujan calcula de manera diferente a la propia. Lo mismo aplica a los casos uruguayos pero aquí el cálculo es automático porque ocurrieron debates
data_honduras <- data.frame(cat_pais = "Honduras",
                            ncat_eleccion = 1985,
                            ncat_ronda = 1,
                            ntc = NA,
                            source_ntc = NA,
                            ntc_caro = NA,
                            all_ntc_caro = NA,
                            ntc_lujan = NA)

missing_ntcandidatos <- missing_ntcandidatos %>% 
  rbind(data_honduras)

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
  mutate(ntc = ifelse(cat_pais=="Honduras"&ncat_eleccion==1985, ntc_caro_missing, ntc)) %>% 
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
  
 
indicador_volatility <- indicador_volatility %>% 
  haven::zap_formats()

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



###### imagen oficialismo ####

# un indicador posible: votos oficialismo

data_incumbentes_tojoin <- data_incumbentes %>% 
  select(cat_pais, ncat_eleccion, nombre_oficialista) %>% 
  mutate(nombre_oficialista = iconv(nombre_oficialista, to = "ASCII//TRANSLIT") %>%  str_trim())

data_all_candidatos_incumbentes <- data_fried_seaw_caro_candidatos %>% 
  mutate(candidate_name = iconv(candidate_name, to = "ASCII//TRANSLIT") %>%  str_trim()) %>% 
  left_join(data_incumbentes_tojoin) %>% 
  subset(!(cat_pais=="Chile"&ncat_eleccion==2017&ncat_ronda==1&nombre_oficialista=="Alejandro Guillier")) %>% 
  subset(!(cat_pais=="Chile"&ncat_eleccion==2017&ncat_ronda==2&nombre_oficialista=="No hay un claro oficialista"))

data_candidatos_incumbentes <- data_all_candidatos_incumbentes %>% 
  mutate(is_oficialista = ifelse(candidate_name==nombre_oficialista,
                                 1, 0)) %>% 
  subset(is_oficialista==1) %>% 
  mutate(source_voteshareincumbent = paste0(source_str_vote_share, source_url_vote_share, sep = " - ")) %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, vote_share, source_voteshareincumbent) 
  

data_no_candidatos_incumbentes <- data_all_candidatos_incumbentes %>% 
  subset(nombre_oficialista=="No hay un claro oficialista") %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  distinct() %>% 
  mutate(vote_share = 0) %>% 
  mutate(source_voteshareincumbent = "Imputado ya que no hay un claro oficialista") 
  

data_disponible_incumbentes <- data_candidatos_incumbentes %>% 
  rbind(data_no_candidatos_incumbentes) %>% 
  dplyr::rename("voteshareincumbent" = vote_share)
 
indicador_voteshareincumbent <- data_base_elecciones %>% 
  left_join(data_disponible_incumbentes) 

indicador_voteshareincumbent <- indicador_voteshareincumbent %>% 
  mutate(source_voteshareincumbent = ifelse(ncat_ronda==2&is.na(voteshareincumbent),
                                    "Imputado ya que no hay un claro oficialista",
                                     source_voteshareincumbent)) %>% 
  mutate(voteshareincumbent = ifelse(ncat_ronda==2&is.na(voteshareincumbent),
                                     0,
                                     voteshareincumbent)) %>% 
  subset(!(cat_pais=="Bolivia"&ncat_eleccion==1978&voteshareincumbent<5)) # por algun motivo en Nohlen hay dos mediciones reportadas para el mismo candidato. Confuso

# voteshareincumbent_tofill <- indicador_voteshareincumbent %>% 
#   subset(is.na(voteshareincumbent))
# voteshareincumbent_tofill %>% write.csv("voteshareincumbent_tofill.csv")

indicador_voteshareincumbent <- indicador_voteshareincumbent %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, voteshareincumbent, source_voteshareincumbent)

# indicador_voteshareincumbent %>% select(cat_pais,ncat_eleccion, ncat_ronda) %>% duplicated()

# agragamos data executive approval 

indicador_voteshareincumbent <- indicador_voteshareincumbent %>% 
  left_join(data_exapproval %>% select(-X))

indicador_exapproval <- indicador_voteshareincumbent %>% 
  mutate(exapproval_notsmoothed = ifelse(str_detect(source_exapproval,
                                                    "Proyectado"),
                                         NA,
                                         exapproval_notsmoothed)) %>%
  mutate(exapproval_smoothed = ifelse(str_detect(source_exapproval,
                                                    "Proyectado"),
                                         NA,
                                         exapproval_smoothed)) %>% 
  dplyr::rename("exapprovalnotsmoothed" = exapproval_notsmoothed,
                "exapprovalsmoothed" = exapproval_smoothed)

           

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


##### internet tecnologia ######
summary(data_tecno_uit)
cor(data_tecno_uit %>% select(propindivinternet, propinternet), use = "pairwise.complete.obs")
# correlacionan muy bien y tengo un poquito mas de data para el "propindiv"

indicador_propinternet <- data_tecno_uit %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, 
                     propindivinternet, propindivinternet2,
                     source_propindivinternet, source_propindivinternet2)

# cambio de orden:

indicador_propinternet <- indicador_propinternet %>% 
  dplyr::rename("propindivinternet" = "propindivinternet2", 
                "propindivinternet2" = "propindivinternet")

##### regulacion especifica debates ######

indicador_regulacion <- data_normativa %>% 
  select(cat_pais, ncat_eleccion, ncat_totreg) %>% 
  mutate(ncat_eleccion = ifelse(cat_pais=="Guatemala"&ncat_eleccion==1990,1991,ncat_eleccion)) %>%  # peq ajuste manual
  mutate(regulaciondico = ifelse(ncat_totreg==0,0,
                                 ifelse(ncat_totreg>0,1,NA))) %>% 
  dplyr::rename("regulacionordinal" = ncat_totreg) %>% 
  mutate(source_regulacion = "Franco H. (2022)")

indicador_regulacion <- data_base_elecciones %>% 
  left_join(indicador_regulacion) %>% 
  select(-dico_debates_eleccion, -eng_cat_pais)

##### acceso a medios regulacion #####

indicador_accesomedios <- electyears %>% select(-X) %>% 
  left_join(data_accesomedios %>% select(-X)) %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda,
         prohibicionpropaganda, accesogratuito, 
         source_accesomedios)


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

##### EEUU debates ######

# source: https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/presidential-campaigns-debates-and-endorsements-0

n_debates_eeuu <- data_debates_USA %>% 
  group_by(year) %>% 
  summarise(n_debates_usa_year = n()) 

indicador_eeuu <- data_elecs_USA %>% 
  dplyr::rename("year" = "electyears") %>% 
  left_join(n_debates_eeuu) %>% 
  mutate(n_debates_usa_year = ifelse(is.na(n_debates_usa_year),0,n_debates_usa_year)) %>% 
  mutate(dico_debates_usa_year = ifelse(n_debates_usa_year==0,0,1))

indicador_eeuu_full <- indicador_eeuu %>%
  summarize(StartYear = min(year), EndYear = max(year)) %>%
  rowwise() %>%
  do(data.frame(year = seq(.$StartYear, .$EndYear, by = 1))) %>%
  ungroup()

indicador_eeuu_full <- indicador_eeuu_full %>%
  left_join(indicador_eeuu, 
            by = c("year"))  %>% 
  mutate(n_debates_usa_year = ifelse(is.na(n_debates_usa_year),0,n_debates_usa_year)) %>% 
  mutate(year = year + 1 ) # sumo un año porque en gral las elecciones en eeuu son hacia fin de año

# vamos a contabilizar debates por periodos electoales
# para eso creamos data que indica años-periodo
# notese que para las primeras elecciones consideramos un ciclo corto de dos años como referencia
# arbitraria pero que busca no ser ni muy distante ni excesivamente reduccionista
# contamos como que el ciclo electoral arranca en la primera ronda
criterio <- 2

years_full <- electyears %>%
  select(-X) %>% 
  group_by(cat_pais) %>%
  summarize(StartYear = min(ncat_election_year)-criterio, EndYear = max(ncat_election_year)) %>%
  rowwise() %>%
  do(data.frame(cat_pais = .$cat_pais, ncat_election_year = seq(.$StartYear, .$EndYear, by = 1))) %>%
  ungroup()

df_complete <- years_full %>%
  left_join(electyears 
            %>% select(-X) %>% subset(ncat_ronda==1), 
            by = c("cat_pais", "ncat_election_year")) 

df_complete <- df_complete %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_election_year) %>% 
  mutate(ciclo_electoral = ncat_eleccion) %>% 
  fill(ciclo_electoral, .direction = "up") %>%
  ungroup()

indicador_eeuu_full <- df_complete %>% 
  left_join(indicador_eeuu_full %>% 
              dplyr::rename("ncat_election_year" = "year") %>% 
              select(-c(winner, challenger, country)))

indicador_eeuu_full <- indicador_eeuu_full %>% 
  mutate(n_debates_usa_year = ifelse(is.na(n_debates_usa_year),
                                     0 , n_debates_usa_year))

indicador_eeuu <- indicador_eeuu_full %>% 
  group_by(cat_pais, ciclo_electoral) %>% 
  summarise(n_debates_usa_ciclo = sum(n_debates_usa_year),
            prop_elec_usa_ciclo = mean(dico_debates_usa_year, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dico_debates_usa_ciclo = ifelse(n_debates_usa_ciclo>0,1,0)) %>% 
  mutate(source_debates_usa_ciclo = "Cuenta con base en información disponible en: https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/presidential-campaigns-debates-and-endorsements-0") %>% 
  dplyr::rename("ncat_eleccion" = "ciclo_electoral") 

indicador_eeuu <- indicador_eeuu %>% 
  mutate(prop_elec_usa_ciclo = ifelse(is.na(prop_elec_usa_ciclo), 0,prop_elec_usa_ciclo))

indicador_eeuu <- data_base_elecciones %>% 
  left_join(indicador_eeuu)

indicador_eeuu <- indicador_eeuu %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, prop_elec_usa_ciclo, n_debates_usa_ciclo, dico_debates_usa_ciclo, source_debates_usa_ciclo )

##### LATAM debates ######
# copiado y adaptado de un archivo anterior

# EN MAING Y PEREZ LIÑAN : 
# In addition to global trends and hegemonic powers, changes in neighboring countries may affect the preferences and resources of domestic coalitions. We estimate the presence of a favorable regional environment using the average score in our democracy scale for the whole region (but excluding the country in question) during the previous year. The coding for this indicator is based on our trichotomous measure of democracy. The value of this variable can theoretically range from 0, if none of the other nineteen countries in the region were democratic in a given year, to 1 if all nineteen countries were democratic in that year. To compute this average, we gave semi-democratic countries a score of 0.5. We exclude the country in question and lag the variable to minimize problems of endogeneity. Therefore, the variable reflecting the regional environment is defined for any country i at time t as: (3.1) where Rit is the value of the regional indicator, Dt–1 is the proportion ofdemocracies in the region during the previous year, St–1 is the proportion of semi-democracies in the region during the previous year, and γit−1 is a correction term that acquires a value of 1/N if the country was democratic, and 1/(2N) if the country was semi-democratic during the previous year (i.e., excludes the country’s score if the regime was competitive during the past year). The second term (N / (N−1)) reweights the proportions to reflect the fact that the specific country was excluded from the denominator.
# FORMULA ES
# REGION = ( PROP ELECCIONES CON DEBATES AÑO ANTERIOR  - (DICO_DEBATES EN PAIS/N) ) * ( N / (N-1))

# necesito primero crear base anual para la region. para esta base anual, contar : cantidad de elecciones. cantidad de elecciones con debates. prop elecciones con debates

base_anual <- data_base_elecciones %>% 
  left_join(electyears %>%  select(-X)) %>% 
  group_by(ncat_election_year) %>% 
  dplyr::summarise(n_elecciones_en_año_region = n(),
                   n_elecciones_en_año_region_con_debates = sum(dico_debates_eleccion)) %>% 
  ungroup() %>% 
  dplyr::mutate(prop_elecciones_en_año_region_con_debates = n_elecciones_en_año_region_con_debates/n_elecciones_en_año_region)

base_elecciones_reducida <- electyears %>% 
  select(cat_pais, ncat_election_year) %>% 
  unique() %>% 
  mutate(previous_year = ncat_election_year -1 ) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_election_year) %>% 
  mutate(start_year = lag(ncat_election_year)) %>% 
  ungroup() %>% 
  mutate(start_year = ifelse(is.na(start_year), 
                             ncat_election_year - criterio,  # mismo criterio que aplicamos para calculo USA
                             start_year))

base_elecciones_reducida_año_anterior <- base_elecciones_reducida %>% 
  left_join(base_anual %>% dplyr::rename("previous_year" = "ncat_election_year", # debe haber forma más automatizada de hacer esto pero no se me ocurre ahora 
                                         "n_elecciones_año_anterior_region" = "n_elecciones_en_año_region",
                                         "n_elecciones_año_anterior_region_con_debates" =  "n_elecciones_en_año_region_con_debates",
                                         "prop_elecciones_año_anterior_region_con_debates" = "prop_elecciones_en_año_region_con_debates")) %>%
  mutate(n_elecciones_año_anterior_region = ifelse(is.na(n_elecciones_año_anterior_region),0, n_elecciones_año_anterior_region)) %>% 
  mutate(n_elecciones_año_anterior_region_con_debates = ifelse( n_elecciones_año_anterior_region==0, 0, n_elecciones_año_anterior_region_con_debates),
         prop_elecciones_año_anterior_region_con_debates  = ifelse( n_elecciones_año_anterior_region==0, 0, prop_elecciones_año_anterior_region_con_debates))

# Calculate average values for periods between elections # CODIGO DE CHATGPT NO FUNCA
df_avg_values <- base_elecciones_reducida %>%
  arrange(cat_pais, ncat_election_year) %>%
  #group_by(cat_pais) %>%
  #filter(row_number() > 1) %>%  # Remove the first row for each country
  rowwise() %>%
  mutate(
    avg_prop = base_anual %>%
      filter(ncat_election_year >= start_year & ncat_election_year <= previous_year) %>%
      summarize(avg_prop = mean(prop_elecciones_en_año_region_con_debates, na.rm = TRUE)) %>%
      pull(avg_prop)
  ) %>%
  select(cat_pais, ncat_election_year, avg_prop) %>%
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
  left_join(df_avg_values, by = c("cat_pais", "ncat_election_year")) %>% 
  left_join(base_elecciones_reducida_año_anterior) 

base_elecciones_reducida2 <- base_elecciones_reducida2 %>% 
  dplyr::rename("avgpropdebatesregionxciclo" = "avg_prop",
                "lagpropdebatesregion" = "prop_elecciones_año_anterior_region_con_debates",
                "lagndebatesregion" = "n_elecciones_año_anterior_region_con_debates")  %>% 
  select(cat_pais, ncat_election_year, 
         avgpropdebatesregionxciclo, lagpropdebatesregion, lagndebatesregion) %>% 
  mutate(avgpropdebatesregionxciclo = ifelse(is.na(avgpropdebatesregionxciclo),0,avgpropdebatesregionxciclo))
 
indicador_region <- electyears %>% 
  left_join(base_elecciones_reducida2) %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda,
         avgpropdebatesregionxciclo, lagpropdebatesregion, lagndebatesregion) %>% 
  mutate(source_region = "Calculo con base en data recabada para Franco H. (2022)")

##### LAGGED debates tradicion ######

calculo_lagged <- data_base_elecciones %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate(dico_debates_ciclo = ifelse(sum(dico_debates_eleccion)>0,1,0)) %>% 
  ungroup() %>% 
  subset(ncat_ronda==1) %>% 
  select(cat_pais, ncat_eleccion, dico_debates_ciclo) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(dico_debates_pastelection = lag(dico_debates_ciclo),
         cumsum_pastciclos = cumsum(dico_debates_ciclo)-dico_debates_ciclo) %>% 
  mutate(dico_2_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_debates_pastelection) == 2 , 1, 0)) %>% 
  mutate(dico_3_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_2_eleccondebatesseguidos) == 2 , 1, 0)) %>%
  mutate(dico_4_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_3_eleccondebatesseguidos) == 2 , 1, 0)) %>%
  mutate(dico_5_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_4_eleccondebatesseguidos) == 2 , 1, 0)) %>%
  mutate(dico_6_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_5_eleccondebatesseguidos) == 2 , 1, 0)) %>%
  mutate(dico_7_eleccondebatesseguidos = ifelse(lag(dico_debates_ciclo) + lag(dico_6_eleccondebatesseguidos) == 2 , 1, 0)) 

indicador_lagged <- data_base_elecciones %>% 
  left_join(calculo_lagged) %>% 
  mutate(source_lagged = "Calculo con base en data recabada para Franco H. (2022)")

indicador_lagged <- indicador_lagged %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, 
         dico_debates_pastelection,
         dico_2_eleccondebatesseguidos,
         dico_3_eleccondebatesseguidos,
         dico_4_eleccondebatesseguidos,
         dico_5_eleccondebatesseguidos,
         dico_6_eleccondebatesseguidos,
         dico_7_eleccondebatesseguidos, 
         cumsum_pastciclos, source_lagged )

### GURADADO INDICADORES chequeo y guardo lo disponible hasta ahora  #########################

summary(indicador_nec)
indicador_nec %>% write.csv("indicador_nec.csv")

summary(indicador_volatility)
indicador_volatility %>% write.csv("indicador_volatility.csv")

summary(indicador_alineamiento)
indicador_alineamiento %>% write.csv("indicador_alineamiento.csv")

summary(indicador_competitividad)
indicador_competitividad %>% write.csv("indicador_competitividad.csv")

summary(indicador_ntc)
indicador_ntc %>% write.csv("indicador_ntc.csv")

summary(indicador_incumbentes)
indicador_incumbentes %>% write.csv("indicador_incumbentes.csv")

summary(indicador_exapproval)
indicador_exapproval %>% write.csv("indicador_exapproval.csv")

summary(indicador_proptv)
indicador_proptv %>% write.csv("indicador_proptv.csv")

summary(indicador_accesomedios)
indicador_accesomedios %>% write.csv("indicador_accesomedios.csv")

summary(indicador_regulacion)
indicador_regulacion %>% write.csv("indicador_regulacion.csv")

summary(indicador_propinternet)
indicador_propinternet %>% write.csv("indicador_propinternet.csv")

summary(indicador_satisfaccion)
indicador_satisfaccion %>% write.csv("indicador_satisfaccion.csv")

summary(indicador_eeuu)
indicador_eeuu %>% write.csv("indicador_eeuu.csv")

summary(indicador_region)
indicador_region %>% write.csv("indicador_region.csv")

summary(indicador_lagged)
indicador_lagged %>% write.csv("indicador_lagged.csv")

### ARMADO DE BASE UNIFICADA INDICADORES #########################
data_base_elecciones <- read.csv("base_base_elecs.csv")  # creada en creacion_base_base.R

indicador_nec <- read.csv("indicador_nec.csv") %>% select(-X)
indicador_volatility <- read.csv("indicador_volatility.csv") %>% select(-X) %>% select(-ncat_ronda)
indicador_alineamiento <- read.csv("indicador_alineamiento.csv") %>% select(-X)
indicador_competitividad <- read.csv("indicador_competitividad.csv") %>% select(-X)
indicador_ntc <- read.csv("indicador_ntc.csv") %>% select(-X)
indicador_incumbentes <- read.csv("indicador_incumbentes.csv") %>% select(-X)
indicador_exapproval <- read.csv("indicador_exapproval.csv") %>% select(-X)
indicador_proptv <- read.csv("indicador_proptv.csv") %>% select(-X)
indicador_accesomedios <- read.csv("indicador_accesomedios.csv") %>% select(-X)
indicador_regulacion <- read.csv("indicador_regulacion.csv") %>% select(-X)
indicador_propinternet <- read.csv("indicador_propinternet.csv") %>% select(-X)
indicador_satisfaccion <- read.csv("indicador_satisfaccion.csv") %>% select(-X)
indicador_eeuu <- read.csv("indicador_eeuu.csv") %>% select(-X)
indicador_region <- read.csv("indicador_region.csv") %>% select(-X)
indicador_lagged <- read.csv("indicador_lagged.csv")

base_unificada <- data_base_elecciones %>% 
  left_join(indicador_nec) %>%
  left_join(indicador_volatility) %>%
  left_join(indicador_alineamiento) %>%
  left_join(indicador_competitividad) %>%
  left_join(indicador_ntc) %>%
  left_join(indicador_incumbentes) %>%
  left_join(indicador_exapproval) %>%
  left_join(indicador_proptv) %>%
  left_join(indicador_accesomedios) %>%
  left_join(indicador_regulacion) %>%
  left_join(indicador_propinternet) %>%
  left_join(indicador_satisfaccion) %>%
  left_join(indicador_eeuu) %>%
  left_join(indicador_region) %>% 
  left_join(indicador_lagged)
  
summary(base_unificada)

base_unificada %>% write.csv("indicadores_elecciones.csv")



######################################################################################################################
######################################################################################################################
# CONTROLES NIVEL ELECCION #####
##### Desarrollo/ PBI per Capita ######

# (WDI) data GDP per capita (constant 2010 US$)
data_gdp <- data_gdp %>% 
  mutate(source_gdpxcapita2 = paste(source_gdpxcapita2, "- WDI Indicators - GDP per capita (constant 2010 US$)" ))

control_gdp <- data_gdp %>% 
  select(-X)


##### Desarrollo/ Urbanizacion ######
summary(qof_controles_desarrollo)

controles_desarrollo <- qof_controles_desarrollo %>% 
  select(-X) %>% 
  dplyr::rename("urbanpop" = "new_wdi_popurb",
                "source_urbanpop" = "source_wdi_popurb",
                "undphdi" = "new_undp_hdi",
                "source_undphdi" = "source_undp_hdi")

##### Desarrollo/ N educativo promedio ######

##### Sist pol/ Turnout ######


control_turnout <- qof_controles_turnout %>% 
  select(-X) %>% 
  dplyr::rename("turnout" = "ideavt_presvt",
                "source_turnout" = "source_ideavt_presvt")

missing_turnout <- control_turnout %>% # completo manualmente missing data
  subset(is.na(turnout)) %>% 
  mutate(turnout = ifelse(cat_pais=="Argentina"&ncat_eleccion==2023&ncat_ronda==1,
                          77.04,
                          ifelse(cat_pais=="Argentina"&ncat_eleccion==2023&ncat_ronda==2,
                                 76.4,
                                 turnout))) %>% 
  mutate(source_turnout = ifelse(is.na(source_turnout)&!is.na(turnout),
                                 "Web Chequeado: https://chequeado.com/el-explicador/elecciones-2023-la-participacion-fue-de-al-menos-el-74-la-mas-baja-en-una-eleccion-presidencial-desde-1983/",
                                 source_turnout)) %>% 
  mutate(turnout = ifelse(cat_pais=="Colombia"&ncat_eleccion==1990&ncat_ronda==1,
                          43.50,
                          ifelse(cat_pais=="Colombia"&ncat_eleccion==1994&ncat_ronda==1,
                                 33.95,
                                 ifelse(cat_pais=="Colombia"&ncat_eleccion==1994&ncat_ronda==2,
                                        43.32,
                                        ifelse(cat_pais=="Colombia"&ncat_eleccion==1998&ncat_ronda==1,
                                               51.55,
                                               ifelse(cat_pais=="Colombia"&ncat_eleccion==1998&ncat_ronda==2,
                                                      58.76,
                                                      turnout)))))) %>% 
  mutate(source_turnout = ifelse(is.na(source_turnout)&!is.na(turnout),
                                 "Web IDEA: https://www.idea.int/data-tools/data/question-country?question_id=9189&country=49&database_theme=293 ",
                                 source_turnout))   

control_turnout <-control_turnout %>% 
  subset(!is.na(turnout)) %>% 
  rbind(missing_turnout)

##### Sist pol/ Clausula runoff, concurrent elections y compulsory voting ######
# runoff: de mainw
# conc_elec de mainw
# ideavt_prescv de Qof 

# paso para unir la data (copiado identico de arriba)
data_mainw_elecciones_tojoin <- data_mainw_elecciones %>% 
  dplyr::rename( "mayus_eng_cat_pais" = "country", 
                 "ncat_election_year" = "election_year" ) 

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

control_sistelect <- check_elecciones3 %>% 
  select(cat_pais, ncat_eleccion, runoff, conc_elec) %>% 
  mutate(source_runoff = ifelse(!is.na(runoff), "Mainwaring & Su (2021) - runoff", NA) ,
         source_concelec = ifelse(!is.na(conc_elec), "Mainwaring & Su (2021) - conc_elec", NA)  ) %>% 
  dplyr::rename("concelec" = "conc_elec")

control_sistelect <- data_base_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  left_join(control_sistelect)

compulsoryvoting <- qof_controles_ohtersregime %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, ideavt_prescv, source_ideavt_prescv) %>% 
  dplyr::rename("compulsoryvoting" = "ideavt_prescv",
                "source_compulsoryvoting" = "source_ideavt_prescv")

control_sistelect <- control_sistelect %>% 
  left_join(compulsoryvoting)

##### Sist pol/ Polarizacion ######
# polar de mainw 
control_polarizacion <- check_elecciones3 %>% 
  select(cat_pais, ncat_eleccion, polar) %>% 
  mutate(source_polar = ifelse(!is.na(polar), "Mainwaring & Su (2021) - runoff", NA))

control_polarizacion <- data_base_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  left_join(control_polarizacion)

# ver si data DLUJAN hace diferencia #ojo en caso afirmativo habria que ver como esta medida 
# GANO unos 8 puntos de datos pero la diferencia de medidas parece incompatible
# 
# data_dlujan_elecciones$polarizdiffideo
# 
# # primero uno data
# # creo variables para unir  
# data_dlujan_elecciones_tojoin <- data_dlujan_elecciones %>% 
#   mutate(ncat_ronda = 1,
#          cat_pais =  iconv(pais, to = "ASCII//TRANSLIT") %>%  str_trim() %>% str_replace("R. Dominicana", "Republica Dominicana"),
#          ncat_eleccion = id_eleccion)  
# 
# # descarto variables menos relevantes de momento
# data_dlujan_elecciones_tojoin <- data_dlujan_elecciones_tojoin %>% 
#   select(-c(pol_coppedge, pol_handlin,	pol_singer, federalismo_Gerring, reglaelectoral, anio, id_eleccion, nepl_golder))
# 
# # unimos
# check_elecciones <- data_base_elecciones %>% 
#   left_join(data_dlujan_elecciones_tojoin)
# 
# # ahora veo compatibildad
# data_polarizacion_lujan <- check_elecciones %>%
#   select(cat_pais, ncat_eleccion, ncat_ronda, polarizdiffideo)
# 
# control_polarizacion2 <- control_polarizacion %>%
#   left_join(data_polarizacion_lujan)
#  
# summary(control_polarizacion2)
# 
# control_polarizacion2 <- control_polarizacion2 %>%
#   mutate(polarizacion = ifelse(is.na(polar), polarizdiffideo, polar)) %>%
#   mutate(source_polarizacion = ifelse(is.na(source_polar)&!is.na(polarizacion),
#                                     "Luján (2020)",
#                                     source_polar))
# 
# summary(control_polarizacion2)

### Regimen/ Duracion del regimen ######

# age_democracy de mainw
# pendiente: crear una base filled con bmr_demdur. excluir años > 100. 

control_edadregimen <- qof_controles_ohtersregime %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda,
         p_durable, source_p_durable,
         bmr_demdur, source_bmr_demdur) %>% 
  dplyr::rename("edadregimenppolity" = "p_durable",
                "source_edadregimenppolity" = "source_p_durable",
                "edadregimenbmr" = "bmr_demdur",
                "source_edadregimenbmr" = "source_bmr_demdur") #Boix-Miller_Rosato Dichotomous Coding

control_edadregimen2 <- check_elecciones3 %>% 
  select(cat_pais, ncat_eleccion, age_demo) %>% 
  dplyr::rename("edadregimenmainw" = "age_demo") %>% 
  mutate(source_edadregimenmainw = "Mainwaring & Su (2021) - age_demo")

control_edadregimen <- control_edadregimen %>% 
  left_join(control_edadregimen2)

control_edadregimen3 <- control_edadregimen %>% 
  subset(ncat_ronda==1) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(edadregimenfilled = ifelse(is.na(edadregimenbmr),
           lag(edadregimenbmr) + ncat_eleccion - lag(ncat_eleccion),
           edadregimenbmr) ) %>% 
  mutate(edadregimenfilled = ifelse(is.na(edadregimenfilled),
                                    lag(edadregimenfilled) + ncat_eleccion - lag(ncat_eleccion),
                                    edadregimenfilled) ) %>%  # solo aplica a un caso, el de ecuador
  ungroup() %>% 
  mutate(source_edadregimenfilled = ifelse(is.na(edadregimenbmr)&!is.na(source_edadregimenbmr),
                                    source_edadregimenbmr,
                                    "Proyectado con base en datos Q of G, bmr_demdur")) %>% 
  mutate(edadregimenfilled = ifelse(edadregimenfilled>100, NA, edadregimenfilled)) %>%  # una alternativa seria puntuar con 0
  mutate(source_edadregimenfilled = ifelse(is.na(edadregimenfilled),
                                           "Dato perdido a propósito. Elección considerada híbrida por bmr_demdur",
                                           edadregimenfilled)) 

control_edadregimen <- control_edadregimen %>% 
  left_join(control_edadregimen3 %>% 
              select(cat_pais, ncat_eleccion, edadregimenfilled, source_edadregimenfilled))

##### Regimen / Calidad del regimen ######

control_democracia <- qof_controles_democracia %>% 
  select(-X) %>% 
  dplyr::rename(democraciappolity = new_p_polity2,
                democraciavdempolyarchy = new_vdem_polyarchy,
                democraciavdemelectoralcomp = new_vdem_edcomp_thick,
                source_democraciappolity = source_p_polity2,
                source_democraciavdempolyarchy = source_vdem_edcomp_thick,
                source_democraciavdemelectoralcomp = source_vdem_edcomp_thick)

##### Sist medios/ Media bias ######

control_medios <- qof_controles_mediaquality %>% 
  select(-X) %>% 
  left_join(data_controles_media_vdem %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, 
                     v2mebias, v2merange, source_media)) %>% 
  dplyr::rename(mediaqualityfreedombti = new_bti_foe,
                mediaqualityconftvwvs = new_wvs_conftv,
                mediaqualitycorruptvdem = new_vdem_mecorrpt,
                mediaqualitybiasnelda = new_nelda_mbbe,
                mediaqualitybiasvdem = v2mebias,
                mediaqualityperspectivesvdem = v2merange,
                source_mediaqualityfreedombti = source_bti_foe,
                source_mediaqualityconftvwvs = source_wvs_conftv,
                source_mediaqualitycorruptvdem = source_vdem_mecorrpt,
                source_mediaqualitybiasnelda = source_nelda_mbbe,
                source_mediaqualitybiasvdem = source_media) %>% 
  mutate(source_mediaqualityperspectivesvdem = source_mediaqualitybiasvdem)


### GURADADO CONTROLES chequeo y guardo lo disponible hasta ahora  #########################

summary(control_gdp)
control_gdp %>% write.csv("control_gdp.csv")

summary(controles_desarrollo)
controles_desarrollo %>% write.csv("control_desarrollo.csv")

summary(control_sistelect)
control_sistelect %>% write.csv("control_sistelect.csv")

summary(control_polarizacion)
control_polarizacion %>% write.csv("control_polarizacion.csv")

summary(control_edadregimen) 
control_edadregimen %>% write.csv("control_edadregimen.csv")

summary(control_democracia) 
control_democracia %>% write.csv("control_democracia.csv")

summary(control_medios)
control_medios %>%write.csv("control_medios.csv")

summary(control_turnout)
control_turnout %>%write.csv("control_turnout.csv")


### ARMADO DE BASE UNIFICADA CONTROLES #########################
data_base_elecciones <- read.csv("base_base_elecs.csv")  # creada en creacion_base_base.R


control_gdp <- read.csv("control_gdp.csv") %>% select(-X)
control_desarrollo <- read.csv("control_desarrollo.csv") %>% select(-X)
control_sistelect <- read.csv("control_sistelect.csv") %>% select(-X)
control_polarizacion <- read.csv("control_polarizacion.csv") %>% select(-X)
control_edadregimen <- read.csv("control_edadregimen.csv") %>% select(-X)
control_democracia <- read.csv("control_democracia.csv") %>% select(-X)
control_medios <- read.csv("control_medios.csv") %>% select(-X)
control_turnout <- read.csv("control_turnout.csv") %>% select(-X)

base_unificada_controles <- data_base_elecciones %>% 
  left_join(control_gdp) %>%
  left_join(control_desarrollo) %>%
  left_join(control_sistelect) %>%
  left_join(control_polarizacion) %>%
  left_join(control_edadregimen) %>%
  left_join(control_democracia) %>%
  left_join(control_medios) %>%
  left_join(control_turnout) 

summary(base_unificada_controles)

base_unificada_controles %>% write.csv("controles_elecciones.csv")

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
##### chequeo de missings ########################################

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

####### paso a explorar data candidatos ####################################

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


