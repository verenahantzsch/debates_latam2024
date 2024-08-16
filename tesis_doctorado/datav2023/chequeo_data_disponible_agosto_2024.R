# CHEQUEO DE DATA DISPONIBLE A AGOSTO 2024 #################################

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
setwd("/home/carolina/Documents/dataexterna")
countrynames <- read.csv("countrynames.csv") %>% 
  mutate(cat_pais =  iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim())

# data para ir viendo que tenemos. APARTE durante la primera parte de este año fuimos trabajando con datos de encuestas. los cargo abajo pero no son la prioridad ahora
#url_fried_seaw_caro_candidatos <- "https://docs.google.com/spreadsheets/d/1PvNGVC8qYMGTcAVrEHtBaO1Ccv-kTZO58ALNBv95p6U/edit?gid=0#gid=0" # versiones originales y backups de esta data en dataexterna
#data_fried_seaw_caro_candidatos <- read_sheet(url_fried_seaw_caro_candidatos)
# versiones originales y backups de esta data en dataexterna
setwd("/home/carolina/Documents/dataexterna")
data_fried_seaw_caro_candidatos <- read.csv("combined_candidates_presdata_friedenberg_seawright_caro.csv")
data_fried_seaw_caro_candidatos <- data_fried_seaw_caro_candidatos %>% 
  left_join(countrynames)  

#url_dlujan_elecciones <- "https://docs.google.com/spreadsheets/d/1h_IMQ7o9vUqL97gGSLEXN83tykmOEfuBASikBKWrtHs/edit?gid=730500075#gid=730500075" # versiones originales y backups de esta data en dataexterna 
#data_dlujan_elecciones <- read_sheet(url_dlujan_elecciones)
# versiones originales y backups de esta data en dataexterna
setwd("/home/carolina/Documents/dataexterna")
data_dlujan_elecciones <- read.csv("Base_elecciones_presidenciales_AL_Luján.csv")

# datos encuestas
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

data_encuestas_elecciones <-read.csv("all_elections_full_dataset.csv") %>% select(-X)
data_encuestas_candidatos <- read.csv("base_candidatos_matcheados2023.csv")


# DATA POR ELECCION #####
# vamos paso por paso. primero vamos a ver que datos de los importantes tenemos para el modelo 1 OBS POR ELECCION

# join de bases disponibles ##########################

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


# vemos si hay data de la base original que por algun motivo NO se unio y eventualmente corregimos manualmente
# queremos ver si hay tantos puntos de datos como filas en base original 
# por lo menos en los que respecta a las variables de id de la base original
# corregimos problema usual de elecciones chile y guatemala. y corregimos tema de acentos para unir
table(data_dlujan_elecciones$pais)
table(check_elecciones$pais %>% ifelse(is.na(.), "MISSING", .))
table(check_elecciones$cat_pais)
nrow(check_elecciones) - 120 == nrow(data_dlujan_elecciones)  
desunidos_dlujan <- data_dlujan_elecciones_tojoin %>% 
  anti_join(data_base_elecciones)


# chequeo anual con base en otra fuente: margen de victoria
# con base en data fried_seaw_caro de candidatos, hago el calculo

data_margen_victoria <- data_fried_seaw_caro_candidatos %>% 
  mutate(vote_share = as.numeric(gsub(",", ".", vote_share))) %>% 
  group_by(eng_cat_pais, ncat_eleccion, ncat_ronda) %>% 
  arrange(desc(vote_share)) %>% 
  slice(1:2) %>% 
  summarise(marginvic = diff(vote_share)) %>% 
  ungroup() %>% 
  left_join(countrynames) %>%  # acomodo algunas cuestiones para poder unir
  mutate(cat_pais = iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim() %>% str_replace("Rep. Dominicana", "Republica Dominicana"))


# join
check_elecciones2 <- data_base_elecciones %>% 
  select(-eng_cat_pais) %>% 
  left_join(data_margen_victoria, by = join_by(cat_pais, ncat_eleccion, ncat_ronda))

desunidos_margen <- data_margen_victoria %>% 
  select(-eng_cat_pais) %>% 
  anti_join(data_base_elecciones, by = join_by(cat_pais, ncat_eleccion, ncat_ronda))

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
