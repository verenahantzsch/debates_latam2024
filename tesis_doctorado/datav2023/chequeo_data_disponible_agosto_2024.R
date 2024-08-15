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

# data para ir viendo que tenemos. APARTE durante la primera parte de este año fuimos trabajando con datos de encuestas. los cargo abajo pero no son la prioridad ahora
#url_fried_seaw_caro_candidatos <- "https://docs.google.com/spreadsheets/d/1PvNGVC8qYMGTcAVrEHtBaO1Ccv-kTZO58ALNBv95p6U/edit?gid=0#gid=0" # versiones originales y backups de esta data en dataexterna
#data_fried_seaw_caro_candidatos <- read_sheet(url_fried_seaw_caro_candidatos)
# versiones originales y backups de esta data en dataexterna
setwd("/home/carolina/Documents/dataexterna")
data_fried_seaw_caro_candidatos <- read.csv("combined_candidates_presdata_friedenberg_seawright_caro.csv")


#url_dlujan_elecciones <- "https://docs.google.com/spreadsheets/d/1h_IMQ7o9vUqL97gGSLEXN83tykmOEfuBASikBKWrtHs/edit?gid=730500075#gid=730500075" # versiones originales y backups de esta data en dataexterna 
#data_dlujan_elecciones <- read_sheet(url_dlujan_elecciones)
# versiones originales y backups de esta data en dataexterna
data_dlujan_elecciones <- read.csv("Base_elecciones_presidenciales_AL_Luján.csv")

# data auxiliar
countrynames <- read.csv("countrynames.csv")

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

# datos encuestas
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
