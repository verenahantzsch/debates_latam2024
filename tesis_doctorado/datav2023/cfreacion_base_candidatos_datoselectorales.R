
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

data_incumbentes <-  read.csv("base_incumbentes_oficialistas.csv")

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

# ajuste general para no trabajar con data duplicada
data_base_candidatos_tojoin <- data_base_candidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos) %>% 
  subset(nombres_candidatos!="sin ausencias conocidas") %>% 
  subset(nombres_candidatos!="sin datos") %>% 
  unique()  

##### votos obtenidos #####

data_voteshare_tojoin <- data_fried_seaw_caro_candidatos %>% 
  dplyr::rename("nombres_candidatos" = "candidate_name") %>% 
  select(-c(dico_debates_eleccion, party_name, status, eng_cat_pais, ISO, mayus_eng_cat_pais)) %>% 
  mutate(nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT") %>%  str_trim())

data_base_candidatos_tojoin <- data_base_candidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos) %>% 
  subset(nombres_candidatos!="sin ausencias conocidas") %>% 
  subset(nombres_candidatos!="sin datos") %>% 
  unique()  
  
indicador_voteshare <- data_base_candidatos_tojoin %>% 
  left_join(data_voteshare_tojoin)

# chequeos
desunidos_voteshare <- data_voteshare_tojoin %>%
  anti_join(data_base_candidatos_tojoin)
missing_voteshare <- indicador_voteshare %>% 
  subset(is.na(vote_share))

# ultimo retoque

indicador_voteshare <- indicador_voteshare %>% 
  mutate(source_voteshare = paste(source_str_vote_share, source_url_vote_share, sep = " ")) %>% 
  select(-c(source_str_vote_share, source_url_vote_share, compilador_vote_share))

indicador_voteshare <- indicador_voteshare %>%   
  dplyr::rename("voteshare" = vote_share)

##### inccumbencia #####
# por como esta guardada la data hay que trabajar primera y segunda ronda por separado

data_incumbentes_firstround_tojoin <- data_incumbentes %>% 
  subset(ncat_ronda==1) %>% 
  #select(-ncat_ronda) %>% 
  select(-dico_debates_eleccion) %>% 
  select(-eng_cat_pais) #%>% 
#select(-X)

indicador_incumbentes_firstround <- data_base_candidatos_tojoin %>% 
  subset(ncat_ronda==1) %>% 
  left_join(data_incumbentes_firstround_tojoin) # hacemos esto porque en la base de incumbentes solo registramos las primeras rondas

indicador_incumbentes_firstround <- indicador_incumbentes_firstround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos, nombre_presidente, nombre_oficialista)) %>% 
  mutate(dico_reeleccion = ifelse(nombres_candidatos==nombre_presidente, 1,0)) %>% 
  mutate(dico_oficialista = ifelse(nombres_candidatos==nombre_oficialista,1,0))

indicador_incumbentes_firstround <- indicador_incumbentes_firstround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos, dico_reeleccion, dico_oficialista))

# ahora agregamos segundas rondas
data_incumbentes_secondround_tojoin <- data_incumbentes %>% 
  subset(!(cat_pais=="Chile"&ncat_eleccion==2017&ncat_ronda==1)) %>% 
  select(-ncat_ronda) %>% 
  select(-dico_debates_eleccion) %>% 
  select(-eng_cat_pais)# %>% 
#select(-X)

data_incumbentes_secondround <- data_base_candidatos_tojoin %>% 
  subset(ncat_ronda==2) %>% 
  left_join(data_incumbentes_secondround_tojoin)  

indicador_incumbentes_secondround <- data_incumbentes_secondround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos, nombre_presidente, nombre_oficialista)) %>% 
  mutate(dico_reeleccion = ifelse(nombres_candidatos==nombre_presidente, 1,0)) %>% 
  mutate(dico_oficialista = ifelse(nombres_candidatos==nombre_oficialista,1,0)) 

indicador_incumbentes_secondround <- indicador_incumbentes_secondround %>% 
  select(c(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos, dico_reeleccion, dico_oficialista))


indicador_incumbentes <- indicador_incumbentes_firstround %>% 
  rbind(indicador_incumbentes_secondround) 

indicador_incumbentes <- indicador_incumbentes %>% 
  mutate(source_incumbencystatus = "Elaboración propia con base en fuentes secundarias")
  
# guardado ############

summary(indicador_voteshare)
indicador_voteshare %>% write_csv("indicador2_voteshare.csv")

summary(indicador_incumbentes)
indicador_incumbentes %>% write_csv("indicador2_incumbentes.csv")



