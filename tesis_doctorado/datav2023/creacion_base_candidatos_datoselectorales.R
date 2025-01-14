
# pendientes: ########################
# - LISTO   indicadores a nivel de debate: tipo de organizador. Se puede hacer con datos dipsponibles. Es un join y 3 dico: OSCUNIV - ESTADO -MEDIOS 
# -indicador a nivel del candidato: participacion en campañas anteriores. No tenemos datos suficientes necesariamente ya que no hemos registrado datos para todos los candidatos para todas las elecciones cuando no hubo debates. Ver
# - LISTO    prop inasistencias pasadas 

# librereias ######################

library(tidyverse)
library(googlesheets4)

# wd ##################
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")


# carga data ##############

# data basica propia
data_base_elecciones <- read.csv("base_base_elecs.csv")  # creada en creacion_base_base.R
data_base_candidatos <- readxl::read_xlsx("all_candidates.xlsx") # esta base fue creada en segunda_limpieza_datos y es usada en creacion_base_candidatos1
data_debates <- read_csv("base_final3v2023.csv") %>% select(-...1, -...2)

# data auxiliar

countrynames <- read.csv("countrynames.csv") %>% 
  mutate(cat_pais =  iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim()) 

electyears <- read.csv("electyears.csv")

# data desagregada a nivel de candidatos
setwd("/home/carolina/Documents/dataexterna")
data_fried_seaw_caro_candidatos <- read.csv("combined_candidates_presdata_friedenberg_seawright_caro2.csv")
data_fried_seaw_caro_candidatos <- data_fried_seaw_caro_candidatos %>% 
  left_join(countrynames)  

data_fried_seaw_caro_candidatos <- data_fried_seaw_caro_candidatos %>% 
  mutate(vote_share = as.numeric(gsub(",", ".", vote_share)))

data_incumbentes <-  read.csv("base_incumbentes_oficialistas.csv")

data_vdem  <-  read.csv("VDEM/df_candidates_vdem.csv")

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
  select(-c(dico_debates_eleccion, party_name, status, eng_cat_pais, ISO)) %>% 
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

##### incumbencia #####
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

##### invitaciones #####
# -Crear indicador de cantidad de invitations en current cycle
# maybe crear su version ln()??

data_invitaciones <- data_base_candidatos %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos) %>% 
  summarise(ninvitaciones = n()) %>% 
  ungroup()

indicador_invitaciones <- data_invitaciones %>% 
  mutate(source_invitaciones = "Elaboracion propia con base en Franco Hantzsch (2022)") 

indicador_invitaciones <- indicador_invitaciones %>% 
  subset(nombres_candidatos!="sin ausencias conocidas") %>% 
  subset(nombres_candidatos!="sin datos")

##### lagged participations/ausencias #####
# -Crear indicador de cantidad de participaciones pasadas
# -Crear indicador de cantidad de ausencias pasadas

data_anual <- data_base_candidatos %>% 
  group_by(cat_pais, ncat_eleccion, nombres_candidatos) %>% 
  summarise(n_invitaciones = n(),
            n_presencias = sum(dico_candidato_presente),
            n_ausencias = n_invitaciones - n_presencias) %>% 
  ungroup() %>% 
  subset(nombres_candidatos!="sin ausencias conocidas") %>% 
  subset(nombres_candidatos!="sin datos") %>% 
  group_by(cat_pais, nombres_candidatos) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(cumsum_presencias = cumsum(n_presencias),
         cumsum_ausencias = cumsum(n_ausencias),
         cumsum_invitaciones = cumsum(n_invitaciones)) %>% 
  mutate(lag_presencias = lag(cumsum_presencias),
         lag_ausencias = lag(cumsum_ausencias),
         lag_invitaciones = lag(cumsum_invitaciones)) %>% 
  ungroup()

indicador_participaciones <- data_anual %>% 
  mutate(npresenciaspasadas = ifelse(is.na(lag_presencias), 0, lag_presencias),
         nausenciaspasadas = ifelse(is.na(lag_ausencias), 0, lag_ausencias),
         ninvitacionespasadas = ifelse(is.na(lag_invitaciones), 0, lag_invitaciones)) %>% 
  mutate(dicopresenciaspasadas = ifelse(npresenciaspasadas==0, 0, 1),
         dicoausenciaspasadas = ifelse(nausenciaspasadas==0, 0, 1),
         propausenciaspasadas = nausenciaspasadas/ninvitacionespasadas) %>% 
  mutate(propausenciaspasadasfilled = ifelse(is.na(propausenciaspasadas),0,1)) %>% 
  select(cat_pais, ncat_eleccion, nombres_candidatos, 
         npresenciaspasadas, nausenciaspasadas,
         dicopresenciaspasadas, dicoausenciaspasadas,
         propausenciaspasadas, propausenciaspasadasfilled) %>% 
  mutate(source_participaciones = "Elaboracion propia con base en Franco Hanztsch (2023)")   
  
### vdem partylevel data ##################

data_vdem_tojoin <- data_vdem %>% 
  select(-X) %>% 
  dplyr::rename("nombres_candidatos" = candidate_name) %>% 
  mutate(nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT") %>%  str_trim())

indicadores_partylevel <- data_base_candidatos_tojoin %>% 
  left_join(data_vdem_tojoin) %>% 
  select(-ncat_election_year,-party_name)

summary(indicadores_partylevel)

# guardado ############

summary(indicador_voteshare)
indicador_voteshare %>% write_csv("indicador2_voteshare.csv")

summary(indicador_incumbentes)
indicador_incumbentes %>% write_csv("indicador2_incumbentes.csv")

summary(indicador_invitaciones)
indicador_invitaciones %>% write_csv("indicador2_invitaciones.csv")

summary(indicador_participaciones)
indicador_participaciones %>% write.csv("indicador2_participaciones.csv")

summary(indicadores_partylevel)
indicadores_partylevel %>% write.csv("indicador2_partylevel.csv")


# debate nivel indicador ######################

base_debates_tojoin <- data_debates %>% 
  select(id_debate, dico_org_educ, dico_org_estado,
         dico_org_mmc, dico_org_mmp, dico_org_osc) %>% 
  unique() %>% 
  mutate(orgosc = ifelse(dico_org_educ==T|dico_org_osc==T,1,0),
         orgestado = ifelse(dico_org_estado==T|dico_org_mmp==T,1,0),
         orgmmc = ifelse(dico_org_mmc==T,1,0))

tipo_debate <- base_debates_tojoin %>% 
  select(id_debate, orgosc, orgestado, orgmmc) %>% 
  mutate(source_orgdebate = "Elaboracion propia con base en Franco Hanztsch (2023)")



# unifico base #########################
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
data_base_candidatos <- readxl::read_xlsx("all_candidates.xlsx") # esta base fue creada en segunda_limpieza_datos y es usada en creacion_base_candidatos1

data_base_candidatos_clean <- data_base_candidatos %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, nombres_candidatos, dico_candidato_presente, id_debate) %>% 
  subset(nombres_candidatos!="sin ausencias conocidas") %>% 
  subset(nombres_candidatos!="sin datos") 

indicador2_voteshare <- read.csv("indicador2_voteshare.csv") #%>% select(-X)
indicador2_incumbentes <- read.csv("indicador2_incumbentes.csv") #%>% select(-X)
indicador2_invitaciones <- read.csv("indicador2_invitaciones.csv") #%>% select(-X)
indicador2_participaciones <- read.csv("indicador2_participaciones.csv") %>% select(-X)
indicador2_partylevel <- read.csv("indicador2_partylevel.csv") %>% select(-X)

indicadores_candidatos <- data_base_candidatos_clean %>% 
 left_join(indicador2_voteshare) %>% 
  left_join(indicador2_incumbentes) %>%
  left_join(indicador2_invitaciones) %>%
  left_join(indicador2_participaciones) %>%
  left_join(indicador2_partylevel) %>% 
  left_join(tipo_debate, by= join_by(id_debate))

summary(indicadores_candidatos)

indicadores_candidatos %>% write.csv("indicadores_candidatos.csv")

