
# librereias ######################

library(tidyverse)
library(googlesheets4)

# wd + data ##################
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
data_base_elecciones <- read.csv("base_base_elecs.csv")  # creada en creacion_base_base.R
data_debates <- read_csv("base_final3v2023.csv") %>% select(-...1, -...2)

base_debates <- data_debates %>% 
  subset(dico_certidumbre==1)

setwd("/home/carolina/Documents/dataexterna")
data_fried_seaw_caro_candidatos <- read.csv("combined_candidates_presdata_friedenberg_seawright_caro2.csv")

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")


# variable dependiente alternativa: dico debates x medios  #######
debates_medios <- base_debates %>% 
  subset(dico_org_mmc==T) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(dico_hubo_debate_mediatico = 1) %>% 
  ungroup()

# variable dependiente alternativa: dico debates c frontrunner  #######

primerosdos <- data_fried_seaw_caro_candidatos %>% 
  mutate(vote_share= as.numeric(vote_share %>%  str_replace(",","."))) %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  arrange(desc(vote_share), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  mutate(order = row_number()) %>% 
  ungroup() %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, candidate_name, order) %>% 
  mutate(candidate_name = iconv(candidate_name, 
                                to = "ASCII//TRANSLIT") 
         %>%  str_trim()) %>% 
  pivot_wider(names_from = order, values_from = candidate_name)

debates_primerosdos <- base_debates %>% 
  mutate(nombres_candidatos_presentes = iconv(str_presentes, 
                                              to = "ASCII//TRANSLIT") 
         %>%  str_trim()) %>% 
  left_join(primerosdos) %>% 
  dplyr::rename("primero" = "1", "segundo" = "2") %>% 
  mutate(winner_presente = ifelse(str_detect(nombres_candidatos_presentes, primero),1,0),
         challenger_presente = ifelse(str_detect(nombres_candidatos_presentes, segundo),1,0)) %>% 
  mutate(winnerorchallenger = ifelse(winner_presente==1|challenger_presente==1,1,0)) %>% 
  select(id_debate, cat_pais, ncat_eleccion, ncat_ronda,
         str_organizador, str_presentes, str_ausentes,
         primero, segundo,
         winner_presente, challenger_presente, winnerorchallenger)
 

elecciones_primerosdos <- debates_primerosdos %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(n_debates_primerosdos = sum(winnerorchallenger),
            dico_debates_primerosdos = ifelse(n_debates_primerosdos!=0,1,0)) %>% 
  ungroup()


# variable dependiente estándar: dico_hubo_debates #######

elecciones_con_debates <- base_debates %>% 
  group_by(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  summarise(dico_hubo_debates = 1) %>% 
  ungroup()


# uno y guardo  #######

variables_dependientes <- data_base_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  left_join(debates_medios) %>% 
  left_join(elecciones_primerosdos %>% select(cat_pais, ncat_eleccion, ncat_ronda, dico_debates_primerosdos)) %>% 
  left_join(elecciones_con_debates) %>% 
  mutate(dico_hubo_debate_mediatico = ifelse(is.na(dico_hubo_debate_mediatico),0,dico_hubo_debate_mediatico),
         dico_debates_primerosdos = ifelse(is.na(dico_debates_primerosdos),0,dico_debates_primerosdos),
         dico_hubo_debates = ifelse(is.na(dico_hubo_debates),0,dico_hubo_debates))

# guardo
summary(variables_dependientes)
variables_dependientes %>% write.csv("variables_dependientes_elecciones.csv")