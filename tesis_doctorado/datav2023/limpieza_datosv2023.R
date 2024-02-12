### ESTE SCRIPT ES PARA UNA PRIMERA LIMPIEZA DE DATOS Y CREACION DE VARIABLES RELEVANTES #####

# LIBRERIAS #####
library(tidyverse)


# DATOS PROPIOS #####

setwd("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado/datav2023")

# base de excel original
data_base_sucia <- readxl::read_xlsx("base_debates_limpiav2023.xlsx", sheet = "debates_agregado")
data_anual_sucia <- readxl::read_xlsx("base_debates_limpiav2023.xlsx", sheet = "debates_normativo")


#setwd("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado")

# peq funcion de ayuda
saltear_NAs <-  function(cadena, operacion){
  
  ifelse(!str_detect(cadena, "NA"), operacion, "NA")
  
}

# nueva base ####################3

# creo id de deabate
data_base_limpia <- data_base_sucia %>% rowid_to_column("id_debate")

data_base_limpia <- data_base_limpia %>% 
  # descarto algunos pocos debates que no aplican
  subset(is.na(dico_precandidaturas)|dico_precandidaturas==FALSE) %>% 
  # primeros cambios
  dplyr::rename("n_presentes2" = n_presentes, "n_duracion" = n_mins) %>% 
  mutate(dico_analytics = ifelse(str_detect(tolower(longstr_formato), "analytics")|dico_analytics==1,
                                        1,
                                        0),
         t_fecha =  openxlsx::convertToDate(t_fecha),
         t_hora =  openxlsx::convertToDateTime(t_hora))  %>% 
  mutate(dico_analytics = ifelse(is.na(dico_analytics),
                                         0,
                                dico_analytics)) %>% 
 # algunas cuentas de cantidades 
  mutate(n_strorganizador = ifelse(str_organizador=="NA","NA",
                                   str_count(str_organizador, ";")+1),
         n_presentes = ifelse(str_presentes=="NA"|str_presentes=="sin datos","NA", 
                              str_count(str_presentes, ";")+1),
         n_catorganizador = ifelse(cat_organizador=="NA","NA", 
                                   str_count(cat_organizador, ";")+1),
         n_panelistas = ifelse(str_panelistas=="NA","NA", 
                               str_count(str_panelistas, ";")+1),
         n_moderadores = ifelse(str_moderadores=="NA","NA", 
                                str_count(str_moderadores, ";")+1),
         n_ausentes = ifelse(str_ausentes=="sin datos","NA", ifelse(
           str_detect(str_ausentes, "sin ausencias conocidas"), 
           0, str_count(str_ausentes, ";")+1)))  %>% 
  select(-n_organizador) %>% 
  mutate(n_ausentes = as.numeric(n_ausentes),
         n_presentes = as.numeric(n_presentes),
         n_invitados = n_presentes + n_ausentes) %>% 
  select(-c(n_presentes2, n_hs))  

# Agrego base anual y creo algunos cambios mas en funcion de estos datos 
data_base_limpia <- data_base_limpia %>% 
  left_join(data_anual_sucia) %>% 
  mutate(n_candidaturas = as.numeric(n_candidaturas),
       n_candidaturas = ifelse(ncat_ronda==2, 2, n_candidaturas),
       n_proporcioninvitados = n_invitados/n_candidaturas)   
  
# creo variables dicotomicas 
data_base_limpia <- data_base_limpia %>% 
  # creo dicotómicas para organizador # esto al final lo hago en "segunda_limpieza_datos"
  # creo dicotómicas para formato
  mutate( dico_formato_apertura = saltear_NAs(cat_formato,str_detect(cat_formato,"apertura")) ,
          dico_formato_libre = saltear_NAs(cat_formato,str_detect(cat_formato,"libre")) ,
          dico_formato_duelo = saltear_NAs(cat_formato,str_detect(cat_formato,"duelo")) ,
          dico_formato_moderadores = saltear_NAs(cat_formato,str_detect(cat_formato,"moderadores")),
          dico_formato_periodistas = saltear_NAs(cat_formato,str_detect(cat_formato,"periodistas")),
          dico_formato_sectores = saltear_NAs(cat_formato,str_detect(cat_formato,"sectores")),
          dico_formato_expertos = saltear_NAs(cat_formato,str_detect(cat_formato,"expertos")),
          dico_formato_virtuales = saltear_NAs(cat_formato,str_detect(cat_formato,"virtuales")),
          dico_formato_expositivo = saltear_NAs(cat_formato,str_detect(cat_formato,"expositivo")),
          dico_formato_presentes = saltear_NAs(cat_formato,str_detect(cat_formato,"presentes"))) %>% 
  # creo dicotómicas para temas
  # antes corrijo cuestion de codebook: debates monotematicos son exclusivos
  mutate(cat_temas = saltear_NAs(cat_temas,ifelse(str_detect(cat_temas, "monotema"), "monotema", cat_temas))) %>% 
  mutate( dico_temas_puntuales = saltear_NAs(cat_temas,str_detect(cat_temas,"puntuales")) ,
          dico_temas_libre = saltear_NAs(cat_temas,str_detect(cat_temas,"libre")) ,
          dico_temas_monotema = saltear_NAs(cat_temas,str_detect(cat_temas,"monotema")) ,
          dico_temas_bloques = saltear_NAs(cat_temas,str_detect(cat_temas,"bloques")) )

# agrego variables externas

# datos_externos <- data_qog_latam_selection_renamed %>% 
#   select(gol_enpres, p_polity2, pais, año_electoral) %>% 
#   dplyr::rename(n_efcandidatos = gol_enpres,
#                 cat_pais = pais,
#                 ncat_eleccion = año_electoral)
# 
# data_base_limpia <- data_base_limpia %>% 
#   left_join(datos_externos)

# guardo  ##########

# xlsx
data_base_limpia %>%  writexl::write_xlsx("base_final1v2023.xlsx")


# creo base simple auxiliar de elecciones ###############

data_anual_limpia <- data_anual_sucia %>% 
  select(cat_pais, ncat_eleccion, cat_ballotage, n_candidaturas)

data_anual_limpia  %>%  writexl::write_xlsx("base_eleccionesv2023.xlsx")


