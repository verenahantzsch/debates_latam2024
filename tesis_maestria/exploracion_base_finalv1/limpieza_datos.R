# LIBRERIAS #####
library(tidyverse)


# DATOS PROPIOS #####
# base de excel original
data_base_sucia <- readxl::read_xlsx("base_debates_limpia.xlsx", sheet = "debates_agregado")
data_anual_sucia <- readxl::read_xlsx("base_debates_limpia.xlsx", sheet = "debates_normativo")

# peq funcion de ayuda
saltear_NAs <-  function(cadena, operacion){
  
  ifelse(!str_detect(cadena, "NA"), operacion, "NA")
  
}

# nueva base
data_base_limpia <- data_base_sucia %>% 
  dplyr::rename("n_presentes2" = n_presentes, "n_duracion" = n_mins) %>% 
  mutate(n_presentes = str_count(str_presentes, ";")+1 ,
         n_ausentes = str_count(str_ausentes, ";")+1,
         dico_analytics = str_detect(tolower(longstr_formato), "analytics"),
         t_fecha =  openxlsx::convertToDate(t_fecha)) %>% 
  select(-c(n_presentes2, n_hs)) %>% 
 # creo dicotómicas para organizador
  mutate( dico_org_mmc = saltear_NAs(cat_organizador,str_detect(cat_organizador,"mmc")) ,
          dico_org_mmp = saltear_NAs(cat_organizador,str_detect(cat_organizador,"mmp")) ,
          dico_org_osc = saltear_NAs(cat_organizador,str_detect(cat_organizador,"osc")) ,
          dico_org_estado = saltear_NAs(cat_organizador,str_detect(cat_organizador,"estado")) ,
          dico_org_educ = saltear_NAs(cat_organizador,str_detect(cat_organizador,"educ")) ) %>% 
  # creo dicotómicas para formato
  mutate( dico_formato_apertura = saltear_NAs(cat_formato,str_detect(cat_formato,"apertura")) ,
          dico_formato_libre = saltear_NAs(cat_formato,str_detect(cat_formato,"libre")) ,
          dico_formato_duelo = saltear_NAs(cat_formato,str_detect(cat_formato,"duelo")) ,
          dico_formato_moderadores = saltear_NAs(cat_formato,str_detect(cat_formato,"moderadores")),
          dico_formato_periodistas = saltear_NAs(cat_formato,str_detect(cat_formato,"periodistas")),
          dico_formato_sectores = saltear_NAs(cat_formato,str_detect(cat_formato,"sectores")),
          dico_formato_expertos = saltear_NAs(cat_formato,str_detect(cat_formato,"expertos")),
          dico_formato_virtuales = saltear_NAs(cat_formato,str_detect(cat_formato,"virtuales")),
          dico_formato_presentes = saltear_NAs(cat_formato,str_detect(cat_formato,"presentes"))) %>% 
  # creo dicotómicas para temas
  mutate( dico_temas_puntuales = saltear_NAs(cat_temas,str_detect(cat_temas,"puntuales")) ,
          dico_temas_libre = saltear_NAs(cat_temas,str_detect(cat_temas,"libre")) ,
          dico_temas_monotema = saltear_NAs(cat_temas,str_detect(cat_temas,"monotema")) ,
          dico_temas_bloques = saltear_NAs(cat_temas,str_detect(cat_temas,"bloques")) ) #%>%
  # creo variable cat_regulacion 
data_base_limpia <- data_base_limpia %>% 
  left_join(data_anual_sucia)


# guardo
# xlsx
data_base_limpia %>%  writexl::write_xlsx("base_final.xlsx")


#más cambios
# leo datos

data_base_sucia <- readxl::read_xlsx("base_final.xlsx", sheet= "data")

# hago cambios 
data_base_limpia <- data_base_sucia %>% 
  mutate(n_strorganizador = str_count(str_organizador, ";")+1,
         n_catorganizador = str_count(cat_organizador, ";")+1,
         n_panelistas = str_count(str_panelistas, ";")+1,
         n_moderadores = str_count(str_moderadores, ";")+1,
         n_ausentes = ifelse(is.na(str_presentes),"NA", ifelse(is.na(str_ausentes), 0, str_count(str_ausentes, ";")+1))) 

data_base_limpia <- data_base_limpia %>% 
  mutate(n_ausentes = as.numeric(n_ausentes),
         n_invitados = n_presentes + n_ausentes)

data_base_limpia <- data_base_limpia %>% 
  mutate(n_candidaturas = as.numeric(n_candidaturas),
         n_candidaturas = ifelse(ncat_ronda==2, 2, n_candidaturas),
         n_proporcioninvitados = n_invitados/n_candidaturas)

data_base_limpia <- data_base_limpia %>% 
  mutate(t_hora =  openxlsx::convertToDateTime(t_hora))

data_base_limpia <- data_base_limpia %>% 
  select(-n_organizador)

# agrego variables externas

datos_externos <- data_qog_latam_selection_renamed %>% 
  select(gol_enpres, p_polity2, pais, año_electoral) %>% 
  dplyr::rename(n_efcandidatos = gol_enpres,
                cat_pais = pais,
                ncat_eleccion = año_electoral)

data_base_limpia <- data_base_limpia %>% 
  left_join(datos_externos)

# guardo otra vez

# xlsx
data_base_limpia %>%  writexl::write_xlsx("base_finalv1.xlsx")

#VIEJO , BORRADORES ####
# aver <-   data_base_limpia %>% 
#   select(n_strorganizador, n_organizador, str_organizador) %>% 
#   mutate(coinciden = n_strorganizador==n_organizador) 

# aver <-   data_base_limpia %>% 
#   select(cat_pais, t_fecha, n_presentes2, n_presentes) %>% 
#   mutate(coinciden = n_presentes2==n_presentes,
#          t_fecha =  openxlsx::convertToDate(t_fecha)) 
# 
# creo dicotómicas para formato
mutate( dico_formato_apertura = saltear_NAs(cat_formato,str_detect(cat_formato,"apertura")) ,
        dico_formato_libre = str_detect(cat_formato,"libre") ,
        dico_formato_duelo = str_detect(cat_formato,"duelo") ,
        dico_formato_moderadores = str_detect(cat_formato,"moderadores") ,
        dico_formato_periodistas = str_detect(cat_formato,"periodistas"),
        dico_formato_sectores = str_detect(cat_formato,"sectores"),
        dico_formato_expertos = str_detect(cat_formato,"expertos"),
        dico_formato_virtuales = str_detect(cat_formato,"virtuales"),
        dico_formato_presentes = str_detect(cat_formato,"presentes")) %>% 
  # creo dicotómicas para temas
  mutate( dico_temas_puntuales = str_detect(cat_temas,"puntuales") ,
          dico_temas_libre = str_detect(cat_temas,"libre") ,
          dico_temas_monotema = str_detect(cat_temas,"monotema") ,
          dico_temas_bloques = str_detect(cat_temas,"bloques")) #%>%
# creo variable cat_regulacion 