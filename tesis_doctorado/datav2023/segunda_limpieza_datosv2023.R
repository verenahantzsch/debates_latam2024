############ ESTE SCRIPT ES PARA CIERTOS TRABAJOS QUE REQUIEREN DE UN AGREGADO DE DATA MANUAL ###########
### SE EJECUTA LUEGO DE LA PRIMERA LIMPIEZA DE DATOS #######################

# LIBRERIAS #####

library(tidyverse)

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

# Importación de datos #####

base <- readxl::read_xlsx("base_final1v2023.xlsx") # base con datos por debate
elecciones <-  readxl::read_xlsx("base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país


# ausencias ########

# creo base con un nombre de ausente por fila

base_ausentes <- base %>% 
  select(id_debate, ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, 
         str_ausentes, n_ausentes, n_invitados, n_presentes, str_presentes) %>%
  mutate(nombres_ausentes = strsplit(str_ausentes, ";")) %>% 
  unnest(nombres_ausentes)
 
# creo base para completar manualmente. SOLO RETENGO PAISES QUE ACTUALICE RESPECTO DE LA ULTIMA VEZ
# saltear paso si no aplica

# base_datosausentes <- base_ausentes %>% 
#   distinct(nombres_ausentes, ncat_eleccion, cat_pais) %>% 
#   #subset(str_detect(cat_pais, "Colombia|Costa|Chile|Brasil")) %>% 
#   subset(ncat_eleccion>2022)
# 
# # guardo base en excel, para su manipulacion manual
# base_datosausentes %>% 
#   writexl::write_xlsx("base_datosausentes_paracompletar.xlsx")

# leo base ya modificada

base_datosausentes <- readxl::read_xlsx("base_datosausentes.xlsx")

# cambio NAS por 0 a los fines del índice. 

base_datosausentes <- base_datosausentes %>% 
  mutate(n_porcentajevotos = ifelse(n_porcentajevotos == "NA"|is.na(n_porcentajevotos),
                                    0,
                                    n_porcentajevotos))

# uno datos a base de todos los ausentes

base_ausentes_added <- base_ausentes %>% 
  left_join(base_datosausentes)  # FALTA CHEQUEAR BOLIVIA 2002

# sumariso (por debate) 

base_ausencias <- base_ausentes_added %>% 
  group_by(ncat_eleccion, cat_pais, t_fecha, str_organizador, id_debate) %>% 
  summarise( n_porcentajeausentes = sum(as.numeric(n_porcentajevotos), na.rm = TRUE))

# uno a base general

base <- base %>% 
  left_join(base_ausencias) %>% 
  mutate( n_proporcionausentes = as.numeric(n_ausentes)/as.numeric(n_invitados)) %>% 
  mutate( n_indexausentes = n_proporcionausentes*n_porcentajeausentes,
          dico_ausencias = ifelse(n_ausentes==0, 0, 1)) %>% 
  mutate(n_indexausentes = ifelse(n_ausentes==0, 0, n_indexausentes),
         n_porcentajeausentes = ifelse(n_ausentes==0, 0, n_porcentajeausentes))


# presencias #######

# creo base con un nombre de presente por fila

base_presentes <- base %>% 
  select(id_debate, ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, 
         str_ausentes, n_ausentes, n_invitados, n_presentes, str_presentes) %>%
  mutate(nombres_candidatos = strsplit(str_presentes, ";")) %>% 
  unnest(nombres_candidatos) %>% 
  mutate(dico_candidato_presente = 1)

# agrego col a base_ausentes para luego unir

base_ausentes <- base_ausentes %>% 
  dplyr::rename("nombres_candidatos" = nombres_ausentes) %>% 
  mutate(dico_candidato_presente = 0)

# uno con base de ausencias
all_candidates <- base_presentes %>% 
  rbind(base_ausentes)

# limpieza automatica 
### ACA ME QUEDE SABADO 9 MARZO 2024 BORRAR ESTO #############
# ESTOY REVISANDO MANUALMENTE BASE DE UNIQUE CANDIDATES GUARDADA ABAJO
# PERO EN PPIO LA BASE A TRABAJAR ES ESA A LA QUE DEBERIAMOS AGREGAR DATOS DE ENCUESTAS EN 7 DIAS PREVIOS POR CANIDDATO, SI ES INCUMBENT, SI REELIGE Y ALGUNA OTRA CONSIDERADA PERTINENTE EJ LEGISLACION, EN OTRO NIVEL

all_candidates <- all_candidates %>% 
  mutate(nombres_candidatos = str_replace(nombres_candidatos, ".*:", ""),
         nombres_candidatos = str_trim(nombres_candidatos, "both"),
         nombres_candidatos = str_replace(nombres_candidatos, "[.]", ""),
         nombres_candidatos = str_replace(nombres_candidatos, ":", ""),
         nombres_candidatos = iconv(nombres_candidatos, to = "ASCII//TRANSLIT"))


# paso intermedio y manual para homogenizar nombres de candidatos
#unique_candidates <- all_candidates %>% 
#  select(cat_pais, ncat_eleccion, nombres_candidatos) %>% 
#  unique() %>% 
#  arrange(cat_pais, nombres_candidatos, ncat_eleccion) %>% 
#  writexl::write_xlsx("base_uniquecandidates_paracompletar.xlsx")

# guardo base #VOLVER A GUARDAR DESPUES DE LIMPIAR  #########

all_candidates %>% 
     writexl::write_xlsx("all_candidates.xlsx")
  

# CREACION DE VARIABLES ORDINALES de NORMATIVA  ####
# vamos a crear tablas para asignar manualmente un numero a una categoria. 
# son tablas de categorias distinct, digamos. de uniqe factors
# luego usaremos joins con las tablas ya cargadas. 

# normativa (tres variables diferentes)

#1
distinct_cat_regcandidatos <- base %>% 
  select(cat_regcandidatos) %>% 
  distinct()

#distinct_cat_regcandidatos %>% 
#  writexl::write_xlsx("distinct_cat_regcandidatos.xlsx")

#2
distinct_cat_regestado <- base %>% 
  select(cat_regestado) %>% 
  distinct()

#distinct_cat_regestado %>% 
#  writexl::write_xlsx("distinct_cat_regestado.xlsx")

#3
distinct_cat_regmedios <- base %>% 
  select(cat_regmedios) %>% 
  distinct()

#distinct_cat_regmedios %>% 
#  writexl::write_xlsx("distinct_cat_regmedios.xlsx")


base <- base %>% 
  left_join(readxl::read_xlsx("distinct_cat_regmedios.xlsx")) %>% 
  left_join(readxl::read_xlsx("distinct_cat_regestado.xlsx")) %>% 
  left_join(readxl::read_xlsx("distinct_cat_regcandidatos.xlsx")) %>% 
  mutate(ncat_totreg = ncat_regmedios + ncat_regestado + ncat_regcandidatos )

# SUBTIPOS DE ORGANIZADORES  #####
#
# creo base para completar manualmente ####


base_organizadores <- base %>% 
  select(id_debate,
         ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, cat_organizador, cat_subtipoorg, 
         n_strorganizador, n_catorganizador) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  unnest(nombres_organizadores)

# guardo  # SOLO GUARDO NECESARIO PARA ACTUALIZAR DATA USADA PARA TESIS DE MAESTRIA

base_organizadores_distinct <- base_organizadores %>% 
  subset(str_detect(cat_pais, "Colombia|Costa|Chile|Brasil")) %>% 
  subset(ncat_eleccion>2020) %>% 
  distinct(nombres_organizadores, ncat_eleccion, cat_pais)

# las guardo

#base_organizadores %>% 
#  writexl::write_xlsx("base_organizadores.xlsx")

#base_organizadores_distinct %>% 
# writexl::write_xlsx("base_organizadores_distinct.xlsx")

# creo una base para categorizarsubtipos en tipos #####

# primero leo base completa (incluyendo data tesis de maestria)

base_organizadores_distinct <- readxl::read_xlsx("base_organizadores_distinct.xlsx")

# crep df con subtipos unicos 
base_subtipos_distinct <- base_organizadores_distinct %>% 
  select(ncat_subtipov2) %>% 
  unique()

# uno data de tesis de maestria 

categorizacion_tesis_maestria <-  readxl::read_xlsx("/home/carolina/Documents/Proyectos R/debates_latam/tesis_maestria/exploracion_base_finalv1/distinct_cat_subtipoorg.xlsx")

base_subtipos_distinct <- base_subtipos_distinct %>% 
  left_join(categorizacion_tesis_maestria)

# guardo para completar faltantes

#base_subtipos_distinct %>%  
#  arrange(cat_tipoorgv2) %>% 
#  writexl::write_xlsx("distinct_cat_subtipoorg.xlsx")


# leo y uno a la base general

categorizacion_orgs <- readxl::read_xlsx("distinct_cat_subtipoorg.xlsx")

base_organizadores_distinct_complete <- base_organizadores_distinct %>% 
  left_join(categorizacion_orgs)

# creo variables dico buscando matches para cada categoria

orgs_mmc <- base_organizadores_distinct_complete %>% 
  subset(cat_tipoorgv2=="mmc")
  
base <- base %>% 
  mutate( dico_org_mmc = str_detect(str_organizador, 
  str_c( orgs_mmc$nombres_organizadores %>%  na.omit() %>% as_vector(), collapse = "|" )))

orgs_osc <- base_organizadores_distinct_complete %>% 
  subset(cat_tipoorgv2=="osc")

base <- base %>% 
  mutate( dico_org_osc = str_detect(str_organizador  %>% str_remove_all("[:punct:]"), 
                                        str_c( orgs_osc$nombres_organizadores  %>% str_remove_all("[:punct:]") %>%  na.omit() %>% as_vector(), collapse = "|" )))

orgs_educ <- base_organizadores_distinct_complete %>% 
  subset(cat_tipoorgv2=="educ")

base <- base %>% 
  mutate( dico_org_educ = str_detect(str_organizador, 
                                        str_c( orgs_educ$nombres_organizadores %>%  na.omit() %>% as_vector(), collapse = "|" )))

orgs_estado <- base_organizadores_distinct_complete %>% 
  subset(cat_tipoorgv2=="estado")

base <- base %>% 
  mutate( dico_org_estado = str_detect(str_organizador, 
                                     str_c( orgs_estado$nombres_organizadores %>%  na.omit() %>% as_vector(), collapse = "|" )))


orgs_mmp <- base_organizadores_distinct_complete %>% 
  subset(cat_tipoorgv2=="mmp")

base <- base %>% 
  mutate( dico_org_mmp = str_detect(str_organizador, 
                                     str_c( orgs_mmp$nombres_organizadores %>%  na.omit() %>% as_vector(), collapse = "|" )))


# GUARDO ESTA BASE ##########################

base %>%  writexl::write_xlsx("base_final2v2023.xlsx")
