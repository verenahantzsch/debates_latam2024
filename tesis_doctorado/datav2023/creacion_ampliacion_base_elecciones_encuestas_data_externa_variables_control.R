################################# SCRIPT PARA AMPLIAR DATOS PROPIOS CON DATOS EXTERNOS ##################
############################### CREADO EL 28 MAYO 2024 POR CAROLINA FRANCO PARA TESIS DE DOCTORADO ##################
#### Este script parte de la base creada en "creacion_base_elecciones" en su ultima version #############
####### y agrega data de fuentes externas que contienen potenciales indicadores para medir variables de control ####

# LIBRERIAS  #####
library(tidyverse)

### CARGA DE DATOS PROPIOS ##############
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
path <- "all_elections.csv" # base creada previamente en "creacion_base_elecciones"
mi_data <- read.csv(path)  %>% select(-X)
reserva <- mi_data

################################################## CARGO DATA LATINOBAROMETRO Y LAPOP ########################

# cargo data

data_latinobarometro <- "/home/carolina/Documents/dataexterna/latinobarometro/grouped_filled_latinobarometro_2024.csv" %>%  read.csv() %>% select(-X)
data_lapop <- "/home/carolina/Documents/dataexterna/lapop/confianza_medios_lapop_2024.csv" %>% read.csv() %>% select(-X)

data_unida_lapop_latinobarometro <- data_latinobarometro  %>% 
  left_join(data_lapop %>% mutate("ncat_eleccion" = as.integer(wave)))

# exploramos maneras alternativas de unificar la data 
data_unida_lapop_latinobarometro <- data_unida_lapop_latinobarometro %>% 
  mutate(average_confianza_tv_medios_latin_lapop = ifelse(is.na(average_confianza_tv), average_confianza_medios_recat, average_confianza_tv),
         average_scaled_confianza_tv_medios_latin_lapop = ifelse(is.na(average_scaled_confianza_tv), average_scaled_confianza_medios, average_scaled_confianza_tv)) %>% #,
  #new_average_confianza = ifelse(is.na(new_average_confianza), int_average_confianza_tv, new_average_confianza))
  arrange(cat_pais, ncat_eleccion) %>% 
  group_by(cat_pais) %>% 
  mutate(int_average_confianza_tv_medios_latin_lapop = ifelse(is.na(average_confianza_tv_medios_latin_lapop), approx(ncat_eleccion, average_confianza_tv_medios_latin_lapop, xout = ncat_eleccion)$y, average_confianza_tv_medios_latin_lapop),
         int_average_scaled_confianza_tv_medios_latin_lapop = ifelse(is.na(average_scaled_confianza_tv_medios_latin_lapop), approx(ncat_eleccion, average_scaled_confianza_tv_medios_latin_lapop, xout = ncat_eleccion)$y, average_scaled_confianza_tv_medios_latin_lapop)) 

#check_similitud <- data_unida %>% 
#  select(average_scaled_confianza_medios, average_scaled_confianza_tv) # esto mejoro mucho, ver si hay manera de poner a prueba compatibiliad. PENDIENTE

# agrego

mi_data2 <- mi_data %>% 
  left_join(data_unida_lapop_latinobarometro)

################################################## CARGO DATA FRIEDENBERG #################################################
# PENDIENTE: FRIEDENBERG TIENE DATOS DE VOLATILIDAD ELECTORAL EN OTRA BASE ##############
# cargo data
data_friedenberg <- "/home/carolina/Documents/dataexterna/OBSREF/filled_friedenberg_2024.csv" %>%  read.csv() %>% select(-X)

# selecciono variables de interes 

data_friedenberg <- data_friedenberg %>% 
  select(pais, año, 
         año_reforma, consec_reforma_pais, 
         prohibicion_propaganda, acceso_gratuito, distribucion_tiempos, franja_presidente)

# recodifico variables 

data_friedenberg$prohibicion_propaganda %>% unique()
data_friedenberg$distribucion_tiempos %>% unique()

data_friedenberg <- data_friedenberg %>%
  mutate(prohibicion_propaganda = str_remove_all(prohibicion_propaganda, "[^0-9]") %>% as.numeric(),
         acceso_gratuito = str_remove_all(acceso_gratuito, "[^0-9]") %>% as.numeric() )#,
#distribucion_tiempos = str_remove_all(prohibicion_propaganda, "[^0-9]") %>% as.numeric(),
#franja_presidente = str_remove_all(prohibicion_propaganda, "[^0-9]") %>% as.numeric() )

# modifico para unir

data_friedenberg <- data_friedenberg %>% 
  dplyr::rename("cat_pais" = "pais", "ncat_eleccion" = "año")

# agrego

mi_data3 <- mi_data2 %>% 
  left_join(data_friedenberg)

################################################## CARGO DATA QUALITY OF GOVERNMENT ###############################################################
# PENDIENTE: ############
# POR AHORA CARGO DATA CON UNION SIMPLE AÑO - AÑO, 
# PENDIENTE PENSAR SI PARA ALGUNAS VARIABLES NO CONVIENE UN PROMEDIO DE AÑOS ANTERIORES O UN LAG

# cargo data
data_qof <- "/home/carolina/Documents/dataexterna/QofG/data_ts_qof_filtered_2024.csv" %>%  read.csv() %>% select(-X)

# evaluo Nas. Todavia no se que conviene hacer con esto (may 28 2024) 
# Count NA values per column
na_per_column <- data_qof %>%
  summarize(across(everything(), ~ sum(is.na(.)) / 1404, .names = "na_{.col}"))

# agrego

mi_data4 <- mi_data3 %>% 
  left_join(data_qof)

# evaluo Nas. Todavia no se que conviene hacer con esto (may 28 2024) 
# Count NA values per column
na_per_column <- mi_data4 %>%
  summarize(across(everything(), ~ sum(is.na(.)) / 1404, .names = "na_{.col}"))

# GUARDAMOS ###################################

# reset de directorio
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

path <- "all_elections_full_dataset.csv" # guardo bajo nombre diferenciado 
mi_data4 %>%
  write.csv(path)