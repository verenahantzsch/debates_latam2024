# LIBRERIAS #####
library(tidyverse)
library(ggforce)
library(patchwork)

# Importación de datos #####

base <- readxl::read_xlsx("base_finalv1.xlsx") # base con datos por debate
elecciones <-  readxl::read_xlsx("base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país


# ORGANIZADORES

# carga de datos
# vamos a tener que hacer un promedio por debate

# creo base con un organizador por fila y 
# luego uno los datos cargados

base_organizadores <- base %>% # BASE CON UN ORG POR FILA 
  select(ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, cat_organizador, cat_subtipoorg, 
         n_strorganizador, n_catorganizador, 
         dico_org_educ, dico_org_estado, dico_org_mmc, dico_org_mmp, dico_org_osc) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  unnest(nombres_organizadores) 
  
  # UNO DATOS CARGADOS

base_organizadores_distinct <-  readxl::read_xlsx("base_organizadores_distinct.xlsx") %>% 
  left_join(readxl::read_xlsx("distinct_cat_subtipoorg.xlsx"))

base_subtipos <- base_organizadores %>%  
  mutate(nombres_organizadores = str_trim(nombres_organizadores)) %>% 
  # con el str trim se resuelven algunos problemas pero OJO que hay registros duplicados
  left_join(base_organizadores_distinct %>% 
              distinct(nombres_organizadores, ncat_eleccion, cat_pais,
                       ncat_subtipov2, cat_tipoorgv2,
                       ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
                       cat_areaorg))
# aplicando de nuevo distinct se resolvio la duplicacion

# creo base POR DEBATE

# distintas medidas de agregacion
# promedio por debate
base_grsubtipo_debate <- base_subtipos %>% 
  group_by(str_organizador, t_fecha) %>% 
  summarise(ncat_meantipoorg_ambito = mean(ncat_tipoorg_ambito),
            ncat_meantipoorg_visibilidad = mean(ncat_tipoorg_visibilidad))

base_ordinales <- base %>% 
  left_join(base_grsubtipo_debate)

# FORMATOS (YA ESTAN HECHAS)


# LEGALES

# NO ENTIENDOO POR QUE NO SE UNEENNNNNNNNNNN
base_ordinales <- base_ordinales %>% 
  left_join(readxl::read_xlsx("distinct_cat_regmedios.xlsx")) %>% 
  left_join(readxl::read_xlsx("distinct_cat_regestado.xlsx")) %>% 
  left_join(readxl::read_xlsx("distinct_cat_regcandidatos.xlsx")) %>% 
  mutate(ncat_totreg = ncat_regmedios + ncat_regestado + ncat_regcandidatos )


# LA CONCHA DE LA LORA SE ME BORRARON TODOS LOS AMADISIMOS GRAFICOS QUIERO MATAR A ALGUIEN HABIA TRABAJADO MUCHO

# base ploteo

base_plot <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  filter(!is.na(ncat_ppac)) %>% 
  filter(!ncat_ppac =="NA") %>% 
  filter(!ncat_competencia =="NA") %>% 
  filter(!is.na(ncat_competencia)) %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
            ncat_meantipoorg_visibilidad = mean(ncat_meantipoorg_visibilidad),
            ncat_totreg = mean(ncat_totreg),
            n_cantidaddebates = n(),
            ncat_ppac = mean(as.numeric(ncat_ppac)),
            ncat_competencia = mean(as.numeric(ncat_competencia)),
            ncat_interactividad =  ncat_competencia + ncat_ppac,
            ncat_eleccion = as.numeric(ncat_eleccion))



# # PAIS ELECCION REGULACION VERSUS AMBITO PROMEDIO DE ORGANIZADORES,
# CON CANTIDAD DE DEBATES EN TAMANIO
# ME GUSTA
plot1 <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
         ncat_totreg = mean(ncat_totreg),
         n_cantidaddebates = n()) %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, ncat_totreg, colour = cat_pais, size= n_cantidaddebates, alpha = ncat_eleccion )) +
  geom_text(aes(ncat_meantipoorg_ambito, ncat_totreg, colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = ncat_eleccion )) +
  theme_minimal() +
  theme(legend.position = "none")

# PAIS ELECCION CANTIDAD DE DEBATES EN LOG VERSUS AMBITO PROMEDIO DE ORGANIZADORES
# ME GUSTA
plot2 <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
         ncat_totreg = mean(ncat_totreg),
         n_cantidaddebates = n()) %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, log(n_cantidaddebates), colour = cat_pais, size= ncat_totreg, alpha = ncat_eleccion )) +
  geom_text(aes(ncat_meantipoorg_ambito, log(n_cantidaddebates), colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = ncat_eleccion )) +
  theme_minimal() +
  theme(legend.position = "none")

# PPAC VERSUS AMBITO 
plot4 <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  filter(!is.na(ncat_ppac)) %>% 
  filter(!ncat_ppac =="NA") %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
         ncat_totreg = mean(ncat_totreg),
         n_cantidaddebates = n(),
         ncat_ppac = mean(as.numeric(ncat_ppac))) %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, log(ncat_ppac), colour = cat_pais, size= n_cantidaddebates, alpha = ncat_totreg )) +
  geom_text(aes(ncat_meantipoorg_ambito, log(ncat_ppac), colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = ncat_eleccion )) +
  theme_minimal() +
  theme(legend.position = "none")

# PPAC VERSUS REGLAS 
plot5 <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  filter(!is.na(ncat_ppac)) %>% 
  filter(!ncat_ppac =="NA") %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
            ncat_totreg = mean(ncat_totreg),
            n_cantidaddebates = n(),
            ncat_ppac = mean(as.numeric(ncat_ppac))) %>% 
  ggplot() +
  geom_point(aes(ncat_totreg, log(ncat_ppac), colour = cat_pais, size= ncat_meantipoorg_ambito, alpha = ncat_meantipoorg_ambito )) +
  geom_text(aes(ncat_totreg, log(ncat_ppac), colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = as.numeric(ncat_eleccion ))) +
  theme_minimal() +
  theme(legend.position = "none")

# PPAC VERSUS DEBATE
plot6 <- base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_competencia, log(ncat_ppac), colour = cat_pais, size= n_cantidaddebates, alpha = as.numeric(ncat_eleccion))) +
  geom_text(aes(ncat_competencia, log(ncat_ppac), colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = as.numeric(ncat_eleccion))) +
  theme_minimal() +
  theme(legend.position = "none")

# DEBATE VERSUS AMBITO
plot7 <- base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_competencia, ncat_meantipoorg_ambito, colour = cat_pais, size= n_cantidaddebates, alpha = as.numeric(ncat_eleccion))) +
  geom_text(aes(ncat_competencia, ncat_meantipoorg_ambito, colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = as.numeric(ncat_eleccion))) +
  theme_minimal() +
  theme(legend.position = "none")

# DEBATE EN EL TIEMPO
plot8 <-  base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, ncat_competencia, colour = cat_pais, size= n_cantidaddebates, alpha = ncat_ppac)) +
  geom_text(aes(ncat_eleccion, ncat_competencia, colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = ncat_ppac)) +
  theme_minimal() +
  theme(legend.position = "none")

# con promedio aditivo de interactividad versus ambito
plot9 <-  base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, ncat_interactividad, colour = cat_pais, size= n_cantidaddebates, alpha = ncat_eleccion*ncat_eleccion)) +
  geom_text(aes(ncat_meantipoorg_ambito, ncat_interactividad, colour = cat_pais, label = str_glue("{cat_pais}\n{ncat_eleccion}"), alpha = ncat_eleccion*ncat_eleccion)) +
  theme_minimal() +
  theme(legend.position = "none")


# PROMEDIO TOTAL PAIS, REGULACION VERSUS AMBITO
plotpais1 <- base_ordinales %>% 
  filter(!is.na(ncat_meantipoorg_ambito)) %>% 
  group_by(cat_pais) %>% 
  mutate(ncat_meantipoorg_ambito = mean(ncat_meantipoorg_ambito),
         ncat_totreg = mean(ncat_totreg),
         n_cantidaddebates = n()) %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, ncat_totreg, colour = cat_pais, size= n_cantidaddebates), alpha = 0.9) +
  geom_text(aes(ncat_meantipoorg_ambito, ncat_totreg, colour = cat_pais, label = cat_pais)) +
  theme_minimal() +
  theme(legend.position = "none")



## VISUALIZACION DESCRIPTIVA ########
##
##
##

#

# REGULACION ####

#

# en este caso las distribuciones por debate y
# el promedio pais-eleccion deberian ser iguales
# medidas solas

# medidas por debate, univariadas
hist1 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(ncat_regmedios)) +
  theme_minimal()

hist2 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(ncat_regestado)) +
  theme_minimal()

hist3 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(ncat_regcandidatos)) +
  theme_minimal()

# descripcion bi/tri variada, por debate
bi1 <- base_ordinales %>% 
  ggplot() +
  geom_point(aes(ncat_regmedios, ncat_regcandidatos, size = ncat_regestado)) +
  theme_minimal()

bi2 <- base_ordinales %>% # en este se ve clara relacion positiva. en el resto tambien pero hay outliers
  ggplot() +
  geom_point(aes(ncat_regestado, ncat_regcandidatos, size = ncat_regmedios)) +
  theme_minimal()

bi3 <- base_ordinales %>% 
  ggplot() +
  geom_point(aes(ncat_regestado, ncat_regmedios, size = ncat_regcandidatos)) +
  theme_minimal()

# simple medida aditiva, por debate
hist4 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(ncat_totreg)) +
  scale_x_continuous(breaks = seq(0,14,1)) +
  theme_minimal()

# medida aditiva, promedio por pais-eleccion
hist5 <- base_plot %>% 
  ggplot() +
  geom_histogram(aes(ncat_totreg)) +
  scale_x_continuous(breaks = seq(0,14,1)) +
  theme_minimal() 

#

# FORMATO ####

#

# por debate

hist6 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(as.numeric(ncat_ppac))) +
  theme_minimal()

hist7 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes(as.numeric(ncat_competencia))) +
  theme_minimal()
    
# descripcion bivariada, por debate 

bi4 <- base_ordinales %>% 
  ggplot() +
  geom_point(aes(as.numeric(ncat_competencia), as.numeric(ncat_ppac))) +
  theme_minimal()
# la relacion positiva, si la hay, es debil. 
# aunque por ahi hay mucha acumulacion de puntos en el mismo lugar

# bivariada, promedios pais-eleccion
bi5 <- base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_competencia, ncat_ppac, size = n_cantidaddebates)) +
  theme_minimal()

# medida aditiva, por debate 

hist8 <- base_ordinales %>% 
  ggplot() +
  geom_histogram(aes( (as.numeric(ncat_competencia)+as.numeric(ncat_ppac) ))) +
  theme_minimal()

# medida aditiva, por promedios pais-eleccion
hist9 <- base_plot %>% 
  ggplot() +
  geom_histogram(aes(ncat_interactividad)) +
  theme_minimal()

#

# CARACTER DEL ORGANIZADOR ####

#

# histogramas univariados, por ORGANIZADOR

hist10 <- base_subtipos %>% 
  ggplot() +
  geom_histogram((aes(ncat_tipoorg_ambito))) +
  theme_minimal()

hist11 <- base_subtipos %>% 
  ggplot() +
  geom_histogram((aes(ncat_tipoorg_visibilidad))) +
  theme_minimal()

# histogramas univariados, por debate
# en este caso la medida es un promedio del ambito en el que inscribimos
# a cada organizador singular

hist12 <- base_ordinales %>% 
  ggplot() +
  geom_histogram((aes(ncat_meantipoorg_ambito))) +
  theme_minimal()
  
hist13 <- base_ordinales %>% 
  ggplot() +
  geom_histogram((aes(ncat_meantipoorg_visibilidad))) +
  theme_minimal()

# histogramas univariados, promedio pais-eleccion

hist14 <- base_plot %>% 
  ggplot() +
  geom_histogram(aes(ncat_meantipoorg_ambito)) +
  theme_minimal()

hist15 <- base_plot %>% 
  ggplot() +
  geom_histogram(aes(ncat_meantipoorg_visibilidad)) +
  theme_minimal()

# descripcion bivariada, por organizador

bi6 <- base_subtipos %>% 
  ggplot() +
  geom_point(aes(ncat_tipoorg_ambito, ncat_tipoorg_visibilidad)) +
  theme_minimal() +
  theme(legend.position = "none")

# descripcion bivariada, por debate

bi7 <- base_ordinales %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, ncat_meantipoorg_visibilidad)) +
  theme_minimal() +
  theme(legend.position = "none")

# descripcion bivariada, promedio pais-eleccion

bi8 <- base_plot %>% 
  ggplot() +
  geom_point(aes(ncat_meantipoorg_ambito, ncat_meantipoorg_visibilidad)) +
  theme_minimal()

# aca definitivamente no me parece adecuado hacer un indice aditivo, 
# son dos cosas conceptualmente diferentes.
# en general prefiero la medida ambito ya que tiene mayor sentido y variabilidad empirica





