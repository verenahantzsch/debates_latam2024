

## librerias
library(tidyverse)
#library(hrbrthemes) #
library(RColorBrewer)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
#library(xlsx) #

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")

# datos
base <- read_csv("./datav2023/base_final3v2023.csv")
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")
base_anual_full <- readxl::read_xlsx("./datav2023/base_anual_fullv2023.xlsx")

nombres_subtipos <- readxl::read_xlsx("./codebooksv2023/nombres_subtiposv2023.xlsx")

colorespais <- base %>% 
  distinct(cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 

plotnumber <- 0 # para guardar con numeracion ascendente

base_anual_full <- base_anual_full %>% 
  subset(ncat_eleccion!=2024)

# SCRIPT ORIGINALMENTE PLANTEADO EN TRABAJO_CAPITULOS, PASADO ACA PARA COMODIDAD
# JULIO 2024

#####################################################################################
######################################################################
####### PARENTESIS: CALCULO DE VARIABILIDAD INTRA Y WITHIN PARA VARIAS VARIABLES, APUNTE CHATGPT ###############################################
# Calculate Cross-Sectional SD
cross_sectional_sd <- data %>%
  group_by(Time) %>%
  summarise(across(starts_with("Var"), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Cross_Sectional_SD")

# Calculate Time Series SD
time_series_sd <- data %>%
  group_by(Group) %>%
  summarise(across(starts_with("Var"), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Time_Series_SD")

# Combine Results
combined_sd <- cross_sectional_sd %>%
  left_join(time_series_sd, by = "Variable") %>%
  arrange(Variable, Time)

print(combined_sd)

# CUENTAS GENERALES #####

base_elecciones_n_e <- base_anual_full %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_elecs = mean(n_debates_año_pais))

summary(base_elecciones_n_e$n_debates_elecs)
sd(base_elecciones_n_e$n_debates_elecs)

# agrupado por año t ################

base_elecciones_n_t <- base_anual_full %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_elecs = mean(n_debates_año_pais))

summary(base_elecciones_n_t$n_debates_elecs)
sd(base_elecciones_n_t$n_debates_elecs)

# PARA TIPOS DE ORG ####
# agrupado por año t ################
# distribuciones de las varibales por año, para ver numericamente variablidad 
# prop de orgs 
# primero para entidades indivs, luego para debates con ppac de entidades

cuenta_tipos_por_ncat_eleccion <- base_organizadores %>% 
  group_by(cat_tipoorgv2, ncat_eleccion) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(ncat_eleccion) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/sum(n_debates_en_participaron)*100) %>% 
  arrange(desc(pr_debates_en_participaron))

tabla_cuenta_tipos_por_ncat_eleccion_individuos <- cuenta_tipos_por_ncat_eleccion %>% 
  select(cat_tipoorgv2, ncat_eleccion, pr_n_individuos) %>% 
  arrange(ncat_eleccion) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)

summary(tabla_cuenta_tipos_por_ncat_eleccion_individuos$estado)
summary(tabla_cuenta_tipos_por_ncat_eleccion_individuos$osc)
summary(tabla_cuenta_tipos_por_ncat_eleccion_individuos$mmc)
summary(tabla_cuenta_tipos_por_ncat_eleccion_individuos$mmp)
summary(tabla_cuenta_tipos_por_ncat_eleccion_individuos$educ)

sd(tabla_cuenta_tipos_por_ncat_eleccion_individuos$estado, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_individuos$osc, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_individuos$mmc, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_individuos$mmp, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_individuos$educ, na.rm = T)

tabla_cuenta_tipos_por_ncat_eleccion_debate <- cuenta_tipos_por_ncat_eleccion %>% 
  select(cat_tipoorgv2, ncat_eleccion, pr_debates_en_participaron) %>% 
  arrange(ncat_eleccion) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron)

# 
summary(tabla_cuenta_tipos_por_ncat_eleccion_debate$estado)
summary(tabla_cuenta_tipos_por_ncat_eleccion_debate$osc)
summary(tabla_cuenta_tipos_por_ncat_eleccion_debate$mmc)
summary(tabla_cuenta_tipos_por_ncat_eleccion_debate$mmp)
summary(tabla_cuenta_tipos_por_ncat_eleccion_debate$educ)

sd(tabla_cuenta_tipos_por_ncat_eleccion_debate$estado, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_debate$osc, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_debate$mmc, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_debate$mmp, na.rm = T)
sd(tabla_cuenta_tipos_por_ncat_eleccion_debate$educ, na.rm = T)

# agrupado por pais e ################
cuenta_tipos_por_cat_pais <- base_organizadores %>% 
  group_by(cat_tipoorgv2, cat_pais) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/sum(n_debates_en_participaron)*100) %>% 
  arrange(desc(pr_debates_en_participaron))

tabla_cuenta_tipos_por_pais_individuos <- cuenta_tipos_por_cat_pais %>% 
  select(cat_tipoorgv2, cat_pais, pr_n_individuos) %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)

n_por_pais_individuos <- cuenta_tipos_por_cat_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_entidades_total_pais = sum(n_individuos)) %>% 
  arrange(cat_pais)

tabla_cuenta_tipos_por_pais_individuos <- tabla_cuenta_tipos_por_pais_individuos %>% 
  left_join(n_por_pais_individuos) %>% 
  #arrange(desc(mmc))
  # arrange(desc(estado))
  arrange(desc(osc))

tabla_cuenta_tipos_por_pais_debates <- cuenta_tipos_por_cat_pais %>% 
  select(cat_tipoorgv2, cat_pais, pr_debates_en_participaron) %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron)

n_por_pais_debates <- cuenta_tipos_por_cat_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_total_pais = sum(n_debates_en_participaron)) %>% 
  arrange(cat_pais)

tabla_cuenta_tipos_por_pais_debates <- tabla_cuenta_tipos_por_pais_debates %>% 
  left_join(n_por_pais_debates) %>% 
  #arrange(desc(n_debates_total_pais))
  #arrange(cat_pais)
  arrange(desc(mmc))
#arrange(desc(estado))
arrange(desc(osc))


# distribuciones de las varibales por pais
# prop de orgs 
summary(tabla_cuenta_tipos_por_pais_debates$estado)
summary(tabla_cuenta_tipos_por_pais_debates$osc)
summary(tabla_cuenta_tipos_por_pais_debates$mmc)
summary(tabla_cuenta_tipos_por_pais_debates$mmp)
summary(tabla_cuenta_tipos_por_pais_debates$educ)

sd(tabla_cuenta_tipos_por_pais_debates$estado, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_debates$osc, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_debates$mmc, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_debates$mmp, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_debates$educ, na.rm = T)

summary(tabla_cuenta_tipos_por_pais_individuos$estado)
summary(tabla_cuenta_tipos_por_pais_individuos$osc)
summary(tabla_cuenta_tipos_por_pais_individuos$mmc)
summary(tabla_cuenta_tipos_por_pais_individuos$mmp)
summary(tabla_cuenta_tipos_por_pais_individuos$educ)

sd(tabla_cuenta_tipos_por_pais_individuos$estado, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_individuos$osc, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_individuos$mmc, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_individuos$mmp, na.rm = T)
sd(tabla_cuenta_tipos_por_pais_individuos$educ, na.rm = T)

# PARA PATRONES DE INTERACCION FORMATOS ###############


# agrupado por año t ################
# tabla de distribucion por año, para calcular
tipos_formatos_año <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  #fct_collapse(cat_tipo_formato,
  #             pr_formatomoderadores = "pr_formatomoderadores",
  #                                             "pr_formatoapertura") %>% 
  group_by(ncat_eleccion) %>% 
  mutate(n_debates_en_año = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(ncat_eleccion, n_debates_en_año, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE),
            n_formato_en_año = n()) %>% 
  mutate(pr_formato_en_año = n_formato_en_año/ n_debates_en_año )

tipos_formatos_año_wide <- tipos_formatos_año %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_año) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100))  

summary(tipos_formatos_año_wide$pr_formatoduelo)
sd(tipos_formatos_año_wide$pr_formatoduelo)
summary(tipos_formatos_año_wide$pr_formatoperiodistas)
sd(tipos_formatos_año_wide$pr_formatoperiodistas)
summary(tipos_formatos_año_wide$pr_formatomoderadores)
sd(tipos_formatos_año_wide$pr_formatomoderadores)
summary(tipos_formatos_año_wide$pr_formatopresentes)
sd(tipos_formatos_año_wide$pr_formatopresentes)
summary(tipos_formatos_año_wide$pr_formatosectores)
sd(tipos_formatos_año_wide$pr_formatosectores)
summary(tipos_formatos_año_wide$pr_formatovirtuales)
sd(tipos_formatos_año_wide$pr_formatovirtuales)
summary(tipos_formatos_año_wide$pr_formatolibre)
sd(tipos_formatos_año_wide$pr_formatolibre)
summary(tipos_formatos_año_wide$pr_formatoexpertos)
sd(tipos_formatos_año_wide$pr_formatoexpertos)
summary(tipos_formatos_año_wide$pr_formatoexpositivo)
sd(tipos_formatos_año_wide$pr_formatoexpositivo)

# agrupado por pais e ################

# tabla de distribucion por año, para calcular

tipos_formatos_pais <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  #fct_collapse(cat_tipo_formato,
  #             pr_formatomoderadores = "pr_formatomoderadores",
  #                                             "pr_formatoapertura") %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_en_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE),
            n_formato_en_pais = n()) %>% 
  mutate(pr_formato_en_pais = n_formato_en_pais/ n_debates_en_pais )

tipos_formatos_pais_wide <- tipos_formatos_pais %>% 
  select(-c(n_peso_formato_xdebate, n_formato_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100)) %>% 
  arrange(n_debates_en_pais)

# Reorder columns
tipos_formatos_pais_wide <- tipos_formatos_pais_wide[, c("cat_pais", "n_debates_en_pais",
                                                         "pr_formatoduelo", "pr_formatolibre",
                                                         "pr_formatoexpositivo", "pr_formatomoderadores",
                                                         "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
                                                         "pr_formatopresentes","pr_formatovirtuales")]

summary(tipos_formatos_pais_wide$pr_formatoduelo)
sd(tipos_formatos_pais_wide$pr_formatoduelo)
summary(tipos_formatos_pais_wide$pr_formatoperiodistas)
sd(tipos_formatos_pais_wide$pr_formatoperiodistas)
summary(tipos_formatos_pais_wide$pr_formatomoderadores)
sd(tipos_formatos_pais_wide$pr_formatomoderadores)
summary(tipos_formatos_pais_wide$pr_formatopresentes)
sd(tipos_formatos_pais_wide$pr_formatopresentes)
summary(tipos_formatos_pais_wide$pr_formatosectores)
sd(tipos_formatos_pais_wide$pr_formatosectores)
summary(tipos_formatos_pais_wide$pr_formatovirtuales)
sd(tipos_formatos_pais_wide$pr_formatovirtuales)
summary(tipos_formatos_pais_wide$pr_formatolibre)
sd(tipos_formatos_pais_wide$pr_formatolibre)
summary(tipos_formatos_pais_wide$pr_formatoexpertos)
sd(tipos_formatos_pais_wide$pr_formatoexpertos)
summary(tipos_formatos_pais_wide$pr_formatoexpositivo)
sd(tipos_formatos_pais_wide$pr_formatoexpositivo)

# PARA TEMAS #############
# agrupado por año t ################
# tabla de distribucion por año, para calcular TEMAS

tipos_temas_año <- base_temas %>% 
  group_by(ncat_eleccion) %>% 
  mutate(n_debates_en_año = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(ncat_eleccion, n_debates_en_año, cat_tipo_tema) %>% 
  summarise(n_peso_temas_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE),
            n_temas_en_año = n())  %>% 
  mutate(pr_temas_en_año = n_temas_en_año/ n_debates_en_año )

tipos_temas_año_wide <- tipos_temas_año %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_temas_en_año) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100))  

summary(tipos_temas_año_wide$pr_temabloques)
sd(tipos_temas_año_wide$pr_temabloques)
summary(tipos_temas_año_wide$pr_temalibre)
sd(tipos_temas_año_wide$pr_temalibre)
summary(tipos_temas_año_wide$pr_temamonotema)
sd(tipos_temas_año_wide$pr_temamonotema)
summary(tipos_temas_año_wide$pr_temapuntuales)
sd(tipos_temas_año_wide$pr_temapuntuales)
# OJO PARA COMPARAR CREO QUE CONVIENE ESTANDARIZAR ESTAS MEDIDAS, LO MISMO APLICARIA A TIPO DE ORGA

# para calcular tabla

tipos_temas_pais <- base_temas %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_en_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE),
            n_tema_en_pais = n()) %>% 
  mutate(pr_tema_en_pais = n_tema_en_pais/ n_debates_en_pais )

# agrupado por pais e ################
# tabla de distribucion por pais, para calcular
tipos_temas_pais <- base_temas %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_en_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE),
            n_tema_en_pais = n()) %>% 
  mutate(pr_tema_en_pais = n_tema_en_pais/ n_debates_en_pais )

tipos_temas_pais_wide <- tipos_temas_pais %>% 
  select(-c(n_peso_tema_xdebate, n_tema_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_tema_en_pais) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100)) %>% 
  arrange(n_debates_en_pais)

summary(tipos_temas_pais_wide$pr_temalibre)
sd(tipos_temas_pais_wide$pr_temalibre)
summary(tipos_temas_pais_wide$pr_temabloques)
sd(tipos_temas_pais_wide$pr_temabloques)
summary(tipos_temas_pais_wide$pr_temapuntuales)
sd(tipos_temas_pais_wide$pr_temapuntuales)
summary(tipos_temas_pais_wide$pr_temamonotema)
sd(tipos_temas_pais_wide$pr_temamonotema)
