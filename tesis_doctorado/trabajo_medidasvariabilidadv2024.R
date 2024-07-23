
##### PREPARACION ###############
## librerias
library(tidyverse)
library(xtsum)


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
# Cross-Sectional Variability: We group by Time and calculate the standard deviation for each variable across groups.
# Time Series Variability: We group by Group and calculate the standard deviation for each variable over time.
# WHITIN : EN EL T PARA UN INDIV
# BETWEEN: ENTRE INDIVS PARA UN T 
#Calculate Within-Group Variability: Calculate the standard deviation within each group.
# Calculate within-group variability
# within_group_variability <- data %>%
#   group_by(Country) %>%
#   summarise(Within_SD = sd(Value, na.rm = TRUE))
# 
# #Calculate Between-Group Variability: First, compute the group means, and then calculate the standard deviation of these means.
# # Calculate group means at each time point
# group_means <- data %>%
#   group_by(Time, Country) %>%
#   summarise(Group_Mean = mean(Value, na.rm = TRUE), .groups = 'drop')
# 
# # Calculate overall mean for each time point
# overall_means <- group_means %>%
#   group_by(Time) %>%
#   summarise(Overall_Mean = mean(Group_Mean, na.rm = TRUE), .groups = 'drop')
# 
# # Merge group means with overall means
# merged_means <- merge(group_means, overall_means, by = "Time")
# 
# # Calculate between-group variability
# between_group_variability <- merged_means %>%
#   group_by(Time) %>%
#   summarise(Between_SD = sqrt(mean((Group_Mean - Overall_Mean)^2)), .groups = 'drop')
 
# VARIABLE DEPENDIENTE CUANTITATIVA #######
# 
# # CREO DATA 
# debates_año_pais <- base %>% 
#   group_by(ncat_eleccion, cat_pais) %>% 
#   summarise(n_debates_año_pais = n()) 
# 
# base_anual_full <- base_anual_full %>% 
#   left_join(debates_año_pais) %>% 
#   mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))  
# 
# # CALCULO VARIACIONES ENTRE INDIVIDUOS Y EN EL TIEMPO 
# base_elecciones_n_e <- base_anual_full %>% 
#   group_by(cat_pais) %>% 
#   summarise(n_debates_elecs = mean(n_debates_año_pais, na.rm = T),
#             sd_n_debates_elecs_within = sd(n_debates_año_pais, na.rm = T))
# 
# # BETWEEN : VARIACION Y PROMEDIO ENTRE PAISES EN LA CANTIDAD DE DEBATES POR ELECCION, incluyendo las que no se hicieron debates
# summary(base_elecciones_n_e$n_debates_elecs) 
# sd(base_elecciones_n_e$n_debates_elecs, na.rm=T) # between
# base_elecciones_n_e
# mean(base_elecciones_n_e$sd_n_debates_elecs_within, na.rm=T) # average within 
# 
# # agrupado por año t ################
# 
# base_elecciones_n_t <- base_anual_full %>% 
#   group_by(ncat_eleccion) %>% 
#   summarise(n_debates_elecs = mean(n_debates_año_pais),
#             sd_n_debates_elecs_between = sd(n_debates_año_pais, na.rm = T))
# 
# # VARACION ENTRE AÑOS PARA TODOS LOS INDIVS EN SU CONJUNTO 
# summary(base_elecciones_n_t$n_debates_elecs) 
# sd(base_elecciones_n_t$n_debates_elecs) # overall within 
# base_elecciones_n_t
# mean(base_elecciones_n_t$sd_n_debates_elecs_between, na.rm=T) # average between
# 
# # en general parece haber mas variacion WITHIN que BETWEEN pero me confunde las medidas precisas a considerar
# 
# 
# # SIGUIENDO SUGERENCIA DE CHATGPT ############
# 
# # Calculate within-group variability
# within_group_variability <- base_anual_full %>%
#   group_by(cat_pais) %>%
#   summarise(Within_SD = sd(n_debates_año_pais, na.rm = TRUE), .groups = 'drop')
# 
# print(within_group_variability) 
# mean(within_group_variability$Within_SD, na.rm = TRUE)
# 
# # Calculate group means at each time point
# group_means <- base_anual_full %>%
#   group_by(ncat_eleccion, cat_pais) %>%
#   summarise(Group_Mean = mean(n_debates_año_pais, na.rm = TRUE), .groups = 'drop')
# 
# # Calculate overall mean for each time point
# overall_means <- group_means %>%
#   group_by(ncat_eleccion) %>%
#   summarise(Overall_Mean = mean(Group_Mean, na.rm = TRUE), .groups = 'drop')
# 
# # Merge group means with overall means
# merged_means <- merge(group_means, overall_means, by = "ncat_eleccion")
# 
# # Calculate between-group variability
# between_group_variability <- merged_means %>%
#   group_by(ncat_eleccion) %>%
#   summarise(Between_SD = sqrt(mean((Group_Mean - Overall_Mean)^2)), .groups = 'drop')
# 
# print(between_group_variability) 
# mean(between_group_variability$Between_SD, na.rm = TRUE)
# 
# # SIGUIENDO COMANDO XTSUM DE STATA ##########
# 
# # xtsum(
# #   data,
# #   variables = NULL,
#   id = NULL,
#   t = NULL,
#   na.rm = FALSE,
#   return.data.frame = TRUE,
#   dec = 3
# )
# Arguments
# 
# data A data.frame or pdata.frame object representing panel data.
# 
# variables (Optional) Vector of variable names for which to calculate statistics. If not provided, all numeric variables in the data will be used.
# 
# id (Optional) Name of the individual identifier variable.
# 
# t (Optional) Name of the time identifier variable.
# 
# na.rm Logical indicating whether to remove NAs when calculating statistics.
# 
# return.data.frame If the return object should be a dataframe
# 
# dec Number of significant digits to report
# CREO DATA 
debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n()) 

base_anual_full <- base_anual_full %>% 
  left_join(debates_año_pais) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))  

xtsum::xtsum(base_anual_full,
             variables = c("n_debates_año_pais"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# confirmado, hay mas variacion WITHIN que BETWEEN !!

xtsum::xtsum(base_anual_full,
             variables = c("dico_hubo_debates"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)
# confirmado, hay mas variacion WITHIN que BETWEEN !!

# nota sobre interpretacion

# The table above can be interpreted as below paraphrased from (StataCorp 2023).
# 
# The overall and within are calculated over N = 28,467 person-years of data. 
# The between is calculated over n = 4,710 persons, 
# and the average number of years a person was observed in the hours data isT = 6.
# 
# xtsum also reports standard deviation(SD), minimums(Min), and maximums(Max).
# 
# Hours worked varied between Overal Min = 1 and Overall Max = 168. 
# Average hours worked for each woman varied between betweenMin = 1 and betweenMax = 83.5. 
# “Hours worked within” varied between within Minwithin = −2.15 and within Max = 130.1, 
# which is not to say that any woman actually worked negative hours. 
# The within number refers to the deviation from each individual’s average, and naturally, 
# some of those deviations must be negative. 
# Then the negative value is not disturbing but the positive value is. 
# Did some woman really deviate from her average by Maxwitihn hours? 
# No. In our definition of within, we add back in the global average of Meanoverall hours. 
# Some woman did deviate from her average by MAXwithin − Meanoverall = 93.5 hours, which is still large.
# 
# The reported standard deviations tell us that the variation in hours worked last week across women is nearly equal to that observed within a woman over time. That is, if you were to draw two women randomly from our data, the difference in hours worked is expected to be nearly equal to the difference for the same woman in two randomly selected years.
# 
# More detailed interpretation can be found in handout(Porter n.d.)



# PARA TIPOS DE ORG ################# 
# agrupado por año t ################
# distribuciones de las varibales por año, para ver numericamente variablidad 
# prop de orgs 
# primero para entidades indivs, luego para debates con ppac de entidades

cuenta_tipos_por_cat_pais_eleccion <- base_organizadores %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate(n_debates_en_pais_año = n_distinct(id_debate)) %>% 
  ungroup() %>%
  group_by(cat_tipoorgv2, cat_pais, ncat_eleccion) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate),
            n_debates_en_pais_año = mean(n_debates_en_pais_año)) %>% 
  ungroup() %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_en_pais_año*100) 

tabla_cuenta_tipos_por_ncat_eleccion_individuos <- cuenta_tipos_por_cat_pais_eleccion %>% 
  select(cat_tipoorgv2, ncat_eleccion, cat_pais, pr_n_individuos) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)

xtsum::xtsum(tabla_cuenta_tipos_por_ncat_eleccion_individuos,
             variables = c("mmc", "mmp", "estado", "educ", "osc"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# ojo que en esta interpretacion no estamos teniendo en cuenta los años sin debates
# si los tenemos en cuenta:  , la variabilidad en el tiempo es la mas importante!!! 
tabla_agregada_tabla_cuenta_tipos_por_ncat_eleccion_individuos <- base_anual_full %>% 
  left_join(tabla_cuenta_tipos_por_ncat_eleccion_individuos) %>% 
  mutate(across(c("mmc", "mmp", "estado", "educ", "osc"), ~ifelse(is.na(.),0,.)))

xtsum::xtsum(tabla_agregada_tabla_cuenta_tipos_por_ncat_eleccion_individuos,
             variables = c("mmc", "mmp", "estado", "educ", "osc"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

tabla_cuenta_tipos_por_ncat_eleccion_debate <- cuenta_tipos_por_cat_pais_eleccion %>% 
  select(cat_tipoorgv2, ncat_eleccion, cat_pais, pr_debates_en_participaron) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron)

xtsum::xtsum(tabla_cuenta_tipos_por_ncat_eleccion_debate,
             variables = c("mmc", "mmp", "estado", "educ", "osc"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)


# PARA PATRONES DE INTERACCION FORMATOS ###############

# tabla de distribucion por año y pais, para calcular
tipos_formatos_año_pais <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  mutate(n_debates_en_año_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(ncat_eleccion, cat_pais, n_debates_en_año_pais, cat_tipo_formato) %>% 
  summarise(n_formato_en_año_pais = n()) %>% 
  mutate(pr_formato_en_año_pais = n_formato_en_año_pais / n_debates_en_año_pais )

tipos_formatos_año_pais_wide <- tipos_formatos_año_pais %>% 
  select(ncat_eleccion, cat_pais, cat_tipo_formato, pr_formato_en_año_pais) %>% 
  pivot_wider(names_from = cat_tipo_formato, values_from = pr_formato_en_año_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100))  

xtsum::xtsum(tipos_formatos_año_pais_wide,
             variables = c("pr_formatoduelo", "pr_formatolibre",
                           "pr_formatoexpositivo", "pr_formatomoderadores",
                           "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
                           "pr_formatopresentes","pr_formatovirtuales"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# aca abajo considerando elecciones sin debates como 0, obviamente en todos los casos la variacion within es mas elevada que la between
tabla_agregada_tipos_formatos_año_pais_wide <- base_anual_full %>% 
  left_join(tipos_formatos_año_pais) %>% 
  select(ncat_eleccion, cat_pais, cat_tipo_formato, pr_formato_en_año_pais) %>% 
  pivot_wider(names_from = cat_tipo_formato, values_from = pr_formato_en_año_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100)) 

xtsum::xtsum(tabla_agregada_tipos_formatos_año_pais_wide,
             variables = c("pr_formatoduelo", "pr_formatolibre",
                           "pr_formatoexpositivo", "pr_formatomoderadores",
                           "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
                           "pr_formatopresentes","pr_formatovirtuales"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)


# MEDIDAS ORDINALES ####################


ncat_ordinales_por_año_pais <- base %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(mean_ncat_competencia_pais_año = mean(ncat_competencia, na.rm=T),
            mean_ncat_ppac_pais_año = mean(ncat_ppac, na.rm=T))

xtsum::xtsum(ncat_ordinales_por_año_pais,
             variables = c("mean_ncat_competencia_pais_año", "mean_ncat_ppac_pais_año"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

ncat_ordinales_por_año_pais_agregada <- base_anual_full %>% 
  left_join(ncat_ordinales_por_año_pais) %>% 
  mutate(mean_ncat_ppac_pais_año = ifelse(is.na(mean_ncat_ppac_pais_año),0,mean_ncat_ppac_pais_año),
         mean_ncat_competencia_pais_año = ifelse(is.na(mean_ncat_competencia_pais_año),0,mean_ncat_competencia_pais_año))

xtsum::xtsum(ncat_ordinales_por_año_pais_agregada,
             variables = c("mean_ncat_competencia_pais_año", "mean_ncat_ppac_pais_año"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# PARA TEMAS #############
# tabla de distribucion por año pais para calcular TEMAS

tipos_tema_año_pais <- base_temas %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  mutate(n_debates_en_año_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(ncat_eleccion, cat_pais, n_debates_en_año_pais, cat_tipo_tema) %>% 
  summarise(n_tema_en_año_pais = n()) %>% 
  mutate(pr_tema_en_año_pais = n_tema_en_año_pais / n_debates_en_año_pais )

tipos_tema_año_pais_wide <- tipos_tema_año_pais %>% 
  select(ncat_eleccion, cat_pais, cat_tipo_tema, pr_tema_en_año_pais) %>% 
  pivot_wider(names_from = cat_tipo_tema, values_from = pr_tema_en_año_pais) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100))  

xtsum::xtsum(tipos_tema_año_pais_wide,
             variables = c("pr_temabloques", "pr_temalibre",
                           "pr_temapuntuales", "pr_temamonotema"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# aca abajo considerando elecciones sin debates como 0, obviamente en todos los casos la variacion within es mas elevada que la between
tabla_agregada_tipos_tema_año_pais_wide <- base_anual_full %>% 
  left_join(tipos_tema_año_pais) %>% 
  select(ncat_eleccion, cat_pais, cat_tipo_tema, pr_tema_en_año_pais) %>% 
  pivot_wider(names_from = cat_tipo_tema, values_from = pr_tema_en_año_pais) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100)) 

xtsum::xtsum(tabla_agregada_tipos_tema_año_pais_wide,
             variables = c("pr_temabloques", "pr_temalibre",
                           "pr_temapuntuales", "pr_temamonotema"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)

# N CANDIDATOS ########################


n_candidatos_año_pais <- base %>% 
  left_join(base_anual_full) %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(mean_n_invitados = mean(n_invitados, na.rm=T),
            mean_n_presentes = mean(n_presentes, na.rm=T),
            mean_n_ausentes = mean(n_ausentes, na.rm=T),
            prop_n_invitados = mean_n_invitados/mean(n_candidaturas))

xtsum::xtsum(n_candidatos_año_pais,
             variables = c("mean_n_invitados", "mean_n_presentes","mean_n_ausentes","prop_n_invitados"),
             id = "cat_pais",
             t = "ncat_eleccion",
             na.rm=T)
