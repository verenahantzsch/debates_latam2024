
# https://arcruz0.github.io/libroadp/logit.html#representaci%C3%B3n-visual-de-los-resultados
# primera version bastante avanzada de este codigo en test_modelos_dic2024.R
# librerias ########### 

library(tidyverse)
library(sandwich) # para clusterizar errores estandares
library(lmtest) # para clusterizar errores estandares

# seed #################
set.seed(111)

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

base_vdependiente <- read.csv("variables_dependientes_elecciones.csv")
base_indicadores <- read.csv("indicadores_elecciones.csv")
base_controles <- read.csv("controles_elecciones.csv")
base_candidatos <-  read.csv("indicadores_candidatos.csv")
diccionario_indicadores <- read.csv("diccionario_indicadores_etiquetas.csv")
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# ELECCIONES ##############################
## PREPARO DATA #####
### data completa (al final no la uso) #### 
data <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-starts_with("X")) %>% 
  select(-eng_cat_pais, -dico_debates_eleccion) %>% 
  left_join(base_controles %>% 
              select(-starts_with("source_")) %>% 
              select(-X, -eng_cat_pais)) %>% 
  left_join(base_vdependiente %>% select(-X)) 

# creo variable para clusterizar SSEE
data <- data %>% 
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

### data sólo democracias #### 
democracias <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-starts_with("X")) %>% 
  select(-eng_cat_pais, -dico_debates_eleccion) %>% 
  left_join(base_controles %>% 
              select(-starts_with("source_")) %>% 
              select(-X, -eng_cat_pais)) %>% 
  left_join(base_vdependiente %>% select(-X)) %>% 
    subset(democraciavdempolyarchy>0.45) 

# creo variable para clusterizar SSEE
democracias <- democracias %>% 
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

### defino id obs ####

democracias <- democracias %>% 
  mutate(obsid = paste(cat_pais, ncat_eleccion, ncat_ronda, sep = " "))

### creo data log y otras transformaciones de variables ####

democracias <- democracias %>% 
  mutate(marginvic = abs(marginvic)) %>% 
  mutate(lnpropindivinternet = log(propindivinternet +1),
         lngdp = log(gdpxcapita),
         lnmarginvic = log(marginvic),
         lnnec = log(nec),
         lnvoteshareincumbent = log(voteshareincumbent + 1),
         lncumsumpastciclos = log(cumsum_pastciclos + 1)) %>% 
  mutate(regulacionotradico = ifelse(regulaciongarantiasdico==1|regulacionposibilidaddico==1,
                                     1,0))


# democracias$marginvic

### data estandarizada (toda data/democracias) #### 
# data_scaled <- data %>%
#   mutate(across(-c(dico_hubo_debates,  # excluyo las variables dep dicotomicas de la escalada
#                    dico_debates_primerosdos,
#                    dico_hubo_debate_mediatico,
#                    dico_nueva_practica_elec,
#                    dico_practica_interrumpida_elec,
#                    dico_nueva_practica_ciclo,
#                    dico_practica_interrumpida_ciclo,
#                    cat_pais,
#                    elecid), scale))

democracias_reservanoescalada <- democracias

democracias <- democracias %>%
  mutate(across(-c(dico_hubo_debates,  # excluyo las variables dep dicotomicas de la escalada
                   dico_debates_primerosdos,
                   dico_hubo_debate_mediatico,
                   dico_nueva_practica_elec,
                   dico_practica_interrumpida_elec,
                   dico_nueva_practica_ciclo,
                   dico_practica_interrumpida_ciclo,
                   cat_pais,
                   elecid,
                   ncat_ronda, #excluyo algunas variables de id
                   ncat_eleccion,
                   obsid), scale))
  
### mini exploracion ####

skimr::skim(democracias)
# problemas de missing en propindiv internet (seguro se puede completar)
# problemas de missing en acceso gratuito (tratar de completar)
# problemas de missing en gdp (una picardia total)

## MODELOS 1: toda la muestra de democracias - V.D = dico_hubo_debates #####
### Modelo contingencia  ####

formula_modelo_contingencia <- "dico_hubo_debates ~ 
                                       #dico_debates_primerosdos ~ # variables V.D alternativas, por ahora no uso, serían secundarias 
                                       #dico_hubo_debate_mediatico ~ 
                              marginvic + 
                              nec +
                             #exapprovalnotsmoothed + # este indicador y el que sigue son alternativos 
                              voteshareincumbent +
                              dico_reeleccion + 
                              regulaciondico +
                              cumsum_pastciclos + # este indicador y el que sigue son alternativos 
                              #dico_debates_pastelection +
                              gdpxcapita +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem"
                               #edadregimenbmr#, #Sacar edad de régimen.  
                               #No tiene sentido incluirla si controlas por la calidad de la democracia 
                               # ademas CAMBIAN MUY POCO resultados relevantes, chequeado

modelo_contingencia <- glm(formula_modelo_contingencia, 
                           family = binomial(link = "logit"), 
                           #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                           data = democracias)
options(scipen=999)
summary(modelo_contingencia)

formula_modelo_contingencia_bis <- "dico_hubo_debates ~ 
                                       #dico_debates_primerosdos ~ # variables V.D alternativas, por ahora no uso, serían secundarias 
                                       #dico_hubo_debate_mediatico ~ 
                               lnmarginvic + # CAMBIE
                                lnnec +
                             #exapprovalnotsmoothed + # este indicador y el que sigue son alternativos 
                              voteshareincumbent +
                              #lnvoteshareincumbent +
                              dico_reeleccion + 
                              regulaciondico +
                              cumsum_pastciclos + # este indicador y el que sigue son alternativos 
                              #dico_debates_pastelection +
                              lngdp +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem"

#edadregimenbmr#, #Sacar edad de régimen.  
#No tiene sentido incluirla si controlas por la calidad de la democracia 
# ademas CAMBIAN MUY POCO resultados relevantes, chequeado

modelo_contingencia_bis <- glm(formula_modelo_contingencia_bis, 
                           family = binomial(link = "logit"), 
                           #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                           data = democracias)
options(scipen=999)
summary(modelo_contingencia_bis)


### Modelo sistemico ####


formula_modelo_sistemico <- "dico_hubo_debates ~ 
                                    alineamiento + 
                                    #volatility + #
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    gdpxcapita +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem"

modelo_sistemico <- glm(formula_modelo_sistemico,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_sistemico) # 


formula_modelo_sistemico_bis <- "dico_hubo_debates ~ 
                                    alineamiento + 
                                    #volatility + #pierdo muchas observaciones! dejar para despues
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    lngdp +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem"

modelo_sistemico_bis <- glm(formula_modelo_sistemico_bis,
                        family = binomial(link = "logit"), 
                        #data = data 
                        data = democracias)

summary(modelo_sistemico_bis) # 

### Modelo marco regulatorio ####

formula_modelo_regulatorio <- "dico_hubo_debates ~ 
                                prohibicionpropaganda +
                                accesogratuito +
                                regulaciondico + 
                                cumsum_pastciclos +
                                #dico_debates_pastelection +
                                gdpxcapita +
                                democraciavdemelectoralcomp +
                                mediaqualitycorruptvdem"
                                #edadregimenbmr"


modelo_regulatorio <- glm(formula_modelo_regulatorio,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_regulatorio)


formula_modelo_regulatorio_bis <- "dico_hubo_debates ~ 
                                prohibicionpropaganda +
                                accesogratuito +
                                regulaciondico + 
                                cumsum_pastciclos +
                                #dico_debates_pastelection +
                                lngdp +
                                democraciavdemelectoralcomp +
                                mediaqualitycorruptvdem"
#edadregimenbmr"


modelo_regulatorio_bis <- glm(formula_modelo_regulatorio_bis,
                          family = binomial(link = "logit"), 
                          #data = data 
                          data = democracias)

summary(modelo_regulatorio_bis)

### Modelo geo/temp ####

formula_modelo_difusion <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_difusion <- glm(formula_modelo_difusion,
                         family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_difusion)

formula_modelo_difusion_bis <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         lngdp +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_difusion_bis <- glm(formula_modelo_difusion_bis,
                       family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_difusion_bis)

### Modelo final / sficativas ####

formula_modelo_sficativas <- "dico_hubo_debates ~ 
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_sficativas <- glm(formula_modelo_sficativas,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas)


### Modelo final / sficativas variantes ####
# variantes modelo sficativas
formula_modelo_sficativas_variantes <- "dico_hubo_debates ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                          #lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           #alineamiento +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           #lncumsumpastciclos +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_sficativas_variantes <- glm(formula_modelo_sficativas_variantes,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes)



### Modelo all #####

formula_modelo_all <- "dico_hubo_debates ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           #lnvoteshareincumbent +
                           dico_reeleccion + 
                           alineamiento +
                           proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_all <- glm(formula_modelo_all,
                                   family = binomial(link = "logit"), 
                                   #data = data 
                                   data = democracias)
options(scipen=999)
summary(modelo_all)
## MODELOS ROBUSTOS   Cluster-Robust Standard Errors #####
 
##### Modelo contingencia  ####

robust_se_cluster_modelo_contingencia <- coeftest(modelo_contingencia_bis, 
                                                  vcov = vcovCL(modelo_contingencia_bis, 
                                                                #cluster = democracias$elecid))
                                                                #cluster = data$elecid))
                                                                cluster = democracias$cat_pais))


print(robust_se_cluster_modelo_contingencia)
# 
# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_contingencia[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_contingencia))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

##### Modelo sistemico ####

robust_se_cluster_modelo_sistemico <- coeftest(modelo_sistemico_bis, 
                                               vcov = vcovCL(modelo_sistemico_bis,
                                                             #cluster = democracias$elecid))
                                                             #cluster = data$elecid))
                                                             cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sistemico)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_sistemico[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_sistemico))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

##### Modelo marco regulatorio ####

robust_se_cluster_modelo_regulatorio <- coeftest(modelo_regulatorio_bis, 
                                                 vcov = vcovCL(modelo_regulatorio_bis, 
                                                               #cluster = democracias$elecid))
                                                               #cluster = data$elecid))
                                                               cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_regulatorio)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_regulatorio[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_regulatorio))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

##### Modelo geo/temp ####

robust_se_cluster_modelo_difusion <- coeftest(modelo_difusion_bis, 
                                              vcov = vcovCL(modelo_difusion_bis, 
                                                            #cluster = democracias$elecid))
                                                            #cluster = data$elecid))
                                                            cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_difusion)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_difusion[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_difusion))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

##### Modelo sficativas ####

robust_se_cluster_modelo_sficativas <- coeftest(modelo_sficativas, 
                                                vcov = vcovCL(modelo_sficativas, 
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_sficativas[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_sficativas))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

##### Modelo final / sficativas  ####
robust_se_cluster_modelo_sficativas_variantes <- coeftest(modelo_sficativas_variantes, 
                                                          vcov = vcovCL(modelo_sficativas_variantes, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes)


#### Modelo all ####


robust_se_cluster_modelo_all <- coeftest(modelo_all, 
                                         vcov = vcovCL(modelo_all,
                                                       cluster = democracias$cat_pais))

print(robust_se_cluster_modelo_all)

### REESTIMACION sin outliers ##### 

# # otros potenciales: Colombia 1998 1 , 
# El salvador 2014 2 para arriba marginvic. 
# Brasil 1980 2. 
# Colombia 1986 1era . 


data_s_outliers <- democracias %>% 
  mutate(filtrar = ifelse(cat_pais=="Argentina"&ncat_eleccion==2003&ncat_ronda==1|
                            cat_pais=="Brasil"&ncat_eleccion==2018&ncat_ronda==2|
                            cat_pais=="Brasil"&ncat_eleccion==1989&ncat_ronda==2|
                            cat_pais=="Bolivia"&ncat_eleccion==2019&ncat_ronda==1|
                            cat_pais=="Colombia"&ncat_eleccion==2018&ncat_ronda==2|
                            cat_pais=="Colombia"&ncat_eleccion==1998&ncat_ronda==1|
                            cat_pais=="Costa Rica"&ncat_eleccion==2014&ncat_ronda==2|
                            cat_pais=="Ecuador"&ncat_eleccion==2017&ncat_ronda==2|
                            cat_pais=="Honduras"&ncat_eleccion==1993&ncat_ronda==1|
                            cat_pais=="Nicaragua"&ncat_eleccion==1990&ncat_ronda==1|
                            cat_pais=="Peru"&ncat_eleccion==1990&ncat_ronda==2|
                            cat_pais=="Republica Dominicana"&ncat_eleccion==2020&ncat_ronda==1|
                            cat_pais=="Republica Dominicana"&ncat_eleccion== 2016&ncat_ronda==1,
                          1, 0)) %>% 
  subset(filtrar==0) %>% 
  select(dico_hubo_debates,
         cat_pais,
         ncat_eleccion,
         ncat_ronda,
         elecid,
         obsid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
         # alineamiento,
         # proptv,
         propindivinternet,
         # prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem,
         lnpropindivinternet,
         lngdp,
         lnmarginvic,
         lnnec,
         lnvoteshareincumbent) %>% 
  na.omit()

modelo_sficativas_s_outliers <- glm(formula_modelo_sficativas,
                                    family = binomial(link = "logit"), 
                                    #data = data 
                                    data = data_s_outliers)
summary(modelo_sficativas_s_outliers)
summary(modelo_sficativas)


# al quitar outliers... 
# dico_reeleccion pierde sficancia
# nec y regulaciondico mejoran ligeramente magnitud

modelo_sficativas_variantes_s_outliers <- glm(formula_modelo_sficativas_variantes,
                                              family = binomial(link = "logit"), 
                                              #data = data 
                                              data = data_s_outliers)
options(scipen=999)
summary(modelo_sficativas_variantes_s_outliers)
summary(modelo_sficativas_variantes)



robust_se_cluster_modelo_sficativas_variantes_s_outliers <- coeftest(modelo_sficativas_variantes_s_outliers, 
                                                                     vcov = vcovCL(modelo_sficativas_variantes_s_outliers, 
                                                                                   #cluster = democracias$elecid))
                                                                                   #cluster = data$elecid))
                                                                                   cluster = data_s_outliers$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes_s_outliers)
# al quitar outliers... 
# simil anterior: dico_reeleccion pierde sficancia
# acceso gratuito gana sficancia 
# simil anterior:nec y regulaciondico mejoran ligeramente magnitud

# ppal conclusion: dico_reeleccion es SENSIBLE a algunos casos...



## MULTINIVEL MODELOS 1 ####
#https://m-clark.github.io/mixed-models-with-R/random_intercepts.html


####  PASO 1 MODELO MULTILEVEL: EMPTY #### 

# Varying-intercept model with no predictors. 
#   M0 <- lmer (y ~ 1 + (1 | county))
# This model simply includes a constant term (the predictor “1”) and allows it to vary by county

empty_model_paises <- lme4::glmer(dico_hubo_debates ~ 
                                    1 + (1 | cat_pais), 
                                  family=binomial("logit"), 
                                  data = democracias)
summary(empty_model_paises)

mlmhelpr::icc(empty_model_paises)
# The ICC represents the proportion of group-level variance to total variance. 
# (para modelo emtpty: ) An ICC of 0.33 means that 33% of the variation in the outcome variable can be accounted for by the clustering stucture of the data. This provides evidence that a multilevel model may make a difference to the model estimates, in comparison with a non-multilevel model. Therefore, the use of multilevel models is necessary and warrantied.
# https://en.wikipedia.org/wiki/Intraclass_correlation

empty_model_years <- lme4::glmer(dico_hubo_debates ~ 
                                   1 + (1 | ncat_eleccion), 
                                 family=binomial("logit"), 
                                 data = democracias)
summary(empty_model_years)

mlmhelpr::icc(empty_model_years)


empty_model_paisyears <- lme4::glmer(dico_hubo_debates ~ 
                                       1 + (1 | ncat_eleccion) + (1 | cat_pais), 
                                     family=binomial("logit"), 
                                     data = democracias)
summary(empty_model_paisyears)

mlmhelpr::icc(empty_model_paisyears)

####  PASO 2 REPORTADO PASO 2 MODELO MULTILEVEL: RANDOM INTERCEPTS BY COUNTRY #### 
# varying-intercept model
# Varying-intercept model with an individual-level predictor. 
# M1 <- lmer (y ~ x + (1 | county))
# This expression starts with the no-pooling model, “y ~ x,” and then adds “(1 | county),” 
# which allows the intercept (the coeﬃcient of the predictor “1,” which is  the column of ones—the constant term in the regression) to vary by county.

#cluster <- democracias$cat_pais %>% as.factor()
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

contingencia_random_intercepts <- lme4::glmer(paste(formula_modelo_contingencia_bis, 
                                                    "(1 | cat_pais)", sep = "+"), 
                                              family=binomial("logit"), 
                                              data = democracias,
                                              # weights = NULL,
                                              control = control)
summary(contingencia_random_intercepts)



sistemico_random_intercepts <- lme4::glmer(paste(formula_modelo_sistemico_bis, 
                                                 "(1 | cat_pais)", sep = "+"), 
                                           family=binomial("logit"), 
                                           data = democracias,
                                           control = control)
summary(sistemico_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(sistemico_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# sistemico_random_intercepts_robust <- coef_test(sistemico_random_intercepts, vcov = vcov_cluster)

regulatorio_random_intercepts <- lme4::glmer(paste(formula_modelo_regulatorio_bis, 
                                                   "(1 | cat_pais)", sep = "+"), 
                                             family=binomial("logit"), 
                                             data = democracias,
                                             control = control)
summary(regulatorio_random_intercepts)
# vcov_cluster <-  clubSandwich::vcovCR(regulatorio_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# regulatorio_random_intercepts_robust <- coef_test(regulatorio_random_intercepts, vcov = vcov_cluster)

difusion_random_intercepts <- lme4::glmer(paste(formula_modelo_difusion_bis, 
                                                "(1 | cat_pais)", sep = "+"), 
                                          family=binomial("logit"), 
                                          data = democracias,
                                          control = control)
summary(difusion_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(difusion_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# difusion_random_intercepts_robust <- coef_test(difusion_random_intercepts, vcov = vcov_cluster)

final_random_intercepts <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                             "(1 | cat_pais)", sep = "+"), 
                                       family=binomial("logit"), 
                                       data = democracias,
                                       control = control)
summary(final_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(final_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# final_random_intercepts_robust <- coef_test(final_random_intercepts, vcov = vcov_cluster)

mlmhelpr::icc(final_random_intercepts)
# The ICC represents the proportion of group-level variance to total variance. 
# (para modelo emtpty: ) An ICC of 0.33 means that 33% of the variation in the outcome variable can be accounted for by the clustering stucture of the data. This provides evidence that a multilevel model may make a difference to the model estimates, in comparison with a non-multilevel model. Therefore, the use of multilevel models is necessary and warrantied.

modelo_random_intercepts <- final_random_intercepts
coef(modelo_random_intercepts)
lme4::fixef(modelo_random_intercepts)  # The estimated regression line in an average county
# The term “ﬁxed eﬀects” is used for the regression coeﬃcients that do not vary by group (such as the coeﬃcient for x in this example) or for group-level coeﬃcients or group averages (such as the average intercept, μα in (12.3)).
lme4::ranef(modelo_random_intercepts)  #  how much the intercept is shifted up or down in particular counties or county level errors
#lme4::confint(gpa_mixed)



## CONTROLES MODELO 1 ####


### Preparaciones p/ control estadistico ####
#### defino data reducida ####
data_reducida <- democracias %>% 
  select(dico_hubo_debates,
         cat_pais,
         ncat_eleccion,
         ncat_ronda,
         elecid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
         alineamiento,
         proptv,
         propindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem,
         lnpropindivinternet,
         lngdp,
         lnmarginvic,
         lnnec,
         lnvoteshareincumbent) %>% 
  na.omit()


data_modelo_sficativas <- democracias %>% 
  select(dico_hubo_debates,
         cat_pais,
         ncat_eleccion,
         ncat_ronda,
         elecid,
         obsid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
        # alineamiento,
        # proptv,
         #propindivinternet,
        # prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem) %>% 
  na.omit() %>% 
  left_join(democracias)

data_modelo_sficativas$elecid==data_s_outliers$elecid

#### defino modelo 0 ####
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

modelo_0 <- lme4::glmer(dico_hubo_debates ~ 1 + (1 | cat_pais), 
                family = binomial(link = "logit"), 
                #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                data = democracias,
                control = control)


#### defino modelos comparables con muestra reducida ####

#### reducida al maximo ####
modelo_0_reducido <- lme4::glmer(dico_hubo_debates ~ 1 + (1 | cat_pais), 
                                    family = binomial(link = "logit"), 
                                    data = data_reducida,
                                 control = control)
modelo_contingencia_reducido <- lme4::glmer(paste(formula_modelo_contingencia_bis, 
                                                  "(1 | cat_pais)", sep = "+"),  
                family = binomial(link = "logit"), 
                data = data_reducida,
                control = control)
modelo_sistemico_reducido <- lme4::glmer(paste(formula_modelo_sistemico_bis, 
                                               "(1 | cat_pais)", sep = "+"),  
                family = binomial(link = "logit"), 
                data = data_reducida,
                control = control)
modelo_regulatorio_reducido <- lme4::glmer(paste(formula_modelo_regulatorio_bis, 
                                                 "(1 | cat_pais)", sep = "+"),  
                family = binomial(link = "logit"), 
                data = data_reducida,
                control = control)
modelo_difusion_reducido <- lme4::glmer(paste(formula_modelo_difusion_bis, 
                                              "(1 | cat_pais)", sep = "+"),  
                family = binomial(link = "logit"), 
                data = data_reducida,
                control = control)
modelo_sficativas_reducido <- lme4::glmer(paste(formula_modelo_sficativas, 
                                                "(1 | cat_pais)", sep = "+"),  
                                family = binomial(link = "logit"), 
                                data = data_reducida,
                                control = control)
modelo_sficativas_variantes_reducido <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                                          "(1 | cat_pais)", sep = "+"),  
                                  family = binomial(link = "logit"), 
                                  data = data_reducida,
                                  control = control)

 

#### reducida sficativas ####
modelo_0_reducido_sficativas <- lme4::glmer(dico_hubo_debates ~ 1 + (1 | cat_pais),
                         family = binomial(link = "logit"), 
                         data = data_modelo_sficativas,
                         control = control)
modelo_contingencia_reducido_sficativas <- lme4::glmer(paste(formula_modelo_contingencia, 
                                                             "(1 | cat_pais)", sep = "+"),  
                                    family = binomial(link = "logit"), 
                                    data = data_modelo_sficativas,
                                    control = control)
modelo_sistemico_reducido_sficativas <- lme4::glmer(paste(formula_modelo_sistemico, 
                                                          "(1 | cat_pais)", sep = "+"),  
                                 family = binomial(link = "logit"), 
                                 data = data_modelo_sficativas,
                                 control = control)
modelo_regulatorio_reducido_sficativas <- lme4::glmer(paste(formula_modelo_regulatorio, 
                                                            "(1 | cat_pais)", sep = "+"),  
                                   family = binomial(link = "logit"), 
                                   data = data_modelo_sficativas,
                                   control = control)
modelo_difusion_reducido_sficativas <- lme4::glmer(paste(formula_modelo_difusion, 
                                                         "(1 | cat_pais)", sep = "+"),  
                                family = binomial(link = "logit"), 
                                data = data_modelo_sficativas,
                                control = control)
modelo_varaintes_reducido <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                               "(1 | cat_pais)", sep = "+"),  
                                           family = binomial(link = "logit"), 
                                           data = data_modelo_sficativas,
                                         control = control)


#### importante: defino modelo de prueba INTERVENIDO : PUSE MULTINIVEL ####

data_modelo_a_probar <- data_modelo_sficativas
modelo_a_probar <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                             "(1 | cat_pais)", sep = "+"), 
                                       family=binomial("logit"), 
                                       data = data_modelo_a_probar,
                                       control = control)


options(scipen=999)
summary(modelo_a_probar)
summary(modelo_sficativas_variantes)

#modelo_a_probar <- modelo_sficativas

### Controles - Control de resiudos ####

##### Graficar los residuos ####
# https://library.virginia.edu/data/articles/understanding-deviance-residuals

quantile(residuals(modelo_a_probar))
sum(residuals(modelo_a_probar)*residuals(modelo_a_probar))
#In general, the reason we might be interested in this summary is to see how well our model is fitting the data. 
# Residuals are the differences between what we observe and what our model predicts.
#It would be nice if our residuals were evenly distributed. 
#We would like for the first quantile and third quantile values 
# and minimum and maximum values to be about the same in absolute value, 
# and for the median to be close to 0. 
# In addition, we would like to see the minimum and maximum valuesbe less than about 3 in absolute value. 
# This is because deviance residuals can be roughly approximated with a standard normal distribution when the model holds (Agresti, 2002). 
# Residuals greater than the absolute value of 3 are in the tails of a standard normal distribution and usually indicate strain in the model.

par(mfrow = c(2, 2))

# Response
# raw residuals
raw_residuals <- residuals(modelo_a_probar, type = "response") 
plot(raw_residuals, main = "Residuos Raw", ylab = "Residuos", xlab = "Índice de observación")
abline(h = 0, col = "red3", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(raw_residuals), y = raw_residuals, 
     labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue4")  # Ajusta 'pos' para la posición del texto

# Residuos deviance
residuals_dev <- residuals(modelo_a_probar, type = "deviance")
plot(residuals_dev, main = "Residuos Deviance", ylab = "Residuos", xlab = "Índice de observación")
abline(h = 0, col = "red3", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_dev), y = residuals_dev, 
     labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue4")  # Ajusta 'pos' para la posición del texto
# These are based on a weird-looking formula derived from the likelihood ratio test for comparing logistic regression models, where we compare our current model to a saturated model. A saturated model is a model with as many coefficients as there are observations. The formula for calculating this test statistic for a single observation produces the deviance residual.
# These are the deviance residuals we see summarized in the model summary output. 
# If we square the deviance residuals and add them all up, we get the residual deviance statistic we see printed at the bottom of the summary output:

# Residuos de Pearson
#Another type of residual is the Pearson residual. It is the raw residual divided by the estimated standard deviation of a binomial distribution with number of trials equal to 1 and p equal to 
# The Pearson residual is basically a rescaled version of the raw residual.  
residuals_pearson <- residuals(modelo_a_probar, type = "pearson")
plot(residuals_pearson, main = "Residuos de Pearson", ylab = "Residuos", xlab = "Índice de observación")
abline(h = 0, col = "red3", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_pearson), y = residuals_pearson, 
     labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue4")  # Ajusta 'pos' para la posición del texto

 
# Residuos Pearsonestandarizados 
#Yet another residual is the standardized Pearson residual. This is the Pearson residual adjusted for the leverage of predictors using what are called "hat values." Hat values measure the distance of individual predictors from the mean of the predictors. High hat values indicate a subject or row could have outlying predictor values. This in turn could mean that a subject or row has substantial leverage in determining the predicted response. This adjustment then increases the absolute value of certain residuals based on the leverage of the associated predictors. We’ll call this residual 
# importantes porque tienen en cuenta el leverage!!
hat_values <- hatvalues(modelo_a_probar)  # Leverage
residuos_pearson_est <- residuals_pearson / sqrt(1 - hat_values)
plot(residuos_pearson_est, main = "Residuos de Pearson Estandarizados", ylab = "Residuos", xlab = "Índice de observación")
abline(h = 0, col = "red3", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuos_pearson_est), y = residuos_pearson_est, 
     labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue4")  # Ajusta 'pos' para la posición del texto

par(mfrow = c(1,2))
plot(modelo_a_probar, which = 1)#, 
    # main = "Residuos vs valores predichos",), ylab = "Residuos de Pearson estandarizados", xlab = "Log odds predichas") # Standardized Pearson residuals are plotted on the y-axis versus predicted log-odds on the x-axis. 
# SI EN EJE VERTICAL PUNTO APARECE MUY ABAJO: PREDIGO ALTA PROBA PERO FALLO, SE OBSERVA NO OCURRENCIA
# SI EN EJE VERTICAL PUNTO APARECE MUY ARRIBA: PREDIGO BAJA PROBA PERO FALLO, SE OBSERVA OCURRENCIA
#Me aplica: It appears at the lower predicted values, we’re under-fitting The observed proportions are larger than the predicted proportions. 
# At the medium predicted values, we're over-fitting for a couple of observations. The observed proportions are much smaller than the predicted proportions.


plot(modelo_a_probar, which = 2)


data_modelo_a_probar$pred <- predict(modelo_a_probar, type = "response")
data_modelo_a_probar$deviance <- residuals(modelo_a_probar)
data_modelo_a_probar[c(162,32,61),"obsid"]
data_modelo_a_probar[c(161,39,72,90),]

# Earlier we mentioned that standardized Pearson residuals have an approximate standard normal distribution if the model fits. This implies looking at a QQ Plot of residuals can provide some assessment of model fit. We can produce this plot using plot() with which = 2. Again the plot created with the group-level model is more informative than the plot created with the subject-level model.
# This suggests our model is holding well in the middle range of predicted values but is maybe suspect in the extremities. It’s worth mentioning that binomial logistic regression models have no error term or normality assumptions. In a standard linear model, this plot assesses normality of residuals, which is one of the assumptions of a linear model. But in a binary logistic regression model, normality of residuals is simply evidence of a decent fitting model. There is no assumption that residuals are random draws from a normal distribution.

qqnorm(residuals_dev, main = "QQ Plot - Residuos de Deviance")
qqline(residuals_dev, col = "red")

# no parecen detectarse patrones claros que indiquen violaciones a supuesto de variable omitida o heterocedasticidad
# si parece haber dos observaciones potencialmente problemáticas (outliers o con leverage)
# COLOMBIA Y BRASIL 2018
# Tambien parece haber mas residuos de un signo que del otro (no me queda claro bien cual)


##### Durbin-Watson para autocorrelacion de errores - NO SE PUDO CON MULTINIVEL ####
# prueba de Durbin-Watson (verificar autocorrelación en los residuos).
# The Durbin-Watson test has the null hypothesis that the autocorrelation of the disturbances is 0. 
# It is possible to test against the alternative that it is greater than, not equal to, or less than 0, respectively. This can be specified by the alternative argument.
# https://medium.com/@analyttica/durbin-watson-test-fde429f79203
# no es lo más adecuado de usar cuando tengo variables rezagadas (como es mi caso)
#dwtest(modelo_a_probar, alternative = "two.sided")
# valor del estadistico: Una regla general que se sigue es: los valores de la estadística de prueba DW en el rango de 1,5 a 2,5 son relativamente aceptables. Los valores fuera de este rango podrían ser motivo de preocupación. Los valores inferiores a 1 o superiores a 3 son un motivo definitivo de preocupación.
# El valor de p constituye una medida de la credibilidad de la hipótesis nula. Cuanto menor sea, más nos inclinaremos a rechazar la hipótesis nula,
# A p-value of 0.3608 for a Durbin-Watson test indicates that the null hypothesis of no autocorrelation is not rejected

### Controles - Casos influyentes y outliers ####

# https://stats.stackexchange.com/questions/22161/how-to-read-cooks-distance-plots

##### Cook ####
# Obtener las estadísticas de Cook
data_modelo_a_probar$cooks_distances <- cooks.distance(modelo_a_probar)
umbral <- 4 / (nrow(data_modelo_a_probar))

# Ver las primeras estadísticas de Cook
tabla_cook <- data_modelo_a_probar %>% 
       select(obsid, cooks_distances) %>%
       subset(cooks_distances>umbral) %>% 
       arrange(desc(cooks_distances))

tabla_cook %>% write.csv("anexos/ENG_tabla_cook.csv")

# Graficar las estadísticas de Cook
par(mfrow = c(1,1))

plot(data_modelo_a_probar$cooks_distances, main = "Estadísticas de Cook", ylab = "Cook's Distance", xlab = "Índice de observación")
abline(h = umbral #- 14 - 1) 
       , col = "red", lty = 2)  # Línea de corte común (influencia alta)
# Identificar observaciones influyentes (por encima del umbral)
influential_obs <- which(data_modelo_a_probar$cooks_distances > (4 / nrow(data_modelo_a_probar)))
text(x = influential_obs, y = data_modelo_a_probar$cooks_distances[influential_obs], 
     labels = data_modelo_a_probar$obsid[influential_obs], pos = 4, cex = 0.7, col = "blue")
#?text

# costa rica 1986 parece ser la mas problematica
# otras potenciales son brasil 2010 segunda ronda, ecuador 1988, guatemala 1999, colombia 2006, entre otras
# la posicuion de costa rica mejora en la variante respecto del de sficativas a secas, pero la de otros casos empeora, el balance quizas es relativamente neutro
# cambio despues de cambio en propindivinternet
# peru 2011 1 # costa rica 2002 2

# : Se suele trazar una línea de corte en 4 / n, donde n es el número de observaciones) para identificar observaciones que tienen una gran influencia en el modelo. Las observaciones por encima de esta línea pueden ser consideradas como influyentes.
#car::influencePlot(modelo_a_probar)
#4/nrow(data_modelo_a_probar)
#data_modelo_a_probar[c(20,61,77,127,162),"obsid"] # NO FUNCO CON MULTINIVEL

#car::influenceIndexPlot(modelo_a_probar, vars = c("Cook", "hat"))
#data_modelo_a_probar[c(20,61,77,127, 162),]
#data_modelo_a_probar[c(27,39,72,90,177,225),]

# observaciones nr 90, 116 para cook , 77 y 78 para hat en modelo sficativas 
# observaciones nr 90, 116 para cook , 7 y 177 para hat en modelo sficativas, aunque tambien vale mencionar que se reducen las escalas del problema 

# ##### dfbetas NO FUNCA CON MULTINIVEL! ####
# 
# #Para cada observación, podemos ver la diferencia en la estimación del coeficiente para la intersección, la variable  disp y la variable  hp que ocurre cuando eliminamos esa observación en particular.
# #Normalmente consideramos que una observación es muy influyente en la estimación de un coeficiente dado si tiene un valor DBETAS mayor que un umbral de 2/√ n, donde  n es el número de observaciones.
# umbral <- 2 / sqrt(nrow(data_modelo_a_probar))
# 
# #especificar 2 filas y 1 columna en la región de trazado
# #par(mfrow=c(2,1))
# 
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# 
# par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))
# 
# 
# for (i in seq(2, ncol(dfbetas))) { # Iterar sobre columnas de dfbetas
#   
#   plot(dfbetas[[i]],   
#        main = paste("Coeficiente:" , colnames(dfbetas)[i]), # Agregar el nombre de la variable como título
#        xlab = "Índice de observaciones", ylab = "DFBETAs")
#   
#   abline(h = umbral, lty = 2, col = "gray50")
#   abline(h = -umbral, lty = 2, col = "gray50")
#   
#   # Agregar etiquetas de texto a los puntos
#   text(seq_along(dfbetas[[i]]), dfbetas[[i]],
#        labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue3")
# }

### Control de multicolinealidad ####
##### correlaciones (al menos <.5, conservador <2.5) ####

# corr_modelo_a_probar <- democracias %>% 
#   select(dico_hubo_debates,
#          #cat_pais,
#          ncat_eleccion,
#          ncat_ronda,
#          #elecid,
#          #obsid,
#          marginvic,
#          lnmarginvic,
#          nec,
#          voteshareincumbent,
#          lnvoteshareincumbent,
#          dico_reeleccion,
#          alineamiento,
#          proptv,
#          propindivinternet,
#          lnpropindivinternet,
#          prohibicionpropaganda,
#          accesogratuito,
#          avgpropdebatesregionxciclo,
#          prop_elec_usa_ciclo,
#          regulaciondico,
#          cumsum_pastciclos,
#          lngdp,
#          gdpxcapita,
#          democraciavdemelectoralcomp,
#          mediaqualitycorruptvdem)

corr_modelo_a_probar <- data_modelo_a_probar %>%
  select(-cat_pais,
         -elecid,
         -obsid) %>% 
  select(ncat_eleccion,
         marginvic,
         lnmarginvic,
         nec,
         voteshareincumbent,
         lnvoteshareincumbent,
         dico_reeleccion,
         alineamiento,
         proptv,
         propindivinternet,
         lnpropindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         lngdp,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem)

summary(corr_modelo_a_probar)
correlation_matrix <- cor(corr_modelo_a_probar , use = "pairwise.complete.obs" )
 
# Generar el gráfico con resaltado


# reorden manual por bloque teorico (abajo hay codigo para un reorden automatico con CHATGPT)

# Define el nuevo orden de las variables basado en bloques teóricos
new_order <- c(
         "ncat_eleccion",
         "accesogratuito",
         "prohibicionpropaganda",
         "regulaciondico",
         "avgpropdebatesregionxciclo",
         "cumsum_pastciclos",
         "prop_elec_usa_ciclo",
         "proptv",
         "lnpropindivinternet",
         "propindivinternet",
         "lngdp",
         "gdpxcapita",
         "alineamiento",
         "democraciavdemelectoralcomp",
         "mediaqualitycorruptvdem",
         "marginvic",
         "lnmarginvic",
         "nec",
         "voteshareincumbent",
         "lnvoteshareincumbent",
         "dico_reeleccion") 
# Reemplaza con los nombres reales de tus variables

# Reorganiza la matriz de correlación manualmente
correlation_matrix <- correlation_matrix[new_order, new_order]


# Generar el gráfico con coordenadas capturadas
corr_plot <- corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("red", "white", "green"))(8), 
  diag = FALSE, 
  plot = NULL # Capturar coordenadas
)
# Crear el gráfico de correlación (parte inferior)
n <- ncol(correlation_matrix) # Número de columnas/filas

# Graficar la matriz de correlación
corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("red", "white", "green"))(8), 
  type = "lower",   # Muestra solo la parte inferior
  addgrid.col = "gray",
  diag = FALSE      # Ocultar diagonal
)

# Resaltar valores absolutos mayores a 0.25 con * y mayores a 0.5 con **
for (j in 1:n) {
  for (i in 1:n) {
    print(paste(i,j))
    if (i > j) { # Solo trabaja en el triángulo inferior (evita asteriscos en la parte oculta)
      if(is.na(correlation_matrix[i, j])){
        text(j, n - i + 1, "", col = "black", cex = 1.5)
      }
      else{
      # Resaltar valores entre 0.25 y 0.5
      if (abs(correlation_matrix[i, j]) > 0.25 && abs(correlation_matrix[i, j]) <= 0.5) {
        text(j, n - i + 1, "*", col = "black", cex = 1.5)
      }
      # Resaltar valores mayores a 0.5
      if (abs(correlation_matrix[i, j]) > 0.5) {
        text(j, n - i + 1, "**", col = "black", cex = 1.5)
      }}
    }
  }
}

# otra funcion de grafico
ggcorrplot::ggcorrplot(correlation_matrix, type = "lower", lab = T, show.legend = F)

# muestra mas chica
corr_base_democracias_reducida <- data_reducida %>% 
  select(dico_hubo_debates,
         #cat_pais,
         ncat_eleccion,
         ncat_ronda,
         #elecid,
         #obsid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
         alineamiento,
         proptv,
         propindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         #prop_elec_usa_ciclo, # NO TIENE VARIABILIDAD 
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem)
summary(corr_base_democracias_reducida)

correlation_matrix <- cor(corr_base_democracias_reducida , use = "pairwise.complete.obs" )

# Reordenar las variables usando agrupamiento jerárquico (CHATGPT)
reorder_corr_matrix <- function(correlation_matrix) {
  hc <- hclust(as.dist(1 - correlation_matrix)) # Agrupamiento jerárquico
  ordered_indices <- hc$order                # Orden de las variables
  reordered_matrix <- correlation_matrix[ordered_indices, ordered_indices] # Reordenar
  return(reordered_matrix)
}

# Aplicar el reordenamiento
correlation_matrix <- reorder_corr_matrix(correlation_matrix)


# Generar el gráfico con resaltado

# Generar el gráfico con coordenadas capturadas
corr_plot <- corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("blue", "white", "red"))(8), 
  diag = FALSE, 
  plot = NULL # Capturar coordenadas
)
# Crear el gráfico de correlación (parte inferior)
n <- ncol(correlation_matrix) # Número de columnas/filas

# Graficar la matriz de correlación
corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("blue", "white", "red"))(8), 
  type = "lower",   # Muestra solo la parte inferior
  addgrid.col = "gray",
  diag = FALSE      # Ocultar diagonal
)

# Resaltar valores absolutos mayores a 0.25 con * y mayores a 0.5 con **
for (j in 1:n) {
  for (i in 1:n) {
    if (i > j) { # Solo trabaja en el triángulo inferior (evita asteriscos en la parte oculta)
      # Resaltar valores entre 0.25 y 0.5
      if (abs(correlation_matrix[i, j]) > 0.25 && abs(correlation_matrix[i, j]) <= 0.5) {
        text(j, n - i + 1, "*", col = "black", cex = 1.5)
      }
      # Resaltar valores mayores a 0.5
      if (abs(correlation_matrix[i, j]) > 0.5) {
        text(j, n - i + 1, "**", col = "black", cex = 1.5)
      }
    }
  }
}

##### vif (raiz de vif < 2) ####
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).

# https://online.stat.psu.edu/stat462/node/180/ ! # 

vifs <- function(modelo){
  vif_values <- car::vif(modelo)
  vif_tibble <- enframe(vif_values, name = "Indicador", value = "Vif_value") 
  vif_tibble$Modelo <- deparse(substitute(modelo))
  #vif_tibble$Sqrt_vif <- sqrt(vif_tibble$Vif_value)
  vif_tibble
}

vifs_1 <- vifs(modelo_contingencia)
vifs_1_bis <- vifs(modelo_contingencia_bis)
vifs_2 <- vifs(modelo_sistemico)
vifs_2_bis <- vifs(modelo_sistemico_bis)
vifs_3 <- vifs(modelo_regulatorio)
vifs_3_bis <- vifs(modelo_regulatorio_bis)
vifs_4 <- vifs(modelo_difusion)
vifs_4_bis <- vifs(modelo_difusion_bis)
vifs_5 <- vifs(modelo_sficativas)
vifs_6 <- vifs(modelo_sficativas_variantes)

all_vifs <- rbind(vifs_1,
                  vifs_1_bis,
                  vifs_2 ,
                  vifs_2_bis,
                  vifs_3,
                  vifs_3_bis,
                  vifs_4 ,
                  vifs_4_bis,
                  vifs_5 ,
                  vifs_6 )

all_vifs2 <- all_vifs %>% 
  left_join(diccionario_indicadores) %>% 
  select(-Indicador) %>% 
  pivot_wider(names_from = Modelo, values_from = Vif_value)  
  

all_vifs2 %>% write_csv("anexos/ENG_vifs_values.csv")

### Controles - Missing data PENDIENTE ####
# jack knife
# imputacion: 
# reemp x valor promedio, 
# reemp x mediana, 
# mcmc multiple imputation approach -> simulacion + iteraciones
# comparar modelos 
### Controles - Fit, Overall significance y comparaciones ####
#### test de Wald - no se pudo det nesting en MULTINIVEl  ####

#modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

lmtest::waldtest(modelo_a_probar)

# podemos rechazar H0 de que todos los coeficientes = 0
# mejora sficancia en caso variantes 

# https://www.geeksforgeeks.org/how-to-perform-a-wald-test-in-r/
# Res.Df: Indica los grados de libertad residuales, que es la diferencia entre el número total de observaciones y el número de parámetros estimados en el modelo.
# Df: Representa el cambio en grados de libertad entre el Modelo 1 y el Modelo 2. En este caso, el Modelo 2 tiene 2 parámetros menos estimados en comparación con el Modelo 1 porque incluye menos predictores.
# F: Esta es la estadística de prueba para la prueba de Wald. Sigue una distribución F bajo la hipótesis nula de que los parámetros en el modelo reducido (Modelo 2) son iguales a cero. En otras palabras, prueba si los predictores adicionales en el Modelo 1 contribuyen significativamente al modelo.
# Pr(>F): es el valor p asociado con el estadístico de la prueba F. Representa la probabilidad de observar un estadístico F tan extremo como el calculado bajo la hipótesis nula. En este caso, el valor p es 0,006863, que es menor que 0,05, lo que sugiere una evidencia sólida contra la hipótesis nula.

#lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_0_reducido)
#lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_contingencia_reducido)
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_sistemico_reducido) # pendiente
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_regulatorio_reducido) # pendiente
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_difusion_reducido) # pendiente
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_sficativas_reducido) # Pendiente

# en gral el modelo mejora significativamente en todos los casos

# COMPARACION MODELO MULTI VS SIMPLE

#lmtest::waldtest(modelo1_reespecificado, final_random_intercepts)

#### lr test (comparar modelos con = cantidad de data, sacando missing) ####
# Chi-squared test of all coefficients An LR test of the hypothesis that all coefficients except the intercept(s) are zero can be computed by comparing the log likelihoods: LR= 2 ln L(MFull) − 2 ln L(MIntercept ). This statistic is sometimes designated as G2 . 
#  First, the two models must be nested. Second, the two models must be estimated on exactly the same sample. 
#lrtest(null_model, full_model)

# Obtener Log-Likelihood, mas alto mejor. solo se pueden comparar modelos anidados
logLik(modelo_a_probar)

# para hacer directamente el test
# lrtest(modelo_0_reducido_sficativas, modelo_sficativas ) # -73.106 (pero mas parametros)
# lrtest(modelo_0_reducido_sficativas, modelo_sficativas_variantes ) # -73.634
# lrtest(modelo_contingencia_reducido, modelo_sficativas_variantes_reducido)
# # lrtest(modelo_sistemico_reducido_sficativas, modelo_sficativas) # pendiente
# lrtest(modelo_regulatorio_reducido_sficativas, modelo_sficativas) # pendiente
#lrtest(modelo_difusion_reducido_sficativas, modelo_sficativas)

lrtest(modelo_a_probar)
lrtest(modelo_sficativas_variantes_reducido , modelo_0_reducido)
lrtest(modelo_sficativas_variantes_reducido , modelo_contingencia_reducido)
# lrtest(modelo_sficativas_variantes_reducido , modelo_sistemico_reducido) # pendiente
# lrtest(modelo_sficativas_variantes_reducido , modelo_regulatorio_reducido) # pendiente
#lrtest(modelo_sficativas_variantes_reducido , modelo_difusion_reducido) # ojo no estan anidados
#lrtest(modelo_sficativas_variantes_reducido , modelo_sficativas_reducido) # ojo no estan anidados

# Model 1 : represents the null model, which includes only the intercept.
# Model 2: represents the full model, which includes predictors
# #Df : This indicates the degree of freedom or the number of parameters involved
# LogLik: This indicates the log-likelihood values for each model.
# Chisq: is the likelihood ratio test statistic, which measures the difference in log-likelihood values between the two models.
# Pr(>Chisq): represents the p-value associated with the likelihood ratio test.

#likelihood_ratio_test <- lrtest(null_model, full_model)
# Interpretation of likelihood ratio test results
# if (likelihood_ratio_test$"Pr(>Chisq)"[2] < 0.05) 
# {
#   cat("Reject the null hypothesis. The full model is significantly better than 
#         the null model.\n")
# } else {
#   cat("Fail to reject the null hypothesis. The null model is sufficient.\n")
# }

# en todos los casos, el modelo full es mejor que el modelo más incompleto. 


#### fit: Pseudos R cuadrados, AIC y BIC ####
 
#modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

# PRUEBAS PRELIMINARES 
# pseudo R cuadrados. #Mas alto, mejores
# https://www.rdocumentation.org/packages/rcompanion/versions/2.4.36/topics/nagelkerke
rcompanion::nagelkerke(modelo_a_probar, restrictNobs = T)
# https://search.r-project.org/CRAN/refmans/fmsb/html/Nagelkerke.html 
fmsb::NagelkerkeR2(modelo_a_probar)
pscl::pR2(modelo_a_probar)

# Obtener AIC y BIC # mas bajos, mejores, aunque se penaliza la inclusion de mas parametros
AIC(modelo_a_probar)
BIC(modelo_a_probar)
AIC(modelo_sficativas_variantes_reducido)
BIC(modelo_sficativas_variantes_reducido)

# DEFINO FUNCION COMPARATIVA

# para comparar modelos con igual n, creo funcion 
 
stats <- function(modelo){
  pseudoR2 <- rcompanion::nagelkerke(modelo, restrictNobs = T)$Pseudo.R.squared.for.model.vs.null %>% 
    as.data.frame() %>% 
    select(Pseudo.R.squared) %>% 
    dplyr::rename("value" = Pseudo.R.squared)
  pseudoR2$stat <- c("McFadden", "CoxSnell", "Nagelkerke")
  rownames(pseudoR2) <- NULL
  
  aic <- data.frame(value = AIC(modelo),
                       stat = "AIC")
  
  bic <- data.frame(value = BIC(modelo),
                       stat = "BIC")
  
  data <- rbind(pseudoR2,
                aic,
                bic)
  
  data$modelo <- deparse(substitute(modelo))  
  
  data
}
stats0 <- stats( modelo_0_reducido)
stats1 <- stats( modelo_contingencia_reducido)
stats2 <- stats( modelo_sistemico_reducido)
stats3 <- stats( modelo_regulatorio_reducido)
stats4 <- stats( modelo_difusion_reducido)
stats5 <- stats( modelo_sficativas_reducido)
stats6 <- stats( modelo_sficativas_variantes_reducido)

all_stats <- rbind(stats0,
                   stats1,
                   stats2,
                   stats3,
                   stats4,
                   stats5,
                   stats6 )

all_stats <- all_stats %>% 
  pivot_wider(names_from = modelo, values_from = value)

#all_stats %>% write_csv("anexos/ENG_stats_values.csv")

# comentario #

# los modelos full performan mejor que los modelos reducidos. Incluso con data reducida! 
# para el caso del AIC, notable que incluso con penalizacion por mas parametros, el valor del estadistico disminuye
# en todos los casos de pseudos R2, el modelo completo es mejor que los más incompletos, y la variante completa mejor que el completo

# comparaciones sobre muestra completa #
stats27 <- stats(modelo_sficativas )
stats28 <- stats(modelo_sficativas_variantes )

all_stats_full <- rbind(stats27,   stats28 )

# stats modelo #
stats_modelo <- stats(modelo_a_probar)

#### fit: desempeño: accuracy ####

#modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

# PRUEBAS PRELIMINARES
data_modelo_a_probar$probabilidades_predichas <- predict(modelo_a_probar, type = "response")
data_modelo_a_probar$predicciones_binarias <- ifelse(data_modelo_a_probar$probabilidades_predichas>0.5,1,0)

# Matriz de confusión
confusion_matrix <- table(data_modelo_a_probar$predicciones_binarias, data_modelo_a_probar$dico_hubo_debates)
print(confusion_matrix)

# Count R²: proporción de predicciones correctas
count_r2 <- mean(data_modelo_a_probar$predicciones_binarias == data_modelo_a_probar$dico_hubo_debates)
# Precisión base: proporción de la clase mayoritaria
baseline_accuracy <- max(table(data_modelo_a_probar$dico_hubo_debates)) / length(data_modelo_a_probar$dico_hubo_debates)
# Adjusted Count R²
adjusted_count_r2 <- (count_r2 - baseline_accuracy) / (1 - baseline_accuracy)
#  The adjusted count R2 is the proportion of correct guesses beyond the number that would be correctly guessed by choosing the largest marginal

# Imprimir resultados
cat("Count R²:", count_r2, "\n")
cat("Adjusted Count R²:", adjusted_count_r2, "\n")

# PARA COMPARAR, DEFINO FUNCION
#modelo <- modelo_0
# para comparar
funcion_count_R2 <- function(modelo, variable_dependiente){
  
  probabilidades_predichas <- predict(modelo, type = "response")
  predicciones_binarias <- ifelse(probabilidades_predichas>0.5,1,0)
 
   # Count R²: proporción de predicciones correctas
  count_r2 <- mean(predicciones_binarias == variable_dependiente)
  # Precisión base: proporción de la clase mayoritaria
  baseline_accuracy <- max(table(variable_dependiente)) / length(variable_dependiente)
  # Adjusted Count R²
  adjusted_count_r2 <- (count_r2 - baseline_accuracy) / (1 - baseline_accuracy)
  #  The adjusted count R2 is the proportion of correct guesses beyond the number that would be correctly guessed by choosing the largest marginal
  
  name <- deparse(substitute(modelo))
  
  dataframe <- tibble(countr2 = count_r2,
                          adjcountr2 = adjusted_count_r2,
                          modelo =   name )
}

countr20 <- funcion_count_R2(modelo_0_reducido, data_reducida$dico_hubo_debates)
countr21 <- funcion_count_R2(modelo_contingencia_reducido, data_reducida$dico_hubo_debates)
countr22 <- funcion_count_R2(modelo_sistemico_reducido, data_reducida$dico_hubo_debates)
countr23 <- funcion_count_R2(modelo_regulatorio_reducido, data_reducida$dico_hubo_debates)
countr24 <- funcion_count_R2(modelo_difusion_reducido, data_reducida$dico_hubo_debates)
countr25 <- funcion_count_R2(modelo_sficativas_reducido, data_reducida$dico_hubo_debates)
countr26 <- funcion_count_R2(modelo_sficativas_variantes_reducido, data_reducida$dico_hubo_debates)

all_count_r2_reducidas <- rbind(countr20 ,
                              countr21 ,
                              countr22 ,
                              countr23 ,
                              countr24 ,
                              countr25 ,
                              countr26 )

all_count_r2_reducidas <- all_count_r2_reducidas %>% 
  pivot_longer(cols= c(countr2, adjcountr2), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = modelo, values_from = value)

#all_count_r2_reducidas %>% write_csv("anexos/ENG_countR2_values.csv")

all_stats <- all_stats %>% 
  rbind(all_count_r2_reducidas)


all_stats %>% write_csv("anexos/ENG_stats_values.csv")

# comentario
# en este caso el modelo variante performa peor
# los modelos completos mejoran la performance de los más incompletos

# quizas vale la pena comparar versiones con mas datos
# o hacer trabajo andidado de verdad, digamos

# comparaciones sobre muestra completa #
countr27 <- funcion_count_R2(modelo_sficativas, data_modelo_sficativas$dico_hubo_debates)
countr28 <- funcion_count_R2(modelo_sficativas_variantes, data_modelo_sficativas$dico_hubo_debates)

#countr29 <- funcion_count_R2(robust_se_cluster_modelo_sficativas_variantes, democracias$dico_hubo_debates)


all_count_r2_full <- rbind(countr27,   countr28 )

countr2_modelo <- funcion_count_R2(modelo_a_probar, data_modelo_a_probar$dico_hubo_debates)

stats_modelo <- stats_modelo %>% 
  rbind( countr2_modelo %>% 
          pivot_longer(cols= c(countr2, adjcountr2), names_to = "stat", values_to = "value")  )

stats_modelo %>% write_csv("anexos/ENG_stats_modelo.csv")

# idem: el desempeño de la variante es ligeramente peor

### Controles COMPARACION SIMPLE MULTI ######

## MODELO NULO ##
 
empty_model_paises_reducido <- lme4::glmer(dico_hubo_debates ~ 
                                    1 + (1 | cat_pais), 
                                  family=binomial("logit"), 
                                  data = data_modelo_a_probar)
#summary(empty_model_paises)

# LR TEST ##
lrtest(modelo_sficativas_variantes , final_random_intercepts)

# modelo1_reespecificado <- lme4::glmer(
#   dico_hubo_debates ~ lnmarginvic + lnnec + voteshareincumbent + 
#     dico_reeleccion + propindivinternet + accesogratuito + 
#     avgpropdebatesregionxciclo + regulaciondico + cumsum_pastciclos + 
#     lngdp + democraciavdemelectoralcomp + mediaqualitycorruptvdem + 
#     (1 | cat_pais),
#   data = democracias,
#   family = binomial,
#   control = control,
#   start = list(theta = c(0))) 

#lrtest(final_random_intercepts, modelo1_reespecificado)
#anova(modelo1_reespecificado, final_random_intercepts, test="Chisq")
#anova(m0, m1, test = "LRT")  # IGUAL RESULTADO QUE CON LRTEST

# https://www.unige.ch/cisa/files/2317/1829/8308/CISA_BM_statsupport_20240617_Randomintercept.pdf
#lmerTest::ranova(final_random_intercepts)
#pchibarsq(6.454045,1,mix=0.5,lower.tail=FALSE)> 0.005535001
#car::Anova(final_random_intercepts, type=3)
#drop1(final_random_intercepts,test="Chisq")
 

# BOOTSTRAP COMENTADO PORQUE TARDA MUCHO, RESULTADOS PEGADOS ABAJO 
# m1 <- glm(dico_hubo_debates ~ 
#             lnmarginvic + 
#             lnnec +
#             voteshareincumbent +
#             dico_reeleccion + 
#             propindivinternet +
#             accesogratuito +
#             avgpropdebatesregionxciclo + 
#             regulaciondico +
#             cumsum_pastciclos +
#             lngdp + 
#             democraciavdemelectoralcomp +
#             mediaqualitycorruptvdem,
#           family = binomial(link = "logit"), 
#           data = democracias)
# m0 <- lme4::glmer(dico_hubo_debates ~ 
#                     lnmarginvic + 
#                     lnnec +
#                     voteshareincumbent +
#                     dico_reeleccion + 
#                     propindivinternet +
#                     accesogratuito +
#                     avgpropdebatesregionxciclo + 
#                     regulaciondico +
#                     cumsum_pastciclos +
#                     lngdp + 
#                     democraciavdemelectoralcomp +
#                     mediaqualitycorruptvdem +
#                     (1 | cat_pais),
#                   family=binomial("logit"), 
#                   data = democracias,
#                   control =  lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# 
# summary(m0)
# summary(m1)
# pbgmmDg1 <- pbnm::pbnm(m0,
#                        m1,
#                        nsim=5000,tasks=10,cores=2,seed=4642782) 
# summary(pbgmmDg1)
# > summary(pbgmmDg1)

# RESULTADOS BOOTSTRAP Parametric bootstrap testing: (Intercept) | cat_pais = 0 
# from: lme4::glmer(formula = dico_hubo_debates ~ lnmarginvic + lnnec +  voteshareincumbent + dico_reeleccion + propindivinternet +  accesogratuito + avgpropdebatesregionxciclo + regulaciondico +  cumsum_pastciclos + lngdp + democraciavdemelectoralcomp +  mediaqualitycorruptvdem + (1 | cat_pais), data = democracias,  family = binomial("logit"), control = lme4::glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 100000))) 
# 5000 samples were taken Tue Jan 21 18:25:54 2025 
# 296 samples had warnings, 296 in alternate model 0 in null model 
# 296 unused samples.  0.0046 <= P(abs((Intercept) | cat_pais) > |0.4531746|) <= 0.06

# COMPARACION MULTINIVEL Y MODELO FINAL SIMPLE

rcompanion::nagelkerke(final_random_intercepts, 
                       #restrictNobs = T,  # no funca con este tipo de modelos
                       null = empty_model_paises_reducido)
 

# https://www.unige.ch/cisa/files/2317/1829/8308/CISA_BM_statsupport_20240617_Randomintercept.pdf

stats_simple <- stats(modelo_sficativas_variantes)

# no puedo aplicar funcion por diferente forma de estimacion
pseudoR2 <- rcompanion::nagelkerke(final_random_intercepts, 
                                      null = empty_model_paises_reducido)$Pseudo.R.squared.for.model.vs.null %>% 
  as.data.frame() %>% 
  select(Pseudo.R.squared) %>% 
  dplyr::rename("value" = Pseudo.R.squared)
pseudoR2$stat <- c("McFadden", "CoxSnell", "Nagelkerke")
rownames(pseudoR2) <- NULL

aic <- data.frame(value = AIC(final_random_intercepts),
                  stat = "AIC")

bic <- data.frame(value = BIC(final_random_intercepts),
                  stat = "BIC")

stats_multi <- rbind(pseudoR2,
              aic,
              bic)

stats_multi$modelo <- deparse(substitute(final_random_intercepts)) 

all_stats_multisimple <- rbind(stats_simple, stats_multi )

# COMPARACION MULTINIVEL Y SIMPLE

countr2_simple <- funcion_count_R2(modelo_sficativas_variantes, data_modelo_a_probar$dico_hubo_debates)
countr2_multi <- funcion_count_R2(final_random_intercepts, data_modelo_a_probar$dico_hubo_debates)
all_countr2_multisimple <- rbind(countr2_simple, countr2_multi ) %>%
  pivot_longer(cols= c(countr2, adjcountr2), names_to = "stat", values_to = "value")

all_stats_multisimple <- all_stats_multisimple %>%
  rbind( all_countr2_multisimple   )  %>% 
  pivot_wider(names_from = modelo, values_from = value) 

all_stats_multisimple %>% write_csv("anexos/ENG_all_stats_multisimple.csv")

## OTROS MODELOS control - NO NECESARIO P PAPER, BORRO ####


## EXPORTO MODELOS SIMPLES #####

# Una vez que estimamos los modelos que irán en la tabla, los agrupamos en una lista usando la función list. Esto ahorra tiempo, porque en lugar de tener que escribir el nombre de los modelos, simplemente nos referiremos a la lista mp_models:

lista1 <-  list(
  robust_se_cluster_modelo_contingencia,
  robust_se_cluster_modelo_sistemico,
  robust_se_cluster_modelo_regulatorio,
  robust_se_cluster_modelo_difusion,
  robust_se_cluster_modelo_sficativas_variantes
) 


texreg::htmlreg(lista1,
                custom.model.names = c("Contingencia",
                                       "Sistémico",
                                       "Regulatorio",
                                       "Difusión",
                                       "Final")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      
                                      "Regulación sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Cápita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupción de medios (VDEM)",
                                      
                                      "Alineamiento partidario",
                                      "Prop. TV por hogar"		,
                                      "Prop. individuos c internet",
                                      "Prohibición propaganda"	 ,
                                      "Acceso gratuito",
                                      "Prop. debates en región",
                                      "Prop. debates en USA" 
                                      
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  
                                  11,
                                  12,
                                  13,
                                  14,
                                  15,
                                  16,
                                  17,
                                  
                                  6,
                                  7,
                                  9,
                                  10,
                                  8
                ),
                file="anexos/ENG_tabla_modelos_logit_robustos.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1,
                #. For example, list("Random effects" = c("YES", "YES", "NO"), Observations = c(25, 25, 26))
                custom.gof.rows = list("AIC" = c(AIC(modelo_contingencia_bis),
                                                 AIC(modelo_sistemico_bis),
                                                 AIC(modelo_regulatorio_bis),
                                                 AIC(modelo_difusion_bis),
                                                 AIC(modelo_sficativas_variantes)),
                                       "BIC" = c(BIC(modelo_contingencia_bis),
                                                 BIC(modelo_sistemico_bis),
                                                 BIC(modelo_regulatorio_bis),
                                                 BIC(modelo_difusion_bis),
                                                 BIC(modelo_sficativas_variantes)),
                                       "Log Likelihood" = c(as.numeric(logLik(modelo_contingencia_bis)),
                                                            as.numeric(logLik(modelo_sistemico_bis)),
                                                            as.numeric(logLik(modelo_regulatorio_bis)),
                                                            as.numeric(logLik(modelo_difusion_bis)),
                                                            as.numeric(logLik(modelo_sficativas_variantes))),
                                       "Num. obs" = c(nobs(modelo_contingencia_bis),
                                                      nobs(modelo_sistemico_bis),
                                                      nobs(modelo_regulatorio_bis),
                                                      nobs(modelo_difusion_bis),
                                                      nobs(modelo_sficativas_variantes)) ) )   
 
 

## EXPORTO MODELOS RANDOM INTERCEPT #####
lista1bis <-  list(contingencia_random_intercepts,
                   sistemico_random_intercepts,
                   regulatorio_random_intercepts,
                   difusion_random_intercepts,
                   final_random_intercepts)

# https://www.rdocumentation.org/packages/texreg/versions/1.39.4/topics/htmlreg

texreg::htmlreg(lista1bis,
                custom.model.names = c("Contingencia",
                                       "Sistémico",
                                       "Regulatorio",
                                       "Difusión",
                                       "Final")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      
                                      "Regulación sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Cápita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupción de medios (VDEM)",
                                      
                                      "Alineamiento partidario",
                                      "Prop. TV por hogar"		,
                                      "Prop. individuos c internet",
                                      "Prohibición propaganda"	 ,
                                      "Acceso gratuito",
                                      "Prop. debates en región",
                                      "Prop. debates en USA"
                                      
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  
                                  11,
                                  12,
                                  13,
                                  14,
                                  15,
                                  16,
                                  17,
                                  
                                  6,
                                  7,
                                  9,
                                  10,
                                  8
                ),
                file="anexos/ENG_tabla_random_intercepts.html",
                caption = "Modelos estimados con efectos aleatorios por país",
                center = T,
                bold = 0.1)


## EXPORTO MODELOS CONTROL - NOT NECESSARY , BORRADO #####
 
## HASTA ACA LLEGUE EN LA INCORPORACION DE COMENTARIOS CELES ######
## INTERPRETACION MODELOS 1 VARIOS PENDIENTES ####

#### INTERPRETACION - importante elegir ####

# modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
# data_modelo_a_interpretar <- data_s_outliers 

# REESTIMO MODELO NO ESTANDARIZADO
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

modelo_a_interpretar <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                          "(1 | cat_pais)", sep = "+"), 
                                    family=binomial("logit"), 
                                    data = democracias_reservanoescalada,
                                    control = control)

# BUSCO DATA NO ESTANDARIZADA
data_modelo_a_interpretar <- democracias_reservanoescalada %>% 
  select(c("dico_hubo_debates",   
           "lnmarginvic",
           "nec",
           "lnnec",
           "voteshareincumbent",
           "dico_reeleccion" ,
           "propindivinternet" ,
           "accesogratuito" ,
           "avgpropdebatesregionxciclo" ,
           "regulaciondico" ,
           "cumsum_pastciclos" ,
           "lngdp",
           "democraciavdemelectoralcomp" ,
           "mediaqualitycorruptvdem",
           "cat_pais")) %>% 
  na.omit()
# vcov_modelo_a_interpretar <- vcovCL(final_random_intercepts, 
#                #cluster = democracias$elecid))
#                #cluster = data$elecid))
#                cluster = democracias$cat_pais)
  
  
# t the coefficient for X is the difference in the log odds.  https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404.

#### INTERPRETACION - ODDS RATIO #####

# a diferencia de la proba predicha, que varia de manera no lineal,
# el odds ratio es constante para distintos valores de x 
# el coef es log(p/1-p) . ---> entonces: p/1-p = exp (covariates).   al reves: 1-p/p = 1/exp(covariates). despejando: 1/p = 1+ 1/exp(covariates). incorporando el 1 . 1/p = exp(c) + 1/ exp(p). despejando . p = exp (cov)/ 1 + exp (cov ). 
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression revisar

odds_ratio <- lme4::fixef(modelo_a_interpretar)["lnnec"] %>%  exp()

# Each exponentiated coefficient is the ratio of two odds, or the change in odds in the multiplicative scale for a unit increase in the corresponding predictor variable holding other variables at certain value.
# por cada aumento en una unidad de lnnec, esperamos un aumento de un cambio_odds_promedio en las chances de que haya un debate 
# un odds ratio mayor que 1 expresa un cambio positivo, mientras que si es menor que 1 (entre 0 y 1) representa un cambio negativo en las probabilidades estimadas.
# También podemos graficar los coeficientes como odds ratios: recuerda que los odds ratios menores que 1 son efectos negativos y mayores que 1 son positivos. El coeficiente se expresa como su valor medio y su intervalo de confianza del 95%. Si el coeficiente es estadísticamente significativo, su intervalo de confianza no pasará de la línea en 1. Si, por el contrario, no son significativos, el efecto cruzará la línea.

# Para utilizar los odds ratios queremos mostrarte cómo puedes cambiar los coeficientes que obtienes directamente de la tabla, que son log odds, y reemplazarlos por odds ratios. Para ello, puedes usar los argumentos override.coef,override.se y override.pvalues de screenreg()
summary(modelo_a_interpretar)

# OJO! que los errores estandar no estan bien calculados en la tabla a continuacion
texreg::screenreg(modelo_a_interpretar,
          custom.model.names = "data_modelo_a_interpretar - Odds Ratios",
          override.coef    = exp(lme4::fixef(modelo_a_interpretar)),
          stars = c(0.001, 0.01, 0.05, 0.1),
          # la siguiente función, odds_*, están en el paquete del libro. OJO - NO PUDE DESCARGAR
          # override.se      = odds_se(modelo_a_interpretar), # no tengo esta funcion ! 
          # override.pvalues = odds_pvalues(modelo_a_interpretar),
          # además, omitiremos el coeficiente del intercepto
          omit.coef = "Inter")

 

#### INTERPRETACION - ESCENARIOS CON VALORES PREDICHOS #####
### calculo de probas predichas # tengo que reducir la data para poder calcular asi nomas
#data_modelo_a_interpretar$probabilidades_predichas <- predict(modelo_a_interpretar, type = "response")
#data_modelo_a_interpretar$predicciones_binarias <- ifelse(data_modelo_a_interpretar$probabilidades_predichas>0.5,1,0)

# Crear un dataset base sobre el cual predecir 

levels <- data_modelo_a_interpretar$cat_pais %>% unique()
nlevels <- length(levels)
# version valores relevantes
valores_relevantes <- c(log(min(data_modelo_a_interpretar$nec)),
                        log(max(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec)),
                       log(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec)),
                       log(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec)),
                       log(1),
                       log(2),
                       log(3))

data_to_predict1 <- data.frame(
  lnnec = rep(valores_relevantes, 2*nlevels), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = 0, # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = mean(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=length(valores_relevantes)*nlevels) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
  cat_pais = levels
)

# Predecir probabilidades # ALTERNATIVAS PARA IGNORAR RANDOM EFFECTS
data_to_predict1$predicted_probs <- predict(modelo_a_interpretar, 
                                           newdata = data_to_predict1, 
                                           type = "response")

data_to_predict1 <- data_to_predict1 %>% 
  group_by(lnnec, regulaciondico) %>% 
  mutate(avg_predicted_probs = mean(predicted_probs)) %>% 
  ungroup()

preds <- marginaleffects::predictions(
  modelo_a_interpretar,
  newdata = data_to_predict1,
 re.form = NA  # Esto ignora los efectos aleatorios
)

data_to_predict1$predicted_probs2 <- preds$estimate %>% round(2)
data_to_predict1$conf.low <- preds$conf.low
data_to_predict1$conf.high <- preds$conf.high

data_to_predict1 <- data_to_predict1 %>% 
  mutate(confint = paste("(", round(conf.low,2),"-", round(conf.high,2), ")", sep = " "))

# yay! son lo msimo!

# tabla

referencias <- tibble( 
  nec = c( round(min(data_modelo_a_interpretar$nec), 2),
           round(max(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec), 2),
          #  round(mean(data_modelo_a_interpretar$nec) + 2*sd(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec), 2),
          1,
          2,
          3), 
  referencia = c("Min. observed value",
                 "Max. observed value",
                 "Observed mean",
                 "Observed mean + one std. dev.",
                 "Observed mean - one std. dev.",
                 "Dominant party",
                 "Bipartidism",
                 "Multipartidism")) 

 
tabla_data_to_predict <- data_to_predict1 %>% 
  mutate(nec = round(exp(lnnec),2)) %>% 
  select(nec, regulaciondico, predicted_probs2, confint) %>% 
  unique() %>% 
  pivot_wider(names_from = regulaciondico,
              values_from = c(predicted_probs2, confint)) %>% 
#  mutate(across(everything(),  ~  round(., 2))) %>%  # ya redondeado
  #mutate(nec = round(nec, 2)) %>% 
  left_join(referencias) %>% 
  arrange(nec) %>% 
  dplyr::rename("Predicted prob. with no regulations" = "predicted_probs2_0",
                "Predicted prob. with regulations" = "predicted_probs2_1",
                "Conf. Int. with no regulations" = "confint_0",
                "Conf. Int. with regulations" = "confint_1") %>% 
  select(referencia, nec, 
         "Predicted prob. with no regulations", "Conf. Int. with no regulations", 
         "Predicted prob. with regulations", "Conf. Int. with regulations")

tabla_data_to_predict %>% write.csv("anexos/ENG_tabla_data_to_predict.csv")

# yay! son casi identicas a lo estimado con modelo logit convencional !

# grafico
# comento porque preferimos el próximo gráfico
# ggplot(data_to_predict) +
#   geom_line(aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico)))

# version con mas valores para graficar 
data_to_predict2 <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                    max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                    #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                    length.out = 20), 2*nlevels), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=20*nlevels) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
  cat_pais = levels
  )

                
# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 

# predicted_probs <- margins::prediction(model = modelo_a_interpretar,
#                               data = data_to_predict2,
#                               type = "response",
#                               vcov = vcov_modelo_a_interpretar,
#                               calculate_se = TRUE)

predicted_probs <- marginaleffects::predictions(
  modelo_a_interpretar,
  newdata = data_to_predict2,
  conf_level = 0.9,
  re.form = NA  # Esto ignora los efectos aleatorios
)

data_to_predict2$predicted_probs <- predicted_probs$estimate

# grafico

plot_interpretacion <- ggplot(predicted_probs) +
  geom_line(aes(x = exp(lnnec), 
                y = estimate, 
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  conf.low, 
                  ymax =  conf.high, 
                  fill = as.factor(regulaciondico)), alpha = 0.3) +
  theme_classic() +
  labs(
    title = "Probabilidad predicha de ocurrencia de un debate presidencial",
    subtitle = "Para distintos valores dentro del rango observado de NEC, con y sin regulación",
    caption = "Elaboración propia. 
    Intervalos de confianza ploteados al 90%.
    Escenarios predichos cuando el resto de las variables se encuentra:
    -en su media (para el caso de los indicadores continuos),
    -en su moda (para el caso de los indicadores dicotómicos).",
    fill = "Regulación sobre debates",
    colour = "Regulación sobre debates"
  ) +
  xlab("Número Efectivo de Candidatos") +
  ylab("Probabilidad predicha de que ocurra un debate") +
  scale_x_continuous(breaks= seq(1, 10, 0.5)) +
  scale_fill_manual(labels = c("No hay", "Hay"), breaks = c(0,1), values = c("grey40", "lawngreen")) +
  scale_colour_manual(labels = c("No hay", "Hay"), breaks = c(0,1), values = c("grey30", "limegreen")) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
  geom_vline(xintercept = c(2, 3), alpha = 0.5, linetype = 2) +
  theme(legend.position = "bottom")
 
plot_interpretacion %>% ggsave(filename = "images/ENG_plot_interpretacion_nec_regulacion.jpg",
                               width = 12,
                               height = 9)




# #### COMENTADO INTERPRETACION - EFECTO MARGINAL PROMEDIO #####
# 
# # chatgpt: 
# # En el contexto de un modelo logit, el efecto marginal se refiere a 
# # cómo cambia la probabilidad predicha de que ocurra el evento de interés (la variable dependiente sea igual a 1) 
# # cuando se modifica una variable independiente, 
# # manteniendo constantes las demás variables del modelo. 
# # En otras palabras, es la tasa de cambio en la probabilidad asociada con un cambio unitario en una variable independiente.
# #  el efecto marginal varía según los valores de 𝑋 X y, en consecuencia, no es constante.
# 
# # formula es: 
# # cambio en P dado cambio en X = beta * p(1-p), donde sabemos que proba predicha varia para distintos valores de Xs
# 
# # interpretacion: 
# # Si el efecto marginal de X en cierto valore es 0.1, 
# # significa que un aumento unitario en ese valor incrementa la probabilidad predicha en un 10%, 
# # manteniendo constantes las demás variables.
# 
# ###### Efecto marginal promedio (Average Marginal Effect, AME): # 
# 
# # Se calcula el efecto marginal para cada observación en el conjunto de datos y luego se promedian estos efectos.
# # Es útil para interpretar un "efecto típico" en la muestra.
# 
# #¿Cuál es el efecto medio del aumento de una unidad de la variable independiente en la probabilidad de que se produzca la variable dependiente? El Efecto Marginal Promedio (AME, por sus siglas en inglés) se utiliza para ver estos efectos, y se logra con el comando plot del paquete de margins.
# #  el efecto marginal es la variación de la variable explicada cuando la variable explicativa aumenta en una unidad.
# 
# # efecto marginal promedio
# #modelo_a_interpretar <- modelo_sficativas_variantes
# 
# marginal_effects <- margins::margins(modelo_a_interpretar,
#                                      vcov = vcov_modelo_a_interpretar)#,
# #level = 0.90)
# 
# marginals_df <- summary(marginal_effects)
# 
# # marginals_df <- margins::margins_summary(modelo_a_interpretar, 
# #                                          vcov = vcov_modelo_a_interpretar,
# #                                          level = 0.90, by_factor = TRUE)
# 
# marginals_df$AME["lnnec"] * 100
# marginals_df$AME["regulaciondico"] * 100
# marginals_df$AME["dico_reeleccion"] * 100
# 
# (marginals_df$AME["lnnec"] - 1.64485*marginals_df$SE["Var_dydx_lnnec"]) * 100
# (marginals_df$AME["lnnec"] + 1.64485*marginals_df$SE["Var_dydx_lnnec"]) * 100
# 
# (marginals_df$AME["regulaciondico"] - 1.64485*marginals_df$SE["Var_dydx_regulaciondico"]) * 100
# (marginals_df$AME["regulaciondico"] + 1.64485*marginals_df$SE["Var_dydx_regulaciondico"]) * 100
# 
# (marginals_df$AME["dico_reeleccion"] - 1.64485*marginals_df$SE["Var_dydx_dico_reeleccion"]) * 100
# (marginals_df$AME["dico_reeleccion"] + 1.64485*marginals_df$SE["Var_dydx_dico_reeleccion"]) * 100
# 
# marginals_df <- marginals_df %>% 
#   left_join(diccionario_indicadores %>% 
#               dplyr::rename("factor" = "Indicador"))
# 
# plot_margins <- ggplot(marginals_df) +
#   geom_point( aes(x = Nombre, y = AME)) +
#   geom_errorbar(aes(x = Nombre, ymin = AME-1.64485*SE, ymax = AME+1.64485*SE), width = 0.2) +
#   geom_hline(aes(yintercept = 0), colour = "gray50", linetype = 2) +
#   # geom_hline(aes(yintercept = 0.25), colour = "red2", linetype = 2, alpha = 0.1) +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(-1,1,0.25)) +
#   labs(title = "Efectos marginales promedio",
#        subtitle = "Modelo final",
#        x = "Predictores", 
#        y = "Efectos marginales promedio",
#        caption = "Elaboración propia sobre la base de modelo final, estimado con regresión logística con errores estándar agrupados por país.
#        Intervalos de confianza estimados al 90%") +
#   theme_classic() +
#   theme(axis.text.y = element_text(size = 12))
# 
# plot_margins %>% ggsave(filename = "images/ENG_plot_margins.jpg", width = 12, height = 8)
# 
# #margins::marginal_effects(margins::margins(modelo_a_interpretar))
# 
# # seccion de efecto marginal por variable en archivo viejo modelos_dic2024.R
# # en la practica entiendo que equivale a escenarios por variable
# 
# ## COMENTADO EFECTO MARGINALO DE UNA VARIABLE #####
# 
# # paso chatgpt 
# efectos_marginales <- marginaleffects::comparisons(
#   modelo_a_interpretar,
#   variables = "regulaciondico",  # el contraste que te interesa
#   newdata =  marginaleffects::datagrid(
#     lnnec = seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
#                 max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
#                 length.out = 20),
#     lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
#     voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
#     dico_reeleccion = 0,
#     propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
#     accesogratuito = 1,
#     avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
#     cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
#     lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
#     democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
#     mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
#     cat_pais = NA  # O podés fijar un país si querés mostrar el efecto dentro de un país específico
#   ),
#   re.form = NA  # para excluir los efectos aleatorios
# )
# 
# ggplot(efectos_marginales, aes(x = exp(lnnec), y = estimate)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
#   labs(
#     title = "Efecto marginal de la regulación según nivel de NEC",
#     x = "NEC",
#     y = "Cambio en probabilidad predicha (regulación 1 vs 0)"
#   ) +
#   theme_minimal()
# # nec es especialmente relevante cuando no hay regulacion
# 
# 
# #### COMENTADO OTROS INTERPRETACION MULTILEVEL ######
# paisesdf <-  democracias %>% 
#   subset(cat_pais!="Venezuela") %>% 
#   select(cat_pais) 
# 
# paises <- paisesdf$cat_pais %>% 
#   unique()
# 
# len <- length(paises)
# 
# data_to_predict_multinivel <- data.frame(
#   cat_pais = rep(paises, 40),
#   lnnec = rep(rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
#                   max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
#                   #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
#                   length.out = 20), 2), len), # Cambiar por los valores que quieras probar
#   lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
#   voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
#   dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
#   propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
#   accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
#   avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
#   regulaciondico = rep(rep(c(0,1),each=20), len) ,
#   cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
#   lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
#   democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
#   mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
# )
# 
# 
# # Predecir probabilidades
# # versiones viejas de calculo en test_modelos_dic2024.R 
# 
# # predicted_probs_multinivel <- predict(final_random_intercepts, 
# #                                       newdata = data_to_predict_multinivel, 
# #                                       type = "response",
# #                                       se.fit = T)  
# # 
# # data_to_predict_multinivel$fitted <- predicted_probs_multinivel$fit
# # 
# # data_to_predict_multinivel$se.fitted <- predicted_probs_multinivel$se.fit
# 
# data_to_predict_multinivel <- margins::prediction(final_random_intercepts,
#                                       data = data_to_predict_multinivel)
# 
# # grafico # ESTO PARECE HABER FUNCIONADO AUNQUE ESTA LEJOS DE ESTAR GENIAL
# 
# lattice::dotplot( lme4::ranef(final_random_intercepts))
# 
# ranint <- lme4::ranef(final_random_intercepts) %>% as_tibble()
# 
# # Calcular intervalos de confianza
# ranint <- ranint %>%
#   mutate(lower_ci = condval - 1.96 * condsd,
#          upper_ci = condval + 1.96 * condsd)
# 
# # Graficar
# plot_ranint <- ggplot(ranint, aes(x = condval, y = reorder(grp, condval))) +
#   geom_point() +  # Puntos para los interceptos aleatorios
#   geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Intervalos de confianza
#   theme_minimal() +
#   labs(x = "Interceptos Aleatorios", y = "Países") +
#   theme_classic() + 
#   theme(axis.text.y = element_text(size = 8)) +
#   geom_vline(aes(xintercept = 0), linetype = 3, colour = "blue4") +
#   labs(title = "Interceptos aleatorios estimados en el modelo final multinivel",
#        subtitle = "Por país, junto a su desvío estándar condicional")
# 
# plot_ranint %>% ggsave(filename = "images/ENG_random_intercepts.jpg", width = 10)
# 
