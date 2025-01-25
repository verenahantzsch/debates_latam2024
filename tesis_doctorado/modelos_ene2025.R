
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
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# ELECCIONES ##############################
## PREPARO DATA #####
### data completa #### 
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

### data estandarizada #### 
data_scaled <- data %>%
  mutate(across(-c(dico_hubo_debates,  # excluyo las variables dep dicotomicas de la escalada
                   dico_debates_primerosdos,
                   dico_hubo_debate_mediatico,
                   dico_nueva_practica_elec,
                   dico_practica_interrumpida_elec,
                   dico_nueva_practica_ciclo,
                   dico_practica_interrumpida_ciclo,
                   cat_pais,
                   elecid), scale))

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
         lncumsumpastciclos = log(cumsum_pastciclos + 1))

# democracias$marginvic

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

formula_modelo_temporal <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_temporal <- glm(formula_modelo_temporal,
                         family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_temporal)

formula_modelo_temporal_bis <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         lngdp +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_temporal_bis <- glm(formula_modelo_temporal_bis,
                       family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_temporal_bis)

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
                           mediaqualitycorruptvdem#, #+
                          # ncat_eleccion,
                          #edadregimenbmr#, "


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
## MODELOS ROBUSTOS #####

#### https://www.geeksforgeeks.org/different-robust-standard-errors-of-logit-regression-in-stata-and-r/

### Heteroscedasticity-Consistent Standard Errors ####

# robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC0"))

##### Modelo contingencia  ####

robust_se_modelo_contingencia <- coeftest(modelo_contingencia,  
                                          vcovHC(modelo_contingencia, type = "HC0"))

print(robust_se_modelo_contingencia)


##### Modelo sistemico ####

robust_se_modelo_sistemico <- coeftest(modelo_sistemico, 
                                       vcovHC(modelo_sistemico, type = "HC0"))
print(robust_se_modelo_sistemico)


##### Modelo marco regulatorio ####

robust_se_modelo_regulatorio <- coeftest(modelo_regulatorio, 
                                         vcovHC(modelo_regulatorio, type = "HC0"))
print(robust_se_modelo_regulatorio)


##### Modelo geo/temp ####

robust_se_modelo_temporal <- coeftest(modelo_temporal, 
                                      vcovHC(modelo_temporal, type = "HC0"))
print(robust_se_modelo_temporal)


##### Modelo final / sficativas ####

robust_se_modelo_sficativas <- coeftest(modelo_sficativas, 
                                        vcovHC(modelo_sficativas, type = "HC0"))
print(robust_se_modelo_sficativas)

##### Modelo final / sficativas variantes ####

robust_se_modelo_sficativas <- coeftest(modelo_sficativas_variantes, 
                                        vcovHC(modelo_sficativas_variantes, type = "HC0"))
print(robust_se_modelo_sficativas)

###  Cluster-Robust Standard Errors ####
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

robust_se_cluster_modelo_temporal <- coeftest(modelo_temporal_bis, 
                                              vcov = vcovCL(modelo_temporal_bis, 
                                                            #cluster = democracias$elecid))
                                                            #cluster = data$elecid))
                                                            cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_temporal)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_temporal[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_temporal))
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
### PASO 0 COMENTADO Version artesanal (COMENTADO CON # ) ####
# 
# #### calculo de media dico_hubo_debates, en general y x pais
# 
# 
# mean_dico_hubo_debates <- data_modelo_a_interpretar$dico_hubo_debates %>% mean()
# 
# mean_dico_hubo_debatesxpais <- data_modelo_a_interpretar %>% 
#   group_by(cat_pais) %>% 
#   summarise(mean_dico_hubo_debatesxpais = mean(dico_hubo_debates),
#          diff_mean_general = mean_dico_hubo_debates - mean_dico_hubo_debatesxpais,
#          abs_diff_mean_general = abs(diff_mean_general)) %>% 
#   arrange(desc(abs_diff_mean_general))
# 
# 
# # uruguay es nuestro país de referencia
# 
# #### creamos variables dico
# data_modelo_a_interpretar$cat_pais %>% unique()
# data_modelo_a_interpretar <- data_modelo_a_interpretar %>% 
#   mutate(dico_argentina = ifelse(cat_pais=="Argentina",1,0),
#          dico_bolivia = ifelse(cat_pais=="Bolivia",1,0),
#          dico_brasil = ifelse(cat_pais=="Brasil",1,0),
#          dico_chile = ifelse(cat_pais=="Chile",1,0),
#          dico_colombia = ifelse(cat_pais=="Colombia",1,0),
#          dico_crica = ifelse(cat_pais=="Costa Rica",1,0),
#          dico_ecuador = ifelse(cat_pais=="Ecuador",1,0),
#          dico_elslv = ifelse(cat_pais=="El Salvador",1,0),
#          dico_guatemala = ifelse(cat_pais=="Guatemala",1,0),
#          dico_honduras = ifelse(cat_pais=="Honduras",1,0),
#          dico_mx = ifelse(cat_pais=="Mexico",1,0),
#          dico_nicaragua = ifelse(cat_pais=="Nicaragua",1,0),
#          dico_panama = ifelse(cat_pais=="Panama",1,0),
#          dico_paraguay = ifelse(cat_pais=="Paraguay",1,0),
#          dico_peru = ifelse(cat_pais=="Peru",1,0),
#          dico_uruguay = ifelse(cat_pais=="Uruguay",1,0), # cat de referencia 
#          dico_repdom = ifelse(cat_pais=="Republica Dominicana",1,0),
#          dico_venezuela = ifelse(cat_pais=="Venezuela",1,0))
# 
# #### corremos modelo incluyendo dico_pais sin cat de referencia
# formula_modelo_multinivel <- "dico_hubo_debates ~  # MODELO SFICATIVAS VARIANTES
#                         #dico_debates_primerosdos ~ 
#                         #dico_hubo_debate_mediatico ~ 
#                            lnmarginvic + # CAMBIE
#                            lnnec +
#                            #exapprovalnotsmoothed + 
#                            voteshareincumbent +
#                            dico_reeleccion + 
#                            #proptv +
#                            propindivinternet +
#                            accesogratuito +
#                            avgpropdebatesregionxciclo + 
#                            #prop_elec_usa_ciclo +
#                            regulaciondico +
#                            cumsum_pastciclos +
#                            #dico_debates_pastelection +
#                            lngdp + # CAMBIE
#                            democraciavdemelectoralcomp +
#                            mediaqualitycorruptvdem +
#                           dico_argentina +
#                           dico_bolivia +
#                           dico_brasil +
#                           dico_chile +
#                           dico_colombia +
#                           dico_crica +
#                           dico_ecuador +
#                           dico_elslv +
#                           dico_guatemala +
#                           dico_honduras +
#                           dico_mx +
#                           dico_nicaragua +
#                           dico_panama+
#                           dico_paraguay +
#                           dico_peru +
#                           dico_repdom +
#                           dico_venezuela"
# 
# 
# modelo_multinivel <- glm(formula_modelo_multinivel,
#                          family = binomial(link = "logit"), 
#                          #data = data 
#                          data = data_modelo_a_interpretar)
# options(scipen=999)
# summary(modelo_multinivel)
# 
# # OJO no se si corresponde clusterizar por pais si ya incorporamos pais
# robust_se_cluster_modelo_multinivel <- coeftest(modelo_multinivel, 
#                                                 vcov = vcovCL(modelo_multinivel, 
#                                                  cluster = data_modelo_a_interpretar$cat_pais))
# print(robust_se_cluster_modelo_multinivel)
# 
# 
# 
# #### modelo interactivo
# 
# 
# 
# formula_modelo_multinivel_interactivo <- "dico_hubo_debates ~  # MODELO SFICATIVAS VARIANTES
#                         #dico_debates_primerosdos ~ 
#                         #dico_hubo_debate_mediatico ~ 
#                            lnmarginvic + # CAMBIE
#                            lnnec +
#                            #exapprovalnotsmoothed + 
#                            voteshareincumbent +
#                            dico_reeleccion + 
#                            #proptv +
#                            propindivinternet +
#                            accesogratuito +
#                            avgpropdebatesregionxciclo + 
#                            #prop_elec_usa_ciclo +
#                            regulaciondico +
#                            cumsum_pastciclos +
#                            #dico_debates_pastelection +
#                            lngdp + # CAMBIE
#                            democraciavdemelectoralcomp +
#                            mediaqualitycorruptvdem +
#                          dico_argentina + # DICO POR PAIS
#                          dico_bolivia +
#                          dico_brasil +
#                          dico_chile +
#                          dico_colombia +
#                          dico_crica +
#                          dico_ecuador +
#                          dico_elslv +
#                          dico_guatemala +
#                          dico_honduras +
#                          dico_mx +
#                          dico_nicaragua +
#                          dico_panama+
#                          dico_paraguay +
#                          dico_peru +
#                          dico_repdom +
#                          dico_venezuela +  
#                          dico_argentina*lnnec + # INTERACCIONES
#                          dico_bolivia*lnnec +
#                          dico_brasil*lnnec +
#                          dico_chile*lnnec +
#                          dico_colombia*lnnec +
#                          dico_crica*lnnec +
#                          dico_ecuador*lnnec +
#                          dico_elslv*lnnec +
#                          dico_guatemala*lnnec +
#                          dico_honduras*lnnec +
#                          dico_mx*lnnec +
#                          dico_nicaragua*lnnec +
#                          dico_panama*lnnec +
#                          dico_paraguay*lnnec +
#                          dico_peru*lnnec +
#                          dico_uruguay*lnnec + # no estoy segura de si agregar
#                          dico_repdom*lnnec +
#                          dico_venezuela*lnnec  "
# 
# 
# modelo_multinivel_interactivo <- glm(formula_modelo_multinivel_interactivo,
#                          family = binomial(link = "logit"), 
#                          #data = data 
#                          data = data_modelo_a_interpretar)
# options(scipen=999)
# summary(modelo_multinivel_interactivo)
# 
# # OJO no se si corresponde clusterizar por pais si ya incorporamos pais
# robust_se_cluster_modelo_multinivel_interactivo <- coeftest(modelo_multinivel_interactivo, 
#                                                 vcov = vcovCL(modelo_multinivel_interactivo, 
#                                                               cluster = data_modelo_a_interpretar$cat_pais))
# print(robust_se_cluster_modelo_multinivel_interactivo)

### Version formula #####
# https://www.publichealth.columbia.edu/research/population-health-methods/multi-level-modeling


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

temporal_random_intercepts <- lme4::glmer(paste(formula_modelo_temporal_bis, 
                                                "(1 | cat_pais)", sep = "+"), 
                                          family=binomial("logit"), 
                                          data = democracias,
                                          control = control)
summary(temporal_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(temporal_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# temporal_random_intercepts_robust <- coef_test(temporal_random_intercepts, vcov = vcov_cluster)

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


####  PASO 3 COMENTADO NO CONVERGE PASO 3 MODELO MULTILEVEL: RANDOM INTERCEPTS AND RANDOM SLOPES BY COUNTRY #### 
#  Varying intercepts and slopes
# M3 <- lmer (y ~ x + (1 + x | county))
# control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# 
# modelo_random_slopes1 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
#                                   # (1 + lnnec | cat_pais), 
#                                     "(1 + regulaciondico | cat_pais)", sep = "+" ), 
#                                  family=binomial("logit"), 
#                                  data = democracias,
#                                  control = control)
# summary(modelo_random_slopes1)
# 
# modelo_random_slopes2 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
#                                            " (1 + lnnec | cat_pais)",  sep = "+" ), 
#                                      family=binomial("logit"), 
#                                      data = democracias,
#                                      control = control)
# summary(modelo_random_slopes2)
# 
# #coef(modelo_random_slopes2)
# 
# modelo_random_slopes3 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
#                                            " (1 + lnnec + regulaciondico | cat_pais)",  sep = "+" ), 
#                                      family=binomial("logit"), 
#                                      data = democracias)#,
#                                      #control = control)
# summary(modelo_random_slopes3) # NO CONVERGE
# 
# #coef(modelo_random_slopes2)
# 
# 
# modelo_random_slopes4 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
#                                            " (1 + lnnec + regulaciondico + 
#                                            lngdp + democraciavdemelectoralcomp + mediaqualitycorruptvdem +
#                                            accesogratuito + propindivinternet | cat_pais)",  sep = "+" ), 
#                                      family=binomial("logit"), 
#                                      data = democracias,
#                                      control = control)
# summary(modelo_random_slopes4)
# 
# coef(modelo_random_slopes3)
# 
# 
# lme4::fixef(modelo_random_slopes) #to see the estimated average coeﬃcients (“ﬁxed eﬀects”):
# lme4::ranef(modelo_random_slopes) #to see the estimated group-level errors (“random eﬀects”):  


#### PASO 4 COMENTADO NO CONVERGE PASO 4: NON NESTED NO CONVERGE #####
# 
# modelo_fixed_effects <- glm(paste(formula_modelo_sficativas_variantes, 
#                                           " as.factor(cat_pais) + ncat_eleccion",  sep = "+" ), 
#                                     family=binomial("logit"), 
#                                     data = democracias)
# 
# summary(modelo_fixed_effects)
# 
# modelo_non_nested <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
#                                            " + (1 | cat_pais) + (1 | ncat_eleccion)",  sep = "+" ), 
#                                      family=binomial("logit"), 
#                                      data = democracias, 
#                                  control = control)
# summary(modelo_non_nested)
# mlmhelpr::icc(modelo_non_nested)
# coef(modelo_non_nested)

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

#### defino modelo 0 ####

modelo_0 <- glm(dico_hubo_debates ~ 1, 
                family = binomial(link = "logit"), 
                #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                data = democracias)

#### defino modelos comparables con muestra reducida ####

#### reducida al maximo ####
modelo_0_reducido <- glm(dico_hubo_debates ~ 1, 
                                    family = binomial(link = "logit"), 
                                    data = data_reducida)
modelo_contingencia_reducido <- glm(formula_modelo_contingencia_bis, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_sistemico_reducido <- glm(formula_modelo_sistemico_bis, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_regulatorio_reducido <- glm(formula_modelo_regulatorio_bis, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_temporal_reducido <- glm(formula_modelo_temporal_bis, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_sficativas_reducido <- glm(formula_modelo_sficativas, 
                                family = binomial(link = "logit"), 
                                data = data_reducida)
modelo_sficativas_variantes_reducido <- glm(formula_modelo_sficativas_variantes, 
                                  family = binomial(link = "logit"), 
                                  data = data_reducida)

#### PENDIENTE no tan relevante: reducida para comparar con version final del modelo ####
modelo_0_reducido <- glm(dico_hubo_debates ~ 1, 
                         family = binomial(link = "logit"), 
                         data = data_reducida)
modelo_contingencia_reducido <- glm(formula_modelo_contingencia_bis, 
                                    family = binomial(link = "logit"), 
                                    data = data_reducida)
modelo_sistemico_reducido <- glm(formula_modelo_sistemico_bis, 
                                 family = binomial(link = "logit"), 
                                 data = data_reducida)
modelo_regulatorio_reducido <- glm(formula_modelo_regulatorio_bis, 
                                   family = binomial(link = "logit"), 
                                   data = data_reducida)
modelo_temporal_reducido <- glm(formula_modelo_temporal_bis, 
                                family = binomial(link = "logit"), 
                                data = data_reducida)
modelo_sficativas_reducido <- glm(formula_modelo_sficativas, 
                                  family = binomial(link = "logit"), 
                                  data = data_reducida)
modelo_sficativas_variantes_reducido <- glm(formula_modelo_sficativas_variantes, 
                                            family = binomial(link = "logit"), 
                                            data = data_reducida)

#### reducida sficativas ####
modelo_0_reducido_sficativas <- glm(dico_hubo_debates ~ 1, 
                         family = binomial(link = "logit"), 
                         data = data_modelo_sficativas)
modelo_contingencia_reducido_sficativas <- glm(formula_modelo_contingencia, 
                                    family = binomial(link = "logit"), 
                                    data = data_modelo_sficativas)
modelo_sistemico_reducido_sficativas <- glm(formula_modelo_sistemico, 
                                 family = binomial(link = "logit"), 
                                 data = data_modelo_sficativas)
modelo_regulatorio_reducido_sficativas <- glm(formula_modelo_regulatorio, 
                                   family = binomial(link = "logit"), 
                                   data = data_modelo_sficativas)
modelo_temporal_reducido_sficativas <- glm(formula_modelo_temporal, 
                                family = binomial(link = "logit"), 
                                data = data_modelo_sficativas)
modelo_varaintes_reducido <- glm(formula_modelo_sficativas_variantes, 
                                           family = binomial(link = "logit"), 
                                           data = data_modelo_sficativas)


#### importante: defino modelo de prueba ####
data_modelo_a_probar <- data_modelo_sficativas
modelo_a_probar <-glm(formula_modelo_sficativas_variantes,
                                                      family = binomial(link = "logit"), 
                                                      #data = data 
                                                      data = data_modelo_a_probar)
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

##### Durbin-Watson para autocorrelacion de errores ####
# prueba de Durbin-Watson (verificar autocorrelación en los residuos).
# The Durbin-Watson test has the null hypothesis that the autocorrelation of the disturbances is 0. 
# It is possible to test against the alternative that it is greater than, not equal to, or less than 0, respectively. This can be specified by the alternative argument.
# https://medium.com/@analyttica/durbin-watson-test-fde429f79203
# no es lo más adecuado de usar cuando tengo variables rezagadas (como es mi caso)
dwtest(modelo_a_probar, alternative = "two.sided")
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

tabla_cook %>% write.csv("anexos/tabla_cook.csv")

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
car::influencePlot(modelo_a_probar)
4/nrow(data_modelo_a_probar)
data_modelo_a_probar[c(20,61,77,127,162),"obsid"]

car::influenceIndexPlot(modelo_a_probar, vars = c("Cook", "hat"))
#data_modelo_a_probar[c(20,61,77,127, 162),]
data_modelo_a_probar[c(27,39,72,90,177,225),]

# observaciones nr 90, 116 para cook , 77 y 78 para hat en modelo sficativas 
# observaciones nr 90, 116 para cook , 7 y 177 para hat en modelo sficativas, aunque tambien vale mencionar que se reducen las escalas del problema 

##### dfbetas ####

#Para cada observación, podemos ver la diferencia en la estimación del coeficiente para la intersección, la variable  disp y la variable  hp que ocurre cuando eliminamos esa observación en particular.
#Normalmente consideramos que una observación es muy influyente en la estimación de un coeficiente dado si tiene un valor DBETAS mayor que un umbral de 2/√ n, donde  n es el número de observaciones.
umbral <- 2 / sqrt(nrow(data_modelo_a_probar))

#especificar 2 filas y 1 columna en la región de trazado
#par(mfrow=c(2,1))

dfbetas <- as.data.frame(dfbetas(modelo_a_probar))


par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))


for (i in seq(2, ncol(dfbetas))) { # Iterar sobre columnas de dfbetas
  
  plot(dfbetas[[i]],   
       main = paste("Coeficiente:" , colnames(dfbetas)[i]), # Agregar el nombre de la variable como título
       xlab = "Índice de observaciones", ylab = "DFBETAs")
  
  abline(h = umbral, lty = 2, col = "gray50")
  abline(h = -umbral, lty = 2, col = "gray50")
  
  # Agregar etiquetas de texto a los puntos
  text(seq_along(dfbetas[[i]]), dfbetas[[i]],
       labels = data_modelo_a_probar$obsid, pos = 4, cex = 0.7, col = "blue3")
}

#### comentario ####

# de cook:
# costa rica 1986 parece ser la mas problematica
# otras potenciales son brasil 2010 segunda ronda, ecuador 1988, guatemala 1999, colombia 2006, entre otras
# la posicuion de costa rica mejora en la variante respecto del de sficativas a secas, pero la de otros casos empeora, el balance quizas es relativamente neutro

# de dfbetas
# conclusiones generales: los resultados no cambian mucho de sficativas a variantes. En algunos casos mejora, en otros empeora la situacion
# conviene quizas reestimar sin nicaragua 1990 primera vuelta y Peru 1990 segunda vuelta y examinar estos casos porque en varias variables aparecio como influyente # ATENTI !!!!! ###
# ademas parece que el pais peru y el pais ecuador estan teniendo algun efecto relevante para algunas variables para las que tiran para abajo y para arriba al mismo tiempo (en distintos años)

# algunos cambios despues de cambio en imputacion de propindivinternet
# colombia 1998, el salvador 2014, argentina 2003, repdom 2020 repdom 2016, nicaragua 1990, peru 1990 2

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
vifs_4 <- vifs(modelo_temporal)
vifs_4_bis <- vifs(modelo_temporal_bis)
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

all_vifs <- all_vifs %>% 
  pivot_wider(names_from = Modelo, values_from = Vif_value)

all_vifs %>% write_csv("anexos/vifs_values.csv")

#### otros? PENDIENTE ####
### Controles - Missing data PENDIENTE ####
# jack knife
# imputacion: 
# reemp x valor promedio, 
# reemp x mediana, 
# mcmc multiple imputation approach -> simulacion + iteraciones
# comparar modelos 
### Controles - Fit, Overall significance y comparaciones ####
#### test de Wald ####

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

lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_0_reducido)
lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_contingencia_reducido)
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_sistemico_reducido) # pendiente
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_regulatorio_reducido) # pendiente
# lmtest::waldtest(modelo_sficativas_variantes_reducido , modelo_temporal_reducido) # pendiente
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
#lrtest(modelo_temporal_reducido_sficativas, modelo_sficativas)

lrtest(modelo_a_probar)
lrtest(modelo_sficativas_variantes_reducido , modelo_0_reducido)
lrtest(modelo_sficativas_variantes_reducido , modelo_contingencia_reducido)
# lrtest(modelo_sficativas_variantes_reducido , modelo_sistemico_reducido) # pendiente
# lrtest(modelo_sficativas_variantes_reducido , modelo_regulatorio_reducido) # pendiente
#lrtest(modelo_sficativas_variantes_reducido , modelo_temporal_reducido) # ojo no estan anidados
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
stats4 <- stats( modelo_temporal_reducido)
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

#all_stats %>% write_csv("anexos/stats_values.csv")

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
countr24 <- funcion_count_R2(modelo_temporal_reducido, data_reducida$dico_hubo_debates)
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

#all_count_r2_reducidas %>% write_csv("anexos/countR2_values.csv")

all_stats <- all_stats %>% 
  rbind(all_count_r2_reducidas)


all_stats %>% write_csv("anexos/stats_values.csv")

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

stats_modelo %>% write_csv("anexos/stats_modelo.csv")

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

all_stats_multisimple %>% write_csv("anexos/all_stats_multisimple.csv")

## OTROS MODELOS control ####

#### Volatility agregada ####
##### modelo sistemico #####
modelo_sistemico_control_convolatilidad <- glm(paste(formula_modelo_sistemico_bis,
                                                     "volatility", sep = "+"),
                                               family = binomial(link = "logit"), 
                                               #data = data 
                                               data = democracias)

summary(modelo_sistemico_control_convolatilidad) # 

robust_se_cluster_modelo_sistemico_control_convolatilidad <- coeftest(modelo_sistemico_control_convolatilidad, 
                                                                      vcov = vcovCL(modelo_sistemico_control_convolatilidad,
                                                                                    cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sistemico_control_convolatilidad)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

sistemico_random_intercepts_control_convolatilidad <- lme4::glmer(
  paste(formula_modelo_sistemico_bis,
        "volatility + (1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(sistemico_random_intercepts_control_convolatilidad)

##### modelo final #####
modelo_final_control_convolatilidad <- glm(paste(formula_modelo_sficativas_variantes,
                                                 "volatility", sep = "+"),
                                           family = binomial(link = "logit"), 
                                           #data = data 
                                           data = democracias)

summary(modelo_final_control_convolatilidad) # 

robust_se_cluster_modelo_final_control_convolatilidad <- coeftest(modelo_final_control_convolatilidad, 
                                                                  vcov = vcovCL(modelo_final_control_convolatilidad,
                                                                                cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_convolatilidad)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_convolatilidad <- lme4::glmer(
  paste(formula_modelo_sficativas_variantes,
        "volatility + (1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_convolatilidad)

#### Alineamiento en modelo final #####

modelo_final_control_conalienamiento <- glm(paste(formula_modelo_sficativas_variantes,
                                                  "alineamiento", sep = "+"),
                                            family = binomial(link = "logit"), 
                                            data = democracias)

summary(modelo_final_control_conalienamiento) # 

robust_se_cluster_modelo_final_control_conalienamiento <- coeftest(modelo_final_control_conalienamiento, 
                                                                   vcov = vcovCL(modelo_final_control_conalienamiento,
                                                                                 cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_conalienamiento)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_conalienamiento <- lme4::glmer(
  paste(formula_modelo_sficativas_variantes,
        "alineamiento + (1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_conalienamiento)

#### final sin Accesogratuito #####

modelo_final_control_sinaccesogratuito <- glm(str_remove(formula_modelo_sficativas_variantes,
                                                         " accesogratuito +"),
                                              family = binomial(link = "logit"), 
                                              data = democracias)

summary(modelo_final_control_sinaccesogratuito) # 

robust_se_cluster_modelo_final_control_sinaccesogratuito <- coeftest(modelo_final_control_sinaccesogratuito, 
                                                                     vcov = vcovCL(modelo_final_control_sinaccesogratuito,
                                                                                   cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_sinaccesogratuito)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_sinaccesogratuito <- lme4::glmer(
  paste(str_remove(formula_modelo_sficativas_variantes,
                   " accesogratuito +"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_sinaccesogratuito)

#### LNcumsum #####

modelo_final_control_lncumsum <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                 "cumsum_pastciclos", "lncumsumpastciclos"),
                                     family = binomial(link = "logit"), 
                                     data = democracias)

summary(modelo_final_control_lncumsum) # 

robust_se_cluster_modelo_final_control_lncumsum <- coeftest(modelo_final_control_lncumsum, 
                                                            vcov = vcovCL(modelo_final_control_lncumsum,
                                                                          cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_lncumsum)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_lncumsum <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "cumsum_pastciclos", "lncumsumpastciclos"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_lncumsum)




#### Dico_debates_pasados #####

modelo_final_control_dicodebatespasteleeccion <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                                 "cumsum_pastciclos", "dico_debates_pastelection"),
                                                     family = binomial(link = "logit"), 
                                                     data = democracias)

summary(modelo_final_control_dicodebatespasteleeccion) # 

robust_se_cluster_modelo_final_control_dicodebatespasteleeccion <- coeftest(modelo_final_control_dicodebatespasteleeccion, 
                                                                            vcov = vcovCL(modelo_final_control_dicodebatespasteleeccion,
                                                                                          cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_dicodebatespasteleeccion)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_dicodebatespasteleeccion <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "cumsum_pastciclos", "dico_debates_pastelection"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_dicodebatespasteleeccion)

#### (no usada) nunca debates diconuncadebates #####

democracias <- democracias %>% 
  mutate(diconuncadebates = ifelse(cumsum_pastciclos==0|is.na(cumsum_pastciclos),
                                   1,
                                   0))

modelo_final_control_diconuncadebates <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                         "cumsum_pastciclos", "diconuncadebates"),
                                             family = binomial(link = "logit"), 
                                             data = democracias)

#summary(modelo_final_control_diconuncadebates) # 

robust_se_cluster_modelo_final_control_diconuncadebates <- coeftest(modelo_final_control_diconuncadebates, 
                                                                    vcov = vcovCL(modelo_final_control_diconuncadebates,
                                                                                  cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_diconuncadebates)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_diconuncadebates <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "cumsum_pastciclos", "diconuncadebates"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_diconuncadebates)


#### Regulacion (tres variantes) #####

modelo_final_control_regulacionalternativa <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                              "regulaciondico", "regulacionobligatoriodico + regulaciongarantiasdico"),
                                                  family = binomial(link = "logit"), 
                                                  data = democracias)

#summary(modelo_final_control_regulacionalternativa) # 

robust_se_cluster_modelo_final_control_regulacionalternativa <- coeftest(modelo_final_control_regulacionalternativa, 
                                                                         vcov = vcovCL(modelo_final_control_regulacionalternativa,
                                                                                       cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_regulacionalternativa)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_regulacionalternativa <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "regulaciondico", "regulacionobligatoriodico + regulaciongarantiasdico"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_regulacionalternativa)

# version 2

democracias <- democracias %>% 
  mutate(regulacionotradico = ifelse(regulaciongarantiasdico==1|regulacionposibilidaddico==1,
                                     1,0))

modelo_final_control_regulacionalternativa2 <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                               "regulaciondico", "regulacionobligatoriodico + regulacionotradico"),
                                                   family = binomial(link = "logit"), 
                                                   data = democracias)

#summary(modelo_final_control_regulacionalternativa2) # 

robust_se_cluster_modelo_final_control_regulacionalternativa2 <- coeftest(modelo_final_control_regulacionalternativa2, 
                                                                          vcov = vcovCL(modelo_final_control_regulacionalternativa2,
                                                                                        cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_regulacionalternativa2)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_regulacionalternativa2 <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "regulaciondico", "regulacionobligatoriodico + regulacionotradico"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_regulacionalternativa2)


# version ordinal

democracias <- democracias %>% 
  mutate(regulacionotradico = ifelse(regulaciongarantiasdico==1|regulacionposibilidaddico==1,
                                     1,0))

modelo_final_control_regulacionalternativa3 <- glm(str_replace(formula_modelo_sficativas_variantes,
                                                               "regulaciondico", "regulacionordinal"),
                                                   family = binomial(link = "logit"), 
                                                   data = democracias)

summary(modelo_final_control_regulacionalternativa3) # 

robust_se_cluster_modelo_final_control_regulacionalternativa3 <- coeftest(modelo_final_control_regulacionalternativa3, 
                                                                          vcov = vcovCL(modelo_final_control_regulacionalternativa3,
                                                                                        cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_final_control_regulacionalternativa3)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_control_regulacionalternativa3 <- lme4::glmer(
  paste(str_replace(formula_modelo_sficativas_variantes,
                    "regulaciondico", "regulacionordinal"),
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_control_regulacionalternativa3)

#### Intercativo #####

# variantes modelo sficativas
formula_modelo_sficativas_variantes_interactivo <- "dico_hubo_debates ~ 
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                          #lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                          regulaciondico*lnnec +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_sficativas_variantes_interactivo <- glm(formula_modelo_sficativas_variantes_interactivo,
                                               family = binomial(link = "logit"), 
                                               #data = data 
                                               data = democracias)
options(scipen=999)
#summary(modelo_sficativas_variantes_interactivo)


robust_se_cluster_modelo_sficativas_variantes_interactivo <- coeftest(modelo_sficativas_variantes_interactivo, 
                                                                      vcov = vcovCL(modelo_sficativas_variantes_interactivo, 
                                                                                    #cluster = democracias$elecid))
                                                                                    #cluster = data$elecid))
                                                                                    cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes_interactivo)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

final_random_intercepts_interactivo <- lme4::glmer(
  paste(formula_modelo_sficativas_variantes_interactivo,
        "(1 | cat_pais)", sep = "+"),
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts_interactivo)





#### prop internet version VIEJA #####
# YA QUEDO TODO ACTUALIZADO CON LA VERSION INTERPOLADA

formula_propinternet2 <- "dico_hubo_debates ~ 
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet2 + #ACA!
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem#, #+
                          # ncat_eleccion,
                          #edadregimenbmr#, "


modelo_propinternet2 <- glm(formula_propinternet2,
                            family = binomial(link = "logit"), 
                            #data = data 
                            data = democracias)
options(scipen=999)
summary(modelo_propinternet2)

robust_se_cluster_modelo_propinternet2 <- coeftest(modelo_propinternet2, 
                                                   vcov = vcovCL(modelo_propinternet2, 
                                                                 #cluster = democracias$elecid))
                                                                 #cluster = data$elecid))
                                                                 cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_propinternet2)


#### Loop / prueba #### 
### primera version de LOOP con DICO HUBO DEBATES para DICO DEBATES PASTELECCION  

# para otra version: sustituyo NAs por 0, para tener mismo n en todas las corridas del loop

data_loop <- democracias %>% 
  mutate(across(.cols = c(dico_debates_pastelection,
                          dico_2_eleccondebatesseguidos,
                          dico_3_eleccondebatesseguidos,
                          dico_4_eleccondebatesseguidos,
                          dico_5_eleccondebatesseguidos,
                          dico_6_eleccondebatesseguidos,
                          dico_7_eleccondebatesseguidos), 
                .fns = ~ ifelse(is.na(.), 0, .)))

# List of predictor variables to loop through
predictors <- c(   "dico_debates_pastelection",
                   "dico_2_eleccondebatesseguidos",
                   "dico_3_eleccondebatesseguidos",
                   "dico_4_eleccondebatesseguidos",
                   "dico_5_eleccondebatesseguidos",
                   "dico_6_eleccondebatesseguidos",
                   "dico_7_eleccondebatesseguidos")

# Create an empty list to store coefficients
coef_results <- list()

# Loop through each predictor
for (var in predictors) {
  # Construct the formula dynamically
  formula <- as.formula(str_replace(formula_modelo_sficativas_variantes, 
                                    "cumsum_pastciclos", var))
  
  # Perform the regression
  model <- glm(formula,
               family = binomial(link = "logit"),
               data = data_loop)
  
  robust_se_cluster <- coeftest(model, vcov = vcovCL(model, cluster = data_loop$elecid))

  # Save the coefficients 
  coef_results[[var]] <- data.frame(
    Estimate = robust_se_cluster[, "Estimate"],
    Std.Error = robust_se_cluster[, "Std. Error"],
    Pr = robust_se_cluster[, "Pr(>|z|)"]
  )
}


# Extract the results for the desired predictor
coef_for_predictor <- lapply(coef_results, function(x) x[rownames(x) %in% predictors, ])

# Convert the list to a data frame for easy comparison
comparison_df <- do.call(rbind, coef_for_predictor) %>% 
  rownames_to_column(var = "Modelo") 
 
comparison_df %>% write_csv("anexos/loop_variable_antecedente.csv")

comparison_df2 <- do.call(rbind, coef_results) %>% 
  subset(Pr<0.1) %>% 
  rownames_to_column(var = "Modelo") %>% 
  mutate(Variable = str_extract(Modelo,'\\b\\w+$')) %>% 
  mutate(Modelo = str_replace(Modelo, Variable, ""))

comparison_df2 %>% write_csv("anexos/loop_otras_variables.csv")

# View the comparison table
comparison_df

#### Non Pooled Cumsum = 0 #####
democracias2.2 <- democracias %>% 
  subset(cumsum_pastciclos==0|is.na(cumsum_pastciclos)) 
#subset(dico_debates_pastelection==0) 
summary(democracias2.2)

# con cumsum == 0 

formula_2.2 <- str_replace(formula_modelo_sficativas_variantes, "cumsum_pastciclos +", "")
modelo_sficativas_variantes2.2 <- glm(formula_2.2,
                                      family = binomial(link = "logit"), 
                                      #data = data2
                                      data = democracias2.2)
options(scipen=999)
#summary(modelo_sficativas_variantes2.2)


robust_se_cluster_modelo_sficativas_variantes2.2 <- coeftest(modelo_sficativas_variantes2.2, 
                                                             vcov = vcovCL(modelo_sficativas_variantes2.2, 
                                                                           #cluster = democracias$elecid))
                                                                           #cluster = data$elecid))
                                                                           cluster = democracias2.2$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes2.2)

modelo_sficativas_variantes2.2_multinivel <-  lme4::glmer(paste(formula_2.2,
                                                              "(1|cat_pais)", sep = "+"),
                                                        family = binomial(link = "logit"), 
                                                        data = democracias2.2)
options(scipen=999)
summary(modelo_sficativas_variantes2.2_multinivel)

## EXPORTO MODELOS SIMPLES #####

# Una vez que estimamos los modelos que irán en la tabla, los agrupamos en una lista usando la función list. Esto ahorra tiempo, porque en lugar de tener que escribir el nombre de los modelos, simplemente nos referiremos a la lista mp_models:

lista1 <-  list(
  robust_se_cluster_modelo_contingencia,
  robust_se_cluster_modelo_sistemico,
  robust_se_cluster_modelo_regulatorio,
  robust_se_cluster_modelo_temporal,
  robust_se_cluster_modelo_sficativas_variantes
) 


texreg::htmlreg(lista1,
                custom.model.names = c("Contingencia",
                                       "Sistemico",
                                       "Regulatorio",
                                       "Temporal",
                                       "Final")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      
                                      "Alineamiento partidario",
                                      "Prop. TV por hogar"		,
                                      "Prop. individuos c internet",
                                      "Prohibicion propaganda"	 ,
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
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
                file="anexos/tabla_modelos_logit_robustos.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

## EXPORTO MODELOS RANDOM INTERCEPT #####
lista1bis <-  list(contingencia_random_intercepts,
                   sistemico_random_intercepts,
                   regulatorio_random_intercepts,
                   temporal_random_intercepts,
                   final_random_intercepts)

# https://www.rdocumentation.org/packages/texreg/versions/1.39.4/topics/htmlreg

texreg::htmlreg(lista1bis,
                custom.model.names = c("Contingencia",
                                       "Sistemico",
                                       "Regulatorio",
                                       "Temporal",
                                       "Final")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      
                                      "Alineamiento partidario",
                                      "Prop. TV por hogar"		,
                                      "Prop. individuos c internet",
                                      "Prohibicion propaganda"	 ,
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
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
                file="anexos/tabla_random_intercepts.html",
                caption = "Modelos estimados con efectos aleatorios por país",
                center = T,
                bold = 0.1)


## EXPORTO MODELOS CONTROL #####
##### S/outliers  ####
lista2 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  #robust_se_cluster_modelo_sficativas,
  robust_se_cluster_modelo_sficativas_variantes_s_outliers
) 

texreg::htmlreg(lista2,
                custom.model.names = c("Final",
                                       "Final s/ casos influyentes")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)") ,
                file="anexos/tabla_anexa_outliers.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

# kable(tabla, format = "latex", align = "c") %>%
#   kable_styling(latex_options = "HOLD_position")

##### Comparaciones varios modelos finales  #####
lista3 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_all,
  robust_se_cluster_modelo_final_control_conalienamiento,
  robust_se_cluster_modelo_final_control_sinaccesogratuito,
  robust_se_cluster_modelo_final_control_convolatilidad ) 

texreg::htmlreg(lista3,
                custom.model.names = c("Final",
                                       "Completo",
                                       "Final c/ alineamiento",
                                       "Final s/ acceso gratuito",
                                       "Final c/ volatilidad"
                )  ,
                
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Alineamiento partidario",
                                      "Prop. TV por hogar",		
                                      "Volatilidad"
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  16,
                                  14,
                                  15,
                                  6,
                                  7,
                                  8,
                                  9,
                                  10,
                                  11,
                                  12,
                                  13),
                file="anexos/tabla_anexa_otrosmodelos.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

##### S/ logaritmo comparaciones #######
lista4 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_sficativas) 

texreg::htmlreg(lista4,
                custom.model.names = c("Final",
                                       "Variante s/ variables log")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Margen de victoria",
                                      "NEC" ,
                                      "Prop. debates en USA" ,
                                      "PBI per Capita"
                ),
                reorder.coef =  c(1,
                                  1+1,
                                  13+1,
                                  2+1,
                                  14+1,
                                  3+1,
                                  4+1,
                                  
                                  5+1,
                                  6+1,
                                  7+1,
                                  15+1,
                                  8+1,
                                  9+1,
                                  10+1,
                                  16+1,
                                  11+1,
                                  12+1
                ),
                file="anexos/tabla_anexa_s_logit.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

##### Cumsum comparaciones  #####
lista5 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_final_control_dicodebatespasteleeccion,
  robust_se_cluster_modelo_final_control_lncumsum) 

texreg::htmlreg(lista5,
                custom.model.names = c("Final",
                                       "Variante: versión dummy de debates antecedentes",
                                       "Variante: versión log de debates antecedentes")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Elección pasada c/ debates",
                                      "log Cant. elecciones pasadas c/ debates"
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  6,
                                  7,
                                  8,
                                  9,
                                  10,
                                  14,
                                  15,
                                  11,
                                  12,
                                  13
                ),
                file="anexos/tabla_anexa_cumsum.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)


##### Cumsum = 0   #####
lista5bis <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_sficativas_variantes2.2,
  modelo_sficativas_variantes2.2_multinivel) 

texreg::htmlreg(lista5bis,
                custom.model.names = c("Final muestra completa",
                                       "Muestra reducida: s/ debates antecedentes (logit robsuto)",
                                       "Muestra reducida: s/ debates antecedentes (multinivel)")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      #"Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Elección pasada c/ debates"
                                      #"log Cant. elecciones pasadas c/ debates"
                ),
                file="anexos/tabla_anexa_cumsum0.html",
                caption = "La muestra reducida es una con casos sin debates antecedentes",
                center = T,
                bold = 0.1)



##### Comparaciones regulacion  #########
lista6 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_final_control_regulacionalternativa2,
  robust_se_cluster_modelo_final_control_regulacionalternativa3) 

texreg::htmlreg(lista6,
                custom.model.names = c("Final",
                                       "Variante: versión dummy de regulación",
                                       "Variante: versión ordinal de regulación"),
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Regulación: debates obligatorios",
                                      "Regulación: otra regulación",
                                      "Exigencia de regulación"
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  6,
                                  7,
                                  8,
                                  9,
                                  14,
                                  15,
                                  16,
                                  10,
                                  11,
                                  12,
                                  13
                ),
                file="anexos/tabla_anexa_regulacion.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

##### Interactivo  ########
lista7 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_sficativas_variantes_interactivo) 

texreg::htmlreg(lista7,
                custom.model.names = c("Final",
                                       "Final c / interacción" )  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "Votos oficialista",
                                      "Incumbente reelije",
                                      "Prop. individuos c internet",
                                      "Acceso gratuito",
                                      "Prop. debates en Region",
                                      "Regulacion sobre debates",
                                      "Cant. elecciones pasadas con debates",
                                      "log PBI per Capita",
                                      "Democracia electoral (VDEM)",
                                      "Corrupcion de medios (VDEM)",
                                      "Regulacion * log NEC"
                ),
                file="anexos/tabla_anexa_interactivo.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)


## INTERPRETACION MODELOS 1 VARIOS PENDIENTES ####

#### INTERPRETACION - importante elegir ####

# modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
# data_modelo_a_interpretar <- data_s_outliers 

modelo_a_interpretar <- modelo_sficativas_variantes
data_modelo_a_interpretar <- democracias 
vcov_modelo_a_interpretar <- vcovCL(modelo_sficativas_variantes, 
               #cluster = democracias$elecid))
               #cluster = data$elecid))
               cluster = democracias$cat_pais)
  
  
# t the coefficient for X is the difference in the log odds.  https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404.

#### INTERPRETACION - ODDS RATIO #####

# a diferencia de la proba predicha, que varia de manera no lineal,
# el odds ratio es constante para distintos valores de x 
# el coef es log(p/1-p) . ---> entonces: p/1-p = exp (covariates).   al reves: 1-p/p = 1/exp(covariates). despejando: 1/p = 1+ 1/exp(covariates). incorporando el 1 . 1/p = exp(c) + 1/ exp(p). despejando . p = exp (cov)/ 1 + exp (cov ). 
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression revisar

odds_ratio <- coef(modelo_a_interpretar)["lnnec"] %>%  exp()

# Each exponentiated coefficient is the ratio of two odds, or the change in odds in the multiplicative scale for a unit increase in the corresponding predictor variable holding other variables at certain value.
# por cada aumento en una unidad de lnnec, esperamos un aumento de un cambio_odds_promedio en las chances de que haya un debate 
# un odds ratio mayor que 1 expresa un cambio positivo, mientras que si es menor que 1 (entre 0 y 1) representa un cambio negativo en las probabilidades estimadas.
# También podemos graficar los coeficientes como odds ratios: recuerda que los odds ratios menores que 1 son efectos negativos y mayores que 1 son positivos. El coeficiente se expresa como su valor medio y su intervalo de confianza del 95%. Si el coeficiente es estadísticamente significativo, su intervalo de confianza no pasará de la línea en 1. Si, por el contrario, no son significativos, el efecto cruzará la línea.

# Para utilizar los odds ratios queremos mostrarte cómo puedes cambiar los coeficientes que obtienes directamente de la tabla, que son log odds, y reemplazarlos por odds ratios. Para ello, puedes usar los argumentos override.coef,override.se y override.pvalues de screenreg()
summary(modelo_a_interpretar)

# OJO! que los errores estandar no estan bien calculados en la tabla a continuacion
texreg::screenreg(modelo_a_interpretar,
          custom.model.names = "data_modelo_a_interpretar - Odds Ratios",
          override.coef    = exp(coef(modelo_a_interpretar)),
          stars = c(0.001, 0.01, 0.05, 0.1),
          # la siguiente función, odds_*, están en el paquete del libro. OJO - NO PUDE DESCARGAR
          # override.se      = odds_se(modelo_a_interpretar), # no tengo esta funcion ! 
          # override.pvalues = odds_pvalues(modelo_a_interpretar),
          # además, omitiremos el coeficiente del intercepto
          omit.coef = "Inter")

exp(cbind(coef(modelo_a_interpretar), confint(modelo_a_interpretar))) 

odds_ratios <- jtools::plot_summs(modelo_a_interpretar, #modelo_contingencia,
                          exp=T, 
                          scale = T,
                          inner_ci_level = .9,
                          # coefs = c("nombre coef" =  "variable",           ....),
                          model.names = c("modelo 1"#, #"modelo 2" ....
                                          ))
# no entiendo bien pq variables que son sficativas acá aparecen como que no

varios_modelos_odds_ratios <- jtools::plot_summs(modelo_contingencia,
                                                 modelo_sistemico,
                                                 modelo_regulatorio,
                                               #  modelo_temporal, # vemos el efecto sobredeterminado de prop_elec_usa-....
                                                # modelo_sficativas,
                                                 modelo_sficativas_variantes,
                                                 modelo_sficativas_variantes_s_outliers,
                                                 exp=T, 
                                                 scale = T,
                                                 inner_ci_level = .9,
                                                 # coefs = c("nombre coef" =  "variable",           ....),
                                                 model.names = c("modelo_contingencia",
                                                                 "modelo_sistemico",
                                                                 "modelo_regulatorio",
                                                               #  "modelo_temporal",
                                                                 #"modelo_sficativas",
                                                                 "modelo_sficativas_variantes",
                                                                 "modelo_sficativas_variantes_s_outliers"
                                                 ))

#### INTERPRETACION - ESCENARIOS CON VALORES PREDICHOS #####
### calculo de probas predichas # tengo que reducir la data para poder calcular asi nomas
#data_modelo_a_interpretar$probabilidades_predichas <- predict(modelo_a_interpretar, type = "response")
#data_modelo_a_interpretar$predicciones_binarias <- ifelse(data_modelo_a_interpretar$probabilidades_predichas>0.5,1,0)

# Crear un dataset base sobre el cual predecir 

# version valores relevantes
valores_relevantes <- c(log(min(data_modelo_a_interpretar$nec)),
                        log(max(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec) + 2*sd(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec) - 2*sd(data_modelo_a_interpretar$nec)))

data_to_predict1 <- data.frame(
  lnnec = rep(valores_relevantes, 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = 0, # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = mean(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=length(valores_relevantes)) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)

# Predecir probabilidades
data_to_predict1$predicted_probs <- predict(modelo_a_interpretar, 
                                           newdata = data_to_predict1, 
                                           type = "response")


# tabla

referencias <- tibble( 
  nec = c( round(min(data_modelo_a_interpretar$nec), 2),
           round(max(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) + 2*sd(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) - 2*sd(data_modelo_a_interpretar$nec), 2)),
  referencia = c("Valor mínimo observado",
                 "Valor máximo observado",
                 "Promedio observado",
                 "Promedio + un desvío estandar",
                 "Promedio + dos desvíos estandar",
                 "Promedio - un desvío estandar",
                 "Promedio - dos desvío estandar"))

tabla_data_to_predict <- data_to_predict1 %>% 
  mutate(nec = exp(lnnec)) %>% 
  select(nec, regulaciondico, predicted_probs) %>% 
  pivot_wider(names_from = regulaciondico,
              values_from = predicted_probs) %>% 
  mutate(across(everything(),  ~  round(., 2))) %>% 
  left_join(referencias) %>% 
  arrange(nec) %>% 
  dplyr::rename("regulacion = 0" = "0",
                "regulacion = 1" = "1")

tabla_data_to_predict %>% write.csv("anexos/tabla_data_to_predict.csv")


# grafico
# comento porque preferimos el próximo gráfico
# ggplot(data_to_predict) +
#   geom_line(aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico)))

# version con mas valores para graficar 
data_to_predict2 <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                    max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                    #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                    length.out = 20), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=20) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)

                
# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 

predicted_probs <- margins::prediction(model = modelo_a_interpretar,
                              data = data_to_predict2,
                              type = "response",
                              vcov = vcov_modelo_a_interpretar,
                              calculate_se = TRUE)

# grafico

plot_interpretacion <- ggplot(predicted_probs) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted, 
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted, 
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
  scale_fill_discrete(labels = c("No hay", "Hay"), breaks = c(0,1)) +
  scale_colour_discrete(labels = c("No hay", "Hay"), breaks = c(0,1)) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
  geom_vline(xintercept = c(2, 3), alpha = 0.5, linetype = 2)
 
plot_interpretacion %>% ggsave(filename = "images/plot_interpretacion_nec_regulacion.jpg",
                               width = 12,
                               height = 9)

# nec es especialmente relevante cuando no hay regulacion

# algunas cuentas para reportar verbalmente
data_to_predict3 <- data.frame(
  lnnec = rep(c(log(2), log(3)), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=2) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)


# Predecir probabilidades
data_to_predict3 <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict3,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)

data_to_predict3 %>% 
  group_by(regulaciondico) %>% 
  summarise(diff_lnnec = diff(fitted))

data_to_predict3 %>% 
  group_by(lnnec) %>% 
  summarise(diff_regulacion = diff(fitted))
 
# PENDIENTE algun calculo variando dico_reeleccion


# version con mas valores para graficar 
data_to_predict4 <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                  max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                  #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                  length.out = 20), 4), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = rep(rep(c(0,1),each=20), times=2), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=40) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
) 


# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 

data_to_predict4 <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict4,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)

data_to_predict4 <- data_to_predict4 %>% 
  mutate(escenario = ifelse(regulaciondico==1&dico_reeleccion==1,
                            "Reelección con regulación",
                            ifelse(regulaciondico==1&dico_reeleccion==0,
                                   "No reelección con regulación",
                                   ifelse(regulaciondico==0&dico_reeleccion==1,
                                          "Reelección sin regulación",
                                          ifelse(regulaciondico==0&dico_reeleccion==0,
                                                 "No reelección sin regulación",NA)))))
# grafico

plot_interpretacion4 <- ggplot(data_to_predict4) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted))+#, 
                #colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted), alpha = 0.3)+#, 
                  #fill = as.factor(regulaciondico)), alpha = 0.3) +
  facet_wrap(~as.factor(escenario)) +
  theme_classic() +
  labs(
    title = "Probabilidad predicha de ocurrencia de un debate presidencial",
    subtitle = "Para distintos valores dentro del rango observado de NEC, y distintos escenarios",
    caption = "Elaboración propia. 
    Intervalos de confianza ploteados al 90%.
    Escenarios predichos cuando el resto de las variables se encuentra:
    -en su media (para el caso de los indicadores continuos),
    -en su moda (para el caso de los indicadores dicotómicos)."#,
    #fill = "Regulación sobre debates",
    #colour = "Regulación sobre debates"
  ) +
  xlab("Número Efectivo de Candidatos") +
  ylab("Probabilidad predicha de que ocurra un debate") +
  scale_x_continuous(breaks= seq(1, 10, 0.5)) +
  #scale_fill_discrete(labels = c("No hay", "Hay"), breaks = c(0,1)) +
  #scale_colour_discrete(labels = c("No hay", "Hay"), breaks = c(0,1)) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) #+
  #geom_vline(xintercept = c(2, 3), alpha = 0.5, linetype = 2)

plot_interpretacion4 %>% ggsave(filename = "images/plot_interpretacion_varios_escenarios.jpg",
                               width = 12,
                               height = 9)


plot_interpretacion4.1 <- ggplot(data_to_predict4) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted,
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted,
                  fill = as.factor(regulaciondico)), alpha = 0.3) +
  facet_wrap(~as.factor(dico_reeleccion)) 

plot_interpretacion4.2 <- ggplot(data_to_predict4) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted,
                colour = as.factor(dico_reeleccion))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted,
                  fill = as.factor(dico_reeleccion)), alpha = 0.3) +
  facet_wrap(~as.factor(regulaciondico))

plot_interpretacion4.3 <- ggplot(data_to_predict4) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted,
                colour = as.factor(escenario))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted,
                  fill = as.factor(escenario)), alpha = 0.3)  


#### INTERPRETACION - EFECTO MARGINAL PROMEDIO #####

# chatgpt: 
# En el contexto de un modelo logit, el efecto marginal se refiere a 
# cómo cambia la probabilidad predicha de que ocurra el evento de interés (la variable dependiente sea igual a 1) 
# cuando se modifica una variable independiente, 
# manteniendo constantes las demás variables del modelo. 
# En otras palabras, es la tasa de cambio en la probabilidad asociada con un cambio unitario en una variable independiente.
#  el efecto marginal varía según los valores de 𝑋 X y, en consecuencia, no es constante.

# formula es: 
# cambio en P dado cambio en X = beta * p(1-p), donde sabemos que proba predicha varia para distintos valores de Xs

# interpretacion: 
# Si el efecto marginal de X en cierto valore es 0.1, 
# significa que un aumento unitario en ese valor incrementa la probabilidad predicha en un 10%, 
# manteniendo constantes las demás variables.

###### Efecto marginal promedio (Average Marginal Effect, AME): # 

# Se calcula el efecto marginal para cada observación en el conjunto de datos y luego se promedian estos efectos.
# Es útil para interpretar un "efecto típico" en la muestra.

#¿Cuál es el efecto medio del aumento de una unidad de la variable independiente en la probabilidad de que se produzca la variable dependiente? El Efecto Marginal Promedio (AME, por sus siglas en inglés) se utiliza para ver estos efectos, y se logra con el comando plot del paquete de margins.
#  el efecto marginal es la variación de la variable explicada cuando la variable explicativa aumenta en una unidad.

# efecto marginal promedio
#modelo_a_interpretar <- modelo_sficativas_variantes

marginal_effects <- margins::margins(modelo_a_interpretar,
                                     vcov = vcov_modelo_a_interpretar)#,
#level = 0.90)

marginals_df <- summary(marginal_effects)

# marginals_df <- margins::margins_summary(modelo_a_interpretar, 
#                                          vcov = vcov_modelo_a_interpretar,
#                                          level = 0.90, by_factor = TRUE)

marginals_df$AME["lnnec"] * 100
marginals_df$AME["regulaciondico"] * 100
marginals_df$AME["dico_reeleccion"] * 100

(marginals_df$AME["lnnec"] - 1.64485*marginals_df$SE["Var_dydx_lnnec"]) * 100
(marginals_df$AME["lnnec"] + 1.64485*marginals_df$SE["Var_dydx_lnnec"]) * 100

(marginals_df$AME["regulaciondico"] - 1.64485*marginals_df$SE["Var_dydx_regulaciondico"]) * 100
(marginals_df$AME["regulaciondico"] + 1.64485*marginals_df$SE["Var_dydx_regulaciondico"]) * 100

(marginals_df$AME["dico_reeleccion"] - 1.64485*marginals_df$SE["Var_dydx_dico_reeleccion"]) * 100
(marginals_df$AME["dico_reeleccion"] + 1.64485*marginals_df$SE["Var_dydx_dico_reeleccion"]) * 100


plot_margins <- ggplot(marginals_df) +
  geom_point( aes(x = factor, y = AME)) +
  geom_errorbar(aes(x = factor, ymin = AME-1.64485*SE, ymax = AME+1.64485*SE), width = 0.2) +
  geom_hline(aes(yintercept = 0), colour = "gray50", linetype = 2) +
  # geom_hline(aes(yintercept = 0.25), colour = "red2", linetype = 2, alpha = 0.1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-1,1,0.25)) +
  labs(title = "Efectos marginales promedio",
       subtitle = "Modelo final",
       x = "Predictores", 
       y = "Efectos marginales promedio",
       caption = "Elaboración propia sobre la base de modelo final, estimado con regresión logística con errores estándar agrupados por país.
       Intervalos de confianza estimados al 90%") +
  theme_classic() 

plot_margins %>% ggsave(filename = "images/plot_margins.jpg", width = 8, height = 8)

#margins::marginal_effects(margins::margins(modelo_a_interpretar))

# seccion de efecto marginal por variable en archivo viejo modelos_dic2024.R
# en la practica entiendo que equivale a escenarios por variable


#### OTROS INTERPRETACION - CUMSUM 0 OTROS ESCENARIOS ######

# Generar combinaciones de varias variables



data_escenarios <- data_modelo_a_interpretar %>% 
  select(c(lnnec,
           lnmarginvic,
           voteshareincumbent,
           dico_reeleccion ,
           propindivinternet ,
           accesogratuito ,
           avgpropdebatesregionxciclo ,
           regulaciondico,
           #cumsum_pastciclos,
           lngdp,
           democraciavdemelectoralcomp,
           mediaqualitycorruptvdem ))

# version con mas valores para graficar de manera exploratoria. LO COMENTO 
#i <- "lnnec"
# for (i in colnames(data_escenarios)){
# 
#   data_to_predict <- data.frame(
#     cumsum_pastciclos = rep(0, each = nrow(data_escenarios)), 
#     
#   lnnec =mean(data_escenarios$lnnec, na.rm = TRUE),
#   lnmarginvic =  mean(data_escenarios$lnmarginvic, na.rm = TRUE),
#   voteshareincumbent = mean(data_escenarios$voteshareincumbent, na.rm = TRUE),
#   dico_reeleccion = median(data_escenarios$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
#   propindivinternet = mean(data_escenarios$propindivinternet, na.rm = TRUE),
#   accesogratuito = median(data_escenarios$accesogratuito, na.rm = TRUE),
#   avgpropdebatesregionxciclo = mean(data_escenarios$avgpropdebatesregionxciclo, na.rm = TRUE),
#   regulaciondico = median(data_escenarios$regulaciondico, na.rm = TRUE),
#   lngdp = mean(data_escenarios$lngdp, na.rm = TRUE),
#   democraciavdemelectoralcomp = mean(data_escenarios$democraciavdemelectoralcomp, na.rm = TRUE),
#   mediaqualitycorruptvdem = mean(data_escenarios$mediaqualitycorruptvdem, na.rm = TRUE)
#  )
# 
#   data_to_predict[,i] <- seq(min(data_escenarios[,i], na.rm = TRUE), 
#                              max(data_escenarios[,i], na.rm = TRUE),
#                              length.out = nrow(data_escenarios))
#   
#   predicted_probs <- margins::prediction(model = modelo_a_interpretar,
#                                          data = data_to_predict,
#                                          type = "response",
#                                          vcov = vcov_modelo_a_interpretar,
#                                          calculate_se = TRUE)
#   
#   
#   plot_interpretacion <- ggplot(predicted_probs) +
#     geom_line(aes(x = predicted_probs[,i], 
#                   y = fitted)) +
#     geom_ribbon(aes(x = predicted_probs[,i],
#                     ymin =  fitted - 1.645*se.fitted, 
#                     ymax =  fitted + 1.645*se.fitted), alpha = 0.3) +
#     theme_classic()  +
#     geom_hline(aes(yintercept = 0.5), linetype = 3, colour = "red3") +
#     labs(title = "Probabilidad de ocurrencia de un debate",
#          subtitle = paste("Cuando nunca hubo debates, para distintos valores de:" ,i ),
#          x = i,
#          y = "Probabilidad predicha",
#          caption = "Elaboración propia con base en los resultados del modelo final preferido.
#          La cantidad de debates antecedentes está fijada a cero.
#          Es decir, el gráfico representa la probabilidad de ocurrencia de un debate para distintos valores del predictor,
#          cuando nunca ocurrieron debates en el papis.
#          El resto de las variables cuantitativas están fijadas en sus medias, las dicotómicas en sus modas.")
#   
#   plot_interpretacion %>% ggsave(filename = paste("images/test_cumsum0_", i, ".jpg", sep = ""))
#   
# } 

# VARIANDO CUMSUM 

data_to_predict_cumsum <- data.frame(
  cumsum_pastciclos = seq(min(data_modelo_a_interpretar$cumsum_pastciclos, na.rm=T),
                          max(data_modelo_a_interpretar$cumsum_pastciclos, na.rm=T),
                          1),
  lnnec = mean(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
  lnmarginvic =  mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = median(data_modelo_a_interpretar$regulaciondico, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)

 

predicted_probs <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict_cumsum,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)


plot_cumsum <- ggplot(predicted_probs) +
  geom_line(aes(x = cumsum_pastciclos, 
                y = fitted)) +
  geom_ribbon(aes(x = cumsum_pastciclos,
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted), alpha = 0.3) +
  theme_classic()  +
  scale_x_continuous(breaks = seq(0,10,1)) +
  geom_hline(aes(yintercept = 0.5), linetype = 3, colour = "red3") +
  labs(title = "Probabilidad de ocurrencia de un debate",
       subtitle = "según la cantidad de elecciones presidenciales antecedentes en las que hubo debates",
       x = "Cantidad de elecciones presidenciales pasadas con debates",
       y = "Probabilidad predicha",
       caption = "Elaboración propia con base en los resultados del modelo final preferido.
         El resto de las variables cuantitativas están fijadas en sus medias, las dicotómicas en sus modas.")

plot_cumsum %>% ggsave(filename = "images/plot_cumsum.jpg")

table(data_modelo_a_interpretar$cumsum_pastciclos )

tablita <- table(data_modelo_a_interpretar$dico_hubo_debates, 
                 data_modelo_a_interpretar$cumsum_pastciclos) %>% data.frame() %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  dplyr::rename("Hubo debates" = Var1)

tablita %>% write_csv("anexos/tabla_freq_debates_antecedentes.csv")

# GRAFICOS DE EFECTOS CUANDO CUMSUM 0 MAS PROLIJOS

# REGULACION
data_to_predict_cumsum0_regulacion <- data.frame(
  cumsum_pastciclos = rep(0, each = length(unique(data_escenarios$regulaciondico))), 
  
  lnnec = mean(data_escenarios$lnnec, na.rm = TRUE),
  lnmarginvic =  mean(data_escenarios$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_escenarios$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_escenarios$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_escenarios$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_escenarios$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_escenarios$avgpropdebatesregionxciclo, na.rm = TRUE),
  
  regulaciondico = unique(data_escenarios$regulaciondico),
  
  lngdp = mean(data_escenarios$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_escenarios$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_escenarios$mediaqualitycorruptvdem, na.rm = TRUE)
)

 

data_to_predict_cumsum0_regulacion <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict_cumsum0_regulacion,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)


plot_data_to_predict_cumsum0_regulacion <- ggplot(data_to_predict_cumsum0_regulacion) +
  geom_point(aes(x = as.factor(regulaciondico), 
               y = fitted) ) +  
  geom_errorbar(aes(x = as.factor(regulaciondico), 
                    ymin = fitted - 1.645 * se.fitted, 
                    ymax = fitted + 1.645 * se.fitted), 
                width = 0.2, color = "black") +
  theme_classic() +
  geom_hline(aes(yintercept = 0.5), linetype = 3, colour = "red3") +
  labs(title = "Probabilidad de ocurrencia de un debate",
       subtitle = "dada la existncia o no de regulaciones sobre la práctica",
       x = "Regulación",
       y = "Probabilidad predicha",
       caption = "Elaboración propia con base en los resultados del modelo final preferido. 
         La cantidad de debates antecedentes está fijada a cero.
         Es decir, el gráfico representa la probabilidad de ocurrencia de un debate para distintos valores del predictor,
         cuando nunca ocurrieron debates en el país.
         El resto de las variables cuantitativas están fijadas en sus medias, las dicotómicas en sus modas.")

plot_data_to_predict_cumsum0_regulacion %>% ggsave(filename = "images/plot_cumsum0_regulacion.jpg")

# VARIANDO NEC 

data_to_predict_cumsum0_nec <- data.frame(
  cumsum_pastciclos = rep(0, each = nrow(data_escenarios)), 
  
  lnnec = seq(min(data_escenarios$lnnec, na.rm = TRUE), 
              max(data_escenarios$lnnec, na.rm = TRUE),
              length.out = nrow(data_escenarios)),
  lnmarginvic =  mean(data_escenarios$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_escenarios$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_escenarios$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_escenarios$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_escenarios$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_escenarios$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = median(data_escenarios$regulaciondico, na.rm = TRUE),
  lngdp = mean(data_escenarios$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_escenarios$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_escenarios$mediaqualitycorruptvdem, na.rm = TRUE)
)
 

data_to_predict_cumsum0_nec <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict_cumsum0_nec,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)


plot_data_to_predict_cumsum0_nec <- ggplot(data_to_predict_cumsum0_nec) +
  geom_line(aes(x = exp(lnnec), 
                y = fitted)) +
  geom_ribbon(aes(x = exp(lnnec),
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted), alpha = 0.3) +
  theme_classic()  +
  geom_hline(aes(yintercept = 0.5), linetype = 3, colour = "red3") +
  labs(title = "Probabilidad de ocurrencia de un debate",
       subtitle = "Cuando nunca hubo debates, para distintos valores de NEC",
       x = "NEC",
       y = "Probabilidad predicha",
       caption = "Elaboración propia con base en los resultados del modelo final preferido.
         La cantidad de debates antecedentes está fijada a cero.
         Es decir, el gráfico representa la probabilidad de ocurrencia de un debate para distintos valores del predictor,
         cuando nunca ocurrieron debates en el país.
         El resto de las variables cuantitativas están fijadas en sus medias, las dicotómicas en sus modas.")

plot_data_to_predict_cumsum0_nec %>% ggsave(filename = "images/plot_cumsum0_nec.jpg")


# VARIANDO PROPINDIVINTERNET

data_to_predict_cumsum0_internet <- data.frame(
  cumsum_pastciclos = rep(0, each = nrow(data_escenarios)), 
  
  lnnec =mean(data_escenarios$lnnec, na.rm = TRUE),
  lnmarginvic =  mean(data_escenarios$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_escenarios$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_escenarios$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = seq(min(data_escenarios$propindivinternet, na.rm = TRUE), 
                          max(data_escenarios$propindivinternet, na.rm = TRUE),
                          length.out = nrow(data_escenarios)),
  accesogratuito = median(data_escenarios$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_escenarios$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = median(data_escenarios$regulaciondico, na.rm = TRUE),
  lngdp = mean(data_escenarios$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_escenarios$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_escenarios$mediaqualitycorruptvdem, na.rm = TRUE)
)

 
data_to_predict_cumsum0_internet <- margins::prediction(model = modelo_a_interpretar,
                                       data = data_to_predict_cumsum0_internet,
                                       type = "response",
                                       vcov = vcov_modelo_a_interpretar,
                                       calculate_se = TRUE)


plot_data_to_predict_cumsum0_internet <- ggplot(data_to_predict_cumsum0_internet) +
  geom_line(aes(x = propindivinternet, 
                y = fitted)) +
  geom_ribbon(aes(x = propindivinternet,
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted), alpha = 0.3) +
  theme_classic()  +
  geom_hline(aes(yintercept = 0.5), linetype = 3, colour = "red3") +
  labs(title = "Probabilidad de ocurrencia de un debate",
       subtitle = "Cuando nunca hubo debates, según la cantidad de individuos c/ acceso a internet",
       x =  "% de individuos con acceso a internet",
       y = "Probabilidad predicha",
       caption = "Elaboración propia con base en los resultados del modelo final preferido.
         La cantidad de debates antecedentes está fijada a cero.
         Es decir, el gráfico representa la probabilidad de ocurrencia de un debate para distintos valores del predictor,
         cuando nunca ocurrieron debates en el país.
         El resto de las variables cuantitativas están fijadas en sus medias, las dicotómicas en sus modas.")

plot_data_to_predict_cumsum0_internet %>% ggsave(filename = "images/plot_cumsum0_internet.jpg")

#### OTROS INTERPRETACION MULTILEVEL ######
paisesdf <-  democracias %>% 
  subset(cat_pais!="Venezuela") %>% 
  select(cat_pais) 

paises <- paisesdf$cat_pais %>% 
  unique()

len <- length(paises)

data_to_predict_multinivel <- data.frame(
  cat_pais = rep(paises, 40),
  lnnec = rep(rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                  max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                  #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                  length.out = 20), 2), len), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(rep(c(0,1),each=20), len) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)


# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 

# predicted_probs_multinivel <- predict(final_random_intercepts, 
#                                       newdata = data_to_predict_multinivel, 
#                                       type = "response",
#                                       se.fit = T)  
# 
# data_to_predict_multinivel$fitted <- predicted_probs_multinivel$fit
# 
# data_to_predict_multinivel$se.fitted <- predicted_probs_multinivel$se.fit

data_to_predict_multinivel <- margins::prediction(final_random_intercepts,
                                      data = data_to_predict_multinivel)

# grafico # ESTO PARECE HABER FUNCIONADO AUNQUE ESTA LEJOS DE ESTAR GENIAL

lattice::dotplot( lme4::ranef(final_random_intercepts))

ranint <- lme4::ranef(final_random_intercepts) %>% as_tibble()

# Calcular intervalos de confianza
ranint <- ranint %>%
  mutate(lower_ci = condval - 1.96 * condsd,
         upper_ci = condval + 1.96 * condsd)

# Graficar
plot_ranint <- ggplot(ranint, aes(x = condval, y = reorder(grp, condval))) +
  geom_point() +  # Puntos para los interceptos aleatorios
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Intervalos de confianza
  theme_minimal() +
  labs(x = "Interceptos Aleatorios", y = "Países") +
  theme_classic() + 
  theme(axis.text.y = element_text(size = 8)) +
  geom_vline(aes(xintercept = 0), linetype = 3, colour = "blue4") +
  labs(title = "Interceptos aleatorios estimados en el modelo final multinivel",
       subtitle = "Por país, junto a su desvío estándar condicional")

plot_ranint %>% ggsave(filename = "images/random_intercepts.jpg", width = 10)

# ME QUEDO CON EL DE ARRIBA PERO ESTÁ ESTA ALTERNATIVA
# Crear etiquetas al final de cada curva
# labels <- data_to_predict_multinivel %>%
#   group_by(cat_pais) %>%
#   filter(exp(lnnec) == min(exp(lnnec))) %>% # Puntos al final de las curvas
#   mutate(label_position = fitted) # Asegurar que la etiqueta esté bien colocada
# 
# # Crear el gráfico
# plot_interpretacion <- ggplot(data_to_predict_multinivel) +
#   geom_line(aes(x = exp(lnnec),
#                 y = fitted, 
#                 color = as.factor(cat_pais))) +
#   geom_text(data = labels, 
#             aes(label = cat_pais, 
#                 x = exp(lnnec), 
#                 y = label_position),
#             size = 3, hjust = 0, vjust = 0.5, show.legend = FALSE) +
#   theme_minimal() +
#   facet_wrap( ~ regulaciondico) +
#   labs(x = "exp(lnnec)", y = "Fitted", color = "Países") +
#   theme(legend.position = "none") # Opcional: esconder la leyenda
 

#### OTROS INTEPRETACION INTERACCIONES #########
#PLOT DE INTERACCIONES SEGUN WEB 

persp(modelo_sficativas_variantes_interactivo, 
      "regulaciondico", "lnnec", 
      what = "prediction",
      #ylab = "Desigualdad", xlab = "Crecimiento económico",
      zlab = "Probabilidad predicha" )

image(modelo_sficativas_variantes_interactivo,  
      "regulaciondico", "lnnec")#, 
#xlab = "Crecimiento económico", ylab = "Desigualdad",
#zlab = "Probabilidad predicha")

#summary(modelo_sficativas_variantes_interactivo)

#  TEST DRIVE DE interaccion 


# version con mas valores para graficar 
data_to_predict_testdrive_interactivo <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                  max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                  #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                  length.out = 20), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=20) ,
  cumsum_pastciclos = 0,
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)


# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 
  

predicted_probs <- margins::prediction(model = modelo_sficativas_variantes_interactivo,
                                       data = data_to_predict_testdrive_interactivo,
                                       type = "response",
                                       vcov = vcovCL(modelo_sficativas_variantes_interactivo, 
                                                     cluster = democracias$cat_pais),
                                       calculate_se = TRUE)

# grafico


plot_interpretacion <- ggplot(predicted_probs) +
  geom_line(aes(x = exp(lnnec),
                y = fitted, 
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  fitted - 1.645*se.fitted, 
                  ymax =  fitted + 1.645*se.fitted, 
                  fill = as.factor(regulaciondico)), alpha = 0.3) +
  theme_classic() 

# NO ME GUSTA NADA



# ############################## ############################## ############################## ##############################
# ############################## ############################## ############################## ##############################
# CANDIDATOS ##############################
## PREPARO DATA #######
data_candidatos <- base_candidatos  %>% 
  select(-starts_with("source_")) %>% 
  select(-X,
         -nombres_candidatos,
         -v2pashname) %>% 
  left_join(base_indicadores) %>% 
  left_join(base_controles)

fulldata_candidatos <- base_candidatos  %>% 
  select(-starts_with("source_")) %>% 
  select(-X,
         -nombres_candidatos,
         -v2pashname) %>% 
  left_join(base_indicadores %>% 
              select(-dico_reeleccion, -dico_oficialista)) %>% 
  left_join(base_controles %>% 
              select(-dico_debates_eleccion, -eng_cat_pais, -X))

# creo variable para clusterizar SSEE
data_candidatos <- data_candidatos %>% # en este caso tb tengo id_debate
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

fulldata_candidatos <- fulldata_candidatos %>% # en este caso tb tengo id_debate
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

# creo data estandarizada
# data_scaled <- data %>%
#   mutate(across(-c(dico_candidato_presente,
#                    cat_pais,
#                    elecid,
#                    id_debate,
#                    eng_cat_pais), scale))

## Pruebo modelos ####

#### modelos parciales ###########
modelo_nivelindiv <- glm(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                           #dico_oficialista + 
                           dico_oficialistanoreeleccion +
                            ninvitaciones + 
                            propausenciaspasadasfilled,
                          family = binomial(link = "logit"),
                          data = data_candidatos)
options(scipen=999)
#summary(modelo_nivelindiv)
robust_se_cluster_modelo_indiv <- coeftest(modelo_nivelindiv, 
                                           vcov = vcovCL(modelo_nivelindiv,
                                                         cluster = data_candidatos$elecid))
print(robust_se_cluster_modelo_indiv)

modelo_nivelindiv_controles <- glm(dico_candidato_presente ~ 
                           voteshare + 
                           v2pariglef_vdem + 
                           v2paactcom_vdem + 
                           dico_reeleccion + 
                           #dico_oficialista + 
                             dico_oficialistanoreeleccion +
                           ninvitaciones + 
                           propausenciaspasadasfilled +
                             log(nec) +
                             mediaqualitycorruptvdem +
                             log(gdpxcapita) +
                             democraciavdemelectoralcomp +
                             regulaciondico +
                             cumsum_pastciclos,
                         family = binomial(link = "logit"),
                         data = fulldata_candidatos)
options(scipen=999)
#summary(modelo_nivelindiv_controles)

robust_se_cluster_modelo_nivelindiv_controles <- coeftest(modelo_nivelindiv_controles, 
                                               vcov = vcovCL(modelo_nivelindiv_controles,
                                                             cluster = fulldata_candidatos$elecid))
print(robust_se_cluster_modelo_nivelindiv_controles)

modelo_niveldebate <- glm(dico_candidato_presente ~ 
                            orgosc + 
                            orgmmc + 
                            orgestado,
                  family = binomial(link = "logit"),
                  data = data_candidatos)
options(scipen=999)
#summary(modelo_niveldebate)

robust_se_cluster_modelo_debate <- coeftest(modelo_niveldebate, 
                                           vcov = vcovCL(modelo_niveldebate,
                                                         cluster = data_candidatos$elecid))
print(robust_se_cluster_modelo_debate)

modelo_niveldebate_controles <- glm(dico_candidato_presente ~ 
                                      orgosc + 
                                      orgmmc + 
                                      orgestado +
                                    log(nec) +
                                     mediaqualitycorruptvdem +
                                      log(gdpxcapita) +
                                     democraciavdemelectoralcomp +
                                     regulaciondico +
                                     cumsum_pastciclos,
                                   family = binomial(link = "logit"),
                                   data = fulldata_candidatos)
options(scipen=999)
#summary(modelo_niveldebate_controles)

robust_se_cluster_modelo_niveldebate_controles <- coeftest(modelo_niveldebate_controles, 
                                        vcov = vcovCL(modelo_niveldebate_controles,
                                                      cluster = fulldata_candidatos$elecid))
print(robust_se_cluster_modelo_niveldebate_controles)



#### modelos con controles nivel eleccion #######

modelo_agregado <- glm(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                         #dico_oficialista + 
                         dico_oficialistanoreeleccion +                            ninvitaciones + 
                            propausenciaspasadasfilled + 
                            orgosc + 
                            orgmmc + 
                            orgestado,
                          family = binomial(link = "logit"),
                          data = data_candidatos)
options(scipen=999)
#summary(modelo_agregado)

robust_se_cluster_modelo_agregado <- coeftest(modelo_agregado, 
                                            vcov = vcovCL(modelo_agregado,
                                                          cluster = data_candidatos$elecid))
print(robust_se_cluster_modelo_agregado)

modelo_agregado_controles <- glm(dico_candidato_presente ~ 
                         voteshare + 
                         v2pariglef_vdem + 
                         v2paactcom_vdem + 
                         dico_reeleccion + 
                           #dico_oficialista + 
                           dico_oficialistanoreeleccion + 
                         ninvitaciones + 
                         propausenciaspasadasfilled + 
                         orgosc + 
                         orgmmc + 
                         orgestado +
                           log(nec) +
                           mediaqualitycorruptvdem +
                           log(gdpxcapita) +
                           democraciavdemelectoralcomp +
                           regulaciondico +
                           cumsum_pastciclos,
                       family = binomial(link = "logit"),
                       data = fulldata_candidatos)
options(scipen=999)
summary(modelo_agregado_controles)

robust_se_cluster_modelo_agregado_controles <- coeftest(modelo_agregado_controles, 
                                              vcov = vcovCL(modelo_agregado_controles,
                                                            cluster = fulldata_candidatos$ca_pais))
print(robust_se_cluster_modelo_agregado_controles)


modelo_agregado_controles_sNAs <- glm(dico_candidato_presente ~ 
                                   voteshare + 
                                   #v2pariglef_vdem + 
                                   #v2paactcom_vdem + 
                                   dico_reeleccion + 
                                     #dico_oficialista + 
                                     dico_oficialistanoreeleccion + 
                                   ninvitaciones + 
                                   propausenciaspasadasfilled + 
                                   orgosc + 
                                   orgmmc + 
                                   orgestado +
                                   log(nec) +
                                   mediaqualitycorruptvdem +
                                   log(gdpxcapita) +
                                   democraciavdemelectoralcomp +
                                   regulaciondico +
                                   cumsum_pastciclos,
                                 family = binomial(link = "logit"),
                                 data = fulldata_candidatos)
options(scipen=999)
summary(modelo_agregado_controles_sNAs)

robust_se_cluster_modelo_agregado_controles_sNAs <- coeftest(modelo_agregado_controles_sNAs, 
                                                        vcov = vcovCL(modelo_agregado_controles_sNAs,
                                                                      cluster = fulldata_candidatos$cat_pais))
print(robust_se_cluster_modelo_agregado_controles_sNAs)

#### multinivel ###########
modelo_multinivel1 <- lme4::glmer(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                              #dico_oficialista + 
                              dico_oficialistanoreeleccion +
                            ninvitaciones + 
                            propausenciaspasadasfilled + 
                            orgosc + 
                            orgmmc + 
                            orgestado + 
                            (1|elecid) ,
                          family = binomial(link = "logit"),
                          data = data_candidatos)
options(scipen=999)
#summary(modelo_multinivel1)

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

modelo_multinivel1_controles <- lme4::glmer(dico_candidato_presente ~ 
                                    voteshare + 
                                    v2pariglef_vdem + 
                                    v2paactcom_vdem + 
                                    dico_reeleccion + 
                                      #dico_oficialista + 
                                      dico_oficialistanoreeleccion +
                                    ninvitaciones + 
                                    propausenciaspasadasfilled + 
                                    orgosc + 
                                    orgmmc + 
                                    orgestado + 
                                      log(nec) +
                                      mediaqualitycorruptvdem +
                                      log(gdpxcapita) +
                                      democraciavdemelectoralcomp +
                                      regulaciondico +
                                      cumsum_pastciclos +
                                    (1|cat_pais), #+ (1|elecid), #PROBLEMAS DE CONVERGENCIA
                                  family = binomial(link = "logit"),
                                  data = fulldata_candidatos,
                                  control = control)
options(scipen=999)
summary(modelo_multinivel1_controles)



modelo_multinivel1_controles_sNAs <- lme4::glmer(dico_candidato_presente ~ 
                                              voteshare + 
                                              #v2pariglef_vdem + 
                                              #v2paactcom_vdem + 
                                              dico_reeleccion + 
                                                #dico_oficialista + 
                                                dico_oficialistanoreeleccion +
                                              ninvitaciones + 
                                              propausenciaspasadasfilled + 
                                              orgosc + 
                                              orgmmc + 
                                              orgestado + 
                                              log(nec) +
                                              mediaqualitycorruptvdem +
                                              log(gdpxcapita) +
                                              democraciavdemelectoralcomp +
                                              regulaciondico +
                                              cumsum_pastciclos +
                                              (1|cat_pais), #+ (1|elecid), #PROBLEMAS DE CONVERGENCIA
                                            family = binomial(link = "logit"),
                                            data = fulldata_candidatos,
                                            control = control)
options(scipen=999)
summary(modelo_multinivel1_controles_sNAs)


## mini control vifs ######


car::vif(modelo_agregado_controles)

#car::vif(modelo_multinivel1_controles)


control_s_cumsum <-  glm(dico_candidato_presente ~ 
                                                       voteshare + 
                                                       v2pariglef_vdem + 
                                                       v2paactcom_vdem + 
                                                       dico_reeleccion + 
                                                       #dico_oficialista + 
                                                       dico_oficialistanoreeleccion + 
                                                       ninvitaciones + 
                                                       propausenciaspasadasfilled + 
                                                       orgosc + 
                                                       orgmmc + 
                                                       orgestado +
                                                       log(nec) +
                                                       mediaqualitycorruptvdem +
                                                       log(gdpxcapita) +
                                                       democraciavdemelectoralcomp +
                                                       regulaciondico , #+
                                                       #cumsum_pastciclos,
                                                     family = binomial(link = "logit"),
                                                     data = fulldata_candidatos)
options(scipen=999)
summary(control_s_cumsum)

robust_se_control_s_cumsum <- coeftest(control_s_cumsum, 
                                       vcov = vcovCL(control_s_cumsum,
                                                     cluster = fulldata_candidatos$cat_pais))
print(robust_se_control_s_cumsum)
  
  
## Exporto modelos de interes ####

lista2 <-  list(
  #robust_se_cluster_modelo_nivelindiv_controles,
  #robust_se_cluster_modelo_niveldebate_controles,
  robust_se_cluster_modelo_agregado_controles,
  modelo_multinivel1_controles,
  robust_se_cluster_modelo_agregado_controles_sNAs,
  modelo_multinivel1_controles_sNAs) 

texreg::htmlreg(lista2,
                custom.model.names = c(#"Niv. individual",
                                       #"Niv. debate",
                                       "Agregado",
                                       "Agregado multinivel",
                                       "Agregado reducido",
                                       "Agregado reducido multinivel")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                # custom.coef.names = c("(Intercepto)",
                #                       "log Margen de victoria",
                #                       "log NEC",
                #                       "Votos oficialista",
                #                       "Incumbente reelije",
                #                       "Prop. individuos c internet",
                #                       "Acceso gratuito",
                #                       "Prop. debates en Region",
                #                       "Regulacion sobre debates",
                #                       "Cant. elecciones pasadas con debates",
                #                       "log PBI per Capita",
                #                       "Democracia electoral (VDEM)",
                #                       "Corrupcion de medios (VDEM)",
                #                       "Margen de victoria",
                #                       "NEC" ,
                #                       "Prop. debates en USA" ,
                #                       "PBI per Capita"
                # ),
                # reorder.coef =  c(1,
                #                   1+1,
                #                   13+1,
                #                   2+1,
                #                   14+1,
                #                   3+1,
                #                   4+1,
                #                   
                #                   5+1,
                #                   6+1,
                #                   7+1,
                #                   15+1,
                #                   8+1,
                #                   9+1,
                #                   10+1,
                #                   16+1,
                #                   11+1,
                #                   12+1
                # ),
                file="anexos/tabla_modelos_candidatos.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)
