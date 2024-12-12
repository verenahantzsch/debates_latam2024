

# librerias ########### 

library(tidyverse)
library(sandwich) # para clusterizar errores estandares
library(lmtest) # para clusterizar errores estandares

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

base_vdependiente <- read.csv("variables_dependientes_elecciones.csv")
base_indicadores <- read.csv("indicadores_elecciones.csv")
base_controles <- read.csv("controles_elecciones.csv")
base_candidatos <-  read.csv("indicadores_candidatos.csv")
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# ELECCIONES ##############################
# CORRELACIONES ENTRE VARIABLES INDEP #####

all_corr_indicadores <- base_indicadores %>% 
  select(ncat_eleccion,
         marginvic,
         exapprovalnotsmoothed,
         dico_reeleccion,
         dico_debates_pastelection,
         alineamiento,
         nec,
         proptv,
         propindivinternet,
         regulaciondico,
         prohibicionpropaganda,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo)
#mutate(across(everything(), as.integer(.)))
summary(all_corr_indicadores)
correlation_matrix <- cor(all_corr_indicadores , use = "pairwise.complete.obs" )
corrplot::corrplot(correlation_matrix)

all_corr_controles <- base_controles %>% 
  select(ncat_eleccion,
         gdpxcapita,
         democraciavdempolyarchy,
         urbanpop,
         mediaqualitycorruptvdem,
         mediaqualitybiasnelda,
         mediaqualityfreedombti,
         edadregimenfilled)
summary(all_corr_controles)
correlation_matrix <- cor(all_corr_controles , use = "pairwise.complete.obs" )
corrplot::corrplot(correlation_matrix)


all_corrs <- base_indicadores %>% 
  left_join(base_controles) %>% 
  left_join(base_vdependiente %>% select(-X)) %>% 
  select(-cat_pais) %>% 
  select(ncat_eleccion,
         proptv,
         propindivinternet,
         dico_debates_pastelection,
         cumsum_pastciclos,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         regulacionordinal,
         edadregimenfilled,
         democraciavdemelectoralcomp,
         urbanpop,
         mediaqualitycorruptvdem,
         mediaqualitybiasnelda,
         #mediaqualityfreedombti,
         prohibicionpropaganda,
         alineamiento,
         marginvic,
         exapprovalnotsmoothed,
         dico_reeleccion,
         nec,
         ncat_ronda,
         dico_hubo_debates,
         dico_nueva_practica_elec,
         dico_practica_interrumpida_elec)

summary(all_corrs)
correlation_matrix <- cor(all_corrs , use = "pairwise.complete.obs" )
corrplot::corrplot(correlation_matrix, diag = F, addCoef.col= "black")
write.csv(correlation_matrix, "correlation_matrix.csv", row.names = TRUE)

# MODELOS DE PRUEBA #####

## preparo data #####
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

## MODELOS 1: toda la muestra - V.D = dico_hubo_debates #####
### Modelo contingencia  ####
 
modelo_contingencia_controles <- glm(dico_hubo_debates ~ 
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
                               mediaqualitycorruptvdem,
                               #edadregimenbmr#, #Sacar edad de régimen.  
                            #No tiene sentido incluirla si controlas por la calidad de la democracia 
                            # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                           family = binomial(link = "logit"), 
                           #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                           data = democracias)
options(scipen=999)
summary(modelo_contingencia_controles)
vif_values <- car::vif(modelo_contingencia_controles) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_contingencia_controles, 
                              vcov = vcovCL(modelo_contingencia_controles, 
                                            cluster = democracias$elecid))
                                            #cluster = data$elecid))

print(robust_se_cluster)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo sistemico ####
modelo_sistemico_controles <- glm(dico_hubo_debates ~ 
                                    alineamiento + 
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    gdpxcapita +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem,
                                    #edadregimenbmr,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_sistemico_controles) # 
vif_values <- car::vif(modelo_sistemico_controles) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sistemico_controles, 
                              vcov = vcovCL(modelo_sistemico_controles,
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo marco regulatorio ####
modelo_regulatorio_controles <- glm(dico_hubo_debates ~ 
                                      prohibicionpropaganda +
                                      accesogratuito +
                                      regulaciondico + 
                                      cumsum_pastciclos +
                                      #dico_debates_pastelection +
                                      gdpxcapita +
                                      democraciavdemelectoralcomp +
                                      mediaqualitycorruptvdem,
                                      #edadregimenbmr,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_regulatorio_controles)
vif_values <- car::vif(modelo_regulatorio_controles) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_regulatorio_controles, 
                              vcov = vcovCL(modelo_regulatorio_controles, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo geo/temp ####
modelo_temporal <- glm(dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem,
                         #edadregimenbmr,
                         family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_temporal)
vif_values <- car::vif(modelo_temporal) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_temporal, 
                              vcov = vcovCL(modelo_temporal, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo final / sficativas ####
modelo_sficativas <- glm(dico_hubo_debates ~ 
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
                           mediaqualitycorruptvdem, #+
                          # ncat_eleccion,
                          #edadregimenbmr#, #Sacar edad de régimen.  
                           #No tiene sentido incluirla si controlas por la calidad de la democracia 
                            # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas)
vif_values <- car::vif(modelo_sficativas) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sficativas, 
                              vcov = vcovCL(modelo_sficativas, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

# Modelo lineal generalizado de efectos mixtos (ejemplo: binomial)

# Estandarizar variables predictoras
democracias_scaled <- democracias %>%
  dplyr::mutate(across(c(marginvic, nec, voteshareincumbent, propindivinternet,
                         avgpropdebatesregionxciclo, prop_elec_usa_ciclo, 
                         gdpxcapita, democraciavdemelectoralcomp, mediaqualitycorruptvdem),
                       ~ scale(.)))
# Verificar colinealidad
vif_model <- lm(dico_hubo_debates ~ marginvic + nec + voteshareincumbent + 
                  dico_reeleccion + propindivinternet + accesogratuito + 
                  avgpropdebatesregionxciclo + prop_elec_usa_ciclo + 
                  regulaciondico + cumsum_pastciclos + gdpxcapita + 
                  democraciavdemelectoralcomp + mediaqualitycorruptvdem, 
                data = democracias)
car::vif(vif_model)

modelo_MLGEM <- lme4::glmer(dico_hubo_debates ~ 
                  #dico_debates_primerosdos ~ 
                  #dico_hubo_debate_mediatico ~ 
                  marginvic + 
                  nec +
                  #exapprovalnotsmoothed + 
                  voteshareincumbent +
                  dico_reeleccion + 
                  #proptv +
                  propindivinternet +
                  #accesogratuito +
                  avgpropdebatesregionxciclo + 
                  prop_elec_usa_ciclo +
                  regulaciondico +
                  cumsum_pastciclos +
                  #dico_debates_pastelection +
                  gdpxcapita +
                  democraciavdemelectoralcomp +
                  mediaqualitycorruptvdem +
                    (1 | cat_pais) + (1| ncat_eleccion), 
                data = democracias_scaled, 
                family = binomial(link = "logit"),
                control = lme4::glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 2e5)))
summary(modelo_MLGEM)
# Calcular errores estándar robustos (Sandwich estimators)
#table(democracias_scaled$cat_pais, useNA = "ifany")
democracias_scaled$cat_pais <- as.factor(democracias_scaled$cat_pais) # ESTOY TENIENDO ALGUN PROBLEMA QUE NO PUDE RESOLVER 
vcov_cr2 <- clubSandwich::vcovCR(modelo_MLGEM, cluster = democracias_scaled$cat_pais, type = "CR2")
robust_se <- clubSandwich::coef_test(modelo_MLGEM, vcov = vcov_cr2)
print(robust_se)
# Residuos y diagnósticos
plot(resid(modelo_MLGEM))

modelo_sficativas_scaled <- glm(dico_hubo_debates ~ 
                           #dico_debates_primerosdos ~ 
                           #dico_hubo_debate_mediatico ~ 
                           avgpropdebatesregionxciclo + 
                           #proptv +
                           propindivinternet +
                          # accesogratuito + para tener menos missing
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem,#+
                           #ncat_eleccion + #cuando agregamos año, internet desaparece
                             #edadregimenbmr,
                         family = binomial(link = "logit"), 
                         data = data_scaled)
options(scipen=999)
summary(modelo_sficativas_scaled)
vif_values <- car::vif(modelo_sficativas_scaled) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sficativas_scaled, vcov = vcovCL(modelo_sficativas_scaled, cluster = data$elecid))

### Loop / prueba #### 
### primera version de LOOP con DICO HUBO DEBATES para DICO DEBATES PASTELECCION  

# para otra version: sustituyo NAs por 0, para tener mismo n en todas las corridas del loop

data <- data %>% 
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
  formula <- as.formula(paste("dico_hubo_debates ~ 
                           avgpropdebatesregionxciclo + 
                           propindivinternet +
                           marginvic + 
                           nec +
                           voteshareincumbent +
                           dico_reeleccion + 
                           regulaciondico +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem +", var))
  
  # Perform the regression
  model <- glm(formula,
               family = binomial(link = "logit"),
               data = data)
  
  robust_se_cluster <- coeftest(model, vcov = vcovCL(model, cluster = data$elecid))
  
  
  # Save the coefficients
  coef_results[[var]] <-data.frame(
    Estimate = robust_se_cluster[, "Estimate"],
    Std.Error = robust_se_cluster[, "Std. Error"],
    #t.value = robust_se_cluster[, "t value"],
    Pr = robust_se_cluster[, "Pr(>|z|)"]
  )
}


# Extract the results for the desired predictor
coef_for_predictor <- lapply(coef_results, function(x) x[rownames(x) %in% predictors, ])

# Convert the list to a data frame for easy comparison
comparison_df <- do.call(rbind, coef_for_predictor)
rownames(comparison_df) <- NULL

# View the comparison table
comparison_df

## MODELOS 2: submuestra sin debates en la eleccion pasada. "Nueva práctica"  #####
### preparo submuestra ####
data2 <- data %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) 
  #subset(dico_debates_pastelection==0) 
summary(data2)

democracias2 <- democracias %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) 
#subset(dico_debates_pastelection==0) 
summary(democracias2)

### Modelo contingencia  ####

modelo_contingencia2 <- glm(dico_hubo_debates ~ 
                            #  dico_nueva_practica_elec ~   # es literalmente lo mismo
                            #  dico_nueva_practica_ciclo ~   
                                       marginvic + 
                                       nec +
                                       #exapprovalnotsmoothed + 
                                       voteshareincumbent +
                                       dico_reeleccion + 
                                       regulaciondico +
                                       #cumsum_pastciclos +
                                       #dico_debates_pastelection +
                                      # gdpxcapita +
                                       democraciavdemelectoralcomp, #+
                                       #mediaqualitycorruptvdem, #+
                                       #edadregimenfilled ,
                                     family = binomial(link = "logit"), 
                                    #data = data2
                                     data = democracias2)
options(scipen=999)
summary(modelo_contingencia2)
vif_values <- car::vif(modelo_contingencia2) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_contingencia2, 
                              vcov = vcovCL(modelo_contingencia2, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2$elecid))
print(robust_se_cluster)

### Modelo sistemico ####
modelo_sistemico2 <- glm(dico_hubo_debates ~ 
                           #  dico_nueva_practica_elec ~  # es literalmente lo mismo
                                    alineamiento + 
                                    proptv +
                                    propindivinternet +
                                    #cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    #gdpxcapita +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem, #+
                                    #edadregimenbmr,
                                  family = binomial(link = "logit"), 
                         #data = data2
                         data = democracias2)

summary(modelo_sistemico2) 
vif_values <- car::vif(modelo_sistemico2) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sistemico2, 
                              vcov = vcovCL(modelo_sistemico2, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2$elecid))
print(robust_se_cluster)

### Modelo marco regulatorio ####
modelo_regulatorio2 <- glm(dico_hubo_debates ~ 
                             #  dico_nueva_practica_elec ~  # es literalmente lo mismo
                                      regulaciondico + 
                                      prohibicionpropaganda +
                                      accesogratuito +
                                      #cumsum_pastciclos +
                                      #dico_debates_pastelection +
                                      gdpxcapita +
                                      democraciavdemelectoralcomp +
                                      mediaqualitycorruptvdem + 
                                      edadregimenbmr,
                                    family = binomial(link = "logit"), 
                           #data = data2
                           data = democracias2)

summary(modelo_regulatorio2)
vif_values <- car::vif(modelo_regulatorio2) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_regulatorio2, 
                              vcov = vcovCL(modelo_regulatorio2, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2$elecid))
print(robust_se_cluster)

### Modelo geo/temp ####
modelo_temporal2 <- glm(dico_hubo_debates ~ 
                          #  dico_nueva_practica_elec ~  # es literalmente lo mismo
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         democraciavdemelectoralcomp +
                         #cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         mediaqualitycorruptvdem +
                         regulaciondico + 
                          edadregimenbmr,
                       family = binomial(link = "logit"), 
                       #data = data2
                       data = democracias2)

summary(modelo_temporal2)
vif_values <- car::vif(modelo_temporal2) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_temporal2, 
                              vcov = vcovCL(modelo_temporal2, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2$elecid))
print(robust_se_cluster)

### Modelo final / sficativas ####
modelo_sficativas <- glm(dico_hubo_debates ~ 
                           #dico_debates_primerosdos ~ 
                           #dico_hubo_debate_mediatico ~ 
                           avgpropdebatesregionxciclo + 
                           prop_elec_usa_ciclo +
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem, #+
                         # ncat_eleccion,
                         #edadregimenbmr#, #Sacar edad de régimen.  
                         #No tiene sentido incluirla si controlas por la calidad de la democracia 
                         # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                         family = binomial(link = "logit"), 
                         #data = data2
                         data = democracias2)
options(scipen=999)
summary(modelo_sficativas)
vif_values <- car::vif(modelo_sficativas) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sficativas, 
                              vcov = vcovCL(modelo_sficativas, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2$elecid))
print(robust_se_cluster)


### Prueba: subumuestra sin debates y sin regulacion ####


data2.2 <- data %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) %>% 
  subset(regulaciondico==0)
#subset(dico_debates_pastelection==0) 
summary(data2)

democracias2.2 <- democracias %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) %>% 
  subset(regulaciondico==0)
#subset(dico_debates_pastelection==0) 
summary(democracias2)

# Modelo contingencia  

modelo_contingencia2.2 <- glm(dico_hubo_debates ~ 
                              #  dico_nueva_practica_elec ~   # es literalmente lo mismo
                              #  dico_nueva_practica_ciclo ~   
                              marginvic + 
                              nec +
                              #exapprovalnotsmoothed + 
                              voteshareincumbent +
                              dico_reeleccion + 
                              #regulaciondico +
                              #cumsum_pastciclos +
                              #dico_debates_pastelection +
                              # gdpxcapita +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem, #+ 
                            #edadregimenfilled ,
                            family = binomial(link = "logit"), 
                            #data = data2.2,
                            data = democracias2.2)
options(scipen=999)
summary(modelo_contingencia2.2)
vif_values <- car::vif(modelo_contingencia2.2) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_contingencia2.2, 
                              vcov = vcovCL(modelo_contingencia2.2, 
                                            cluster = democracias2.2$elecid))
                                            #cluster = data2.2$elecid))
print(robust_se_cluster)

modelo_sficativas <- glm(dico_hubo_debates ~ 
                           #dico_debates_primerosdos ~ 
                           #dico_hubo_debate_mediatico ~ 
                           avgpropdebatesregionxciclo + 
                           prop_elec_usa_ciclo +
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           #regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem, #+
                         # ncat_eleccion,
                         #edadregimenbmr#, #Sacar edad de régimen.  
                         #No tiene sentido incluirla si controlas por la calidad de la democracia 
                         # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                         family = binomial(link = "logit"), 
                         #data = data2
                         data = democracias2.2)
options(scipen=999)
summary(modelo_sficativas)
vif_values <- car::vif(modelo_sficativas) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sficativas, 
                              vcov = vcovCL(modelo_sficativas, 
                                            #cluster = data2$elecid))
                                            cluster = democracias2.2$elecid))
print(robust_se_cluster)

### MODELOS 3: submuestra con debates en eleccion anterior. "Interrupción de práctica"  #####
### preparo data ####
data3 <- data %>% 
  subset(dico_debates_pastelection==1) 
summary(data3)

democracias3 <- democracias %>% 
  subset(dico_debates_pastelection==1) 
summary(democracias3)

### Modelo contingencia ####
modelo_contingencia3 <- glm(dico_practica_interrumpida_elec ~ 
                              #dico_practica_interrumpida_ciclo ~   
                              marginvic + 
                              nec +
                              #exapprovalnotsmoothed + 
                              voteshareincumbent +
                              dico_reeleccion + 
                              regulaciondico +
                              #cumsum_pastciclos +
                              #dico_debates_pastelection +
                              gdpxcapita +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem +
                              edadregimenfilled,
                            family = binomial(link = "logit"), 
                           # data = data3
                            data = democracias3)
options(scipen=999)
summary(modelo_contingencia3)
vif_values <- car::vif(modelo_contingencia3) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_contingencia3, 
                              vcov = vcovCL(modelo_contingencia3,
                                            #cluster = data3$elecid))
                                            cluster = democracias3$elecid))
                                            
print(robust_se_cluster)

### Modelo sistemico ####
modelo_sistemico3 <- glm(dico_practica_interrumpida_elec ~ 
                           alineamiento + 
                           proptv +
                           propindivinternet +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem,
                         #edadregimenbmr,
                         family = binomial(link = "logit"), 
                         # data = data3
                         data = democracias3)

summary(modelo_sistemico3) # indeterminado
vif_values <- car::vif(modelo_sistemico3) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sistemico3, 
                              vcov = vcovCL(modelo_sistemico3, 
                                            #cluster = data3$elecid))
                                            cluster = democracias3$elecid))
print(robust_se_cluster)

### Modelo marco regulatorio ####
modelo_regulatorio3 <- glm(dico_practica_interrumpida_elec ~ 
                             regulaciondico + 
                             prohibicionpropaganda +
                             accesogratuito +
                             cumsum_pastciclos +
                             #dico_debates_pastelection +
                             gdpxcapita +
                             democraciavdemelectoralcomp +
                             mediaqualitycorruptvdem,
                           #edadregimenbmr,
                           family = binomial(link = "logit"), 
                           # data = data3
                           data = democracias3)

summary(modelo_regulatorio3)
vif_values <- car::vif(modelo_regulatorio3) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_regulatorio3, 
                              vcov = vcovCL(modelo_regulatorio3, 
                                            #cluster = data3$elecid))
                                            cluster = democracias3$elecid))
print(robust_se_cluster)

### Modelo geo/temp ####
# resultados raros en este siguiente modelo. estoy teniendo algun problem que no detecto 
modelo_temporal3 <- glm(dico_practica_interrumpida_elec ~ 
                          avgpropdebatesregionxciclo + 
                          prop_elec_usa_ciclo +
                          democraciavdemelectoralcomp +
                          #cumsum_pastciclos +
                          #dico_debates_pastelection +
                          gdpxcapita +
                          mediaqualitycorruptvdem +
                          regulaciondico,
                        #edadregimenbmr,
                        family = binomial(link = "logit"), 
                        # data = data3
                        data = democracias3)
summary(modelo_temporal3)
vif_values <- car::vif(modelo_temporal3) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_temporal3, 
                              vcov = vcovCL(modelo_temporal3, 
                                            #cluster = data3$elecid))
                                            cluster = democracias3$elecid))
print(robust_se_cluster)

### Modelo final / sficativas ####
modelo_sficativas <- glm(dico_hubo_debates ~ 
                           #dico_debates_primerosdos ~ 
                           #dico_hubo_debate_mediatico ~ 
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem, #+
                         # ncat_eleccion,
                         #edadregimenbmr#, #Sacar edad de régimen.  
                         #No tiene sentido incluirla si controlas por la calidad de la democracia 
                         # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                         family = binomial(link = "logit"), 
                         #data = data3
                         data = democracias3)
options(scipen=999)
summary(modelo_sficativas)
vif_values <- car::vif(modelo_sficativas) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_sficativas, 
                              vcov = vcovCL(modelo_sficativas, 
                                            #cluster = data3$elecid))
                                            cluster = democracias3$elecid))
print(robust_se_cluster)

# CANDIDATOS ##############################
# acomodo data preparacion #######


base <- base_candidatos %>% 
  select(-starts_with("source_")) %>% 
  select(-X) 

variable_dependiente <- base$dico_candidato_presente

options(scipen=999)


# CORRELACIONES ENTRE VARIABLES INDEP #####

all_corr_indicadores <- base_candidatos %>% 
  select(voteshare,
         v2pariglef_vdem,
         v2paactcom_vdem,
         dico_reeleccion,
         dico_oficialista,
         ninvitaciones,
         propausenciaspasadas,
         propausenciaspasadasfilled,
         dicoausenciaspasadas,
         dicopresenciaspasadas,
         nausenciaspasadas,
         npresenciaspasadas,
         orgosc,
         orgmmc,
         orgestado,
         dico_candidato_presente)

summary(all_corr_indicadores)
correlation_matrix <- cor(all_corr_indicadores , use = "pairwise.complete.obs" )
corrplot::corrplot(correlation_matrix, diag = F, addCoef.col= "black")
write.csv(correlation_matrix, "correlation_matrix.csv", row.names = TRUE)

# MODELOS DE PRUEBA #####

data <- base_candidatos  %>% 
  select(-starts_with("source_")) %>% 
  select(-X,
         -nombres_candidatos,
         -v2pashname)

# creo variable para clusterizar SSEE
data <- data %>% # en este caso tb tengo id_debate
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

# creo data estandarizada
data_scaled <- data %>%
  mutate(
    # across(-c(dico_candidato_presente,
    #                cat_pais,
    #                elecid,
    #                id_debate), as.integer),
         across(-c(dico_candidato_presente,
                   cat_pais,
                   elecid,
                   id_debate), scale))


modelo_all <- glm(dico_candidato_presente ~ 
                    voteshare + 
                  #v2pariglef_vdem + 
                  #v2paactcom_vdem + 
                  dico_reeleccion + 
                  #dico_oficialista + 
                  ninvitaciones + 
                  #propausenciaspasadas + 
                  propausenciaspasadasfilled + 
                  #dicoausenciaspasadas + 
                  #dicopresenciaspasadas + 
                  #nausenciaspasadas + 
                  #npresenciaspasadas + 
                  orgosc + 
                  #orgmmc + 
                  orgestado,
                  family = binomial(link = "logit"),
                  data = data)
options(scipen=999)
summary(modelo_all)
vif_values <- car::vif(modelo_all) # cheq de multicolinealidad segun chatgpt
print(vif_values) 
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).
robust_se_cluster <- coeftest(modelo_all, vcov = vcovCL(modelo_all, cluster = data$elecid))
print(robust_se_cluster)
