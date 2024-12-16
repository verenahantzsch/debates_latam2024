

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

#### defino id obs ####

democracias <- democracias %>% 
  mutate(obsid = paste(cat_pais, ncat_eleccion, ncat_ronda, sep = " "))

## MODELOS 1: toda la muestra - V.D = dico_hubo_debates #####
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


### Modelo sistemico ####


formula_modelo_sistemico <- "dico_hubo_debates ~ 
                                    alineamiento + 
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    gdpxcapita +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem"
                                    #edadregimenbmr"

modelo_sistemico <- glm(formula_modelo_sistemico,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_sistemico) # 


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


### Modelo geo/temp ####

formula_modelo_temporal <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem#,
                         #edadregimenbmr,"

modelo_temporal <- glm(formula_modelo_temporal,
                         family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_temporal)

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

## CONTROLES MODELO 1 ####
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
         mediaqualitycorruptvdem) %>% 
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
         propindivinternet,
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
modelo_contingencia_reducido <- glm(formula_modelo_contingencia, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_sistemico_reducido <- glm(formula_modelo_sistemico, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_regulatorio_reducido <- glm(formula_modelo_regulatorio, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_temporal_reducido <- glm(formula_modelo_temporal, 
                family = binomial(link = "logit"), 
                data = data_reducida)
modelo_sficativas_reducido <- glm(formula_modelo_sficativas, 
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


### control de resiudos ####

##### Graficar los residuos ####
par(mfrow = c(1, 2))

# Residuos deviance
residuals_dev <- residuals(modelo_sficativas, type = "deviance")
plot(residuals_dev, main = "Residuos Deviance", ylab = "Residuos", xlab = "Índice")
abline(h = 0, col = "red", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_dev), y = residuals_dev, 
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")  # Ajusta 'pos' para la posición del texto

# Residuos de Pearson
residuals_pearson <- residuals(modelo_sficativas, type = "pearson")
plot(residuals_pearson, main = "Residuos de Pearson", ylab = "Residuos", xlab = "Índice")
abline(h = 0, col = "blue", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_pearson), y = residuals_pearson, 
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")  # Ajusta 'pos' para la posición del texto

 
# Residuos Pearsonestandarizados 
hat_values <- hatvalues(modelo_sficativas)  # Leverage
residuos_pearson_est <- residuals_pearson / sqrt(1 - hat_values)
plot(residuos_pearson_est, main = "Residuos de Pearson Estandarizados", ylab = "Residuos", xlab = "Índice")
abline(h = 0, col = "blue", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuos_pearson_est), y = residuos_pearson_est, 
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")  # Ajusta 'pos' para la posición del texto

qqnorm(residuals_dev, main = "QQ Plot - Residuos de Deviance")
qqline(residuals_dev, col = "red")

# no parecen detectarse patrones claros que indiquen violaciones a supuesto de variable omitida o heterocedasticidad
# si parece haber dos observaciones potencialmente problemáticas (outliers o con leverage)

##### Durbin-Watson para autocorrelacion de errores ####
# prueba de Durbin-Watson (verificar autocorrelación en los residuos).
# The Durbin-Watson test has the null hypothesis that the autocorrelation of the disturbances is 0. 
# It is possible to test against the alternative that it is greater than, not equal to, or less than 0, respectively. This can be specified by the alternative argument.
# https://medium.com/@analyttica/durbin-watson-test-fde429f79203
# no es lo más adecuado de usar cuando tengo variables rezagadas (como es mi caso)
dwtest(modelo_sficativas)
# valor del estadistico: Una regla general que se sigue es: los valores de la estadística de prueba DW en el rango de 1,5 a 2,5 son relativamente aceptables. Los valores fuera de este rango podrían ser motivo de preocupación. Los valores inferiores a 1 o superiores a 3 son un motivo definitivo de preocupación.
# no podemos rechazar (tenemos evidencia favorable a) la H nula de que no hay autocorrelación
# A p-value of 0.3608 for a Durbin-Watson test indicates that the null hypothesis of no autocorrelation is not rejected

### casos influyentes y outliers ####

# https://stats.stackexchange.com/questions/22161/how-to-read-cooks-distance-plots

##### Cook ####
# Obtener las estadísticas de Cook
cooks_distances <- cooks.distance(modelo_sficativas)

# Ver las primeras estadísticas de Cook
head(cooks_distances)

# Graficar las estadísticas de Cook
plot(cooks_distances, main = "Estadísticas de Cook", ylab = "Cook's Distance", xlab = "Índice de observación")
abline(h = 4 / (length(cooks_distances) - 14 - 1) , col = "red", lty = 2)  # Línea de corte común (influencia alta)
# Identificar observaciones influyentes (por encima del umbral)
influential_obs <- which(cooks_distances > 4 / length(cooks_distances))
text(x = influential_obs, y = cooks_distances[influential_obs], 
     labels = democracias$obsid[influential_obs], pos = 4, cex = 0.7, col = "blue")

# : Se suele trazar una línea de corte en 4 / n, donde n es el número de observaciones) para identificar observaciones que tienen una gran influencia en el modelo. Las observaciones por encima de esta línea pueden ser consideradas como influyentes.

car::influenceIndexPlot(modelo_sficativas, vars = c("Cook", "hat"))

##### dfbetas ####

dfbetas <- as.data.frame(dfbetas(modelo_sficativas))
#Para cada observación, podemos ver la diferencia en la estimación del coeficiente para la intersección, la variable  disp y la variable  hp que ocurre cuando eliminamos esa observación en particular.
#Normalmente consideramos que una observación es muy influyente en la estimación de un coeficiente dado si tiene un valor DBETAS mayor que un umbral de 2/√ n, donde  n es el número de observaciones.
umbral <- 2 / sqrt(nrow(data_modelo_sficativas))

#especificar 2 filas y 1 columna en la región de trazado
par(mfrow=c(2,1))

#plot DFBETAS para marginvic
plot(dfbetas$marginvic)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$marginvic,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para nec  
plot(dfbetas$nec)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$nec,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para voteshareincumbent  
plot(dfbetas$voteshareincumbent)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$voteshareincumbent,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para dico_reeleccion  
plot(dfbetas$dico_reeleccion)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$dico_reeleccion,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para propindivinternet  
plot(dfbetas$propindivinternet)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$propindivinternet,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para accesogratuito  
plot(dfbetas$accesogratuito)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$accesogratuito,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")


#plot DFBETAS para avgpropdebatesregionxciclo  
plot(dfbetas$avgpropdebatesregionxciclo)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$avgpropdebatesregionxciclo,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")


#plot DFBETAS para prop_elec_usa_ciclo  
plot(dfbetas$prop_elec_usa_ciclo)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$prop_elec_usa_ciclo,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para regulaciondico  
plot(dfbetas$regulaciondico)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$regulaciondico,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")


#plot DFBETAS para cumsum_pastciclos  
plot(dfbetas$cumsum_pastciclos)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$cumsum_pastciclos,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")


#plot DFBETAS para gdpxcapita  
plot(dfbetas$gdpxcapita)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$gdpxcapita,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para democraciavdemelectoralcomp  
plot(dfbetas$democraciavdemelectoralcomp)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$democraciavdemelectoralcomp,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

#plot DFBETAS para mediaqualitycorruptvdem  
plot(dfbetas$mediaqualitycorruptvdem)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$mediaqualitycorruptvdem,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

### control de multicolinealidad ####
##### correlaciones (al menos <.5, conservador <2.5) ####

corr_base_democracias <- democracias %>% 
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
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem)

summary(corr_base_democracias)
correlation_matrix <- cor(corr_base_democracias , use = "pairwise.complete.obs" )
 
# Generar el gráfico con resaltado
corrplot::corrplot(
  correlation_matrix, 
  method = "circle",        # Puedes usar otros métodos como "color", "number"
  col = colorRampPalette(c("blue", "white", "red"))(8), # Escala de colores
  #addCoef.col = "black",    # Añadir los coeficientes al gráfico
  #tl.col = "black",         # Color de las etiquetas
  #sig.level = 0.5,          # Nivel de significancia (umbral de 0.5 para el resaltado)
  #insig = "label_sig",      # Mostrar "*" en los valores destacados
  #pch.cex = 1.5,            # Tamaño del marcador "*"
  diag = FALSE              # No graficar la diagonal
)
# Resaltar valores absolutos mayores a 0.5
for (j in 1:ncol(correlation_matrix)) {
  for (i in 1:nrow(correlation_matrix)){
    if (abs(correlation_matrix[j, i]) > 0.5) { # Ignorar la diagonal
      text(j, i, "**", col = "black", cex = 1.5) # Añadir un "*" en las celdas relevantes
    }
  }
}
correlation_matrix[1, 2]

# PENDIENTE PROBLEMA #########
corr_base_democracias_reducida <- data_reducida %>% 
  select()
summary(corr_base_democracias)
correlation_matrix <- cor(corr_base_democracias , use = "pairwise.complete.obs" )
corrplot::corrplot(corr_base_democracias)

##### vif (raiz de vif < 2) ####
# VIF < 5: The variable has low multicollinearity (no significant issue).
#VIF between 5 and 10: The variable has moderate multicollinearity (requires further investigation).
#VIF > 10: The variable has high multicollinearity (serious issue, consider mitigation techniques).

vif_values <- car::vif(modelo_contingencia_controles)  
print(vif_values) 

vif_values <- car::vif(modelo_sistemico)  
print(vif_values)

vif_values <- car::vif(modelo_regulatorio)  
print(vif_values) 

vif_values <- car::vif(modelo_temporal)  
print(vif_values) 

vif_values <- car::vif(modelo_sficativas)  
print(vif_values) 

# otros
### missing data ####
### overall significance y comparaciones ####
##### test de Wald ####


lmtest::waldtest(modelo_sficativas)

# podemos rechazar H0 de que todos los coeficientes = 0

# https://www.geeksforgeeks.org/how-to-perform-a-wald-test-in-r/
# Res.Df: Indica los grados de libertad residuales, que es la diferencia entre el número total de observaciones y el número de parámetros estimados en el modelo.
# Df: Representa el cambio en grados de libertad entre el Modelo 1 y el Modelo 2. En este caso, el Modelo 2 tiene 2 parámetros menos estimados en comparación con el Modelo 1 porque incluye menos predictores.
# F: Esta es la estadística de prueba para la prueba de Wald. Sigue una distribución F bajo la hipótesis nula de que los parámetros en el modelo reducido (Modelo 2) son iguales a cero. En otras palabras, prueba si los predictores adicionales en el Modelo 1 contribuyen significativamente al modelo.
# Pr(>F): es el valor p asociado con el estadístico de la prueba F. Representa la probabilidad de observar un estadístico F tan extremo como el calculado bajo la hipótesis nula. En este caso, el valor p es 0,006863, que es menor que 0,05, lo que sugiere una evidencia sólida contra la hipótesis nula.

lmtest::waldtest(modelo_sficativas , modelo_0_reducido_sficativas)
lmtest::waldtest(modelo_sficativas , modelo_contingencia_reducido_sficativas)
#lmtest::waldtest(modelo_sficativas , modelo_sistemico_reducido_sficativas) # pendiente
#lmtest::waldtest(modelo_sficativas , modelo_regulatorio_reducido_sficativas) # pendiente
lmtest::waldtest(modelo_sficativas , modelo_temporal_reducido_sficativas)

# en gral el modelo mejora significativamente en todos los casos

##### lr test (comparar modelos con = cantidad de data, sacando missing) ####

#  First, the two models must be nested. Second, the two models must be estimated on exactly the same sample. 
#lrtest(null_model, full_model)

likelihood_ratio_test <- lrtest(modelo_0_reducido_sficativas, modelo_sficativas )
likelihood_ratio_test <- lrtest(modelo_contingencia_reducido_sficativas, modelo_sficativas)
likelihood_ratio_test <- lrtest(modelo_sistemico_reducido_sficativas, modelo_sficativas) # pendiente
likelihood_ratio_test <- lrtest(modelo_regulatorio_reducido_sficativas, modelo_sficativas) # pendiente
likelihood_ratio_test <- lrtest(modelo_temporal_reducido_sficativas, modelo_sficativas)


# Model 1 : represents the null model, which includes only the intercept.
# Model 2: represents the full model, which includes predictors
# #Df : This indicates the degree of freedom or the number of parameters involved
# LogLik: This indicates the log-likelihood values for each model.
# Chisq: is the likelihood ratio test statistic, which measures the difference in log-likelihood values between the two models.
# Pr(>Chisq): represents the p-value associated with the likelihood ratio test.

#likelihood_ratio_test <- lrtest(null_model, full_model)
# Interpretation of likelihood ratio test results
if (likelihood_ratio_test$"Pr(>Chisq)"[2] < 0.05) 
{
  cat("Reject the null hypothesis. The full model is significantly better than 
        the null model.\n")
} else {
  cat("Fail to reject the null hypothesis. The null model is sufficient.\n")
}

# en todos los casos, el modelo full es mejor que el modelo más incompleto. 



### fit: Pseudos R cuadrados, AIC y BIC ####
 

# pseudo R cuadrados. #Mas alto, mejores
# https://www.rdocumentation.org/packages/rcompanion/versions/2.4.36/topics/nagelkerke
rcompanion::nagelkerke(modelo_sficativas)
# https://search.r-project.org/CRAN/refmans/fmsb/html/Nagelkerke.html 
fmsb::NagelkerkeR2(modelo_sficativas)
pscl::pR2(modelo_sficativas)

# Obtener AIC y BIC # mas bajos, mejores, aunque se penaliza la inclusion de mas parametros
AIC(modelo_sficativas)
BIC(modelo_sficativas)

# Obtener Log-Likelihood, mas alto mejor. solo se pueden comparar modelos anidados
logLik(modelo_sficativas)

# Realizar prueba de razón de verosimilitudes
lmtest::lrtest(modelo_0, modelo_sficativas)



# para comparar modelos con igual n, creo funcion 
 
stats <- function(modelo){
  pseudoR2 <- rcompanion::nagelkerke(modelo)$Pseudo.R.squared.for.model.vs.null %>% 
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

all_stats <- rbind(stats0,
                   stats1,
                   stats2,
                   stats3,
                   stats4,
                   stats5 )


### fit: desempeño: accuracy ####


data_modelo_sficativas$probabilidades_predichas <- predict(modelo_sficativas, type = "response")
data_modelo_sficativas$predicciones_binarias <- ifelse(probabilidades_predichas>0.5,1,0)

# Matriz de confusión
confusion_matrix <- table(data_modelo_sficativas$predicciones_binarias, data_modelo_sficativas$dico_hubo_debates)
print(confusion_matrix)

# Count R²: proporción de predicciones correctas
count_r2 <- mean(data_modelo_sficativas$predicciones_binarias == data_modelo_sficativas$dico_hubo_debates)
# Precisión base: proporción de la clase mayoritaria
baseline_accuracy <- max(table(data_modelo_sficativas$dico_hubo_debates)) / length(data_modelo_sficativas$dico_hubo_debates)
# Adjusted Count R²
adjusted_count_r2 <- (count_r2 - baseline_accuracy) / (1 - baseline_accuracy)
#  The adjusted count R2 is the proportion of correct guesses beyond the number that would be correctly guessed by choosing the largest marginal

# Imprimir resultados
cat("Count R²:", count_r2, "\n")
cat("Adjusted Count R²:", adjusted_count_r2, "\n")

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

countr20 <- funcion_count_R2(modelo_0, democracias$dico_hubo_debates)
countr21 <- funcion_count_R2(modelo_contingencia_reducido, data_reducida$dico_hubo_debates)
countr22 <- funcion_count_R2(modelo_sistemico_reducido, data_reducida$dico_hubo_debates)
countr23 <- funcion_count_R2(modelo_regulatorio_reducido, data_reducida$dico_hubo_debates)
countr24 <- funcion_count_R2(modelo_temporal_reducido, data_reducida$dico_hubo_debates)
countr25 <- funcion_count_R2(modelo_sficativas_reducido, data_reducida$dico_hubo_debates)

comparacion_count_r2 <- rbind(countr20 ,
                              countr21 ,
                              countr22 ,
                              countr23 ,
                              countr24 ,
                              countr25 )

# el mejor sigue siendo el modelo full
# quizas vale la pena comparar versiones con mas datos
# o hacer trabajo andidado de verdad, digamos

### MODELOS ROBUSTOS #####
### Modelo contingencia  ####

robust_se_cluster_modelo_contingencia <- coeftest(modelo_contingencia, 
                              vcov = vcovCL(modelo_contingencia_controles, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))


print(robust_se_cluster_modelo_contingencia)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster_modelo_contingencia[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster_modelo_contingencia))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo sistemico ####
 
robust_se_cluster_modelo_sistemico <- coeftest(modelo_sistemico, 
                              vcov = vcovCL(modelo_sistemico,
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster_modelo_sistemico)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster_modelo_sistemico[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster_modelo_sistemico))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo marco regulatorio ####

 robust_se_cluster_modelo_regulatorio <- coeftest(modelo_regulatorio, 
                              vcov = vcovCL(modelo_regulatorio, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster_modelo_regulatorio)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster_modelo_regulatorio[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster_modelo_regulatorio))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo geo/temp ####

robust_se_cluster_modelo_temporal <- coeftest(modelo_temporal, 
                              vcov = vcovCL(modelo_temporal, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster_modelo_temporal)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster_modelo_temporal[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster_modelo_temporal))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

### Modelo final / sficativas ####

robust_se_cluster_modelo_sficativas <- coeftest(modelo_sficativas, 
                              vcov = vcovCL(modelo_sficativas, 
                                            #cluster = data$elecid))
                                            cluster = democracias$elecid))
print(robust_se_cluster_modelo_sficativas)

# para pasar a excel
robust_se_cluster_df <- robust_se_cluster_modelo_sficativas[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(robust_se_cluster_modelo_sficativas))
writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

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
