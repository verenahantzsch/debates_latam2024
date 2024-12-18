
# https://arcruz0.github.io/libroadp/logit.html#representaci%C3%B3n-visual-de-los-resultados

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
         lnvoteshareincumbent = log(voteshareincumbent + 1))

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



# variantes modelo sficativas
formula_modelo_sficativas_variantes <- "dico_hubo_debates ~ 
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
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


modelo_sficativas_variantes <- glm(formula_modelo_sficativas_variantes,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes)



### borrador prueba Modelo lineal generalizado de efectos mixtos (ejemplo: binomial) ####

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
# democracias_scaled$cat_pais <- as.factor(democracias_scaled$cat_pais) # ESTOY TENIENDO ALGUN PROBLEMA QUE NO PUDE RESOLVER 
# vcov_cr2 <- clubSandwich::vcovCR(modelo_MLGEM, cluster = democracias_scaled$cat_pais, type = "CR2")
# robust_se <- clubSandwich::coef_test(modelo_MLGEM, vcov = vcov_cr2)
# print(robust_se)
# # Residuos y diagnósticos
# plot(resid(modelo_MLGEM))

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
### Preparaciones ####
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


#### importante: defino modelo de prueba ####

modelo_a_probar <- modelo_sficativas_variantes
#modelo_a_probar <- modelo_sficativas

### Control de resiudos ####

##### Graficar los residuos ####

par(mfrow = c(1, 2))

# Residuos deviance
residuals_dev <- residuals(modelo_a_probar, type = "deviance")
plot(residuals_dev, main = "Residuos Deviance", ylab = "Residuos", xlab = "Índice")
abline(h = 0, col = "red", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_dev), y = residuals_dev, 
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")  # Ajusta 'pos' para la posición del texto

# Residuos de Pearson
residuals_pearson <- residuals(modelo_a_probar, type = "pearson")
plot(residuals_pearson, main = "Residuos de Pearson", ylab = "Residuos", xlab = "Índice")
abline(h = 0, col = "blue", lty = 2)
# Añadir los IDs junto a los puntos
text(x = 1:length(residuals_pearson), y = residuals_pearson, 
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")  # Ajusta 'pos' para la posición del texto

 
# Residuos Pearsonestandarizados 
hat_values <- hatvalues(modelo_a_probar)  # Leverage
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
# COLOMBIA Y BRASIL 2018
# Tambien parece haber mas residuos de un signo que del otro (no me queda claro bien cual)

##### Durbin-Watson para autocorrelacion de errores ####
# prueba de Durbin-Watson (verificar autocorrelación en los residuos).
# The Durbin-Watson test has the null hypothesis that the autocorrelation of the disturbances is 0. 
# It is possible to test against the alternative that it is greater than, not equal to, or less than 0, respectively. This can be specified by the alternative argument.
# https://medium.com/@analyttica/durbin-watson-test-fde429f79203
# no es lo más adecuado de usar cuando tengo variables rezagadas (como es mi caso)
dwtest(modelo_a_probar)
# valor del estadistico: Una regla general que se sigue es: los valores de la estadística de prueba DW en el rango de 1,5 a 2,5 son relativamente aceptables. Los valores fuera de este rango podrían ser motivo de preocupación. Los valores inferiores a 1 o superiores a 3 son un motivo definitivo de preocupación.
# no podemos rechazar (tenemos evidencia favorable a) la H nula de que no hay autocorrelación
# A p-value of 0.3608 for a Durbin-Watson test indicates that the null hypothesis of no autocorrelation is not rejected

### Casos influyentes y outliers ####

# https://stats.stackexchange.com/questions/22161/how-to-read-cooks-distance-plots

##### Cook ####
# Obtener las estadísticas de Cook
cooks_distances <- cooks.distance(modelo_a_probar)

# Ver las primeras estadísticas de Cook
head(cooks_distances)

# Graficar las estadísticas de Cook
plot(cooks_distances, main = "Estadísticas de Cook", ylab = "Cook's Distance", xlab = "Índice de observación")
abline(h = 4 / (length(cooks_distances) - 14 - 1) , col = "red", lty = 2)  # Línea de corte común (influencia alta)
# Identificar observaciones influyentes (por encima del umbral)
influential_obs <- which(cooks_distances > 4 / length(cooks_distances))
text(x = influential_obs, y = cooks_distances[influential_obs], 
     labels = democracias$obsid[influential_obs], pos = 4, cex = 0.7, col = "blue")


# costa rica 1986 parece ser la mas problematica
# otras potenciales son brasil 2010 segunda ronda, ecuador 1988, guatemala 1999, colombia 2006, entre otras
# la posicuion de costa rica mejora en la variante respecto del de sficativas a secas, pero la de otros casos empeora, el balance quizas es relativamente neutro

# : Se suele trazar una línea de corte en 4 / n, donde n es el número de observaciones) para identificar observaciones que tienen una gran influencia en el modelo. Las observaciones por encima de esta línea pueden ser consideradas como influyentes.

car::influenceIndexPlot(modelo_a_probar, vars = c("Cook", "hat"))

# observaciones nr 90, 116 para cook , 77 y 78 para hat en modelo sficativas 
# observaciones nr 90, 116 para cook , 7 y 177 para hat en modelo sficativas, aunque tambien vale mencionar que se reducen las escalas del problema 

##### dfbetas ####

#Para cada observación, podemos ver la diferencia en la estimación del coeficiente para la intersección, la variable  disp y la variable  hp que ocurre cuando eliminamos esa observación en particular.
#Normalmente consideramos que una observación es muy influyente en la estimación de un coeficiente dado si tiene un valor DBETAS mayor que un umbral de 2/√ n, donde  n es el número de observaciones.
umbral <- 2 / sqrt(nrow(data_modelo_sficativas))

#especificar 2 filas y 1 columna en la región de trazado
par(mfrow=c(2,1))

#plot DFBETAS para marginvic
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$marginvic)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$marginvic,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# modelo sficativas: costa rica 2014 2 

plot(dfbetas$lnmarginvic)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lnmarginvic,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# modelo variantes: ninguna tan influeyente, eventualmente algunas problematicas 

# plot DFBETAS para nec  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$nec)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$nec,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: 7 obs tirando para abajo 

plot(dfbetas$lnnec)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lnnec,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: mismas observaciones tiran para abajo pero menos potentemente

#plot DFBETAS para voteshareincumbent 
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$voteshareincumbent)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$voteshareincumbent,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: 7 por debajo, 5 por arriba, nada demasiado exagerado

plot(dfbetas$lnvoteshareincumbent)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lnvoteshareincumbent,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: OJO parece haber mas observaciones por fuera del umbral, quizas esto empeora ** 

#plot DFBETAS para dico_reeleccion 
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$dico_reeleccion)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$dico_reeleccion,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: argentina 2011, bolivia 2014, ecuador 200 aparecen como las mas influyentes. rango +.4 a -.4 aprox
# sficativas: sin cambios muy notorios, algunas observaciones menos, otras mas inflyentes. 

#plot DFBETAS para propindivinternet  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$propindivinternet)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$propindivinternet,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: nada demasiado grave, unas 5 obs por debajo y por arriba, rango -.2 a +.2
# sficativas: no muy diferente, aunque mas obs tiran para abajo y menos para arriba 

#plot DFBETAS para accesogratuito  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$accesogratuito)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$accesogratuito,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: ecuador tira para arriba claramente, pero tb para abajo. honduras 1993 tb -.4
# variantes: no parecen observarse grandes cambios, quizás las observaciones un poco mas lejos del umbral (i.e. peor)

#plot DFBETAS para avgpropdebatesregionxciclo  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$avgpropdebatesregionxciclo)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$avgpropdebatesregionxciclo,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: panama 19994, peru 2001 son las mas llamativas, otros 4-5 casos por sobre el umbral, unos 98 por debajo, rango -.3 +.3
# variantes: 6 obs por debajo , mismas que en grafico anterior por arriba, quizas mas cerca del umbral

#plot DFBETAS para prop_elec_usa_ciclo  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$prop_elec_usa_ciclo)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$prop_elec_usa_ciclo,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: quitamos esta variable
# sficativas: ninguna por fuera del umbral

#plot DFBETAS para regulaciondico  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$regulaciondico)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$regulaciondico,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: salvo lo comentado abajo, idem . 5 obs por arriba (poquito) , 7 x debajo, son las mismas para los dos casos
# variantes: desempeño relativamente mejor del lado superior del umbral, que es mas estrecho. baja mexico 1994. pero empeora el lado inferior del umbral, en particular nicaragua 1990 # FIJAR QUE CATEGORIZACION ESTE CASO 

#plot DFBETAS para cumsum_pastciclos  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$cumsum_pastciclos)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$cumsum_pastciclos,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: unos 8 casos ligeramente sobre el umbral, unos 6 por debajo (rango aprox -.3 +.3)
# variantes: de nuevo parece emperoar nicaragua 1990. pero a la vez desaparece el salvador 2014 del umbral superior. Mejora el umbral inferior, menos casos

#plot DFBETAS para gdpxcapita  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$gdpxcapita)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$gdpxcapita,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: Mexico 1994 quizas el peor caso, aprox en .3, otros dos casos para arriba, varios casos uruguay tiran para abajo, tb argentina, entre otros (aprox 8 para abajo)

plot(dfbetas$lngdp)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lngdp,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: otra vez nicaragua 1990 un problema. definitivamente chequear! 3 casos por arriba (la mitad diferente del modelo sficativas)) y 8 por abajo, mejora el rango para abajo respecto de sficativas

#plot DFBETAS para democraciavdemelectoralcomp  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$democraciavdemelectoralcomp)   
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$democraciavdemelectoralcomp,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: aprox 5 sobre umbral, a hasta .3, 6debajo, -.3. Colombia 1986 tira bastante para abajo (junto a otras 7 aprox), bolivia 2019 y peru 1990 para arriba
# variantes: muy parecido. mejora ligeramente bolivia 2019. Sigue peru 1990 2 como problematico. mas observaciones se alejan del umbral para abajo (misma cantidad y casos que anterior), pero no mucho

#plot DFBETAS para mediaqualitycorruptvdem  
dfbetas <- as.data.frame(dfbetas(modelo_a_probar))

plot(dfbetas$mediaqualitycorruptvdem)   
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$mediaqualitycorruptvdem,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: otra vez peru 1990 como problematica para abajo.  unas 5 obs hacia arriba en .2. varias mas hacia abajo, nicaragua, paraguay, peru en rango -.3 (y peru 1990 en -.4)
# variantes: bastante parecido el lado superior. Aparecen algunas obs pero otras se acercan un poco mas. Peru tira muy para arriba y muy para abajo.  Peru 1990 sigue siendo la mas problematica. 8 arriba (.3 aprox), 10 abajo (varias cerquita y Peru 1990)

#### comentario ####

# de cook:
# costa rica 1986 parece ser la mas problematica
# otras potenciales son brasil 2010 segunda ronda, ecuador 1988, guatemala 1999, colombia 2006, entre otras
# la posicuion de costa rica mejora en la variante respecto del de sficativas a secas, pero la de otros casos empeora, el balance quizas es relativamente neutro

# de dfbetas
# conclusiones generales: los resultados no cambian mucho de sficativas a variantes. En algunos casos mejora, en otros empeora la situacion
# conviene quizas reestimar sin nicaragua 1990 primera vuelta y Peru 1990 segunda vuelta y examinar estos casos porque en varias variables aparecio como influyente # ATENTI !!!!! ###
# ademas parece que el pais peru y el pais ecuador estan teniendo algun efecto relevante para algunas variables para las que tiran para abajo y para arriba al mismo tiempo (en distintos años)


# reestimaciones sin estos casos: 
 
data_s_outliers <- democracias %>% 
 mutate(filtrar = ifelse(cat_pais=="Peru"&ncat_eleccion==1990&ncat_ronda==2|
                           cat_pais=="Nicaragua"&ncat_eleccion==1990&ncat_ronda==1|
                           cat_pais=="Costa Rica"&ncat_eleccion==1986&ncat_ronda==1, 
                         1, 0)) %>% 
  subset(filtrar==0)


modelo_sficativas_s_outliers <- glm(formula_modelo_sficativas,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = data_s_outliers)

modelo_sficativas_variantes_s_outliers <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data 
                                    data = data_s_outliers)
options(scipen=999)
summary(modelo_sficativas_s_outliers)
summary(modelo_sficativas_variantes_s_outliers)
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
# al quitar outliers... 
# simil anterior: dico_reeleccion pierde sficancia
# acceso gratuito gana sficancia 
# simil anterior:nec y regulaciondico mejoran ligeramente magnitud

# ppal conclusion: dico_reeleccion es SENSIBLE a algunos casos...

### Control de multicolinealidad ####
##### correlaciones (al menos <.5, conservador <2.5) ####

corr_base_democracias <- democracias %>% 
  select(dico_hubo_debates,
         #cat_pais,
         ncat_eleccion,
         ncat_ronda,
         #elecid,
         #obsid,
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

summary(corr_base_democracias)
correlation_matrix <- cor(corr_base_democracias , use = "pairwise.complete.obs" )
 
# Generar el gráfico con resaltado

# otra funcion de grafico
ggcorrplot::ggcorrplot(correlation_matrix, type = "lower", lab = T, show.legend = F)

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

vif_values1 <- car::vif(modelo_contingencia)   
print(vif_values1) 
# ninguno parece problematico

vif_values2 <- car::vif(modelo_sistemico)  
print(vif_values2)
# gdpxcapita problematico
# propindivinternet y democraciavdem border 

vif_values3 <- car::vif(modelo_regulatorio)  
print(vif_values3) 
# ninguno parece problematico

vif_values4 <- car::vif(modelo_temporal)  
print(vif_values4) 
# ninguno parece problematico. El mas border es democracia pero esta relativamente lejos (3.02)

vif_values5 <- car::vif(modelo_sficativas)  
print(vif_values5) 
# gdp per capita es problematico. 4.444897. idem propindivinternet: 5.32 
# Del resto, democracia vdem esta border. 3.87. El resto, el que sigue es cumsum 3.06

vif_values6 <- car::vif(modelo_sficativas_variantes)  
print(vif_values6) 
# mejora gdp (ahora ln: 3.455), tambien mejora democracia que estaba border (ahora 3.47). 
# internet mejora: 4.2, pero sigue problematico

#### otros? PENDIENTE ####
### Missing data PENDIENTE ####
### Fit, Overall significance y comparaciones ####
##### test de Wald ####

modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

lmtest::waldtest(modelo_a_probar)

# podemos rechazar H0 de que todos los coeficientes = 0
# mejora sficancia en caso variantes 

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
lmtest::waldtest(modelo_sficativas , modelo_sficativas_variantes)

# en gral el modelo mejora significativamente en todos los casos

##### lr test (comparar modelos con = cantidad de data, sacando missing) ####
# Chi-squared test of all coefficients An LR test of the hypothesis that all coefficients except the intercept(s) are zero can be computed by comparing the log likelihoods: LR= 2 ln L(MFull) − 2 ln L(MIntercept ). This statistic is sometimes designated as G2 . 
#  First, the two models must be nested. Second, the two models must be estimated on exactly the same sample. 
#lrtest(null_model, full_model)

# Obtener Log-Likelihood, mas alto mejor. solo se pueden comparar modelos anidados
logLik(modelo_a_probar)

# para hacer directamente el test
lrtest(modelo_0_reducido_sficativas, modelo_sficativas ) # -73.106 (pero mas parametros)
lrtest(modelo_0_reducido_sficativas, modelo_sficativas_variantes ) # -73.634
lrtest(modelo_contingencia_reducido_sficativas, modelo_sficativas)
lrtest(modelo_sistemico_reducido_sficativas, modelo_sficativas) # pendiente
lrtest(modelo_regulatorio_reducido_sficativas, modelo_sficativas) # pendiente
lrtest(modelo_temporal_reducido_sficativas, modelo_sficativas)


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
 
modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

# pseudo R cuadrados. #Mas alto, mejores
# https://www.rdocumentation.org/packages/rcompanion/versions/2.4.36/topics/nagelkerke
rcompanion::nagelkerke(modelo_a_probar)
# https://search.r-project.org/CRAN/refmans/fmsb/html/Nagelkerke.html 
fmsb::NagelkerkeR2(modelo_a_probar)
pscl::pR2(modelo_a_probar)

# Obtener AIC y BIC # mas bajos, mejores, aunque se penaliza la inclusion de mas parametros
AIC(modelo_a_probar)
BIC(modelo_a_probar)


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
stats6 <- stats( modelo_sficativas_variantes_reducido)

all_stats <- rbind(stats0,
                   stats1,
                   stats2,
                   stats3,
                   stats4,
                   stats5,
                   stats6 )

# comentario #
# los modelos full performan mejor que los modelos reducidos. Incluso con data reducida! 
# para el caso del AIC, notable que incluso con penalizacion por mas parametros, el valor del estadistico disminuye
# en todos los casos de pseudos R2, el modelo completo es mejor que los más incompletos, y la variante completa mejor que el completo

#### fit: desempeño: accuracy ####

modelo_a_probar <- modelo_sficativas
modelo_a_probar <- modelo_sficativas_variantes

data_modelo_sficativas$probabilidades_predichas <- predict(modelo_a_probar, type = "response")
data_modelo_sficativas$predicciones_binarias <- ifelse(data_modelo_sficativas$probabilidades_predichas>0.5,1,0)

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
countr26 <- funcion_count_R2(modelo_sficativas_variantes_reducido, data_reducida$dico_hubo_debates)

comparacion_count_r2_reducidas <- rbind(countr20 ,
                              countr21 ,
                              countr22 ,
                              countr23 ,
                              countr24 ,
                              countr25 ,
                              countr26 )

# en este caso el modelo variante performa peor
# los modelos completos mejoran la performance de los más incompletos

# quizas vale la pena comparar versiones con mas datos
# o hacer trabajo andidado de verdad, digamos

countr27 <- funcion_count_R2(modelo_sficativas, data_modelo_sficativas$dico_hubo_debates)
countr28 <- funcion_count_R2(modelo_sficativas_variantes, data_modelo_sficativas$dico_hubo_debates)

comparacion_count_r2_full <- rbind(countr27 ,
                                   countr28 )

# idem: el desempeño de la variante es ligeramente peor

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

robust_se_cluster_modelo_contingencia <- coeftest(modelo_contingencia, 
                                                  vcov = vcovCL(modelo_contingencia, 
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

robust_se_cluster_modelo_sistemico <- coeftest(modelo_sistemico, 
                                               vcov = vcovCL(modelo_sistemico,
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

robust_se_cluster_modelo_regulatorio <- coeftest(modelo_regulatorio, 
                                                 vcov = vcovCL(modelo_regulatorio, 
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

robust_se_cluster_modelo_temporal <- coeftest(modelo_temporal, 
                                              vcov = vcovCL(modelo_temporal, 
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

##### Modelo final / sficativas - variantes ####

robust_se_cluster_modelo_sficativas_variantes <- coeftest(modelo_sficativas_variantes, 
                                                vcov = vcovCL(modelo_sficativas_variantes, 
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes)

robust_se_cluster_modelo_sficativas_s_outliers <- coeftest(modelo_sficativas_variantes_s_outliers, 
                                                          vcov = vcovCL(modelo_sficativas_variantes_s_outliers, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = data_s_outliers$cat_pais))
print(robust_se_cluster_modelo_sficativas_s_outliers)

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_sficativas_variantes[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_sficativas_variantes))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

## INTERPRETACION MODELOS 1 VARIOS PENDIENTES ####

### importante elegir ####

modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
data_modelo_a_interpretar <- data_s_outliers

### calculo de probas predichas ####
data_modelo_a_interpretar$probabilidades_predichas <- predict(modelo_a_interpretar, type = "response")
data_modelo_a_interpretar$predicciones_binarias <- ifelse(data_modelo_a_interpretar$probabilidades_predichas>0.5,1,0)

### calculo ODDS RATIO #####
# a diferencia de la proba predicha, que varia de manera no lineal,
# el odds ratio es constante para distintos valores de x 

minlnnec <- data_modelo_a_interpretar$lnnec %>% min()
exp(minlnnec)
minlnnec1 <- minlnnec + 1
probaminlnnec
probaminlnnec1
probaminlnnec / probaminlnnec1
# un odds ratio mayor que 1 expresa un cambio positivo, mientras que si es menor que 1 (entre 0 y 1) representa un cambio negativo en las probabilidades estimadas.

# Para utilizar los odds ratios queremos mostrarte cómo puedes cambiar los coeficientes que obtienes directamente de la tabla, que son log odds, y reemplazarlos por odds ratios. Para ello, puedes usar los argumentos override.coef,override.se y override.pvalues de screenreg()
#summary(modelo_a_interpretar)
# OJO! que los errores estandar no estan bien calculados en la tabla a continuacion
texreg::screenreg(modelo_a_interpretar,
          custom.model.names = "data_modelo_a_interpretar - Odds Ratios",
          override.coef    = exp(coef(modelo_a_interpretar)),
          stars = c(0.001, 0.01, 0.05, 0.1),
          # la siguiente función, odds_*, están en el paquete del libro
         # override.se      = odds_se(modelo_a_interpretar), # no tengo esta funcion ! 
         # override.pvalues = odds_pvalues(modelo_a_interpretar),
          # además, omitiremos el coeficiente del intercepto
          omit.coef = "Inter")

# También podemos graficar los coeficientes como odds ratios: recuerda que los odds ratios menores que 1 son efectos negativos y mayores que 1 son positivos. El coeficiente se expresa como su valor medio y su intervalo de confianza del 95%. Si el coeficiente es estadísticamente significativo, su intervalo de confianza no pasará de la línea en 1. Si, por el contrario, no son significativos, el efecto cruzará la línea.

exp(coef(modelo_a_interpretar))

odds_ratios <- jtools::plot_summs(modelo_sficativas_variantes_s_outliers, #modelo_contingencia,
                          exp=T, 
                          scale = T,
                          inner_ci_level = .9,
                          # coefs = c("nombre coef" =  "variable",
                          #           ....),
                          model.names = c("modelo 1"#,
                                          #"modelo 2"
                                          ))
# 
# odds_ratios + labs(x = "Coeficientes exponenciados", y = NULL) # RARISIMO ! 

# interesante que nec y regulacion dico parecen tener los odds mas grandes

# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression revisar


### escenarios #####
# Crear un dataset base con valores constantes MIN-MEAN-MAX
data_to_predict <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                    max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                    #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                    length.out = 10), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  lnvoteshareincumbent = mean(data_modelo_a_interpretar$lnvoteshareincumbent, na.rm = TRUE),
  dico_reeleccion = 0, # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = mean(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=10) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE)
)

# Predecir probabilidades
data_to_predict$predicted_probs <- predict(modelo_a_interpretar, 
                                     newdata = data_to_predict, 
                                     type = "response")

# Graficar probabilidades predichas
# plot(data_to_predict$lnnec, data_to_predict$predicted_probs, 
#      type = "l", col = "blue", lwd = 2, 
#      xlab = "lnmarginvic", 
#      ylab = "Probabilidad Predicha", 
#      main = "Impacto de lnnec en la Probabilidad Predicha de un debate")

ggplot(data_to_predict) +
  geom_line(aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico)))



# version mas simple


predict_model <- prediction::prediction(
  modelo_a_interpretar, 
  data = data_to_predict,
  at = list(lnnec = unique(data_to_predict$lnnec))
)
summary(predict_model)

ggplot(summary(predict_model),
                      aes(x = `at(lnnec)`, y = Prediction,
                          ymin = lower, ymax = upper,
                          group = 1)) +
  geom_line() +
  geom_errorbar(width = 0.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "lnnec", y = "Pr(debate)")



cdat <- margins::cplot(modelo_a_interpretar, "lnnec", what = "prediction",
              main = "Pr(debate)", draw = F)

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Pr. debate",
       x = "lnnec", y = "Prob. predicha")

# El resumen nos ofrece un tibble donde tenemos la probabilidad prevista de nuestra variable dependiente para cada valor observado de poder_presid y su significado estadístico. Esto se convierte fácilmente en una cifra, y para ello hay dos alternativas que recomendamos.

### efecto marginal #####
#¿Cuál es el efecto medio del aumento de una unidad de la variable independiente en la probabilidad de que se produzca la variable dependiente? El Efecto Marginal Promedio (AME, por sus siglas en inglés) se utiliza para ver estos efectos, y se logra con el comando plot del paquete de margins.
#  el efecto marginal es la variación de la variable explicada cuando la variable explicativa aumenta en una unidad.

# efecto marginal promedio

marginal_ef <- margins::margins(modelo_a_interpretar)

plot(marginal_ef, 
     labels = c("lnmarginvic",
                "lnnec",
                "lnvoteshareincumbent",
                "dico_reeleccion",
                "propindivinternet",
                "accesogratuito",
                "avgpropdebatesregionxciclo",
                 "regulaciondico",
                 "cumsum_pastciclos",
                 "lngdp",
                 "democraciavdemelectoralcomp",
                 "mediaqualitycorruptvdem"),
     ylab = "AME")

# También podemos estar interesados, no en el efecto promedio, sino en el efecto marginal completo de una variable. Dada la no linealidad de estos modelos, el efecto marginal de una variable sobre la probabilidad de ocurrencia de la variable dependiente no es constante ni es significativo en toda la variable. Para ello utilizamos la función cplot del paquete margins y luego lo personalizamos con las opciones de ggplot2

marginal_nec <- margins::cplot(modelo_a_interpretar, "lnnec", what = "effect", 
                          main = "Ef.Marg(Quiebre)",
                          draw = F)

ggplot(marginal_nec, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Modelo a interpretar",
       x = "NEC", y = "Efecto marginal")

# el efecto de la variable disminuye a medida que aumenta su valor, consistente con la idea que tenemos de que su efecto es log
# el efecto es sficativo para todos los valores de la variable




###  borrador agrego errores estandar  #####
# 
# # a las proba predichas (chatgpt)
# # Simular coeficientes del modelo con matriz robusta
# sim_coef <- arm::sim(modelo_a_interpretar, n.sims = 1000, vcovCL(modelo_a_interpretar,
#                                                             cluster = data_modelo_a_interpretar$cat_pais))
# # Obtener predicciones simuladas
# X <- model.matrix(modelo_a_interpretar)
# pred_sim <- plogis(X %*% t(coef(sim_coef)))  # Probabilidades predichas simuladas
# 
# # Calcular media e intervalos de confianza
# data_modelo_a_interpretar$probabilidad_media <- rowMeans(pred_sim)
# data_modelo_a_interpretar$probabilidad_ci_inf <- apply(pred_sim, 1, quantile, probs = 0.025)
# data_modelo_a_interpretar$probabilidad_ci_sup <- apply(pred_sim, 1, quantile, probs = 0.975)
# 
# # a los escenarios # (chatgpt)
# 
# 
# 
# # Obtener matriz de varianza-covarianza robusta clusterizada
# vcov_cluster <- vcovCL(modelo_a_interpretar, 
#                        cluster = data_modelo_a_interpretar$cat_pais)
# 
# # Crear un objeto "coeftest" con los errores estándar robustos
# robust_model <- coeftest(modelo_a_interpretar, vcov. = vcov_cluster)
# 
# # Matriz de diseño para las predicciones
# X <- model.matrix(~ lnnec + lnmarginvic + lnvoteshareincumbent + 
#                     dico_reeleccion + propindivinternet + accesogratuito + 
#                     avgpropdebatesregionxciclo + regulaciondico + 
#                     cumsum_pastciclos + lngdp + democraciavdemelectoralcomp + 
#                     mediaqualitycorruptvdem, 
#                   data = data_to_predict)
# 
# # Coeficientes estimados
# beta <- coef(robust_model)
# 
# # Probabilidades predichas
# # data_to_predict$predicted_probs2 <- 
# # test <- plogis(X %*% beta) #%>% tibble()
# 
# 
# # Varianza de las predicciones
# pred_var <- diag(X %*% vcov_cluster %*% t(X))
# 
# # Calcular errores estándar
# se <- sqrt(pred_var)
# 
# # Intervalos de confianza
# data_to_predict$lower_ci <- plogis(qlogis(data_to_predict$predicted_probs) - 1.96 * data_to_predict$se)
# data_to_predict$upper_ci <- plogis(qlogis(data_to_predict$predicted_probs) + 1.96 * data_to_predict$se)
# 
# 
# # Graficar probabilidades predichas con bandas de confianza
# ggplot(data_to_predict, 
#        aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico))) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = as.factor(regulaciondico)), 
#               alpha = 0.2, colour = NA) +
#   labs(
#     x = "nec (Escala Original)",
#     y = "Probabilidad Predicha",
#     colour = "Regulación Dicótica",
#     fill = "Regulación Dicótica",
#     title = "Impacto del NEC en la Probabilidad Predicha de que ocurra un debate, con Intervalos de Confianza"
#   ) +
#   theme_minimal() +
#   scale_colour_manual(values = c("blue", "red")) +
#   scale_fill_manual(values = c("blue", "red"))

# # Para hacer tabla de escenarios - Crear combinaciones de varios valores
# scenarios <- expand.grid(
#   lnmarginvic = seq(0, 5, 1),
#   dico_reeleccion = c(0, 1),
#   accesogratuito = mean(data_modelo_sficativas$accesogratuito, na.rm = TRUE),
#   # Agregar otras variables necesarias...
# )
# 
# # Calcular probabilidades predichas para cada escenario
# scenarios$predicted_probs <- predict(modelo_sficativas, newdata = scenarios, type = "response")
# 
# # Ver resultados
# print(scenarios)

## MULTINIVEL MODELOS 1 ####

### Version artesanal ####

#### calculo de media dico_hubo_debates, en general y x pais #####


mean_dico_hubo_debates <- data_modelo_a_interpretar$dico_hubo_debates %>% mean()

mean_dico_hubo_debatesxpais <- data_modelo_a_interpretar %>% 
  group_by(cat_pais) %>% 
  summarise(mean_dico_hubo_debatesxpais = mean(dico_hubo_debates),
         diff_mean_general = mean_dico_hubo_debates - mean_dico_hubo_debatesxpais,
         abs_diff_mean_general = abs(diff_mean_general)) %>% 
  arrange(desc(abs_diff_mean_general))


# uruguay es nuestro país de referencia



#### creamos variables dico ####
data_modelo_a_interpretar$cat_pais %>% unique()
data_modelo_a_interpretar <- data_modelo_a_interpretar %>% 
  mutate(dico_argentina = ifelse(cat_pais=="Argentina",1,0),
         dico_bolivia = ifelse(cat_pais=="Bolivia",1,0),
         dico_brasil = ifelse(cat_pais=="Brasil",1,0),
         dico_chile = ifelse(cat_pais=="Chile",1,0),
         dico_colombia = ifelse(cat_pais=="Colombia",1,0),
         dico_crica = ifelse(cat_pais=="Costa Rica",1,0),
         dico_ecuador = ifelse(cat_pais=="Ecuador",1,0),
         dico_elslv = ifelse(cat_pais=="El Salvador",1,0),
         dico_guatemala = ifelse(cat_pais=="Guatemala",1,0),
         dico_honduras = ifelse(cat_pais=="Honduras",1,0),
         dico_mx = ifelse(cat_pais=="Mexico",1,0),
         dico_nicaragua = ifelse(cat_pais=="Nicaragua",1,0),
         dico_panama = ifelse(cat_pais=="Panama",1,0),
         dico_paraguay = ifelse(cat_pais=="Paraguay",1,0),
         dico_peru = ifelse(cat_pais=="Peru",1,0),
         dico_uruguay = ifelse(cat_pais=="Uruguay",1,0), # cat de referencia 
         dico_repdom = ifelse(cat_pais=="Republica Dominicana",1,0),
         dico_venezuela = ifelse(cat_pais=="Venezuela",1,0))

#### corremos modelo incluyendo dico_pais sin cat de referencia ####
formula_modelo_multinivel <- "dico_hubo_debates ~  # MODELO SFICATIVAS VARIANTES
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem +
                          dico_argentina +
                          dico_bolivia +
                          dico_brasil +
                          dico_chile +
                          dico_colombia +
                         dico_crica +
                         dico_ecuador +
                         dico_elslv +
                         dico_guatemala +
                         dico_honduras +
                         dico_mx +
                         dico_nicaragua +
                         dico_panama+
                         dico_paraguay +
                         dico_peru +
                         dico_repdom +
                         dico_venezuela"


modelo_multinivel <- glm(formula_modelo_multinivel,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = data_modelo_a_interpretar)
options(scipen=999)
summary(modelo_multinivel)

# OJO no se si corresponde clusterizar por pais si ya incorporamos pais
robust_se_cluster_modelo_multinivel <- coeftest(modelo_multinivel, 
                                                vcov = vcovCL(modelo_multinivel, 
                                                 cluster = data_modelo_a_interpretar$cat_pais))
print(robust_se_cluster_modelo_multinivel)



#### modelo interactivo #####



formula_modelo_multinivel_interactivo <- "dico_hubo_debates ~  # MODELO SFICATIVAS VARIANTES
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem +
                          dico_argentina +
                          dico_bolivia +
                          dico_brasil +
                          dico_chile +
                          dico_colombia +
                         dico_crica +
                         dico_ecuador +
                         dico_elslv +
                         dico_guatemala +
                         dico_honduras +
                         dico_mx +
                         dico_nicaragua +
                         dico_panama+
                         dico_paraguay +
                         dico_peru +
                         dico_repdom +
                         dico_venezuela +  
                          dico_argentina*lnnec +
                          dico_bolivia*lnnec +
                          dico_brasil*lnnec +
                          dico_chile*lnnec +
                          dico_colombia*lnnec +
                         dico_crica*lnnec +
                         dico_ecuador*lnnec +
                         dico_elslv*lnnec +
                         dico_guatemala*lnnec +
                         dico_honduras*lnnec +
                         dico_mx*lnnec +
                         dico_nicaragua*lnnec +
                         dico_panama*lnnec +
                         dico_paraguay*lnnec +
                         dico_peru*lnnec +
                         dico_uruguay*lnnec + # no estoy segura de si agregar
                         dico_repdom*lnnec +
                         dico_venezuela*lnnec  "


modelo_multinivel_interactivo <- glm(formula_modelo_multinivel_interactivo,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = data_modelo_a_interpretar)
options(scipen=999)
summary(modelo_multinivel_interactivo)

# OJO no se si corresponde clusterizar por pais si ya incorporamos pais
robust_se_cluster_modelo_multinivel_interactivo <- coeftest(modelo_multinivel_interactivo, 
                                                vcov = vcovCL(modelo_multinivel_interactivo, 
                                                              cluster = data_modelo_a_interpretar$cat_pais))
print(robust_se_cluster_modelo_multinivel_interactivo)


## EXPORTO MODELOS #####

# Una vez que estimamos los modelos que irán en la tabla, los agrupamos en una lista usando la función list. Esto ahorra tiempo, porque en lugar de tener que escribir el nombre de los modelos, simplemente nos referiremos a la lista mp_models:
 
mp_models <- texreg::list(modelo_a_interpretar)
htmlreg(mp_models,
        custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3"),
        custom.coef.names = c("Intercepto", ".... " ),
        file="tabla_1.html") # nombre de su archivo html. Se guardará en tu  directorio de trabajo por defecto.
# con funcion screenreg podemos chequear tablas antes de exportarlas



# MODELOS 2: submuestra sin debates en la eleccion pasada. "Nueva práctica"  #####
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
