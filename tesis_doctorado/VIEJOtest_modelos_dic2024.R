
# https://arcruz0.github.io/libroadp/logit.html#representaci%C3%B3n-visual-de-los-resultados

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
                              lnvoteshareincumbent +
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
                                    #edadregimenbmr"

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
#edadregimenbmr"

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
                         mediaqualitycorruptvdem#,
                         #edadregimenbmr,"

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
                         mediaqualitycorruptvdem#,
                         #edadregimenbmr,"

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
                           #lnvoteshareincumbent +
                           voteshareincumbent +
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


### Modelo final / sficativas variantes + interaccion ####
# variantes modelo sficativas
formula_modelo_sficativas_variantes_interactivo <- "dico_hubo_debates ~ 
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
                          regulaciondico*lnnec +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem#, #+
                          # ncat_eleccion,
                          #edadregimenbmr#, "


modelo_sficativas_variantes_interactivo <- glm(formula_modelo_sficativas_variantes_interactivo,
                                   family = binomial(link = "logit"), 
                                   #data = data 
                                   data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes_interactivo)



### Modelo all #####

formula_modelo_all <- "dico_hubo_debates ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           lnvoteshareincumbent +
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

### Control de resiudos ####

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
data_modelo_a_probar[c(162,32,61),]
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

### Casos influyentes y outliers ####

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
data_modelo_a_probar[c(20,61,77,127,162),"obsid"]
4/nrow(data_modelo_a_probar)
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

#plot DFBETAS para marginvic
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$marginvic)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$marginvic,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# modelo sficativas: costa rica 2014 2 

plot(dfbetas$lnmarginvic)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lnmarginvic,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# modelo variantes: ninguna tan influeyente, eventualmente algunas problematicas 

# plot DFBETAS para nec  
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$nec)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$nec,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# sficativas: 7 obs tirando para abajo 

plot(dfbetas$lnnec)  #, type=' h ')
abline(h = umbral, lty = 2)
abline(h = -umbral, lty = 2)
text(dfbetas$lnnec,
     labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

# variantes: mismas observaciones tiran para abajo pero menos potentemente

#plot DFBETAS para voteshareincumbent 
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$voteshareincumbent)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$voteshareincumbent,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

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
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$propindivinternet)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$propindivinternet,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

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
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$prop_elec_usa_ciclo)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$prop_elec_usa_ciclo,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

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
# dfbetas <- as.data.frame(dfbetas(modelo_a_probar))
# 
# plot(dfbetas$gdpxcapita)  #, type=' h ')
# abline(h = umbral, lty = 2)
# abline(h = -umbral, lty = 2)
# text(dfbetas$gdpxcapita,
#      labels = data_modelo_sficativas$obsid, pos = 4, cex = 0.7, col = "blue")

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
### Missing data PENDIENTE ####
# jack knife
# imputacion: 
# reemp x valor promedio, 
# reemp x mediana, 
# mcmc multiple imputation approach -> simulacion + iteraciones
# comparar modelos 
### Fit, Overall significance y comparaciones ####
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

#### lr test (comparar modelos con = cantidad de data, sacando missing) ####
# Chi-squared test of all coefficients An LR test of the hypothesis that all coefficients except the intercept(s) are zero can be computed by comparing the log likelihoods: LR= 2 ln L(MFull) − 2 ln L(MIntercept ). This statistic is sometimes designated as G2 . 
#  First, the two models must be nested. Second, the two models must be estimated on exactly the same sample. 
#lrtest(null_model, full_model)

# Obtener Log-Likelihood, mas alto mejor. solo se pueden comparar modelos anidados
logLik(modelo_a_probar)

# para hacer directamente el test
lrtest(modelo_0_reducido_sficativas, modelo_sficativas ) # -73.106 (pero mas parametros)
lrtest(modelo_0_reducido_sficativas, modelo_sficativas_variantes ) # -73.634
lrtest(modelo_contingencia_reducido, modelo_sficativas_variantes_reducido)
# lrtest(modelo_sistemico_reducido_sficativas, modelo_sficativas) # pendiente
# lrtest(modelo_regulatorio_reducido_sficativas, modelo_sficativas) # pendiente
#lrtest(modelo_temporal_reducido_sficativas, modelo_sficativas)

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
rcompanion::nagelkerke(modelo_a_probar)
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

all_count_r2_full <- rbind(countr27,   countr28 )

countr2_modelo <- funcion_count_R2(modelo_a_probar, data_modelo_a_probar$dico_hubo_debates)

stats_modelo <- stats_modelo %>% 
  rbind( countr2_modelo %>% 
          pivot_longer(cols= c(countr2, adjcountr2), names_to = "stat", values_to = "value")  )

stats_modelo %>% write_csv("anexos/stats_modelo.csv")

# idem: el desempeño de la variante es ligeramente peor

## MODELOS ROBUSTOS #####

#### https://www.geeksforgeeks.org/different-robust-standard-errors-of-logit-regression-in-stata-and-r/

##### REESTIMACION sin outliers ##### 

# # otros potenciales: Colombia 1998 1 , 
# El salvador 2014 2 para arriba marginvic. 
# Brasil 1980 2. 
# Colombia 1986 1era . 
 

data_s_outliers <- democracias %>% 
  mutate(filtrar = ifelse(cat_pais=="Brasil"&ncat_eleccion==2018&ncat_ronda==2|
    cat_pais=="Colombia"&ncat_eleccion==2018&ncat_ronda==2|
    cat_pais=="Peru"&ncat_eleccion==1990&ncat_ronda==2|
      cat_pais=="Nicaragua"&ncat_eleccion==1990&ncat_ronda==1|
      cat_pais=="Costa Rica"&ncat_eleccion==1986&ncat_ronda==1|
      cat_pais=="Republica Dominicana"&ncat_eleccion==2020&ncat_ronda==1|
      cat_pais=="Costa Rica"&ncat_eleccion==2014&ncat_ronda== 2|
      cat_pais=="Republica Dominicana"&ncat_eleccion== 2016&ncat_ronda== 1|
      cat_pais=="Ecuador"&ncat_eleccion==2017&ncat_ronda==2, 
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

modelo_sficativas_variantes_s_outliers <- glm(formula_modelo_sficativas_variantes,
                                              family = binomial(link = "logit"), 
                                              #data = data 
                                              data = data_s_outliers)
options(scipen=999)

summary(modelo_sficativas_variantes_s_outliers)
summary(modelo_sficativas_variantes)

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

##### Modelo final / sficativas - variantes ####

robust_se_cluster_modelo_sficativas_variantes <- coeftest(modelo_sficativas_variantes, 
                                                vcov = vcovCL(modelo_sficativas_variantes, 
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes)

robust_se_cluster_modelo_sficativas_variantes_s_outliers <- coeftest(modelo_sficativas_variantes_s_outliers, 
                                                          vcov = vcovCL(modelo_sficativas_variantes_s_outliers, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = data_s_outliers$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes_s_outliers)

robust_se_cluster_modelo_sficativas_variantes_interactivo <- coeftest(modelo_sficativas_variantes_interactivo, 
                                                                     vcov = vcovCL(modelo_sficativas_variantes_interactivo, 
                                                                                   #cluster = democracias$elecid))
                                                                                   #cluster = data$elecid))
                                                                                   cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes_interactivo)

#### Modelo all ####


robust_se_cluster_modelo_all <- coeftest(modelo_all, 
                                         vcov = vcovCL(modelo_all,
                                                       cluster = democracias$cat_pais))

print(robust_se_cluster_modelo_all)


## EXPORTO MODELOS #####

# # para pasar a excel
# robust_se_cluster_df <- robust_se_cluster_modelo_sficativas_variantes[,] %>% 
#   as_tibble() %>%
#   mutate(variable = rownames(robust_se_cluster_modelo_sficativas_variantes))
# writexl::write_xlsx(robust_se_cluster_df, "robust_se_cluster.xlsx")

# Una vez que estimamos los modelos que irán en la tabla, los agrupamos en una lista usando la función list. Esto ahorra tiempo, porque en lugar de tener que escribir el nombre de los modelos, simplemente nos referiremos a la lista mp_models:

#mp_models <- texreg::list(modelo_a_interpretar)
# htmlreg(mp_models,
#         custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3"),
#         custom.coef.names = c("Intercepto", ".... " ),
#         file="tabla_1.html") # nombre de su archivo html. Se guardará en tu  directorio de trabajo por defecto.
# con funcion screenreg podemos chequear tablas antes de exportarlas

lista1 <-  list(
  robust_se_cluster_modelo_contingencia,
  robust_se_cluster_modelo_sistemico,
  robust_se_cluster_modelo_regulatorio,
  robust_se_cluster_modelo_temporal,
  robust_se_cluster_modelo_sficativas_variantes
) 

lista2 <-  list(
  robust_se_cluster_modelo_sficativas_variantes,
  robust_se_cluster_modelo_sficativas,
  robust_se_cluster_modelo_sficativas_variantes_s_outliers
) 

# lista3 <-  list(robust_se_cluster_modelo_sficativas_variantes,
#                 robust_se_cluster_modelo_multinivel,
#                 robust_se_cluster_modelo_multinivel_interactivo)

# https://www.rdocumentation.org/packages/texreg/versions/1.39.4/topics/htmlreg

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
                                      "log Votos oficialista",
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
                file="anexos/tabla_1_bis.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

texreg::htmlreg(lista2,
                custom.model.names = c("Final",
                                       "Variante",
                                       "Final s/ casos influyentes")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercepto)",
                                      "log Margen de victoria",
                                      "log NEC",
                                      "log Votos oficialista",
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
                                      "Votos oficialista",
                                      "Prop. debates en USA" ,
                                      "PBI per Capita"
                ),
                reorder.coef =  c(1,
                                  14,
                                  2,
                                  15,
                                  3,
                                  16,
                                  4,
                                  5,
                                  
                                  6,
                                  7,
                                  8,
                                  17,
                                  
                                  9,
                                  10,
                                  18,
                                  11,
                                  12,
                                  13
                ),
                file="anexos/tabla_2_bis.html",
                caption = "Todos los modelos están calculados con errores estándar agrupados por país",
                center = T,
                bold = 0.1)

# texreg::htmlreg(lista3,
#                 custom.model.names = c("Final con variantes",
#                                        "Final con variantes sin outliers",
#                                        "Multinivel",
#                                        "Multinivel interactivo")  ,
#                 stars = c(0.001, 0.01, 0.05, 0.1),
#                 # custom.coef.names = c("Intercepto",                             
#                 # "log Margen de victoria",
#                 # "log NEC" ,
#                 # "log Votos oficialista",
#                 # "Incumbente reelije",
#                 # "Regulacion sobre debates",
#                 # "Cant. elecciones pasadas con debates",
#                 # "log PBI per Capita",
#                 # "Democracia electoral (VDEM)",
#                 # "Corrupcion de medios (VDEM)"),
#                 file="anexos/tabla_3_bis.html",
#                 caption = "Todos los modelos están calculados con errores estándar agrupados por país",
#                 center = T,
#                 bold = 0.1)



## INTERPRETACION MODELOS 1 VARIOS PENDIENTES ####

### importante elegir ####

modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
data_modelo_a_interpretar <- data_s_outliers 

modelo_a_interpretar <- modelo_sficativas_variantes
data_modelo_a_interpretar <- democracias 

# t the coefficient for X is the difference in the log odds.  https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404.

### ODDS RATIO #####

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

### ESCENARIOS CON VALORES PREDICHOS #####
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

data_to_predict <- data.frame(
  lnnec = rep(valores_relevantes, 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  lnvoteshareincumbent = mean(data_modelo_a_interpretar$lnvoteshareincumbent, na.rm = TRUE),
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
data_to_predict$predicted_probs <- predict(modelo_a_interpretar, 
                                           newdata = data_to_predict, 
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

tabla_data_to_predict <- data_to_predict %>% 
  mutate(nec = exp(lnnec)) %>% 
  select(nec, regulaciondico, predicted_probs) %>% 
  pivot_wider(names_from = regulaciondico,
              values_from = predicted_probs) %>% 
  mutate(across(everything(),  ~  round(., 2))) %>% 
  left_join(referencias) %>% 
  arrange(nec) 




# grafico

ggplot(data_to_predict) +
  geom_line(aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico)))

# version generica
data_to_predict <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                    max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                    #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                    length.out = 20), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  lnvoteshareincumbent = mean(data_modelo_a_interpretar$lnvoteshareincumbent, na.rm = TRUE),
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
predicted_probs <- predict(modelo_a_interpretar, 
                           newdata = data_to_predict, 
                           type = "response",
                           se.fit = T)  

data_to_predict$predicted_probs <- predicted_probs$fit
data_to_predict$se_predicted_probs <- predicted_probs$se.fit

# grafico

plot_interpretacion <- ggplot(data_to_predict) +
  geom_line(aes(x = exp(lnnec), 
                y = predicted_probs, 
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  predicted_probs - 1.645*se_predicted_probs, 
                  ymax =  predicted_probs + 1.645*se_predicted_probs, 
                  fill = as.factor(regulaciondico)), alpha = 0.3) +
  theme_classic() +
  labs(
    title = "Probabilidad predicha de ocurrencia de un debate presidencial",
    subtitle = "Para distintos valores dentro del rango observado de NEC, con y sin regulación",
    caption = "Elaboración propia. Intervalos de confianza ploteados al 90%",
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
 

# nec es especialmente relevante cuando no hay regulacion

# algunas cuentas para reportar verbalmente
data_to_predict <- data.frame(
  lnnec = rep(c(log(2), log(3)), 2), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  lnvoteshareincumbent = mean(data_modelo_a_interpretar$lnvoteshareincumbent, na.rm = TRUE),
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
predicted_probs <- predict(modelo_a_interpretar, 
                           newdata = data_to_predict, 
                           type = "response",
                           se.fit = T)  

data_to_predict$predicted_probs <- predicted_probs$fit
data_to_predict$se_predicted_probs <- predicted_probs$se.fit

data_to_predict %>% 
  group_by(regulaciondico) %>% 
  summarise(diff_lnnec = diff(predicted_probs))

data_to_predict %>% 
  group_by(lnnec) %>% 
  summarise(diff_regulacion = diff(predicted_probs))
  
# version mas simple #
# no esta claro a qué valores de las demás variables me esta caluclando los graficos, pendiente ver
vcov_cluster <- vcovCL(modelo_a_interpretar, 
                       cluster = data_modelo_a_interpretar$cat_pais)

predict_model <- prediction::prediction(
  modelo_a_interpretar, 
  data = data_to_predict,
  vcov = vcov_cluster,
  at = list(lnnec = unique(data_to_predict$lnnec),
            regulaciondico =  unique(data_to_predict$regulaciondico))
)
summary(predict_model)

# Convertir a data frame
pred_df <- as.data.frame(predict_model)

# Graficar
ggplot(pred_df, 
       aes(x = exp(lnnec), 
           y = fitted, 
           colour = as.factor(regulaciondico))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = fitted - 1.96*se.fitted, 
                  ymax =  fitted + 1.96*se.fitted, 
                  fill = as.factor(regulaciondico)), 
              alpha = 0.2) +
  labs(
    x = "exp(lnnec) = NEC",
    y = "Probabilidad Predicha de un debate",
    colour = "Regulación Dicotómica",
    fill = "Regulación Dicotómica",
    title = "Predicciones a diferentes valores de lnnec y regulaciondico"
  ) +
  theme_minimal() +
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"))

# otra version

cdat <- margins::cplot(modelo_a_interpretar, 
                       "lnnec", 
                       what = "prediction",
                       #vcov = vcov_cluster, # no veo que cambie mucho
              main = "Pr(debate)", 
              draw = F)

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "Pr. debate",
       x = "lnnec", y = "Prob. predicha")
  

### EFECTO MARGINAL #####

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

##### Efecto marginal promedio (Average Marginal Effect, AME): ####

# Se calcula el efecto marginal para cada observación en el conjunto de datos y luego se promedian estos efectos.
# Es útil para interpretar un "efecto típico" en la muestra.

#¿Cuál es el efecto medio del aumento de una unidad de la variable independiente en la probabilidad de que se produzca la variable dependiente? El Efecto Marginal Promedio (AME, por sus siglas en inglés) se utiliza para ver estos efectos, y se logra con el comando plot del paquete de margins.
#  el efecto marginal es la variación de la variable explicada cuando la variable explicativa aumenta en una unidad.

# efecto marginal promedio

marginal_effects <- margins::margins(modelo_a_interpretar)

marginals_df <- summary(marginal_effects)

plot_margins <- ggplot(marginals_df) +
  geom_point( aes(x = factor, y = AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(title = "Efectos marginales promedio",
       subtite = "Modelo final",
       x = "Predictores", 
       y = "Efectos marginales promedio",
       caption = "Elaboración propia") +
  theme_minimal()

margins::marginal_effects(margins::margins(modelo_a_interpretar))


#### efecto marginal por variable #####
# También podemos estar interesados, no en el efecto promedio, sino en el efecto marginal completo de una variable.
# Dada la no linealidad de estos modelos, el efecto marginal de una variable sobre la probabilidad de ocurrencia de la variable dependiente no es constante ni es significativo en toda la variable. 
# Para ello utilizamos la función cplot del paquete margins y luego lo personalizamos con las opciones de ggplot2

# el efecto de la variable disminuye a medida que aumenta su valor, consistente con la idea que tenemos de que su efecto es log
# el efecto es sficativo para todos los valores de la variable

#Columns containing marginal effects are distinguished by their name (prefixed by dydx_). These columns can be extracted from a “margins” object using, for example, marginal_effects(margins(model)). Columns prefixed by Var_ specify the variances of the average marginal effects, whereas (optional) columns prefixed by SE_ contain observation-specific standard errors


pred_lnnec <- margins::margins(modelo_a_interpretar, 
                      data = data_to_predict, #%>% subset(regulaciondico==1),
                      vcov = vcov_cluster, 
                      variables = "lnnec")
summary(pred_lnnec)  # Resumen de los efectos marginales

pred_lnnec <- pred_lnnec %>%
  mutate(SE = sqrt(Var_dydx_lnnec))


# Graficar el efecto marginal
ggplot(pred_lnnec, aes(x = lnnec, y = dydx_lnnec, color = as.factor(regulaciondico))) +
  geom_line() +
  geom_ribbon(aes(ymin = dydx_lnnec - SE, ymax = dydx_lnnec + SE, color = as.factor(regulaciondico)), alpha = 0.2) +
  labs(x = "lnnec", y = "Efecto marginal", title = "Efecto marginal de lnnec") +
  theme_minimal()

# version mas simple, pero no me queda claro a que valores estan las demas variables 

cdat1 <- margins::cplot(modelo_a_interpretar, 
                        "lnnec", 
                        what = "effect", 
                        vcov = vcov_cluster, 
                        main = "Efecto marginal de nec sobre proba de debates",
                        scattter = T) # para guardar en lugar de plotear automaticamente

ggplot(cdat1, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(title = " Efecto de lnnec",
       x = "lnnec", y = "Efecto marginal") 

 
# para regulaciondico # ojo no se si lo que estoy haciendo está bien o tiene sentido 

pred_regulaciondico <- margins::margins(modelo_a_interpretar, 
                                        data = data_to_predict, #%>% subset(regulaciondico==1),
                                        vcov = vcov_cluster, 
                                        variables = "regulaciondico")
summary(pred_regulaciondico)  # Resumen de los efectos marginales


pred_regulaciondico <- pred_regulaciondico %>%
  mutate(SE = sqrt(Var_dydx_regulaciondico))

effect_regulacion <- pred_regulaciondico %>% 
  group_by(lnnec) %>% 
  summarise(
    efecto_marg_regulaciondico = predicted_probs[regulaciondico == 1] - 
      predicted_probs[regulaciondico == 0],
    SE = mean(SE)
  )

ggplot(effect_regulacion) + 
  geom_point( aes(x = lnnec, y = efecto_marg_regulaciondico)) +
  geom_errorbar(aes(x = lnnec, 
                    ymin = efecto_marg_regulaciondico - 1.96*SE,
                    ymax = efecto_marg_regulaciondico + 1.96*SE), width = 0.2)   

# cuenta simple para valor medio
  
diff(pred_regulaciondico$predicted_probs)
cat("Efecto marginal de regulaciondico 
    cuando el resto de las variables están en sus medias:", effect_regulacion, "\n")

### visualizacion de interacciones para modelo INTERACTIVO ####

persp(modelo_sficativas_variantes_interactivo, 
      "regulaciondico", "lnnec", 
      what = "prediction",
      #ylab = "Desigualdad", xlab = "Crecimiento económico",
      zlab = "Probabilidad predicha" )

image(modelo_sficativas_variantes_interactivo,  
      "regulaciondico", "lnnec")#, 
      #xlab = "Crecimiento económico", ylab = "Desigualdad",
      #zlab = "Probabilidad predicha")

summary(modelo_sficativas_variantes_interactivo)

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
                         dico_argentina + # DICO POR PAIS
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
                         dico_argentina*lnnec + # INTERACCIONES
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

### Version formula #####
# https://www.publichealth.columbia.edu/research/population-health-methods/multi-level-modeling


####  PASO 1 MODELO MULTILEVEL: EMPTY #### 

# Varying-intercept model with no predictors. 
#   M0 <- lmer (y ~ 1 + (1 | county))
# This model simply includes a constant term (the predictor “1”) and allows it to vary by county

empty_model <- lme4::glmer(dico_hubo_debates ~ 
                      1 + (1 | cat_pais), 
                    family=binomial("logit"), 
                    data = democracias)
summary(empty_model)

####  PASO 2 MODELO MULTILEVEL: RANDOM INTERCEPTS BY COUNTRY #### 
# varying-intercept model
# Varying-intercept model with an individual-level predictor. 
# M1 <- lmer (y ~ x + (1 | county))
# This expression starts with the no-pooling model, “y ~ x,” and then adds “(1 | county),” 
# which allows the intercept (the coeﬃcient of the predictor “1,” which is  the column of ones—the constant term in the regression) to vary by county.

modelo_random_intercepts <- lme4::glmer(dico_hubo_debates ~ 
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
                                          (1 | cat_pais), 
                                        family=binomial("logit"), 
                                        data = democracias)
summary(modelo_random_intercepts)
modelo_random_intercepts
coef(modelo_random_intercepts)
lme4::fixef(modelo_random_intercepts)  # The estimated regression line in an average county
# The term “ﬁxed eﬀects” is used for the regression coeﬃcients that do not vary by group (such as the coeﬃcient for x in this example) or for group-level coeﬃcients or group averages (such as the average intercept, μα in (12.3)).
lme4::ranef(modelo_random_intercepts)  #  how much the intercept is shifted up or down in particular counties or county level errors

 

####  PASO 3 MODELO MULTILEVEL: RANDOM INTERCEPTS BY COUNTRY AND RANDOM SLOPES BY NEC) #### 
#  Varying intercepts and slopes
# M3 <- lmer (y ~ x + (1 + x | county))

modelo_random_slopes1 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                  # (1 + lnnec | cat_pais), 
                                    "(1 + regulaciondico | cat_pais)", sep = "+" ), 
                                 family=binomial("logit"), 
                                 data = democracias)
summary(modelo_random_slopes1)

modelo_random_slopes2 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                           " (1 + lnnec | cat_pais)",  sep = "+" ), 
                                     family=binomial("logit"), 
                                     data = democracias)
summary(modelo_random_slopes2)

coef(modelo_random_slopes2)


modelo_random_slopes3 <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                           " (1 + lnnec + regulaciondico + 
                                           lngdp + democraciavdemelectoralcomp + mediaqualitycorruptvdem +
                                           accesogratuito + propindivinternet | cat_pais)",  sep = "+" ), 
                                     family=binomial("logit"), 
                                     data = democracias)
summary(modelo_random_slopes3)

coef(modelo_random_slopes3)


lme4::fixef(modelo_random_slopes) #to see the estimated average coeﬃcients (“ﬁxed eﬀects”):
lme4::ranef(modelo_random_slopes) #to see the estimated group-level errors (“random eﬀects”):  


### PASO 4: NON NESTED #####

modelo_fixed_effects <- glm(paste(formula_modelo_sficativas_variantes, 
                                          " as.factor(cat_pais) + ncat_eleccion",  sep = "+" ), 
                                    family=binomial("logit"), 
                                    data = democracias)

summary(modelo_fixed_effects)

modelo_non_nested <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                           " + (1 | cat_pais) + (1 | ncat_eleccion)",  sep = "+" ), 
                                     family=binomial("logit"), 
                                     data = democracias)
summary(modelo_non_nested)

coef(modelo_non_nested)


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

democracias2.2 <- democracias %>% 
  subset(cumsum_pastciclos==0|is.na(cumsum_pastciclos)) 
#subset(dico_debates_pastelection==0) 
summary(democracias2.2)

##### Modelo contingencia  ####

modelo_contingencia2 <- glm(formula_modelo_contingencia_bis,
                            family = binomial(link = "logit"), 
                            #data = data2
                            data = democracias2)
options(scipen=999)
summary(modelo_contingencia2)

robust_se_cluster_modelo_contingencia2 <- coeftest(modelo_contingencia2, 
                                                  vcov = vcovCL(modelo_contingencia2, 
                                                                #cluster = democracias$elecid))
                                                                #cluster = data$elecid))
                                                                cluster = democracias2$cat_pais))


print(robust_se_cluster_modelo_contingencia2)


##### Modelo sistemico ####

modelo_sistemico2 <- glm(formula_modelo_sistemico_bis,
                         family = binomial(link = "logit"), 
                         #data = data2
                         data = democracias2)

summary(modelo_sistemico2) 

robust_se_cluster_modelo_sistemico2 <- coeftest(modelo_sistemico2, 
                                               vcov = vcovCL(modelo_sistemico2,
                                                             #cluster = democracias$elecid))
                                                             #cluster = data$elecid))
                                                             cluster = democracias2$cat_pais))
print(robust_se_cluster_modelo_sistemico2)


##### Modelo marco regulatorio ####

modelo_regulatorio2 <- glm(formula_modelo_regulatorio_bis,
                           family = binomial(link = "logit"), 
                           #data = data2
                           data = democracias2)

summary(modelo_regulatorio2)

robust_se_cluster_modelo_regulatorio2 <- coeftest(modelo_regulatorio2, 
                                                 vcov = vcovCL(modelo_regulatorio2, 
                                                               #cluster = democracias$elecid))
                                                               #cluster = data$elecid))
                                                               cluster = democracias2$cat_pais))
print(robust_se_cluster_modelo_regulatorio2)


##### Modelo geo/temp ####

modelo_temporal2 <- glm(formula_modelo_temporal_bis,
                        family = binomial(link = "logit"), 
                        #data = data2
                        data = democracias2)

summary(modelo_temporal2)

robust_se_cluster_modelo_temporal2 <- coeftest(modelo_temporal2, 
                                              vcov = vcovCL(modelo_temporal2, 
                                                            #cluster = democracias$elecid))
                                                            #cluster = data$elecid))
                                                            cluster = democracias2$cat_pais))
print(robust_se_cluster_modelo_temporal2)


##### Modelo sficativas ####

modelo_sficativas2 <- glm(formula_modelo_sficativas,
                          family = binomial(link = "logit"), 
                          #data = data2
                          data = democracias2)
options(scipen=999)
summary(modelo_sficativas2)

robust_se_cluster_modelo_sficativas2 <- coeftest(modelo_sficativas2, 
                                                vcov = vcovCL(modelo_sficativas2, 
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias2$cat_pais))
print(robust_se_cluster_modelo_sficativas2)


##### Modelo final / sficativas - variantes ####

skimr::skim(democracias2)
skimr::skim(democracias2.2)

# con dico_past == 0
modelo_sficativas_variantes2 <- glm(formula_modelo_sficativas_variantes,
                          family = binomial(link = "logit"), 
                          #data = data2
                          data = democracias2)
options(scipen=999)
summary(modelo_sficativas_variantes2)

robust_se_cluster_modelo_sficativas_variantes2 <- coeftest(modelo_sficativas_variantes2, 
                                                          vcov = vcovCL(modelo_sficativas_variantes2, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = democracias2$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes2)


# con cumsum == 0 
modelo_sficativas_variantes2.2 <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data2
                                    data = democracias2.2)
options(scipen=999)
summary(modelo_sficativas_variantes2.2)

robust_se_cluster_modelo_sficativas_variantes2.2 <- coeftest(modelo_sficativas_variantes2.2, 
                                                           vcov = vcovCL(modelo_sficativas_variantes2.2, 
                                                                         #cluster = democracias$elecid))
                                                                         #cluster = data$elecid))
                                                                         cluster = democracias2.2$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes2.2)


### MODELOS 3: subumuestra sin regulacion ####


data3 <- data %>% 
  subset(regulaciondico==0)
summary(data3)

democracias3 <- democracias %>% 
  subset(regulaciondico==0)
summary(democracias3)

##### Modelo contingencia  ####

modelo_contingencia3 <- glm(formula_modelo_contingencia_bis,
                            family = binomial(link = "logit"), 
                            #data = data3
                            data = democracias3)
options(scipen=999)
summary(modelo_contingencia3)

robust_se_cluster_modelo_contingencia3 <- coeftest(modelo_contingencia3, 
                                                   vcov = vcovCL(modelo_contingencia3, 
                                                                 #cluster = democracias$elecid))
                                                                 #cluster = data$elecid))
                                                                 cluster = democracias3$cat_pais))


print(robust_se_cluster_modelo_contingencia3)


##### Modelo sistemico ####

modelo_sistemico3 <- glm(formula_modelo_sistemico_bis,
                         family = binomial(link = "logit"), 
                         #data = data3
                         data = democracias3)

summary(modelo_sistemico3) 

robust_se_cluster_modelo_sistemico3 <- coeftest(modelo_sistemico3, 
                                                vcov = vcovCL(modelo_sistemico3,
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias3$cat_pais))
print(robust_se_cluster_modelo_sistemico3)


##### Modelo marco regulatorio ####

modelo_regulatorio3 <- glm(formula_modelo_regulatorio_bis,
                           family = binomial(link = "logit"), 
                           #data = data3
                           data = democracias3)

summary(modelo_regulatorio3)

robust_se_cluster_modelo_regulatorio3 <- coeftest(modelo_regulatorio3, 
                                                  vcov = vcovCL(modelo_regulatorio3, 
                                                                #cluster = democracias$elecid))
                                                                #cluster = data$elecid))
                                                                cluster = democracias3$cat_pais))
print(robust_se_cluster_modelo_regulatorio3)


##### Modelo geo/temp ####

modelo_temporal3 <- glm(formula_modelo_temporal_bis,
                        family = binomial(link = "logit"), 
                        #data = data3
                        data = democracias3)

summary(modelo_temporal3)

robust_se_cluster_modelo_temporal3 <- coeftest(modelo_temporal3, 
                                               vcov = vcovCL(modelo_temporal3, 
                                                             #cluster = democracias$elecid))
                                                             #cluster = data$elecid))
                                                             cluster = democracias3$cat_pais))
print(robust_se_cluster_modelo_temporal3)


##### Modelo sficativas ####

modelo_sficativas3 <- glm(formula_modelo_sficativas,
                          family = binomial(link = "logit"), 
                          #data = data3
                          data = democracias3)
options(scipen=999)
summary(modelo_sficativas3)

robust_se_cluster_modelo_sficativas3 <- coeftest(modelo_sficativas3, 
                                                 vcov = vcovCL(modelo_sficativas3, 
                                                               #cluster = democracias$elecid))
                                                               #cluster = data$elecid))
                                                               cluster = democracias3$cat_pais))
print(robust_se_cluster_modelo_sficativas3)


##### Modelo final / sficativas - variantes ####

skimr::skim(democracias3)

modelo_sficativas_variantes3 <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data3
                                    data = democracias3)
options(scipen=999)
summary(modelo_sficativas_variantes3)

robust_se_cluster_modelo_sficativas_variantes3 <- coeftest(modelo_sficativas_variantes3, 
                                                           vcov = vcovCL(modelo_sficativas_variantes3, 
                                                                         #cluster = democracias$elecid))
                                                                         #cluster = data$elecid))
                                                                         cluster = democracias3$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes3)



### MODELOS 4: submuestra con debates en eleccion anterior. "Interrupción de práctica"  #####
### preparo data ####
data4 <- data %>% 
  subset(dico_debates_pastelection==1) %>% 
  mutate(dico_hubo_debates = ifelse(dico_hubo_debates==1,0,
                                      ifelse(dico_hubo_debates==0,1,
                                             NA)))
summary(data4)

democracias4 <- democracias %>% 
  subset(dico_debates_pastelection==1) %>% 
  mutate(dico_hubo_debates = ifelse(dico_hubo_debates==1,0,
                                      ifelse(dico_hubo_debates==0,1,
                                             NA)))
summary(democracias4)

democracias4.4 <- democracias %>% 
  subset(cumsum_pastciclos>1) %>% 
  mutate(dico_hubo_debates = ifelse(dico_hubo_debates==1,0,
                                      ifelse(dico_hubo_debates==0,1,
                                             NA)))
summary(democracias4.4)

##### Modelo contingencia  ####

modelo_contingencia4 <- glm(formula_modelo_contingencia_bis,
                            family = binomial(link = "logit"), 
                            #data = data4
                            data = democracias4)
options(scipen=999)
summary(modelo_contingencia4)

robust_se_cluster_modelo_contingencia4 <- coeftest(modelo_contingencia4, 
                                                   vcov = vcovCL(modelo_contingencia4, 
                                                                 #cluster = democracias$elecid))
                                                                 #cluster = data$elecid))
                                                                 cluster = democracias4$cat_pais))


print(robust_se_cluster_modelo_contingencia4)


##### Modelo sistemico ####

modelo_sistemico4 <- glm(formula_modelo_sistemico_bis,
                         family = binomial(link = "logit"), 
                         #data = data4
                         data = democracias4)

summary(modelo_sistemico4) 

robust_se_cluster_modelo_sistemico4 <- coeftest(modelo_sistemico4, 
                                                vcov = vcovCL(modelo_sistemico4,
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias4$cat_pais))
print(robust_se_cluster_modelo_sistemico4)


##### Modelo marco regulatorio ####

modelo_regulatorio4 <- glm(formula_modelo_regulatorio_bis,
                           family = binomial(link = "logit"), 
                           #data = data4
                           data = democracias4)

summary(modelo_regulatorio4)

robust_se_cluster_modelo_regulatorio4 <- coeftest(modelo_regulatorio4, 
                                                  vcov = vcovCL(modelo_regulatorio4, 
                                                                #cluster = democracias$elecid))
                                                                #cluster = data$elecid))
                                                                cluster = democracias4$cat_pais))
print(robust_se_cluster_modelo_regulatorio4)


##### Modelo geo/temp ####

modelo_temporal4 <- glm(formula_modelo_temporal_bis,
                        family = binomial(link = "logit"), 
                        #data = data4
                        data = democracias4)

summary(modelo_temporal4)

robust_se_cluster_modelo_temporal4 <- coeftest(modelo_temporal4, 
                                               vcov = vcovCL(modelo_temporal4, 
                                                             #cluster = democracias$elecid))
                                                             #cluster = data$elecid))
                                                             cluster = democracias4$cat_pais))
print(robust_se_cluster_modelo_temporal4)


##### Modelo sficativas ####

modelo_sficativas4 <- glm(formula_modelo_sficativas,
                          family = binomial(link = "logit"), 
                          #data = data4
                          data = democracias4)
options(scipen=999)
summary(modelo_sficativas4)

robust_se_cluster_modelo_sficativas4 <- coeftest(modelo_sficativas4, 
                                                 vcov = vcovCL(modelo_sficativas4, 
                                                               #cluster = democracias$elecid))
                                                               #cluster = data$elecid))
                                                               cluster = democracias4$cat_pais))
print(robust_se_cluster_modelo_sficativas4)


##### Modelo final / sficativas - variantes ####

skimr::skim(democracias4)
skimr::skim(democracias4.4)

# c pastdico = 1
modelo_sficativas_variantes4 <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data4
                                    data = democracias4)
options(scipen=999)
summary(modelo_sficativas_variantes4)

robust_se_cluster_modelo_sficativas_variantes4 <- coeftest(modelo_sficativas_variantes4, 
                                                           vcov = vcovCL(modelo_sficativas_variantes4, 
                                                                         #cluster = democracias$elecid))
                                                                         #cluster = data$elecid))
                                                                         cluster = democracias4$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes4)


# c cumsum > 1
modelo_sficativas_variantes4.4 <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data4
                                    data = democracias4.4)
options(scipen=999)
summary(modelo_sficativas_variantes4.4)

robust_se_cluster_modelo_sficativas_variantes4.4 <- coeftest(modelo_sficativas_variantes4.4, 
                                                           vcov = vcovCL(modelo_sficativas_variantes4.4, 
                                                                         #cluster = democracias$elecid))
                                                                         #cluster = data$elecid))
                                                                         cluster = democracias4.4$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes4.4)

### MODELOS 5: subumuestra sin regulacion y sin debates ####


data5 <- data %>% 
  subset(regulaciondico==0&dico_debates_pastelection==0|regulaciondico==0&is.na(dico_debates_pastelection))
summary(data5)

democracias5 <- democracias %>% 
  subset(regulaciondico==0&dico_debates_pastelection==0|regulaciondico==0&is.na(dico_debates_pastelection))
summary(democracias5)

democracias5.5 <- democracias %>% 
  subset(regulaciondico==0&cumsum_pastciclos==0|regulaciondico==0&is.na(cumsum_pastciclos))
summary(democracias5.5)


##### Modelo contingencia  ####

modelo_contingencia5 <- glm(formula_modelo_contingencia_bis,
                            family = binomial(link = "logit"), 
                            #data = data5
                            data = democracias5)
options(scipen=999)
summary(modelo_contingencia5)

robust_se_cluster_modelo_contingencia5 <- coeftest(modelo_contingencia5, 
                                                   vcov = vcovCL(modelo_contingencia5, 
                                                                 #cluster = democracias$elecid))
                                                                 #cluster = data$elecid))
                                                                 cluster = democracias5$cat_pais))


print(robust_se_cluster_modelo_contingencia5)


##### Modelo sistemico ####

modelo_sistemico5 <- glm(formula_modelo_sistemico_bis,
                         family = binomial(link = "logit"), 
                         #data = data5
                         data = democracias5)

summary(modelo_sistemico5) 

robust_se_cluster_modelo_sistemico5 <- coeftest(modelo_sistemico5, 
                                                vcov = vcovCL(modelo_sistemico5,
                                                              #cluster = democracias$elecid))
                                                              #cluster = data$elecid))
                                                              cluster = democracias5$cat_pais))
print(robust_se_cluster_modelo_sistemico5)


##### Modelo marco regulatorio ####

modelo_regulatorio5 <- glm(formula_modelo_regulatorio_bis,
                           family = binomial(link = "logit"), 
                           #data = data5
                           data = democracias5)

summary(modelo_regulatorio5)

robust_se_cluster_modelo_regulatorio5 <- coeftest(modelo_regulatorio5, 
                                                  vcov = vcovCL(modelo_regulatorio5, 
                                                                #cluster = democracias$elecid))
                                                                #cluster = data$elecid))
                                                                cluster = democracias5$cat_pais))
print(robust_se_cluster_modelo_regulatorio5)


##### Modelo geo/temp ####

modelo_temporal5 <- glm(formula_modelo_temporal_bis,
                        family = binomial(link = "logit"), 
                        #data = data5
                        data = democracias5)

summary(modelo_temporal5)

robust_se_cluster_modelo_temporal5 <- coeftest(modelo_temporal5, 
                                               vcov = vcovCL(modelo_temporal5, 
                                                             #cluster = democracias$elecid))
                                                             #cluster = data$elecid))
                                                             cluster = democracias5$cat_pais))
print(robust_se_cluster_modelo_temporal5)


##### Modelo sficativas ####

modelo_sficativas5 <- glm(formula_modelo_sficativas,
                          family = binomial(link = "logit"), 
                          #data = data5
                          data = democracias5)
options(scipen=999)
summary(modelo_sficativas5)

robust_se_cluster_modelo_sficativas5 <- coeftest(modelo_sficativas5, 
                                                 vcov = vcovCL(modelo_sficativas5, 
                                                               #cluster = democracias$elecid))
                                                               #cluster = data$elecid))
                                                               cluster = democracias5$cat_pais))
print(robust_se_cluster_modelo_sficativas5)


##### Modelo final / sficativas - variantes ####

skimr::skim(democracias5)

modelo_sficativas_variantes5 <- glm(formula_modelo_sficativas_variantes,
                                    family = binomial(link = "logit"), 
                                    #data = data5
                                    data = democracias5)
options(scipen=999)
summary(modelo_sficativas_variantes5)

robust_se_cluster_modelo_sficativas_variantes5 <- coeftest(modelo_sficativas_variantes5, 
                                                           vcov = vcovCL(modelo_sficativas_variantes5, 
                                                                         #cluster = democracias$elecid))
                                                                         #cluster = data$elecid))
                                                                         cluster = democracias5$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes5)

### OTROS MODELOS ####

### regulacion version separada PENDIENTE #####

### prop internet version VIEJA #####
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

# CANDIDATOS ##############################
## PREPARO DATA #######

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
  mutate(across(-c(dico_candidato_presente,
                   cat_pais,
                   elecid,
                   id_debate), scale))

## Pruebo modelos ####

modelo_nivelindiv <- glm(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                            dico_oficialista + 
                            ninvitaciones + 
                            propausenciaspasadasfilled,
                          family = binomial(link = "logit"),
                          data = data)
options(scipen=999)
summary(modelo_nivelindiv)


modelo_niveldebate <- glm(dico_candidato_presente ~ 
                            orgosc + 
                            orgmmc + 
                            orgestado,
                  family = binomial(link = "logit"),
                  data = data)
options(scipen=999)
summary(modelo_niveldebate)

modelo_agregado <- glm(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                            dico_oficialista + 
                            ninvitaciones + 
                            propausenciaspasadasfilled + 
                            orgosc + 
                            orgmmc + 
                            orgestado,
                          family = binomial(link = "logit"),
                          data = data)
options(scipen=999)
summary(modelo_agregado)

modelo_multinivel1 <- lme4::glmer(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            dico_reeleccion + 
                            dico_oficialista + 
                            ninvitaciones + 
                            propausenciaspasadasfilled + 
                            orgosc + 
                            orgmmc + 
                            orgestado + 
                            (1|id_debate),
                          family = binomial(link = "logit"),
                          data = data)
options(scipen=999)
summary(modelo_multinivel1)

modelo_multinivel2 <- lme4::glmer(dico_candidato_presente ~ 
                            voteshare + 
                            v2pariglef_vdem + 
                            v2paactcom_vdem + 
                            #dico_reeleccion + 
                            dico_oficialista + 
                            ninvitaciones + 
                            propausenciaspasadasfilled + 
                            orgosc + 
                            #orgmmc + 
                            orgestado + 
                            (1 | id_debate) + 
                            (1 | cat_pais) ,
                          family = binomial(link = "logit"),
                          data = data)
options(scipen=999)
summary(modelo_multinivel2)

lmer(formula = y ~ black + female + black:female + v.prev.full +
       (1 | age) + (1 | edu) + (1 | age.edu) + (1 | state) +
       (1 | region.full), family = binomial(link = "logit"))