# MODELO DE PROBA DE AUSENCIAS PARA LOS CANDIATOS

# version borrador 8 abril 2024

# LIBRERIAS #####

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_elecciones <- read.csv("all_elections.csv") %>% select(-X)

# TRABAJO ########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")
options(scipen=999)

# exploracion de VDs alternativas
table(base_elecciones$dico_debates_eleccion)
table(base_elecciones$dico_frontrunner_presente)
table(base_elecciones$dico_frontORcha_presentes)

# PENDIENTES/ AYUDAMEMORIA #####
# recirdar que para inclusion de VIS importa ortogonalidad o no, ademas de relevancia
# recordar interpretar sustantivamente la magnitud de los coeficientes

######################### MODELOS #########################

# MODELOS CON VARIABLE COMPET COMO BADE ##########

# vd original #####
modelo_base0 <- glm(dico_debates_eleccion ~ competitividad,
                      data = base_elecciones,
                      family = "binomial")
summary(modelo_base)

# signo esperado, magnitud pequeña, soprrendentemente parecida a la de modelos candidato; significavito al 0.1
# cuanto mas distancia entre 1 y 2, menos proba de debate

# repetimos para otras VD: ####
modelo_base1 <- glm(dico_frontrunner_presente ~ competitividad,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_base1)  # crece en magnitud, levemente, y en sginificancia. Signo sigue siendo el esperado

modelo_base2 <- glm(dico_frontORcha_presentes ~ competitividad,
                   data = base_elecciones,
                   family = "binomial") # resultados similares a version original en todo sentido
summary(modelo_base2)

# AGREGAMOS COMPLICACIONES AL MODELO ############
# MAS VARIABLES SOBRE PARTIDOS #############

modelo_1_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_0)

# crece en magnitud, aunque lgieramente, coef competitividad. mejora sficancia al 0.05
# concentracion es sficativo al 0.1. magnitud pequeña. (pendiente interpretar magnitudes en terminos sustantivos)
# signo es el esperado: a mas concentracion, menos proba de debates, lo que abreva de mi idea de fragmentacion

# probamos con VD alternativas 

modelo_1_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_1) 
# coef competitividad parecido a modelo base, tb un poco mas grande que para modelo 1_0. concetracion signo esperado pero pierde sficancia
# tiene sentido con idea de que fragmentacion aumenta proba de debates, pero no con candidato ppal presente


modelo_1_2 <- glm(dico_frontORcha_presentes ~ competitividad + concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_2)
# consistente con idea anterior, aqui concentracion vuelve a ser sficativo (igual su magnitud sigue siendo reducida),
# indicando que hay oportunidades en las que fragmentacion ayuda a que haya debate, aunque sin necesariamente garantizar presencia de ppales contenedores
# en este modelo competitividad tiene performance parecida al modelo base 


# VARIABLES LAGGED SOBRE DEBATES ##############

modelo_21_0 <- glm(dico_debates_eleccion ~ competitividad + lagged_dico_debates_eleccion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_21_0)

modelo_22_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion + lagged_dico_debates_eleccion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_22_0)

# debate en eleccion anterior es por lejos mejor predictor, magnitud mucho mas grande y sficativo al 0.001
# igual para pensar: magnitud tiene que ver con escalas. competitividad y concetrancion varian del 1 al 100 (hipoteticamente), lagged es dicotomica. 
# interesante es que variables de competencia y concentracion se mantienen parecidas!

# probamos con variables VD alternativas
modelo_21_1 <- glm(dico_frontrunner_presente ~ competitividad + lagged_dico_frontrunner_presente,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_21_1)

modelo_22_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + lagged_dico_frontrunner_presente,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_22_1)
# idem comentario modelo_2 de arriba: se mantiene muy parecido a modelo_1, pero lagged aparece claramente como un predictor mas pontente, todavia mas cierto para el caso de la asistencia del frontrunner
# un peq parentesis, usando otra lagged con esta VD: 
modelo_23_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + lagged_dico_debates_eleccion,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_23_1) # coef sigue siendo muy grande, aunque magnitud disminuye muy ligeramente respecto de 21_1. Idem en linea con expectativas

modelo_21_2 <- glm(dico_frontORcha_presentes ~ competitividad + lagged_dico_frontORcha_presentes,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_21_2)

modelo_22_2 <- glm(dico_frontORcha_presentes ~ competitividad + concentracion + lagged_dico_frontORcha_presentes,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_22_2) 
# simil comentarios anteriores, en este caso modelo si difiere de anteriores en coef concentracion, que pierde sficancia
# en lo demas idem: modelo muy parecido a modelo_1, pero lagged emerge como mejor predictor 

# FIXED EFFECTS POR PAIS ######

modelo_31_0 <- glm(dico_debates_eleccion ~ competitividad + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_0)
# competitividad sigue teniendo signo esperado, mejora muy ligeramente signo y nivel de sficancia
# fixed effects muchos no son sficativos, pero pendiente ver como assess esto creo que prueba F

modelo_32_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion + lagged_dico_debates_eleccion + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_32_0)
# variable concentracion pierde sficancia, idem lagged, aunque mantienen su signo y en el caso de concentracion su magnitud
# estas variables parecen mas estructurales (sobre todo lagged) , por lo que fixed effects parecen comerse su efecto

# VDs alternativas


modelo_31_1 <- glm(dico_frontrunner_presente ~ competitividad + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_1) # idem comentarios que para 31_0, notese que aqui competitividad aumenta bastante su magnitud 

modelo_32_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + lagged_dico_frontrunner_presente + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_32_1) # idem


modelo_31_2 <- glm(dico_frontORcha_presentes ~ competitividad + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_2) # idem comentarios anteriores

modelo_32_2 <- glm(dico_frontORcha_presentes ~ competitividad + concentracion + lagged_dico_frontORcha_presentes + cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_32_2) # idem

# el hecho de que competitividad aumente tanto su magnitud
# parece ser indicativo de que hay otros factores estructurales que no estamos considerando y que diluyen su efecto
# porque estarian correlacionados con esta variable (((para pensar: en que direccion???)))

# VARIABLES CONFIANZA LATINOBAROMETRO (PENDIENTE! EMPROLIJAR) ############

# modelo con interpolacion # pueden variarse los indicadores de confianza 
# OJO ATENTI pendiente ver como homogenizar esta variable tomada de fuentes alternativas,
# quizas sea mas adecuado estandarizar y recalcular

modelo_4_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion + lagged_dico_debates_eleccion + int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                data = base_elecciones,
                family = "binomial")
summary(modelo_4_0) 
# definitivamente sin resultados sficativos, en este modelo completo competitividad recupera sficancia aunque al 0.1, y aumenta en magnitud, mantiene signo esperado
# coefs de confianza cambian mucho y son definitivamente no sficativos }
# los resultados se mantienen similares para los distintos indicadores de confianza en medios utilizados

modelo_4_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + lagged_dico_frontrunner_presente + int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_4_1) 

# VARIABLES LEGISLACION #############
# pendiente 
modelo_5_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion + 
                    lagged_dico_debates_eleccion + 
                    int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    ncat_totreg,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_5_0) 

modelo_5_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + 
                    lagged_dico_frontrunner_presente + 
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    ncat_totreg,
                data = base_elecciones,
                family = "binomial")
summary(modelo_5_1) 
# muy interesante la diferencia entre estos dos modelos!!! 
# para probar mas detenidamente, apenas los mire
# es decir que: alli donde NO hay tradicion NI regulacion, seguro importa la competencia,no?
# probar con variables dico de legislacion

# PENDIENTE: VER SI TIENE SENTIDO SELECCION DE CASOS COMO DEVIANT
# IGUAL NO SE SI TENEMOS DATOS PARA ESAS INSTANCIAS

# probamos con variable dico de normativa 

base_elecciones <- base_elecciones %>% 
  mutate(dico_obligatorio_candidato = ifelse(ncat_regcandidatos==3,1,0),
         dico_derechos_candidato = ifelse(ncat_regcandidatos==2,1,0))

modelo_51_0 <- glm(dico_debates_eleccion ~ competitividad + concentracion + 
                    lagged_dico_debates_eleccion + 
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_obligatorio_candidato + dico_derechos_candidato,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_51_0) 

modelo_51_1 <- glm(dico_frontrunner_presente ~ competitividad + concentracion + 
                    lagged_dico_frontrunner_presente + 
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_obligatorio_candidato + dico_derechos_candidato,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_51_1) 
# falsa de significancia en dico_obligatorio_candidatos ha de deberse a muy bajo n, pero tamaño (!) y signo del coeficiente son sugestivos


####### INTERPRETACION #################################
# buen enlace para leer output :  https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression 
# otro https://stats.oarc.ucla.edu/r/dae/logit-regression/
modelo_a_interpretar <- modelo_51_1 # ajustar segun deseado

# interpretacion de coeficientes:

# The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable.
#We can use the confint function to obtain confidence intervals for the coefficient estimates. Note that for logistic models, confidence intervals are based on the profiled log-likelihood function. We can also get CIs based on just the standard errors by using the default method.
confint(modelo_a_interpretar)
#You can also exponentiate the coefficients and interpret them as odds-ratios. R will do this computation for you. To get the exponentiated coefficients, you tell R that you want to exponentiate (exp), and that the object you want to exponentiate is called coefficients and it is part of mylogit (coef(mylogit)). We can use the same logic to get odds ratios and their confidence intervals, by exponentiating the confidence intervals from before. To put it all in one table, we use cbind to bind the coefficients and confidence intervals column-wise.
exp(coef(modelo_a_interpretar))
exp(cbind(OR = coef(modelo_a_interpretar), confint(modelo_a_interpretar)))

#You can also use predicted probabilities to help you understand the model. Predicted probabilities can be computed for both categorical and continuous predictor variables. In order to create predicted probabilities we first need to create a new data frame with the values we want the independent variables to take on to create our predictions.
#We will start by calculating the predicted probability of ... at each value of ...., holding .. and .. at their means. First we create and view the data frame.

# queremos comenzar para predicted proba de debate a cada valor de competitividad  para un pais con oblig = 0 y derechos = 0
valores_posibles <- seq(range(base_elecciones$competitividad, na.rm=T)[1],range(base_elecciones$competitividad, na.rm=T)[2], 0.1)

newdata1 <- with(base_elecciones, 
                 data.frame(lagged_dico_frontrunner_presente = 0,
                            dico_obligatorio_candidato = 0,
                            dico_derechos_candidato = 0,
                            concentracion = mean(concentracion, na.rm=T), 
                            competitividad = valores_posibles))

newdata1$predicted_proba <- predict(modelo_a_interpretar, newdata = newdata1, type = "response")


newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
# ACA ME QUEDE #########The code to generate the predicted probabilities (the first line below) is the same as before, except we are also going to ask for standard errors so we can plot a confidence interval. We get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
# https://stackoverflow.com/questions/47486589/what-is-the-difference-between-type-response-terms-and-link-in-predict-f
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata