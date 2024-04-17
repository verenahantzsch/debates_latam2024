# MODELO DE PROBA DE AUSENCIAS PARA LOS CANDIATOS

# version borrador 8 abril 2024

# APUNTES: LISTA DE PENDIENTES #############

# PENDIENTE: AGREGAR CONTROL INTERACTIVO BALLOTAGE==1 que deberia volver insignificante el efecto de la concentracion. LISTO. RESULTADOS RAROS. PARA PENSAR / PREGUNTAR MAURICIO

# PENDIENTE CONSTRUIR TABLAS PARA INTERPRETAR VARIANDO VALORES DE TODAS VARIABLES
# PENDIENTE: PLOTEAR GRAFICOS INVIRTIENDO VARIABLES QUE SE VUELVEN VISIBLES, EJ NORMATIVA/ TRADICION, probado ya , igual definitivo cuando tengamos modelo definitivo
# PENDIENTE pensar pos de agregar todas variantes en un mismo grafico, o en un mismo panel

# PENDIENTE: ESTADISTICAS DESCRIPTIVAS! # estoy haciendo tablas y diferencias de medias, abr 2024, ver si algo mas es relevante
# PENDIENTE: QUIZAS SE PUEDEN INTERPRETAR RELAC VIBARIADAS COMO CUASI NEC Y CUASI SUF , P PENSAR# 

# IMPORTANTE PENDIENTE: QUIZAS CONSIDERAR UNA VD DE DEBATES SI PERO SIN FRONTRUNNER PARA ID CAUSAS SOBRE ESTE SUBSET,
# AUNQUE ATENTI PUEDE SER UN N MUY BAJO PARA LA CATEGORIA
# EN GRAL RESULTADOS A HOY ABRIL 11 2024 VIENEN SUGIRIENDO QUE DETS SON PARECIDOS PERO DIFERENTES
# A INCUMBENT SI O SI LE IMPORTA LA COMPETITIVIDAD
# PARA EL RESTO ES UN POCO MAS IMPORTANTE LA CONCENTRACION, LA COMPETITIVIDAD PIERDE IMP 

# PENDIENTES/ AYUDAMEMORIA 
# recirdar que para inclusion de VIS importa ortogonalidad o no, ademas de relevancia
# recordar interpretar sustantivamente la magnitud de los coeficientes

# LIBRERIAS #####

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_elecciones <- read.csv("all_elections.csv") %>% select(-X)
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")
options(scipen=999)
# TRABAJO ########

# CREACION DE VARIABLES AUXILIARES ##########

# variables dico de normativa

base_elecciones <- base_elecciones %>% 
  mutate(dico_obligatorio_candidato = ifelse(ncat_regcandidatos==3,1,0),
         dico_derechos_candidato = ifelse(ncat_regcandidatos==2,1,0))

base_elecciones <- base_elecciones %>% 
  mutate(dico_any_normativa = ifelse(ncat_regcandidatos>=2,1,0))

base_elecciones <- base_elecciones %>% 
  mutate(log_competitividad = log(competitividad))

# recategorizamos ballotage
base_elecciones <- base_elecciones %>% 
  mutate(ballotage = ifelse(ncat_ronda==2,1,0))

# base sin outlier competitividad # para probar modelos, comentar cuando aplique
full_data <- base_elecciones 
base_elecciones <- base_elecciones %>% 
  subset(electionid!="El Salvador 2024-02-04")
 
# EXPLORACION DESCRIPTIVA ###########

# exploracion de VDs alternativas
table(base_elecciones$dico_debates_eleccion)
table(base_elecciones$dico_frontrunner_presente)
table(base_elecciones$dico_frontORcha_presentes)

# chequeamos distribs bivariadas

# VIS CONTINUAS #

# COMPETITIVIDAD #
mean_values <- aggregate(competitividad ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# hay mas competitividad (menos dif entre candidatos) cuando hay debates

mean_values <- aggregate(competitividad ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# idem, hay mas competitividad (menos dif entre candidatos) cuando hay debates, la diferencia es ligeramente mas amplia

# distrib
hist(base_elecciones$competitividad) # ojo, hay un outlier que puede ser problematico, calculo que se trata de el salvador
hist(base_elecciones$log_competitividad) # tiene mas sentido. otra alternativa sacar outlier?

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), competitividad))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), competitividad))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), log_competitividad))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), log_competitividad))

# CONCENTRACION #
mean_values <- aggregate(concentracion ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# las elecciones estan mas concentradas cuando no hay debates, o mas dispersas cuando hay debates

mean_values <- aggregate(concentracion ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# idem, pero al diferencia se estrecha a casi la mitad (6 puntos versus 3 puntos de diferencia en la diferencia de promedios)

# distrib
hist(base_elecciones$concentracion) 

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), concentracion))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), concentracion))

# TOTREG #
mean_values <- aggregate(ncat_totreg ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# la regulacion es mas estricta cuando hay debates

mean_values <- aggregate(ncat_totreg ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# idem, y la diferencia de medias se agranda para este caso

# distrib
hist(base_elecciones$ncat_totreg) 

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), ncat_totreg))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), ncat_totreg))

# CONFIANZA EN MEDIOS # # PENDIENTE CORREGIR ESTE INDICADOR Y PROBAR ALTERNATIVOS 
mean_values <- aggregate(int_average_confianza_tv_medios_latin_lapop ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# hay mas confianza en medios cuando hay debates dif de 0.6 puntos aprox en escala de 0 a 4

mean_values <- aggregate(int_average_confianza_tv_medios_latin_lapop ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# diferencia se reduce e invierte: hay mas confianza en medios cuando NO hay debates.
# IGUAL CATEGORIZACION DEL INDICADOR PUEDE ESTAR GENERANDO PROBLEMAS

hist(base_elecciones$int_average_confianza_tv_medios_latin_lapop)

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), int_average_confianza_tv_medios_latin_lapop))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), int_average_confianza_tv_medios_latin_lapop))

# probamos version escalada
mean_values <- aggregate(int_average_scaled_confianza_tv_medios_latin_lapop ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# hay menos confianza alli donde no hay debates
mean_values <- aggregate(int_average_scaled_confianza_tv_medios_latin_lapop ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# casi no hay diferencia

hist(base_elecciones$int_average_scaled_confianza_tv_medios_latin_lapop)

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), int_average_scaled_confianza_tv_medios_latin_lapop))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), int_average_scaled_confianza_tv_medios_latin_lapop))

# CONFIANZA EN PPOLS # 
mean_values <- aggregate(int_average_confianza_ppols ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
# hay ligeramente mas confianza en ppols cuando NO hay debates

mean_values <- aggregate(int_average_confianza_ppols ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
# idem, y distancia se agranda ligeramente

hist(base_elecciones$int_average_confianza_ppols)

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), int_average_confianza_ppols))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), int_average_confianza_ppols))

# TRADICION: N DEBATES / N ELECCIONES PREVIAS # 
# cumsum pasadas elec con debates
mean_values <- aggregate(lagged_all_previous_elec ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
mean_values <- aggregate(lagged_all_previous_elec_frontrunner_presente ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
mean_values <- aggregate(lagged_all_previous_elec ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)

hist(base_elecciones$lagged_all_previous_elec)
hist(base_elecciones$lagged_all_previous_elec_frontrunner_presente)

# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), lagged_all_previous_elec))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), lagged_all_previous_elec)) 
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), lagged_all_previous_elec_frontrunner_presente))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), lagged_all_previous_elec_frontrunner_presente))
# las diferencias entre las outcome variables alternativas están para pensar.
# either frontrunners sienten mas el peso de la trad / o son mas fuertes en marcar tendencia futura

# n: tot debates en pasadas elec 
mean_values <- aggregate(lagged_all_previous_debates ~ dico_debates_eleccion, base_elecciones, mean, na.rm = TRUE)
mean_values <- aggregate(lagged_all_previous_debates_frontrunner_presente ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)
mean_values <- aggregate(lagged_all_previous_debates ~ dico_frontrunner_presente, base_elecciones, mean, na.rm = TRUE)

hist(base_elecciones$lagged_all_previous_debates)
hist(base_elecciones$lagged_all_previous_debates_frontrunner_presente)

# distrib bivariada
# distrib bivariada
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), lagged_all_previous_debates))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), lagged_all_previous_debates)) 
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_debates_eleccion), lagged_all_previous_debates_frontrunner_presente))
ggplot(base_elecciones) + geom_boxplot(aes(as.factor(dico_frontrunner_presente), lagged_all_previous_debates_frontrunner_presente))


# VARIABLES DICO # # PENDIENTE: QUIZAS SE PUEDEN INTERPRETAR ESSTAS VARIABLES COMO CUASI NEC Y CUASI SUF , P PENSAR# 

# RONDA # 
base_elecciones$ballotage %>% table()  
table(base_elecciones$ballotage, base_elecciones$dico_debates_eleccion)
# la distrib sugiere que esta variable es practicamente irrelevante , aunque hay ligeramente mas   
49/16 # en primera ronda 
22/8 # en segunda ronda # hay proporcionalmente mas debates en segunda que en primera ronda
table(base_elecciones$ballotage, base_elecciones$dico_frontrunner_presente)
25/40 # la relacion se vuelve MUCHO mas marcada para debates con fronturnner presente, casi por definicion 
# (--ya todos los debates que teniamos en segunda ronda en la cuenta anterior tenian al frontrunner presente--. 
#p pensar, hay que ver que hacer c esto)

# TRADICION # 
base_elecciones$lagged_dico_debates_eleccion %>% table()  
table(base_elecciones$lagged_dico_debates_eleccion, base_elecciones$dico_debates_eleccion)
# cuando no hubo debates en eleccion anterior, es ligeramente mas probable que no haya debates
# la relacion se invierte y por gran dif cuando si hubo debates (de cualq tipo) en eleccion anterior

table(base_elecciones$lagged_dico_debates_eleccion, base_elecciones$dico_frontrunner_presente)
# resultados similares, pero muy marcados para cuando no hubo debates en elec anterior 
# menos marcados para cuando hubo debates cualq en eleccion anterior 
# es decir, aun habiendo debates en elecc anterior no se coacciona del todo a frontrunner
# y que no haya habido debates en elecc anterior definitivamente da rienda mas suelta al frontrunner

base_elecciones$lagged_dico_frontrunner_presente %>% table()  
table(base_elecciones$lagged_dico_frontrunner_presente, base_elecciones$dico_debates_eleccion)
# se invierte para el caso de no debates (c frontrn) en elec anterior (vs cualq debate),en el sentido de que 
# aun no habiendo habido debates c frontrun, es mas prob que haya un debate (cualq) en elec actual que que no

table(base_elecciones$lagged_dico_frontrunner_presente, base_elecciones$dico_frontrunner_presente)
# aqui de nuevo results consistentes con narrativa en el sentido de que
# cuando frontrunner no asistio a ningun debate en elec anterior, es mas probable que no lo haga en actual eleccion (de hecho se espeja diferencia respecto de cuenta inmediata anterior)
# cuando frontrunner si asistio a debate en elec anterior, es mucho mas probable que si asista en la que siga

base_elecciones$lagged_dico_any_previous_elec %>% table()  
table(base_elecciones$lagged_dico_any_previous_elec, base_elecciones$dico_debates_eleccion)
table(base_elecciones$lagged_dico_any_previous_elec, base_elecciones$dico_frontrunner_presente)

base_elecciones$lagged_dico_any_previous_elec_frontrunner_presente %>% table()  
table(base_elecciones$lagged_dico_any_previous_elec_frontrunner_presente, base_elecciones$dico_debates_eleccion)
table(base_elecciones$lagged_dico_any_previous_elec_frontrunner_presente, base_elecciones$dico_frontrunner_presente)

# NOMRATIVA DICO # 
base_elecciones$dico_obligatorio_candidato %>% table()   
# ojo que esta variable tiene una distrib muy sesgada
table(base_elecciones$dico_obligatorio_candidato, base_elecciones$dico_debates_eleccion)
# cuando no hay obligatoriedad, es igualmente mas probable que haya algun debate
# cuando hay obligatoriedad, 100% hay debate

table(base_elecciones$dico_obligatorio_candidato, base_elecciones$dico_frontrunner_presente)
# simil resultados anteriores, solo que proba de debate c frontrunner sin obligatoriedad disminuye respecto de cuenta anterior

base_elecciones$dico_derechos_candidato %>% table()  
table(base_elecciones$dico_derechos_candidato, base_elecciones$dico_debates_eleccion)
# cuando no hay derechos reconocidos, es mas proba que haya debates, aunque la dif se reduce respecto de la cuenta anterior
# (es relativamente mas proba que no haya debates que cuando no hay obligacion)

table(base_elecciones$dico_derechos_candidato, base_elecciones$dico_frontrunner_presente)
# cuando hay derechos reconocidos, es mucho mas proba que haya debates
# aun asi, hay ligera proba de que no haya debates

base_elecciones$dico_any_normativa %>% table()  
table(base_elecciones$dico_any_normativa, base_elecciones$dico_debates_eleccion)
# cuando no hay normativa, hay igualmente chances de que haya debates, pero la diferencia se achica respecto de anteriores, acercandose al 50/50 de chances
# es decir que esta variable se vuelve indiferente cuando es 0, ergo no es NEC para debates
# cuando hay normativa, casi siempre hay debates, salvo excepciones
table(base_elecciones$dico_any_normativa, base_elecciones$dico_frontrunner_presente)
# idem comentario para cuando hay normativa
# las chances se vuelven casi 50/50 para el caso de debates c asistencia de frontrunner (respecto de cualq debate) cuando NO hay normativa


### MULTICOLINEALIDAD CRUZADA ######
# Calculate the correlation matrix
#full_data <- base_elecciones # reserva

cor_base_elecciones <- base_elecciones %>% 
  select(dico_debates_eleccion,
         dico_frontrunner_presente,
         dico_frontORcha_presentes,
         log_competitividad,
         competitividad,
         concentracion,
         int_average_confianza_ppols,
         int_average_confianza_tv,
         int_average_scaled_confianza_tv_medios_latin_lapop,
         ncat_totreg,
         dico_any_normativa,
         dico_obligatorio_candidato,
         dico_derechos_candidato,
         lagged_dico_debates_eleccion,
         lagged_dico_frontrunner_presente,
         lagged_all_previous_elec,
         lagged_all_previous_debates_frontrunner_presente,
         lagged_all_previous_debates,
         lagged_all_previous_debates_frontrunner_presente,
         ballotage)

correlation_matrix <- cor(cor_base_elecciones, use= "na.or.complete")

# Print the correlation matrix
print(correlation_matrix)

#install.packages("corrplot")
library(corrplot)

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle")

# ballotage y concentracion efectivamente presentan un problema de multicolinealidad 
# no se bien si/como abordar esto 
# quizas lo que quiero capturar es justamente del ballotage su mayor concentracion y competitividad
# por ende no tiene mucho sentido agregarlo
# PENDIENTE PREG CELES

######################### MODELOS #########################

# MODELOS CON VARIABLE COMPET COMO BADE ##########

# vd original #####
modelo_base0 <- glm(dico_debates_eleccion ~ 
                      competitividad,
                    #log_competitividad,
                      data = base_elecciones,
                      family = "binomial")
summary(modelo_base0)

# signo esperado, magnitud pequeña, soprrendentemente parecida a la de modelos candidato; significavito al 0.1
# cuanto mas distancia entre 1 y 2, menos proba de debate
# para version log: perdemos significancia. signo sigue igual al esperado

# repetimos para otras VD: ####
modelo_base1 <- glm(dico_frontrunner_presente ~ 
                      competitividad,
                    #log_competitividad,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_base1)  # crece en magnitud, levemente, y en sginificancia. Signo sigue siendo el esperado
# para version log, crece en magnitud, signo igual al esperado, pero sigue sin ser sficativo
# para version normal pero sin outlier, es sficativa al 0.1. magnitud peq pero similar a version con outlier. signo esperado

modelo_base2 <- glm(dico_frontORcha_presentes ~ 
                      competitividad,
                      #log_competitividad,
                   data = base_elecciones,
                   family = "binomial") # resultados similares a version original en todo sentido
summary(modelo_base2)

# AGREGAMOS COMPLICACIONES AL MODELO ############
# MAS VARIABLES SOBRE PARTIDOS #############

modelo_1_0 <- glm(dico_debates_eleccion ~ 
                    competitividad +
                    #log_competitividad + 
                    concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_0)

# crece en magnitud, aunque lgieramente, coef competitividad. mejora sficancia al 0.05
# concentracion es sficativo al 0.1. magnitud pequeña. (pendiente interpretar magnitudes en terminos sustantivos)
# signo es el esperado: a mas concentracion, menos proba de debates, lo que abreva de mi idea de fragmentacion
# para version log: no sficativo

# probamos con VD alternativas 

modelo_1_1 <- glm(dico_frontrunner_presente ~ 
                    competitividad +
                    #log_competitividad + 
                    concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_1) 
# coef competitividad parecido a modelo base, tb un poco mas grande que para modelo 1_0. concetracion signo esperado pero pierde sficancia
# tiene sentido con idea de que fragmentacion aumenta proba de debates, pero no con candidato ppal presente
# version log: simil anterior: crece en magnitud, signo esperado, pero sin significancia
# idem version sin outlier, sficativa al 0.1

modelo_1_2 <- glm(dico_frontORcha_presentes ~                     
                    competitividad +
                    #log_competitividad + 
                    concentracion,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_1_2)
# consistente con idea anterior, aqui concentracion vuelve a ser sficativo (igual su magnitud sigue siendo reducida),
# indicando que hay oportunidades en las que fragmentacion ayuda a que haya debate, aunque sin necesariamente garantizar presencia de ppales contenedores
# en este modelo competitividad tiene performance parecida al modelo base 
# version log: simil comentarios anteriores. 
# EN CASOS QUE SIGUEN SIGUE OCURRIENDO PARECIDO SALVO INDICACION EN CONTRARIO: VERSION LOG PIERDE SFICANCIA ESTADISTICA

# VARIABLES LAGGED SOBRE DEBATES ##############

modelo_21_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad +
                  #lagged_dico_debates_eleccion,
                   lagged_all_previous_elec,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_21_0)

modelo_22_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad + 
                     concentracion + 
                     #lagged_dico_debates_eleccion,
                     lagged_all_previous_elec,
                   data = base_elecciones,
                  family = "binomial")
summary(modelo_22_0)

# debate en eleccion anterior es por lejos mejor predictor, magnitud mucho mas grande y sficativo al 0.001
# igual para pensar: magnitud tiene que ver con escalas. competitividad y concetrancion varian del 1 al 100 (hipoteticamente), lagged es dicotomica. 
# interesante es que variables de competencia y concentracion se mantienen parecidas!

# probamos con variables VD alternativas
modelo_21_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad + 
                     lagged_all_previous_elec_frontrunner_presente,
                     #lagged_dico_frontrunner_presente,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_21_1)

modelo_22_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad + 
                     concentracion + 
                     lagged_all_previous_elec_frontrunner_presente,
                   #lagged_dico_frontrunner_presente,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_22_1)
# idem comentario modelo_2 de arriba: se mantiene muy parecido a modelo_1, pero lagged aparece claramente como un predictor mas pontente, todavia mas cierto para el caso de la asistencia del frontrunner
# con variable lagged all elec, competitividad pierde sficancia

# un peq parentesis, usando otra lagged con esta VD: 
modelo_23_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad + 
                     concentracion + 
                     lagged_all_previous_elec,
                     #lagged_dico_debates_eleccion,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_23_1) # coef sigue siendo muy grande, aunque magnitud disminuye muy ligeramente respecto de 21_1. Idem en linea con expectativas

modelo_21_2 <- glm(dico_frontORcha_presentes ~ 
                     competitividad + 
                     #log_competitividad +
                     lagged_dico_frontORcha_presentes,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_21_2)

modelo_22_2 <- glm(dico_frontORcha_presentes ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + 
                     lagged_dico_frontORcha_presentes,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_22_2) 
# simil comentarios anteriores, en este caso modelo si difiere de anteriores en coef concentracion, que pierde sficancia
# en lo demas idem: modelo muy parecido a modelo_1, pero lagged emerge como mejor predictor 

# FIXED EFFECTS POR PAIS ######

modelo_31_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad +
                     cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_0)
# competitividad sigue teniendo signo esperado, mejora tamaño y nivel de sficancia
# fixed effects muchos no son sficativos, pero pendiente ver como assess esto creo que prueba F

modelo_32_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad + 
                     concentracion + 
                     #lagged_dico_debates_eleccion + 
                     lagged_all_previous_elec +
                     cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_32_0)
# variable concentracion pierde sficancia, idem lagged, aunque mantienen su signo y en el caso de concentracion su magnitud
# estas variables parecen mas estructurales (sobre todo lagged) , por lo que fixed effects parecen comerse su efecto
# curioso, en modelo sin outlier competitivdad adquiere sficancia estadistica. lagged la manteiene pero reducida

# VDs alternativas


modelo_31_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad + 
                     cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_1) # idem comentarios que para 31_0, notese que aqui competitividad aumenta bastante su magnitud (x3)

modelo_32_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad + 
                     concentracion + 
                     #lagged_dico_frontrunner_presente + 
                     lagged_all_previous_elec_frontrunner_presente +
                     cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_32_1) # idem


modelo_31_2 <- glm(dico_frontORcha_presentes ~ 
                     competitividad + 
                     #log_competitividad +
                     cat_pais,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_31_2) # idem comentarios anteriores

modelo_32_2 <- glm(dico_frontORcha_presentes ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + lagged_dico_frontORcha_presentes + cat_pais,
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

modelo_4_0 <- glm(dico_debates_eleccion ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                    #lagged_dico_debates_eleccion +
                    lagged_all_previous_elec +
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                data = base_elecciones,
                family = "binomial")
summary(modelo_4_0) 
# definitivamente sin resultados sficativos, en este modelo completo competitividad recupera sficancia aunque al 0.1, y aumenta en magnitud, mantiene signo esperado
# coefs de confianza cambian mucho y son definitivamente no sficativos }
# los resultados se mantienen similares para los distintos indicadores de confianza en medios utilizados
# version log de competitividad no es sficativa

modelo_4_1 <- glm(dico_frontrunner_presente ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                    #lagged_dico_frontrunner_presente + 
                    lagged_all_previous_elec_frontrunner_presente +
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_4_1) 

# VARIABLES LEGISLACION #############
# pendiente 
modelo_5_0 <- glm(dico_debates_eleccion ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                    #lagged_dico_debates_eleccion + 
                    lagged_all_previous_elec +
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    ncat_totreg,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_5_0) 

modelo_5_1 <- glm(dico_frontrunner_presente ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                    #lagged_dico_frontrunner_presente + 
                    lagged_all_previous_elec_frontrunner_presente +
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
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

# probamos con variables dico de normativa 

modelo_51_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + 
                     lagged_all_previous_elec +
                     #lagged_dico_debates_eleccion + 
                     int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_obligatorio_candidato + dico_derechos_candidato,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_51_0) 

modelo_51_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + 
                     #lagged_dico_frontrunner_presente + 
                     lagged_all_previous_elec_frontrunner_presente +
                     int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                    #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_obligatorio_candidato + dico_derechos_candidato,
                  data = base_elecciones,
                  family = "binomial")
summary(modelo_51_1) 
# falsa de significancia en dico_obligatorio_candidatos ha de deberse a muy bajo n, pero tamaño (!) y signo del coeficiente son sugestivos

# probamos con variable dico de normativa pero mas general


modelo_52_0 <- glm(dico_debates_eleccion ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + 
                     #lagged_dico_debates_eleccion + 
                     lagged_all_previous_elec +
                     int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_any_normativa,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_52_0) 

modelo_52_1 <- glm(dico_frontrunner_presente ~ 
                     competitividad + 
                     #log_competitividad +
                     concentracion + 
                     #lagged_dico_frontrunner_presente + 
                     lagged_all_previous_elec_frontrunner_presente +
                     int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_any_normativa,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_52_1) 


######## AGREGAMOS VARIABLE RONDA #############################

modelo_6_0 <- glm(dico_debates_eleccion ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                     ballotage + ballotage*concentracion +
                     lagged_dico_debates_eleccion + 
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_any_normativa,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_6_0) 
# no se bien como interpretar resultados de este modelo, todo pierde sficancia salvo lagged_ :( ???
# igual notese que tamaño y signos de los coeficientes no cambian mucho, podemos estar simplemente teniendo un problema de poca precision, poca info para def parametros
# quizas lo mejor es tener un mejor indicador de fragmentacion y punto
# o quizas el problema es justamente la multicolinealidad entre variables!
# en version log: seguimos sin tener sficancia estadistica

modelo_6_1 <- glm(dico_frontrunner_presente ~ 
                    competitividad + 
                    #log_competitividad +
                    concentracion + 
                    ballotage + ballotage*concentracion +
                     lagged_dico_frontrunner_presente + 
                    int_average_scaled_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     #int_average_confianza_tv_medios_latin_lapop + int_average_confianza_ppols +
                     dico_any_normativa,
                   data = base_elecciones,
                   family = "binomial")
summary(modelo_6_1) 
# conforme con expectativas teoricas, cuando ballotage = 0, concentracion es sficativa. 
# la sficancia de ronda no puede interpretarse a simple vista puesto que en la tabla indica la sficancia de ronda solo cuando competitividad = 0 (elecciones muy muy competitivas, virtual empate)
# signo y valor indican que debates son MENOS probables en un ballotage


####### INTERPRETACION #################################
# buen enlace para leer output :  https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression 
# otro https://stats.oarc.ucla.edu/r/dae/logit-regression/
modelo_a_interpretar <- modelo_52_1 # ajustar segun deseado, por ahora (abr 2024), prefe es modelo_52_1
# EN CUALQUIER CASO CREO QUE ES INTERESANTE COMPRAR MODELOS CON VD ALTERNATIVA
# POR AHORA PARECE QUE VD CUALQ DEBATE ES MAS DEPENDIENTE DE LA CONCENTRACION, MIENTRAS QEU VD DEBATE CON CANDIDATO LIDER ES MAS DEPENIDENTE DE LA COMPETITIVIDAD
# ESTE ES UN RESULTADO INTERESANTE QUE VA EN LA LINEA DE MIS SOSPECHAS PREVIAS

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
valores_posibles_competitividad <- seq(range(log(base_elecciones$competitividad), na.rm=T)[1],range(base_elecciones$competitividad, na.rm=T)[2], 0.1)
valores_posibles_dico <- c(1,0)

# newdata1 <- with(base_elecciones, 
#                  data.frame(lagged_dico_frontrunner_presente = 0,
#                             dico_obligatorio_candidato = 0,
#                             dico_derechos_candidato = 0,
#                             concentracion = mean(concentracion, na.rm=T), 
#                             competitividad = valores_posibles))
# 
# newdata1$predicted_proba <- predict(modelo_a_interpretar, newdata = newdata1, type = "response")

# aca calculamos predicted proba de debate a cada valor de competitividad  para unvarios valores de normativva / trad

newdata2 <- with(base_elecciones, 
                 data.frame(lagged_dico_frontrunner_presente = rep(valores_posibles_dico, times = length(valores_posibles_competitividad)),
                            #dico_obligatorio_candidato = 0,#c(0,1),
                            #dico_derechos_candidato = 0,#c(0,1),
                            dico_any_normativa = 0,
                            concentracion = mean(concentracion, na.rm=T), 
                            competitividad = rep(valores_posibles_competitividad, each = length(valores_posibles_dico))))

#newdata2$predicted_proba <- predict(modelo_a_interpretar, newdata = newdata2, type = "response")

# The code to generate the predicted probabilities (the first line below) is the same as before, except we are also going to ask for standard errors so we can plot a confidence interval. We get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.

newdata3 <- cbind(newdata2, 
                  predict(modelo_a_interpretar, 
                          newdata = newdata2, 
                          type = "link",
                          se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# It can also be helpful to use graphs of predicted probabilities to understand and/or present the model. We will use the ggplot2 package for graphing. Below we make a plot with the predicted probabilities, and 95% confidence intervals.

ggplot(newdata3 %>% 
         mutate(lagged_dico_frontrunner_presente = as.factor(lagged_dico_frontrunner_presente)), 
       aes(x = competitividad, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill= lagged_dico_frontrunner_presente), alpha = 0.2) + 
  geom_line(aes(colour = lagged_dico_frontrunner_presente), size = 1)

# aca calculamos predicted proba de debate a cada valor de competitividad  para unvarios valores de normativva / trad

newdata2 <- with(base_elecciones, 
                 data.frame(lagged_dico_frontrunner_presente = rep(valores_posibles_dico, times = length(valores_posibles_competitividad)),
                            #dico_obligatorio_candidato = 0,#c(0,1),
                            #dico_derechos_candidato = 0,#c(0,1),
                            dico_any_normativa = 1,
                            concentracion = mean(concentracion, na.rm=T), 
                            competitividad = rep(valores_posibles_competitividad, each = length(valores_posibles_dico))))

#newdata2$predicted_proba <- predict(modelo_a_interpretar, newdata = newdata2, type = "response")

# The code to generate the predicted probabilities (the first line below) is the same as before, except we are also going to ask for standard errors so we can plot a confidence interval. We get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.

newdata3 <- cbind(newdata2, 
                  predict(modelo_a_interpretar, 
                          newdata = newdata2, 
                          type = "link",
                          se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# It can also be helpful to use graphs of predicted probabilities to understand and/or present the model. We will use the ggplot2 package for graphing. Below we make a plot with the predicted probabilities, and 95% confidence intervals.

ggplot(newdata3 %>% 
         mutate(lagged_dico_frontrunner_presente = as.factor(lagged_dico_frontrunner_presente)), 
       aes(x = competitividad, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill= lagged_dico_frontrunner_presente), alpha = 0.2) + 
  geom_line(aes(colour = lagged_dico_frontrunner_presente), size = 1)

# REPETIMOS PERO ahora ploteamos cambiando los valores de variantes en la normativa
# igual a ajustar segun modelo finalmente elegido

newdata2 <- with(base_elecciones, 
                 data.frame(lagged_dico_frontrunner_presente = 0,
                            #dico_obligatorio_candidato = 0,#c(0,1), # no entiendo bien que pasa cuando esta v es 1 y la que sigue 0, quizas se debe a la falta de casos reales, insuf variablilidad, sobre det # claro si, predicted proba es siempre 1 
                            #dico_derechos_candidato = 1,#c(0,1),
                            dico_any_normativa = rep(valores_posibles_dico, times = length(valores_posibles_competitividad)),
                            concentracion = mean(concentracion, na.rm=T), 
                            competitividad = rep(valores_posibles_competitividad, each = length(valores_posibles_dico))))

# The code to generate the predicted probabilities (the first line below) is the same as before, except we are also going to ask for standard errors so we can plot a confidence interval. We get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.

newdata3 <- cbind(newdata2, 
                  predict(modelo_a_interpretar, 
                          newdata = newdata2, 
                          type = "link",
                          se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# It can also be helpful to use graphs of predicted probabilities to understand and/or present the model. We will use the ggplot2 package for graphing. Below we make a plot with the predicted probabilities, and 95% confidence intervals.

ggplot(newdata3 %>% 
         mutate(dico_any_normativa = as.factor(dico_any_normativa)), 
       aes(x = competitividad, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill= dico_any_normativa), alpha = 0.2) + 
  geom_line(aes(colour = dico_any_normativa), size = 1)
# ATENTI: CUANDO HAY TRADICION, EFECTO DE NORMATIVA TB SE VUELVE INDISTINGUIBLE


# invertimos posicion de variables en este ploteo ####

newdata2 <- with(base_elecciones, 
                 data.frame(lagged_dico_frontrunner_presente = 1,
                            #dico_obligatorio_candidato = 0,#c(0,1), # no entiendo bien que pasa cuando esta v es 1 y la que sigue 0, quizas se debe a la falta de casos reales, insuf variablilidad, sobre det # claro si, predicted proba es siempre 1 
                            #dico_derechos_candidato = 1,#c(0,1),
                            dico_any_normativa = rep(valores_posibles_dico, times = length(valores_posibles_competitividad)),
                            concentracion = mean(concentracion, na.rm=T), 
                            competitividad = rep(valores_posibles_competitividad, each = length(valores_posibles_dico))))


# The code to generate the predicted probabilities (the first line below) is the same as before, except we are also going to ask for standard errors so we can plot a confidence interval. We get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.

newdata3 <- cbind(newdata2, 
                  predict(modelo_a_interpretar, 
                          newdata = newdata2, 
                          type = "link",
                          se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# It can also be helpful to use graphs of predicted probabilities to understand and/or present the model. We will use the ggplot2 package for graphing. Below we make a plot with the predicted probabilities, and 95% confidence intervals.

ggplot(newdata3 %>% 
         mutate(dico_any_normativa = as.factor(dico_any_normativa)), 
       aes(x = competitividad, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill= dico_any_normativa), alpha = 0.2) + 
  geom_line(aes(colour = dico_any_normativa), size = 1)
# y cuando no hay tradicion tb? no se bien que hacer de esto

# assesing fit: distancia entre data original y proba predicha ###############


base_elecciones_predicted_outcome <- cbind(base_elecciones, 
                  predict(modelo_a_interpretar, 
                          newdata = base_elecciones, 
                          type = "link",
                          se = TRUE))

base_elecciones_predicted_outcome$predicted_outcome <-  predict(modelo_a_interpretar, 
                                                                        newdata = base_elecciones, 
                                                                        type = "response")

base_elecciones_predicted_outcome <- base_elecciones_predicted_outcome %>% 
  mutate(diff_predicted_outcome = dico_frontrunner_presente - predicted_outcome) %>% 
  mutate(abs_diff_predicted_outcome = diff_predicted_outcome %>% abs())

check <- base_elecciones_predicted_outcome %>% 
  select(abs_diff_predicted_outcome, diff_predicted_outcome, electionid, 
         dico_debates_eleccion, dico_frontrunner_presente, predicted_outcome,
         competitividad, concentracion, lagged_dico_frontrunner_presente, dico_any_normativa,
         ballotage)