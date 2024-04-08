# MODELO DE PROBA DE AUSENCIAS PARA LOS CANDIATOS

# version borrador 8 abril 2024

# LIBRERIAS #####

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_elecciones <- read.csv("all_elections.csv")

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

# VARIABLES LEGISLACION #############
# pendiente 

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