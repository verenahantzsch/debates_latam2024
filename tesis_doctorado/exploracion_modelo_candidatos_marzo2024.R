# MODELO DE PROBA DE AUSENCIAS PARA LOS CANDIATOS

# version borrador 12 marzo 2024

# LIBRERIAS #####

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_candidatos_matcheados <- read.csv("base_candidatos_matcheados2023.csv")

# TRABAJO ########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")
options(scipen=999)

# para estos modelos conviene sacar segunda ronda, ya que por definicion no hay debates fallidos, es sesgada

data <- base_candidatos_matcheados %>% 
  subset(ncat_ronda==1)

# exploracion de VD
table(data$dico_candidato_presente)

# modelos

# MODELOS CON POPULARIDAD DE CANDIDATO COMO BASE ##########

# modelo base: popularidad del candidato 
#posibilidad de mejor indicador para popularidad, que contemple popularidad *relativa*
#-desvio estandar de algo?
#-diferencia con algo?
#-dico_delantera?
modelo_base <- glm(dico_candidato_presente ~ mean_encuestas,
                      data = data,
                      family = "binomial")
summary(modelo_base)

#predict_reg <- predict(modelo_base,
#                       base_candidatos_matcheados, type = "response")

# el signo es conforme a lo esperado 
# si o si deberia incorporar si hubo debates anteriormente / si candidato asistio a debate anterior
# para precisar estimacion, por ejemplo abajo, si agrego un efecto fijo por pais,
# el mean_encuestas se vuelve estadisticamente sficativo y aumenta ligeramente en magnitud
# tal vez pueda tb hacer alguna estimacion multinivel

# modelo con fixed effects por pais 
modelo_1 <- glm(dico_candidato_presente ~ mean_encuestas + cat_pais,
                   data = data,
                   family = "binomial")
summary(modelo_1)

# modelo con incumbency 
modelo_2 <- glm(dico_candidato_presente ~ mean_encuestas + gov_,
                data = data,
                family = "binomial")
summary(modelo_2)

# modelo con incumbency y fixed effects
modelo_3 <- glm(dico_candidato_presente ~ mean_encuestas + gov_ + cat_pais,
                data = data,
                family = "binomial")
summary(modelo_3)

# siempre el signo es en la direccion esperada, siempre estadisticamente significativo
# magnitud varia ligeramente entre especificaciones
# magnitud es relativamente pequeña. ver como interpretar. 


# MODELOS CON ORDEN DE LIDERAZGO DE CANDIDATO COMO BASE ##########

# creamos variables
ranking <- data %>% 
  select(cat_pais, ncat_eleccion, nombres_candidatos, mean_encuestas) %>%
  unique() %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  mutate(rank = rank(100-mean_encuestas))

data <- data %>% 
  left_join(ranking) %>% 
  mutate(frontrunner = ifelse(rank==1,1,0),
         challenger = ifelse(rank==2,1,0),
         tail = ifelse(frontrunner==0&challenger==0,1,0))

# nignuno de los modelos basicos que siguen da resultados significativos
modelo_base <- glm(dico_candidato_presente ~ rank,
                   data = data,
                   family = "binomial")
summary(modelo_base)

modelo_1 <- glm(dico_candidato_presente ~ frontrunner + challenger, # dejamos fuera tail como cat base
                   data = data,
                   family = "binomial")
summary(modelo_1)

modelo_2 <- glm(dico_candidato_presente ~ frontrunner + challenger + cat_pais, # dejamos fuera tail como cat base
                data = data,
                family = "binomial")
summary(modelo_2)

# MODELO CON DIFERENCIA O DISTANCIA EN LA COMPETENCIA ##########
# pendiente pensar como construir indicador