# MODELO DE PROBA DE AUSENCIAS PARA LOS CANDIATOS

# version borrador 12 marzo 2024

# LIBRERIAS #####

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_candidatos_matcheados <- read.csv("base_candidatos_matcheados2023.csv")
base_elecciones <- read.csv("all_elections.csv") 

#u_elec <- base_elecciones$electionid %>% unique()
#u_elec <- base_candidatos_matcheados$electionid %>% unique()

# TRABAJO ########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")
options(scipen=999)

# DATA WRANGLING #############
# para estos modelos conviene sacar segunda ronda, ya que por definicion no hay debates fallidos, es sesgada

data <- base_candidatos_matcheados %>% 
  subset(ncat_ronda==1)

# uno data anual
base_elecciones <- base_elecciones %>% 
  select(-X) %>% 
  subset(dico_debates_eleccion==1&ncat_ronda==1) %>% 
  select(-electionid) %>% 
  mutate(electid = paste(cat_pais, ncat_eleccion))

u_elec_encuestas <- base_elecciones$electid %>% unique()

data <- data %>% 
  left_join(base_elecciones)

u_elec_debates <- data$electid %>% unique() 
# por algun motivo hay 7 elecciones de las que tenemos pero no tenemos data. 
#tiene que ver con las fechas, si extendemos la ventana de encuestas los abarcamos casi todos

# CHEQUEO
# Elements present in vector2 but not in vector1
#missing_in_vector <- u_elec_encuestas[!u_elec_encuestas %in% u_elec_debates]
#[1] "Chile 1989"      "Chile 2000"      "Costa Rica 2010" "Guatemala 2007"  "Mexico 1994"     "Mexico 2000"     "Paraguay 2023"  

# creamos variables # QUIZAS SERIA MEJOR IMPORTAR ESTAS VARIABLES DESDE EL DATASET COMPLETO 
# ranking <- data %>% 
#   select(cat_pais, ncat_eleccion, nombres_candidatos, mean_encuestas) %>%
#   unique() %>% 
#   group_by(ncat_eleccion, cat_pais) %>% 
#   mutate(rank = rank(100-mean_encuestas))
# 
# data <- data %>% 
#   left_join(ranking) %>% 
#   mutate(frontrunner = ifelse(rank==1,1,0),
#          challenger = ifelse(rank==2,1,0),
#          tail = ifelse(frontrunner==0&challenger==0,1,0))

data <- data %>% 
  mutate(dico_frontrunner = ifelse(nombres_candidatos==frontrunner,1,0),
         dico_challenger = ifelse(nombres_candidatos==challenger,1,0),
         dico_tail = ifelse(dico_frontrunner==0&dico_challenger==0,1,0))

# exploracion de VD #############
table(data$dico_candidato_presente)

# exploracion de VIs ############## 

# VENTAJA ABSOLUTA #
hist(data$mean_encuestas) # ojo que la variable esta algo sesgada y quizas haya un outlier
boxplot(data$mean_encuestas) # hay un outlier

mean_values <- aggregate(mean_encuestas ~ dico_candidato_presente, data, mean, na.rm = TRUE)
ggplot(data) + geom_boxplot(aes(as.factor(dico_candidato_presente), mean_encuestas))
# interesante que igual outlier estuvo presente

# VENTAJA RELATIVA #
table(data$dico_frontrunner)
table(data$dico_challenger)
table(data$dico_tail)

mean_values <- aggregate(dico_frontrunner ~ dico_candidato_presente, data, mean, na.rm = TRUE)
mean_values <- aggregate(dico_challenger ~ dico_candidato_presente, data, mean, na.rm = TRUE)
mean_values <- aggregate(dico_tail ~ dico_candidato_presente, data, mean, na.rm = TRUE)
# frontrunner mas ausente que presente
# challenger casi igual, pero ligeramente mas presente
# tail mas presente que ausente

# VENTAJA EN DIFERENCIA #
# PENDIENTE ARMAR VARIABLE #

# NIVEL ELECCION: AGENDA #
hist(data$n_debates_eleccion)
boxplot(data$n_debates_eleccion) # hay una tail asimetrica pero no hay outliers
mean_values <- aggregate(n_debates_eleccion ~ dico_candidato_presente, data, mean, na.rm = TRUE)
ggplot(data) + geom_boxplot(aes(as.factor(dico_candidato_presente), n_debates_eleccion))
# hay mas ausentes cuando hay mas debates en la campaña

# INCUMBENCY #
table(data$gov_)
table(data$gov_, data$dico_candidato_presente) # ojo, tenemos muy pocos incumbents y muchos missing, mejor quizas no usar

mean_values <- aggregate(gov_ ~ dico_candidato_presente, data, mean, na.rm = TRUE)

# TRAD DEBATES # # considerar tambien versiones VD "frontrunner"
# dadas las caracts de este modelo (sesgado a solo elec con debates), y supuestos, quizas es mas pertinente segunda version (cuanti)
# dico: ult eleccion #
table(data$lagged_dico_debates_eleccion) 
table(data$lagged_dico_debates_eleccion, data$dico_candidato_presente)  

mean_values <- aggregate(lagged_dico_debates_eleccion ~ dico_candidato_presente, data, mean, na.rm = TRUE) # pocas diferencias, pero ligeramente mas presentes cuando HUBO en elec pasada

# cuanti: cuantas elecs anteriores tuvieron debates # 
hist(data$lagged_all_previous_elec)
boxplot(data$lagged_all_previous_elec) # es asimetrica pero sin outliers
mean_values <- aggregate(lagged_all_previous_elec ~ dico_candidato_presente, data, mean, na.rm = TRUE)
ggplot(data) + geom_boxplot(aes(as.factor(dico_candidato_presente), lagged_all_previous_elec))
# muy pocas diferencias aunque pareceria que va contra la expectativa teorica
# quizas por hecho de que correlaciona con AGENDA

# NORMATIVA #
# versiones dico #
data <- data %>% 
  mutate(dico_obligatorio_candidato = ifelse(ncat_regcandidatos==3,1,0),
         dico_derechos_candidato = ifelse(ncat_regcandidatos==2,1,0)) %>% 
  mutate(dico_any_normativa = ifelse(ncat_regcandidatos>=2,1,0))

table(data$dico_obligatorio_candidato)  # ojo con asimetria
table(data$dico_obligatorio_candidato, data$dico_candidato_presente)  # curioso hay 5 que faltan cuando es obligatorio? chequear
mean_values <- aggregate(dico_obligatorio_candidato ~ dico_candidato_presente, data, mean, na.rm = TRUE)  

check <- data %>% 
  subset(dico_obligatorio_candidato==1&dico_candidato_presente==0) # claro, candidatos que faltaron a debates otros que los obligatorios 

table(data$dico_derechos_candidato) 
table(data$dico_derechos_candidato, data$dico_candidato_presente)  
mean_values <- aggregate(dico_derechos_candidato ~ dico_candidato_presente, data, mean, na.rm = TRUE) # la ley derechos efectivamente es menos efectiva en prevenir faltar 

table(data$dico_any_normativa) 
table(data$dico_any_normativa, data$dico_candidato_presente)  
mean_values <- aggregate(dico_any_normativa ~ dico_candidato_presente, data, mean, na.rm = TRUE) # pocas diferencias, pero ligeramente mas presentes cuando no hay normativa, puede de nuevo tener que ver con correlac con TRAD y AGENDA

# version ordinal # 
hist(data$ncat_totreg)
boxplot(data$ncat_totreg) # es asimetrica pero sin outliers
mean_values <- aggregate(ncat_totreg ~ dico_candidato_presente, data, mean, na.rm = TRUE)
ggplot(data) + geom_boxplot(aes(as.factor(dico_candidato_presente), ncat_totreg))


# OTRAS DE MODELO ELECCIONES QUE SEAN APLICABLES? LAS SFICATIVAS? TODAS? #
# OTRAS QUE PODAMOS RECONSTRUIR DE CANDIDATOS? IDEO? GENERO? PARTIDO? EDAD? #

### MULTICOLINEALIDAD CRUZADA ######
# Calculate the correlation matrix
#full_data <- base_elecciones # reserva

cor_data <- data %>% 
  select(dico_candidato_presente,
         mean_encuestas,
         n_debates_eleccion,
         dico_frontrunner,
         dico_challenger,
         dico_tail,
         gov_,
         lagged_dico_debates_eleccion,
         lagged_all_previous_elec,
         dico_obligatorio_candidato,
         dico_derechos_candidato,
         dico_any_normativa,
         ncat_totreg #,
         )

correlation_matrix <- cor(cor_data, use= "na.or.complete")

# Visualize the correlation matrix
corrplot::corrplot(correlation_matrix, method = "circle")


# modelos #########################

# MODELO PARA IR PROBANDO INDICADORES ALTERNATIVOS ##############

modelo_test <- glm(dico_candidato_presente ~ 
                   
                   #mean_encuestas +
                   dico_frontrunner +
                   dico_challenger +
                   #dico_tail +
                   #gov_ +
                   #lagged_dico_debates_eleccion +
                   lagged_all_previous_elec +
                   dico_obligatorio_candidato +
                   dico_derechos_candidato +
                   #dico_any_normativa +
                   #ncat_totreg +
                   n_debates_eleccion,
                  
                   data = data,
                   family = "binomial")
summary(modelo_test)

# en lineas generales, por ahora resultados interesantes y en linea con las expectativas teoricas
# variable gov_ tira muchas observaciones
# estaria bueno incorporar mas variables del nivel de los candidatos . PENDIENTE pensar como abordar esto metodologicamente

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

# el signo es conforme a lo esperado (negativo: mas puntaje, menos proba de asistir al debate)
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

# este no da resultados significativos
modelo_base <- glm(dico_candidato_presente ~ rank,
                   data = data,
                   family = "binomial")
summary(modelo_base)

# estos si 
modelo_1 <- glm(dico_candidato_presente ~ frontrunner + challenger, # dejamos fuera tail como cat base
                   data = data,
                   family = "binomial")
summary(modelo_1)

modelo_2 <- glm(dico_candidato_presente ~ frontrunner + challenger + cat_pais, # dejamos fuera tail como cat base
                data = data,
                family = "binomial")
summary(modelo_2) #curioso que en este modelo el challenger tb tiene menos proba de asistir 

# MODELO CON DIFERENCIA O DISTANCIA EN LA COMPETENCIA ##########
# pendiente pensar como construir indicador
# quizas un puntaje z intra pais??