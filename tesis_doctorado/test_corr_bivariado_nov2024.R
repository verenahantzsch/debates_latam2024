

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
# acomodo data preparacion #######

# para interpretacion del Taub b
# https://blogs.sas.com/content/iml/2023/04/05/interpret-spearman-kendall-corr.html
# https://datatab.net/tutorial/kendalls-tau

# CHATGPT OJO: For practical interpretation, you might use these general thresholds:
#   
#   0.00 to ±0.10: Very weak to no association
# ±0.10 to ±0.19: Weak association
# ±0.20 to ±0.29: Moderate association
# ±0.30 and above: Strong association

base <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-X, -eng_cat_pais) %>% 
  left_join(base_vdependiente)



variable_dependiente <- base$dico_hubo_debates
options(scipen=999)


## INDICADORES. tests bivariados ##############


# año electoral - STRONG POSITIVE + SIGNIFICATIVA #######

variable_independiente <-  base$ncat_eleccion

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

# ronda - VWEAK / NO + NO SFICATIVA #######

base <- base %>% 
  mutate(ncat_ronda = ifelse(ncat_ronda==2,1,
                             ifelse(ncat_ronda==1,0,NA)))
# variable_independiente <-  base$ncat_ronda %>% mutate(ifelse(ncat_ronda==1,0,
#                                                              ifelse(ncat_ronda==2,1)))
variable_independiente <-  base$ncat_ronda

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 


# fragmentacion: NEC - WEAK POSITIVE + SFICATIVA #######

variable_independiente <-  base$nec
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# fragmentacion: n_candidatos - VWEAK/NO + NO SFICATIVA #######

variable_independiente <- base$ntc
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# volatilidad: overall - VWEAK/NO + NO SFICATIVA #######

variable_independiente <- base$volatility
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# volatilidad: intrasistemica - VWEAK/NO + Signo esperado, pero NO SFICATIVA #######

variable_independiente <- base$withinsv
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# volatilidad: extrasistemica - VWEAK/NO + NO SFICATIVA #######

variable_independiente <- base$newparties
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# desalineamiento (% personas que YES se identifican con partido). MODERATE NEGATIVE COR + SFICATIVA  #######

variable_independiente <- base$alineamiento
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# descontento WEAK NEGATIVE + NO SFICATIVA #######

variable_independiente <- base$satisfaccion
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# competitividad (marginvic) VWEAK/NO + SFICATIVA  #######

# basem <- base %>%
#  subset(marginvic>-75)
# variable_independiente <- abs(basem$marginvic)
# variable_dependiente <- basem$dico_debates_eleccion

variable_independiente <- abs(base$marginvic)
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# incumbent: vote share incumbent VWEAK/NO + NO SFICATIVA #######

variable_independiente <- base$voteshareincumbent
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# incumbent: exec approval (img) WEAK NEGATIVE + SFICATIVA #######
# Calculate Kendall's Tau-b correlation
tau_b <- cor(base$exapprovalsmoothed, base$dico_debates_eleccion, method = "kendall", use = "complete.obs")
tau_b
tau_b <- cor(base$exapprovalnotsmoothed, base$dico_debates_eleccion, method = "kendall", use = "complete.obs")
tau_b


variable_independiente <- base$exapprovalnotsmoothed
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# incumbent: reelije WEAK NEGATIVE  + SFICATIVA #######

variable_independiente <- base$dico_reeleccion
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa


# incumbent: oficialismo tiene candidato - VWEAK/NO  + NO SFICATIVA #######

variable_independiente <- base$dico_oficialista
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa


# media : proptv - WEAK POSITIVE  + SFICATIVA #######

variable_independiente <- base$proptv
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# media : propint  - STRONG POSITIVE + SFICATIVA #######

variable_independiente <- base$propindivinternet
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# regulaciones: prohibicion propaganda - MODERATE POSITIVE + SFICATIVA  #######

variable_independiente <- base$prohibicionpropaganda
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa


# regulaciones: acceso gratuito - VWEAK/NO + SFICATIVA #######

variable_independiente <- base$accesogratuito
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# regulaciones sobre debates: ordinal - STRONG POSITIVE + SFICATIVA #######

variable_independiente <- base$regulacionordinal
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# regulaciones sobre debates: dicotomica - STRONG POSITIVE  + SFICATIVA #######

variable_independiente <- base$regulaciondico
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa


# lagged: dico ciclo anterior - STRONG POSITIVE + SFICATIVA #######

variable_independiente <- base$dico_debates_pastelection
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa


# lagged: cumsum ciclos anteriores - STRONG POSITIVE #######

variable_independiente <- base$cumsum_pastciclos
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# region  avgprop x ciclo - MODERATE/STRONG POSITIVE + SFICATIVA #######

variable_independiente <- base$avgpropdebatesregionxciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# region lag n  - MODERATE POSITIVE + SFICATIVA #######

variable_independiente <- base$lagndebatesregion
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# region lag prop  - MODERATE POSITIVE + SFICATIVA #######

variable_independiente <- base$lagpropdebatesregion
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# USA prop - WEAK POSITIVE + SFICATIVA #######

variable_independiente <- base$prop_elec_usa_ciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# USA n - WEAK/MODERATE POSITIVE + SFICATIVA #######

variable_independiente <- base$n_debates_usa_ciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# USA dico - MODERATE POSITIVE + SFICATIVA #######

variable_independiente <- base$dico_debates_usa_ciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

## CONTROLES. tests bivariados ######
# acomodo data preparacion #######


controles <- base_controles %>% 
  select(-starts_with("source_")) %>% 
  select(-X, -eng_cat_pais) %>% 
  left_join(base_vdependiente)

variable_dependiente <- controles$dico_hubo_debates

options(scipen=999)

# Desarrollo/PBI per capita: - MODERATE POSITIVE + SFICATIVA #######

# ajustados a 2015
variable_control <-  controles$gdpxcapita
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# ajustados a 2010
variable_control <-  controles$gdpxcapita2
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferenc

# Desarrollo/Urban pop - Moderada + sficativa #######

variable_control <-  controles$urbanpop
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 


# Desarrollo/ indice undp hdi - moderada + sficativa #######

variable_control <-  controles$undphdi
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado)  

# Polarizacion: VWEAK/NO + negative (signo esperado) + no sficativa #######

variable_control <-  controles$polar
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Democracia: MODERATE + POSITIVA + SFICATIVA #######

# PPOLITY 
variable_control <-  controles$democraciappolity
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 


# VDEM  
variable_control <-  controles$democraciavdempolyarchy
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# VDEM - electoral component
variable_control <- controles$democraciavdemelectoralcomp
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Sistema electoral/runoff clausula: VWEAK/NO + NO SFICATIVA #######

variable_control <-  controles$runoff
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Sistema electoral/concurrent: VWEAK/NO + NO SFICATIVA #######

variable_control <-  controles$concelec
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Sistema electoral/compulsory voting: VWEAK/NO + NO SFICATIVA #######

variable_control <-  controles$compulsoryvoting
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Turnout: VWEAK/NO + NO SFICATIVA  #######

variable_control <-  controles$turnout
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 


# Edad regimen: MODERADA + SFICATIVA #######

# preferido: bmr_demdur extendido
variable_control <-  controles$edadregimenfilled
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# bmr_demdur # no sficativa pero ojo que combina mediciones de no democraticos
variable_control <-  controles$edadregimenbmr
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# duracion seg ppolity
variable_control <-  controles$edadregimenppolity
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 


# duracion seg mainw
variable_control <-  controles$edadregimenmainw
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Medios / VDEM : MODERADO + SFICATIVO  #######

# preferido: vdem
variable_control <-  controles$mediaqualitycorruptvdem
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 


# Medios/ conf tv seg wvs: WEAK - SIGNO CONTRARIO AL ESPERADO - SFICATIVO ####
# (137 observations deleted due to missingness)
# ordinal, mas alto mas confianza

variable_control <-  controles$mediaqualityconftvwvs
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Medios/ nelda bias before election: WEAK + signo negativo (esperado) + sficativo ####
# Values:
#   0. No
# 1. Yes
# 3. Unclear
controles <-  controles %>% 
  mutate(mediaqualitybiasnelda = ifelse(mediaqualitybiasnelda==3, NA, mediaqualitybiasnelda))

variable_control <-  controles$mediaqualitybiasnelda 
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# Medios/ freedom: Moderate + positive + sficativa ####
#   (134 observations deleted due to missingness)

variable_control <-  controles$mediaqualityfreedombti 
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 



## CORRELACIONES ENTRE VARIABLES INDEP #####

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

## MODELOS DE PRUEBA #####

### preparo data #####
# data completa 
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

# creo data estandarizada
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

# data solo democracias
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

### modelos convencionales. DICO HUBO DEBATES #####
# Modelo contingencia  
 
modelo_contingencia_controles <- glm(dico_hubo_debates ~ 
                                       #dico_debates_primerosdos ~ 
                                       #dico_hubo_debate_mediatico ~ 
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
                               mediaqualitycorruptvdem,
                               #edadregimenbmr#, #Sacar edad de régimen.  
                            #No tiene sentido incluirla si controlas por la calidad de la democracia 
                            # ademas CAMBIAN MUY POCO resultados relevantes, chequeado
                           family = binomial(link = "logit"), 
                           #data = data 
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

modelo_sistemico_controles <- glm(dico_hubo_debates ~ 
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
                                  #data = data 
                                  data = democracias)

summary(modelo_sistemico_controles) # indeterminado
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

modelo_regulatorio_controles <- glm(dico_hubo_debates ~ 
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

modelo_temporal <- glm(dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         democraciavdemelectoralcomp +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         mediaqualitycorruptvdem +
                         regulaciondico,
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

# all <- glm(dico_hubo_debates ~ 
#              ncat_eleccion +
#            gdpxcapita +
#            #democraciavdempolyarchy +
#              democraciavdemelectoralcomp +
#            urbanpop +
#            mediaqualitycorruptvdem +
#            #mediaqualitybiasnelda +
#            #mediaqualityfreedombti +
#            edadregimenfilled +
#            marginvic +
#            #exapprovalnotsmoothed +
#              voteshareincumbent +
#            dico_reeleccion +
#            dico_debates_pastelection +
#            alineamiento +
#            nec +
#            proptv +
#            propindivinternet +
#            regulaciondico +
#            prohibicionpropaganda +
#            avgpropdebatesregionxciclo +
#            prop_elec_usa_ciclo,
#            family = binomial(link = "logit"), 
#            data = data)
# summary(all)


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
 
### primera version de LOOP con DICO HUBO DEBATES para DICO DEBATES PASTELECCION #####

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

### modelos nada interesante por ahora, repensar seg MAINW, toca subd la muestra DICO NUEVA PRACTICA  #####
# PENDIENTES, no sirve asi 

data2 <- data %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) 
  #subset(dico_debates_pastelection==0) 
summary(data2)

democracias2 <- democracias %>% 
  subset(dico_debates_pastelection==0|is.na(dico_debates_pastelection)) 
#subset(dico_debates_pastelection==0) 
summary(democracias2)

# Modelo contingencia  

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


# sin debates y sin regulacion 


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

### modelos nada interesante por ahora PRACTICA INTERRUMPIDA  #####
# PENDIENTES, no sirve asi, repensar seg MAINW, toca subd la muestra 


data3 <- data %>% 
  subset(dico_debates_pastelection==1) 
summary(data3)

democracias3 <- democracias %>% 
  subset(dico_debates_pastelection==1) 
summary(democracias3)

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

## INDICADORES CANDIDATOS. tests bivariados ##############


# voteshare #######

variable_independiente <-  base$voteshare
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 
 

# v2pariglef #######

variable_independiente <-  base$v2pariglef_vdem
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# v2paactcom_vdem #######

variable_independiente <-  base$v2paactcom_vdem
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# dico_reeleccion #######

variable_independiente <-  base$dico_reeleccion
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# dico_oficialista #######

variable_independiente <-  base$dico_oficialista
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# ninvitaciones #######

variable_independiente <-  base$ninvitaciones
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# experiencia pasada: propausenciaspasadas #######

variable_independiente <-  base$propausenciaspasadas
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# experiencia pasada: propausenciaspasadasfilled #######

variable_independiente <-  base$propausenciaspasadasfilled
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# experiencia pasada: npresenciaspasadas #######

variable_independiente <-  base$npresenciaspasadas
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# experiencia pasada: nausenciaspasadas #######

variable_independiente <-  base$nausenciaspasadas
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

# experiencia pasada: dicoausenciaspasadas #######

variable_independiente <-  base$dicoausenciaspasadas
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# experiencia pasada: dicopresenciaspasadas #######

variable_independiente <-  base$dicopresenciaspasadas
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa

# tipo debate: osc #######

variable_independiente <-  base$orgosc
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# tipo debate: estado #######

variable_independiente <-  base$orgestado
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

# tipo debate: mmc #######

variable_independiente <-  base$orgmmc
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) 

## CORRELACIONES ENTRE VARIABLES INDEP #####

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

## MODELOS DE PRUEBA #####

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

# ## OLD primeras versiones viejas #######
# # test basicos
# totest <- base_indicadores %>% 
#   select(-starts_with("source_")) %>% 
#   select(-cat_pais, -eng_cat_pais) #%>% 
# #mutate(across(everything(), as.integer(.)))
# summary(totest)
# correlation_matrix <- cor(totest , use = "pairwise.complete.obs" )
# corrplot::corrplot(correlation_matrix)
# 
# logit_model <- glm(dico_debates_eleccion ~ 
#                      nec + 
#                      marginvic + 
#                      #newparties + 
#                      #withinsv + 
#                      #alineamiento +
#                      dico_reeleccion +
#                      voteshareincumbent + 
#                      #proptv +
#                      propindivinternet +
#                      #prohibicionpropaganda +
#                      regulaciondico +
#                      #satisfaccion +
#                      #prop_elec_usa_ciclo +
#                      lagpropdebatesregion, 
#                    family = binomial(link = "logit"), 
#                    data = totest)
# summary(totest)
# summary(logit_model)
