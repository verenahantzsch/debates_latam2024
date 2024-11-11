

# librerias ########### 

library(tidyverse)

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_unificada <- read.csv("indicadores_elecciones.csv")
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

#### primeras versiones viejas #######
# test basicos
totest <- base_unificada %>% 
  select(-starts_with("source_")) %>% 
  select(-cat_pais, -eng_cat_pais) #%>% 
#mutate(across(everything(), as.integer(.)))
summary(totest)
correlation_matrix <- cor(totest , use = "pairwise.complete.obs" )
corrplot::corrplot(correlation_matrix)

logit_model <- glm(dico_debates_eleccion ~ 
                     nec + 
                     marginvic + 
                     #newparties + 
                     #withinsv + 
                     #alineamiento +
                     dico_reeleccion +
                     voteshareincumbent + 
                     #proptv +
                     propindivinternet +
                     #prohibicionpropaganda +
                     regulaciondico +
                     #satisfaccion +
                     #prop_elec_usa_ciclo +
                     lagpropdebatesregion, 
                   family = binomial(link = "logit"), 
                   data = totest)
summary(totest)
summary(logit_model)


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

base <- base_unificada  %>% 
  select(-starts_with("source_")) %>% 
  select(-X, -eng_cat_pais)


variable_dependiente <- base$dico_debates_eleccion
options(scipen=999)

# tests bivariados ##############


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

variable_independiente <- base$marginvic
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


# regulaciones: acceso gratuito - VWEAK/NO + NO SFICATIVA #######

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

