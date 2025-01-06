

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

## REPITO PARA VARIABLES SELECCIONADAS Y DATA FILTRADA (AGREGADO POSTERIORMENTE)  #####


democracias <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-starts_with("X")) %>% 
  select(-eng_cat_pais, -dico_debates_eleccion) %>% 
  left_join(base_controles %>% 
              select(-starts_with("source_")) %>% 
              select(-X, -eng_cat_pais)) %>% 
  left_join(base_vdependiente %>% select(-X)) %>% 
  subset(democraciavdempolyarchy>0.45) 


variable_dependiente <- democracias$dico_hubo_debates
options(scipen=999)

# año electoral - STRONG POSITIVE + SIGNIFICATIVA #######
nombre_variable <- "Año Elección"
variable_independiente <-  democracias$ncat_eleccion

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 
coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     #coef_estandarizado_tbl,
                     coef_tbl)


data_descriptiva <- summary_tbl

# fragmentacion: NEC - WEAK POSITIVE + SFICATIVA #######
nombre_variable <- "N.E.C."

variable_independiente <-  democracias$nec
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log()) # quizas amerita

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# desalineamiento (% personas que YES se identifican con partido). MODERATE NEGATIVE COR + SFICATIVA  #######
nombre_variable <- "Alineamiento partidario"

variable_independiente <- democracias$alineamiento
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# competitividad (marginvic) VWEAK/NO + SFICATIVA  #######
nombre_variable <- "Margen de victoria"

# basem <- base %>%
#  subset(marginvic>-75)
# variable_independiente <- abs(basem$marginvic)
# variable_dependiente <- basem$dico_debates_eleccion

variable_independiente <- abs(democracias$marginvic)
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log()) # quizas amerita

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# incumbent: vote share incumbent VWEAK/NO + NO SFICATIVA #######
nombre_variable <- "% voto oficialista"

variable_independiente <- democracias$voteshareincumbent
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# media : proptv - WEAK POSITIVE  + SFICATIVA #######
nombre_variable <- "% hogares con TV"

variable_independiente <- democracias$proptv
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# media : propint  - STRONG POSITIVE + SFICATIVA #######
nombre_variable <- "% individuos con Internet"

variable_independiente <- democracias$propindivinternet
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# regulaciones: prohibicion propaganda - MODERATE POSITIVE + SFICATIVA  #######
nombre_variable <- "Prohibición propaganda en TV (dummy)"

variable_independiente <- democracias$prohibicionpropaganda
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
#hist(variable_independiente %>%  log())

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# regulaciones: acceso gratuito - VWEAK/NO + SFICATIVA #######
nombre_variable <- "Acceso gratuito a TV (dummy)"

variable_independiente <- democracias$accesogratuito
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
#hist(variable_independiente %>%  log())

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# regulaciones sobre debates: dicotomica - STRONG POSITIVE  + SFICATIVA #######
nombre_variable <- "Debates televisados regulados (dummy)"

variable_independiente <- democracias$regulaciondico
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
#hist(variable_independiente %>%  log())

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# lagged: cumsum ciclos anteriores - STRONG POSITIVE #######
nombre_variable <- "Cantidad de elecciones pasadas con debates"

variable_independiente <- democracias$cumsum_pastciclos
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


# region  avgprop x ciclo - MODERATE/STRONG POSITIVE + SFICATIVA #######
nombre_variable <- "Prop. promedio de elecciones con debates en región"

variable_independiente <- democracias$avgpropdebatesregionxciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# USA prop - WEAK POSITIVE + SFICATIVA #######
nombre_variable <- "Prop. de elecciones con debates en USA"

variable_independiente <- democracias$prop_elec_usa_ciclo
standardized_variable_independiente <- (variable_independiente - mean(variable_independiente, na.rm = TRUE)) / sd(variable_independiente, na.rm = TRUE)

summary_obj <- summary(variable_independiente) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_independiente, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_independiente = variable_independiente,
               variable_dependiente = variable_dependiente)

hist(variable_independiente)
table(variable_independiente)
#hist(variable_independiente %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
#table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_independiente,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_independiente,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# Desarrollo/PBI per capita: - MODERATE POSITIVE + SFICATIVA #######
nombre_variable <- "PBI per cápita"

# ajustados a 2015
variable_control <-  democracias$gdpxcapita
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

summary_obj <- summary(variable_control) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_control, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

hist(variable_control)
hist(variable_control %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b
pearson <- cor(variable_control, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# Democracia VDEM #######
nombre_variable <- "Nivel de democracia (componente electoral VDEM)"

variable_control <-  democracias$democraciavdemelectoralcomp
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

summary_obj <- summary(variable_control) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_control, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

hist(variable_control)
hist(variable_control %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_control, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# Medios / VDEM : MODERADO + SFICATIVO  #######
nombre_variable <- "Nivel de corrupción en medios (VDEM)"

# preferido: vdem
variable_control <-  democracias$mediaqualitycorruptvdem
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

summary_obj <- summary(variable_control) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_control, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

hist(variable_control)
hist(variable_control %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_control, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# Resultados globales para pasar a word ######
# en cuenta: variable dependiente debería haber sido descrita antes!

# creo data para plotear
data_to_plot <- democracias %>% 
  select(ncat_eleccion,
         nec,
         alineamiento,
         marginvic,
         voteshareincumbent,
         proptv,
         propindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         regulaciondico,
         cumsum_pastciclos,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         gdpxcapita,
         democraciavdemelectoralcomp,
         mediaqualitycorruptvdem) %>% 
  mutate(marginvic = abs(marginvic))

data_to_plot_long <- data_to_plot %>% 
  pivot_longer(cols= everything())

# referencia
skimr::skim(data_to_plot)

# TABLA con data descriptiva

data_descriptiva_wide <- data_descriptiva %>% 
  mutate(Value = Value %>%  round(2)) %>% 
  pivot_wider(names_from = Statistic,
               values_from = Value) 


data_descriptiva_univariada <- data_descriptiva_wide %>% 
  select(c("variable", 
           "Min.",
           "1st Qu.",
           "Median",
           "Mean",
           "3rd Qu.",
           "Max.",
           "Std. Dev",
           "NA's")) %>% 
  dplyr::rename("NAs" = "NA's") %>% 
  mutate(NAs = ifelse(is.na(NAs), 0,  NAs))  

data_descriptiva_bivariada <- data_descriptiva_wide %>% 
  select(c("variable",
           "Tau B",
           "Pearson",
           "Estimate Coef.",
           "Estimate Coef. Estandarizado",
           "Pr(>|z|) Coef."))  %>% 
  dplyr::rename("Pr" = "Pr(>|z|) Coef.") %>% 
  mutate(Stars = ifelse(Pr<0.001, "***",
                        ifelse(Pr<0.01, "**",
                               ifelse(Pr<0.05, "*",
                                      ifelse(Pr<0.1, ".",""))))) %>% 
  dplyr::rename("Pr(>|z|)" = "Pr") 

summary(modelo)

data_descriptiva_univariada %>% write.csv("data_descriptiva_univariada.csv")
data_descriptiva_bivariada %>% write.csv("data_descriptiva_bivariada.csv")

# HISTOGRAMAS 

custom_labels <- as_labeller(c(accesogratuito = "Acceso gratuito a TV",
                  alineamiento = "Alineamiento partidario",
                  avgpropdebatesregionxciclo = "Prop. elecciones c/debates región",
                  cumsum_pastciclos = "Cant. elecciones pasadas c/debates",
                  democraciavdemelectoralcomp = "N. democracia electoral (VDEM)",
                  gdpxcapita = "PBI x capita",
                  marginvic = "Margen de victoria",
                  mediaqualitycorruptvdem = "N. corrupción en medios (VDEM)",
                  ncat_eleccion = "Año electoral",
                  nec = "N.E.C.",
                  prohibicionpropaganda = "Propaganda prohibida",
                  prop_elec_usa_ciclo =  "Prop. elecciones c/debates USA",
                  propindivinternet = "% individuos c/Internet",
                  proptv = "% hogares con TV",
                  regulaciondico = "Debates regulados",
                  voteshareincumbent = "% voto oficialista"))

  
histograms <- data_to_plot_long %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  labs(title = "Distribución univariada de variables independientes",
       caption = "Elaboración propia") +
  theme(strip.text = element_text(size = 14))


# correlaciones entre variables independientes

correlation_matrix <- cor(data_to_plot , use = "pairwise.complete.obs" )

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

## ORIGINAL INDICADORES. tests bivariados ##############

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

# democracia VDEM - electoral component ####
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

## ORIGINAL CONTROLES. tests bivariados ######
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


# VDEM bias #####
nombre_variable <- "Sesgo mediático hacia el gobierno (VDEM)"

# preferido: vdem
variable_control <-  democracias$mediaqualitybiasvdem 
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

summary_obj <- summary(variable_control) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_control, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

hist(variable_control)
hist(variable_control %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_control, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# VDEM perspectives #####

nombre_variable <- "Representación de perspectivas en medios (VDEM)"

# preferido: vdem
variable_control <-  democracias$mediaqualityperspectivesvdem
standardized_variable_control <- (variable_control - mean(variable_control, na.rm = TRUE)) / sd(variable_control, na.rm = TRUE)

summary_obj <- summary(variable_control) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_control, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_control = variable_control,
               variable_dependiente = variable_dependiente)

hist(variable_control)
hist(variable_control %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_control))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_control)))
#table(variable_control, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_control, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_control, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_control ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_control,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_control,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)


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


# CANDIDATOS ##############################
#  acomodo data preparacion #######


base <- base_candidatos %>% 
  select(-starts_with("source_")) %>% 
  select(-X) 

variable_dependiente <- base$dico_candidato_presente

options(scipen=999)

# voteshare #######
nombre_variable <- "% votos obtenidos"
variable_candidatos <-  base$voteshare

standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log()) # quizas amerita

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- summary_tbl # primero es diferente

# v2parglief #######

nombre_variable <- "Ideología izq-der"

variable_candidatos <-  base$v2pariglef_vdem
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log()) # quizas amerita

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# v2paactcom_vdem  #######
nombre_variable <- "Fuerza organizacional"

variable_candidatos <-  base$v2paactcom_vdem
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# ninvitaciones  #######
nombre_variable <- "Cant. invitaciones en elección"

# preferido: vdem
variable_candidatos <-  base$ninvitaciones
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# dico_oficialista  #######
nombre_variable <- "Es oficialista (dummy)"

# preferido: vdem
variable_candidatos <-  base$dico_oficialista
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# dico_reeleccion  #######
nombre_variable <- "Reelige (dummy)"

# preferido: vdem
variable_candidatos <-  base$dico_reeleccion
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# propausenciaspasadasfilled  #######
nombre_variable <- "Prop. ausencias pasadas (incl. 0)"

# preferido: vdem
variable_candidatos <-  base$propausenciaspasadasfilled
standardized_variable_candidatos <- (variable_candidatos - mean(variable_candidatos, na.rm = TRUE)) / sd(variable_candidatos, na.rm = TRUE)

summary_obj <- summary(variable_candidatos) 
summary_tbl <- enframe(summary_obj, name = "Statistic", value = "Value") %>%  
  mutate(variable = nombre_variable)

std_dev <- tibble(Statistic = "Std. Dev",
                  Value = sd(variable_candidatos, na.rm = T),
                  variable = nombre_variable )

summary_tbl <- rbind(summary_tbl,
                     std_dev)

data <- tibble(variable_candidatos = variable_candidatos,
               variable_dependiente = variable_dependiente)

hist(variable_candidatos)
hist(variable_candidatos %>%  log())

ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
#ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
#table(variable_candidatos, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_candidatos, variable_dependiente, method = "kendall", use = "complete.obs")
tau_b

pearson <- cor(variable_candidatos, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_candidatos ~ variable_dependiente, data, mean, na.rm = TRUE)
mean_values

modelo <- glm(variable_dependiente ~ variable_candidatos,
              data = data,
              family = "binomial")
summary(modelo) # para ver si diferencia es significativa 

coef <- summary(modelo)$coefficients[2,c(1,4)]
coef_tbl <- enframe(coef, 
                    name = "Name", 
                    value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef.")) %>% 
  mutate(variable = nombre_variable)

modelo_estandarizado <- glm(variable_dependiente ~ standardized_variable_candidatos,
                            data = data,
                            family = "binomial")
summary(modelo_estandarizado) # para ver si diferencia es significativa 

coef_estandarizado <- summary(modelo_estandarizado)$coefficients[2,c(1,4)]
coef_estandarizado_tbl <- enframe(coef_estandarizado, 
                                  name = "Name", 
                                  value = "Value") %>% 
  dplyr::rename("Statistic" = "Name") %>% 
  mutate(Statistic = paste(Statistic, "Coef. Estandarizado")) %>% 
  mutate(variable = nombre_variable)

summary_tbl <- rbind(summary_tbl,
                     coef_estandarizado_tbl,
                     coef_tbl)

data_descriptiva <- data_descriptiva %>% rbind(summary_tbl)

# Resultados globales para pasar a word ######
# en cuenta: variable dependiente debería haber sido descrita antes!

# creo data para plotear
data_to_plot <- base %>% 
  select(voteshare,
         v2pariglef_vdem,
         v2paactcom_vdem,
         ninvitaciones,
         dico_oficialista,
         dico_reeleccion,
         propausenciaspasadasfilled)

data_to_plot_long <- data_to_plot %>% 
  pivot_longer(cols= everything())

# referencia
skimr::skim(data_to_plot)

# TABLA con data descriptiva

data_descriptiva_wide <- data_descriptiva %>% 
  mutate(Value = Value %>%  round(2)) %>% 
  pivot_wider(names_from = Statistic,
              values_from = Value) 


data_descriptiva_univariada_candidatos <- data_descriptiva_wide %>% 
  select(c("variable", 
           "Min.",
           "1st Qu.",
           "Median",
           "Mean",
           "3rd Qu.",
           "Max.",
           "Std. Dev",
           "NA's")) %>% 
  dplyr::rename("NAs" = "NA's") %>% 
  mutate(NAs = ifelse(is.na(NAs), 0,  NAs))  

data_descriptiva_bivariada_candidatos <- data_descriptiva_wide %>% 
  select(c("variable",
           "Tau B",
           "Pearson",
           "Estimate Coef.",
           "Estimate Coef. Estandarizado",
           "Pr(>|z|) Coef."))  %>% 
  dplyr::rename("Pr" = "Pr(>|z|) Coef.") %>% 
  mutate(Stars = ifelse(Pr<0.001, "***",
                        ifelse(Pr<0.01, "**",
                               ifelse(Pr<0.05, "*",
                                      ifelse(Pr<0.1, ".",""))))) %>% 
  dplyr::rename("Pr(>|z|)" = "Pr") 

data_descriptiva_univariada_candidatos %>% write.csv("data_descriptiva_univariada_candidatos.csv")
data_descriptiva_bivariada_candidatos %>% write.csv("data_descriptiva_bivariada_candidatos.csv")

# HISTOGRAMAS 

custom_labels <- as_labeller(c(dico_oficialista = "Es oficialista",
                               dico_reeleccion = "Reelige",
                               ninvitaciones = "Cant. invitaciones",
                               propausenciaspasadasfilled  = "Prop. ausencias pasadas",
                               v2paactcom_vdem = "Fuerza organizacional",
                               v2pariglef_vdem = "Ideología izq.-der.",
                               voteshare = "% votos"))


histograms_candidatos <- data_to_plot_long %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  labs(title = "Distribución univariada de variables independientes",
       caption = "Elaboración propia") +
  theme(strip.text = element_text(size = 14))


# correlaciones entre variables independientes

correlation_matrix <- cor(data_to_plot , use = "pairwise.complete.obs" )

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



## ORIGINAL INDICADORES CANDIDATOS. tests bivariados ##############


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

