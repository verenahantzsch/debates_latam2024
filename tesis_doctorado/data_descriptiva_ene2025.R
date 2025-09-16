
# version original de este script se encuentra en test_corr_bivariado_nov2024.R
# creamos estadisticas descriptivas de las variables indep del modelo

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

# ELECCIONES PENDIENTE: AGREGAR VARIACION ENTRE CASOS Y EN EL TIEMPO ##############################

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

## año electoral - STRONG POSITIVE + SIGNIFICATIVA #######
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

## fragmentacion: NEC - WEAK POSITIVE + SFICATIVA #######
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

nec_crica <- democracias %>% 
  select(nec, ntc,
         cat_pais, ncat_eleccion, ncat_ronda) %>% 
  subset(ncat_ronda==1) %>% 
  group_by(cat_pais) %>% 
  mutate(mean_ntc = mean(ntc, na.rm=T)) %>% 
  ungroup()

## desalineamiento (% personas que YES se identifican con partido). MODERATE NEGATIVE COR + SFICATIVA  #######
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


## competitividad (marginvic) VWEAK/NO + SFICATIVA  #######
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


## incumbent: vote share incumbent VWEAK/NO + NO SFICATIVA #######
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


## incumbent: reelije WEAK NEGATIVE  + SFICATIVA #######

nombre_variable <- "Presidente compite p/ reelección (dummy)"
variable_independiente <- democracias$dico_reeleccion

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

## media : proptv - WEAK POSITIVE  + SFICATIVA #######
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

## media : propint  - STRONG POSITIVE + SFICATIVA #######
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

## regulaciones: prohibicion propaganda - MODERATE POSITIVE + SFICATIVA  #######
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

## regulaciones: acceso gratuito - VWEAK/NO + SFICATIVA #######
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

## regulaciones sobre debates: dicotomica - STRONG POSITIVE  + SFICATIVA #######
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


## lagged: cumsum ciclos anteriores - STRONG POSITIVE #######
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


## region  avgprop x ciclo - MODERATE/STRONG POSITIVE + SFICATIVA #######
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

## USA prop - WEAK POSITIVE + SFICATIVA #######
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

## Desarrollo/PBI per capita: - MODERATE POSITIVE + SFICATIVA #######
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

## Democracia VDEM #######
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

## Medios / VDEM : MODERADO + SFICATIVO  #######
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

## creo data para plotear ####

data_to_plot_countrynames <- democracias %>% 
  select(ncat_eleccion,
         nec,
         alineamiento,
         marginvic,
         voteshareincumbent,
         dico_reeleccion,
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
         mediaqualitycorruptvdem,
         cat_pais,
         dico_hubo_debates) %>% 
  mutate(marginvic = abs(marginvic),
         dico_hubo_debates = as.factor(dico_hubo_debates))

data_to_plot <- data_to_plot_countrynames %>% 
  select(-cat_pais, -dico_hubo_debates)

data_to_plot_long <- data_to_plot %>% 
  pivot_longer(cols= everything())

data_to_plot_long2 <- data_to_plot_countrynames %>% 
  pivot_longer(cols= c(-cat_pais, -dico_hubo_debates))

# referencia
skimr::skim(data_to_plot)

## TABLA CON VARIACION WITHIN Y BETWEEN #####

# Entre grupos (between): variación entre casos −i en diferentes momentos en el tiempo. Este tipo de comparación no controla por factores particulares para cada grupo.

# Dentro grupos (within): variación dentro de un caso −i en diferentes momentos en el tiempo. Permite controlar factores propios de cada caso que no varían en el tiempo (factor fijo), y que podrían alternar la comparación entre grupos.


xtsum <- xtsum::xtsum(
  data_to_plot_countrynames,
  variables = NULL,
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)

xtsum %>% write.csv("anexos/estadistica_descriptiva_panel.csv")

# intentos parciales con mismo paquete
# variacion entre casos

# between_sd <- xtsum::between_sd(data_to_plot_countrynames, 
#                                 variable = "nec", 
#                                 id = "cat_pais", 
#                                 t = "ncat_eleccion",
#                                 na.rm=T)
# 
# # variacion en el tiempo
# within_sd <- xtsum::within_sd(data_to_plot_countrynames, 
#                               variable = "nec", 
#                               id = "cat_pais", 
#                               t = "ncat_eleccion",
#                               na.rm=T
# )


# intento propio: no me da igual y no entiendo del todo por que


# mean_within <- data_to_plot_countrynames %>% 
#   select(-dico_hubo_debates) %>% 
#   group_by(cat_pais) %>% 
#   summarise(across(-ncat_eleccion, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}")) 
# 
# variacion_within <- data_to_plot_countrynames %>% 
#   select(-dico_hubo_debates) %>% 
#   group_by(cat_pais) %>% 
#   summarise(across(-ncat_eleccion, ~ sd(.x, na.rm = TRUE), .names = "sd_{col}"))
# 
# variacion_within$sd_nec %>% sd()
# 
# mean_between <- data_to_plot_countrynames %>% 
#   select(-dico_hubo_debates) %>% 
#   group_by(ncat_eleccion) %>% 
#   summarise(across(-cat_pais, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}")) 
# 
# variacion_between <- data_to_plot_countrynames %>% 
#   select(-dico_hubo_debates) %>% 
#   group_by(ncat_eleccion) %>% 
#   summarise(across(-cat_pais, ~ sd(.x, na.rm = TRUE), .names = "sd_{col}"))
# variacion_between$sd_nec %>% sd(na.rm=T)
# 
# democracias$nec %>% sd(na.rm=T)

## TABLA con data descriptiva ####

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


data_descriptiva_univariada %>% write.csv("anexos/data_descriptiva_univariada_elecciones.csv")
data_descriptiva_bivariada %>% write.csv("anexos/data_descriptiva_bivariada_elecciones.csv")

## histogramas x variable #####

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
                  voteshareincumbent = "% voto oficialista",
                  dico_reeleccion = "Presidente a reelección"))

  
histograms <- data_to_plot_long %>% 
  subset(name!="ncat_eleccion") %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels,
             nrow = 4) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  labs(title = "Distribución univariada de variables independientes",
       caption = "Elaboración propia") +
  theme(strip.text = element_text(size = 14))

histograms %>% ggsave(filename = "images/histogramas_vis_elecciones_univariados.jpg", 
                      width = 18, height = 12)

histograms2 <- data_to_plot_long2 %>% 
  subset(name!="ncat_eleccion") %>% 
  ggplot() +
  geom_histogram(aes(value, fill = dico_hubo_debates), position = "dodge") +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels,
             nrow = 4) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  labs(title = "Distribución de variables independientes",
       subtitle = "condicional a la ocurrencia o no de debates",
       caption = "Elaboración propia" ) +
  scale_fill_manual(breaks = c("1", "0"),
                    labels =c("Hubo debates", "No hubo debates"),
                    values = c("green", "grey10"),
                    name = "") +
  theme(legend.position = "bottom") +
  theme(strip.text = element_text(size = 14))  

histograms2 %>% ggsave(filename = "images/histogramas_vis_elecciones_condicionales.jpg", 
                      width = 18, height = 12)

## Graficos x año x variable x país #####
# chequeo visual
 
for (i in colnames(data_to_plot_countrynames)) {
  #print(i)
 # i <- "nec"
  if (i != "ncat_eleccion" & i != "cat_pais") {
    # Generate the plot
    plot <- ggplot(data_to_plot_countrynames) +
      geom_smooth(aes(x = ncat_eleccion, y = data_to_plot_countrynames[,i]), se = F, alpha = 0.1, color = "grey90") +
      geom_point(aes(x = ncat_eleccion, y = data_to_plot_countrynames[,i], 
                     colour = dico_hubo_debates, shape = dico_hubo_debates), size = 5) +
      facet_wrap(~cat_pais, scales = "free", nrow = 3) +
      theme_classic() +
      xlab("Año") +
      ylab(i) +
      labs(title = "Evolución temporal de variable:",
           subtitle = i) +
      scale_colour_manual(breaks = c(1, 0),
                        labels =c("Hubo debates", "No hubo debates"),
                        values = c("green", "grey10"),
                        name = "") +
    scale_shape_manual(values = c( 19, 4),
                       name = "",
                       breaks = c(1, 0),
                       labels =c("Hubo debates", "No hubo debates")) +
      theme(legend.position = "bottom") 
      
    
    # Save the plot
    ggsave(filename = paste("images/ev_variablexpais", i, ".jpg", sep = ""), 
           plot = plot, width = 12, height = 10)
    
  } else {
    print(i)
  }
}


## correlaciones entre variables independientes #####

correlation_matrix <- cor(data_to_plot , use = "pairwise.complete.obs" )

# Generar el gráfico con resaltado

# otra funcion de grafico
corrplot <- ggcorrplot::ggcorrplot(correlation_matrix, type = "lower", lab = T, show.legend = F)

corrplot %>% ggsave(filename = "images/corrplot_indicadores_elecciones.jpg",
                    width = 15, height = 15)

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




# Excluidos de control #####

# data_excluida <- 
## volatilidad ####

nombre_variable <- "Volatilidad agregada"

variable_independiente <-  democracias$volatility
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

data_excluida <- summary_tbl
#data_excluida <- data_excluida %>% rbind(summary_tbl)

## ntc #####

nombre_variable <- "Número total de candidatos"

variable_independiente <-  democracias$ntc
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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## descontento ####

nombre_variable <- "Descontento"

variable_independiente <-  democracias$satisfaccion
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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## exeapproval ####

nombre_variable <- "Aprobación presidencial"

variable_independiente <-  democracias$exapprovalnotsmoothed
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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## dico_oficialista #####

nombre_variable <- "Oficialista compite (dummy)"

variable_independiente <-  democracias$dico_oficialista
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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## dico debates pasados ####

nombre_variable <- "Hubo debates en ciclo anterior (dummy)"

variable_independiente <-  democracias$dico_debates_pastelection
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

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_independiente))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_independiente)))
table(variable_independiente, variable_dependiente)

# Calculate Kendall's Tau-b correlation
tau_b <- cor(variable_independiente, variable_dependiente, method = "kendall", use = "complete.obs")
#tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
#mean_values

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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## regulacionordinal #####


nombre_variable <- "Exigencia de regulación ordinal"

variable_independiente <-  democracias$regulacionordinal
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
#tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
#mean_values

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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## Bias de medios  ####

nombre_variable <- "Bias de medios"

variable_independiente <-  democracias$mediaqualitybiasvdem
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
#tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
#mean_values

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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)

## representacion de perspectivas #######

nombre_variable <- "Representación de perspectivas en medios"

variable_independiente <-  democracias$mediaqualityperspectivesvdem
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
#tau_b

pearson <- cor(variable_independiente, variable_dependiente, method = "pearson", use = "complete.obs")

summary_tbl <- rbind(summary_tbl,
                     tibble(Statistic = "Tau B",
                            Value = tau_b,
                            variable = nombre_variable),
                     tibble(Statistic = "Pearson",
                            Value = pearson,
                            variable = nombre_variable))

mean_values <- aggregate(variable_independiente ~ variable_dependiente, data, mean, na.rm = TRUE)
#mean_values

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

#data_excluida <- summary_tbl
data_excluida <- data_excluida %>% rbind(summary_tbl)


## exporto #####


data_excluida_wide <- data_excluida %>% 
  mutate(Value = Value %>%  round(2)) %>% 
  pivot_wider(names_from = Statistic,
              values_from = Value) 


data_excluida_univariada <- data_excluida_wide %>% 
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

data_excluida_bivariada <- data_excluida_wide %>% 
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


data_excluida_univariada %>% write.csv("anexos/data_excluida_univariada_elecciones.csv")
data_excluida_bivariada %>% write.csv("anexos/data_excluida_bivariada_elecciones.csv")

# CANDIDATOS ##############################
##  acomodo data preparacion #######


base <- base_candidatos %>% 
  select(-starts_with("source_")) %>% 
  select(-X) %>% 
  left_join(base_controles %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, democraciavdempolyarchy))%>% 
  subset(democraciavdempolyarchy>0.45) 

variable_dependiente <- base$dico_candidato_presente

options(scipen=999)

## voteshare #######
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

## v2parglief #######

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

## v2paactcom_vdem  #######
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

## ninvitaciones  #######
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

## dico_oficialista  #######
nombre_variable <- "Es oficialista (dummy)"

# preferido: vdem
#variable_candidatos <-  base$dico_oficialista 
variable_candidatos <-  base$dico_oficialistanoreeleccion
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

## dico_reeleccion  #######
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

## propausenciaspasadasfilled  #######
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

## Org es OSC  #######
nombre_variable <- "OSC organizadora"

# preferido: vdem
variable_candidatos <-  base$orgosc
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

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
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


## Org es MMC  #######
nombre_variable <- "MMC organizadora"

# preferido: vdem
variable_candidatos <-  base$orgmmc
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

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
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



## Org es E  #######
nombre_variable <- "Estado organizador"

# preferido: vdem
variable_candidatos <-  base$orgestado
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

#ggplot(data) + geom_boxplot(aes(as.factor(variable_dependiente), variable_candidatos))
ggplot(data) + geom_jitter(aes(as.factor(variable_dependiente), as.factor(variable_candidatos)))
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

## creo data para plotear #####
data_to_plot <- base %>% 
  select(voteshare,
         v2pariglef_vdem,
         v2paactcom_vdem,
         ninvitaciones,
         #dico_oficialista,
         dico_oficialistanoreeleccion,
         dico_reeleccion,
         propausenciaspasadasfilled,
         orgosc,
         orgmmc,
         orgestado)

data_to_plot_long <- data_to_plot %>% 
  pivot_longer(cols= everything())

data_to_plot2 <- base %>% 
  select(voteshare,
         v2pariglef_vdem,
         v2paactcom_vdem,
         ninvitaciones,
        # dico_oficialista,
         dico_oficialistanoreeleccion,
         dico_reeleccion,
         propausenciaspasadasfilled,
         orgosc,
         orgmmc,
         orgestado,
         dico_candidato_presente)

data_to_plot_long2 <- data_to_plot2 %>% 
  pivot_longer(cols= everything())

data_to_plot_long3 <- data_to_plot2 %>% 
  pivot_longer(cols= c(-dico_candidato_presente))

# referencia
skimr::skim(data_to_plot)

## TABLA con data descriptiva ####

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

data_descriptiva_univariada_candidatos %>% write.csv("anexos/data_descriptiva_univariada_candidatos.csv")
data_descriptiva_bivariada_candidatos %>% write.csv("anexos/data_descriptiva_bivariada_candidatos.csv")

## HISTOGRAMAS  #####

custom_labels <- as_labeller(c(dico_oficialistanoreeleccion = "Es oficialista",
                               dico_reeleccion = "Reelige",
                               ninvitaciones = "Cant. invitaciones",
                               propausenciaspasadasfilled  = "Prop. ausencias pasadas",
                               v2paactcom_vdem = "Fuerza organizacional",
                               v2pariglef_vdem = "Ideología izq.-der.",
                               voteshare = "% votos",
                               dico_candidato_presente = "Presente en debate",
                               orgosc = "OSC organizadora",
                               orgmmc = "MMC organizador",
                               orgestado = "Estado organizador"))


histograms_candidatos <- data_to_plot_long %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels,
             nrow = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic()  +
  labs(title = "Distribución univariada de variables independientes",
       caption = "Elaboración propia") +
  theme(strip.text = element_text(size = 14))

histograms_candidatos %>% ggsave(filename = "images/histogramas_vis_candidatos_univariados.jpg", 
                      width = 18, height = 10)


histograms_candidatos2 <- data_to_plot_long3 %>% 
  ggplot() +
  geom_histogram(aes(value, fill = as.factor(dico_candidato_presente)), position = "dodge") +
  facet_wrap(~ name, 
             scales = "free",
             labeller = custom_labels,
             nrow = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  labs(title = "Distribución univariada de variables independientes",
       caption = "Elaboración propia")  +
  scale_fill_manual(values = c("grey10", "green"),
                    name= "",
                    breaks = c(0,1),
                    labels = c("Ausente", "Presente")  ) +
  theme(strip.text = element_text(size = 14),
        legend.position = "bottom")

histograms_candidatos2 %>% ggsave(filename = "images/histogramas_vis_candidatos_condicional.jpg", 
                                 width = 18, height = 10)

## correlaciones entre variables independientes ####

correlation_matrix <- cor(data_to_plot , use = "pairwise.complete.obs" )

# Generar el gráfico con resaltado

# otra funcion de grafico
ggcorrplot::ggcorrplot(correlation_matrix, type = "lower", lab = T, show.legend = F)

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




## estadistica descriptiva variable dependiente ######
length(unique(base$id_debate))


summary(variable_dependiente)
sd(variable_dependiente)
length(variable_dependiente)
descriptiva_VD_candidatos <- skimr::skim(variable_dependiente)

variable_dependiente_reducida <- data_to_plot2 %>% 
  na.omit() %>% 
  select(dico_candidato_presente) 

descriptiva_VD_candidatos_reducida <-  skimr::skim(variable_dependiente_reducida$dico_candidato_presente)
nrow(variable_dependiente_reducida)

variable_dependiente_reducida2 <- data_to_plot2 %>% 
  select(-v2pariglef_vdem, -v2paactcom_vdem) %>% 
  na.omit() %>% 
  select(dico_candidato_presente) 

descriptiva_VD_candidatos_reducida2 <-  skimr::skim(variable_dependiente_reducida2$dico_candidato_presente)
nrow(variable_dependiente_reducida2)

descriptiva_VD_candidatos <- descriptiva_VD_candidatos %>% 
  mutate(muestra = "muestra completa") %>% 
  rbind(descriptiva_VD_candidatos_reducida  %>% 
          mutate( muestra = "muestra reducida [1]")) %>% 
  rbind(descriptiva_VD_candidatos_reducida2 %>% 
          mutate( muestra =  "muestra reducida [2]") )

descriptiva_VD_candidatos %>% write_csv("anexos/descriptiva_VD_candidatos.csv")

## test de diferencia de proporciones ############
prop1 <- descriptiva_VD_candidatos$numeric.mean[1]
prop2 <- descriptiva_VD_candidatos$numeric.mean[2]
n1 <- length(variable_dependiente)
n2 <- nrow(variable_dependiente_reducida)
success1 <- prop1*n1
success2 <- prop2*n2
prop.test(x = c(success1, success2),
          n = c(n1, n2),
          alternative = "two.sided", correct = TRUE)


histograma_VD_candidatos <- data_to_plot2 %>% 
  ggplot() +
  geom_histogram(aes(dico_candidato_presente)) +
  theme_classic()  +
  labs(title = "Distribución univariada de variable dependiente",
       caption = "Elaboración propia",
       x = "Presencia de candidato en debate") +
  scale_x_continuous(breaks = c(0,1),
                   labels = c("Ausente", "Presente"))
  