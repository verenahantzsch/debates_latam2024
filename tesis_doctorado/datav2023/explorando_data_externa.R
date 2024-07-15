## exploracion de fuentes de data externa

library(tidyverse)

# DATA PROPIA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_year <- read.csv("base_base.csv")  


### UIT ##################
# DATA EXTERNA PARA COMPLEMENTAR CON LAPOP LATINOB Y PROYECCIONES ####
setwd("/home/carolina/Documents/dataexterna/UIT")
data_externa <- read.csv("households-with-a-tv_1720457361872.csv")

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(eng_cat_pais = "entityName",
                                           year = "dataYear"))

test <- test %>% subset(!is.na(dataValue))
table(test$cat_pais, test$year)


# DATA EXTERNA ####
setwd("/home/carolina/Documents/dataexterna/UIT")
data_externa <- read.csv("level-of-competition_1721068124736.csv")
# no sirve mucho

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(eng_cat_pais = "entityName",
                                           year = "dataYear"))

test <- test %>% subset(!is.na(dataValue))
table(test$cat_pais, test$year)


# DATA EXTERNA ####
setwd("/home/carolina/Documents/dataexterna/UIT")
data_externa <- read.csv("households-with-internet-access-at-home_1721068103197.csv")

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(eng_cat_pais = "entityName",
                                           year = "dataYear"))

test <- test %>% subset(!is.na(dataValue))
table(test$cat_pais, test$year)


# DATA EXTERNA VERY NICE ####
setwd("/home/carolina/Documents/dataexterna/UIT")
data_externa <- read.csv("individuals-using-the-internet_1721068158782.csv")

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(eng_cat_pais = "entityName",
                                           year = "dataYear"))

test <- test %>% subset(!is.na(dataValue))
table(test$cat_pais, test$year)


### OBSERVATORIOREFORMAS ##################
# DATA EXTERNA  sobre medios, sirve! hay que fijarse momento inicial a primera reforma ####
setwd("/home/carolina/Documents/dataexterna/OBSREF")
data_externa <- readxl::read_xlsx("18.01.2022_ObservatorioReformas_BD_AccesoMedios.xlsx")

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(cat_pais = "pais",
                                           year = "año_reforma"))

test <- test %>% subset(!is.na(prohibicion_propaganda))
table(test$cat_pais, test$year)

test <- test %>% subset(!is.na(acceso_gratuito))
table(test$cat_pais, test$year)

# DATA EXTERNA sirve! hay que  hacer algunas correciones en excel. ALTERNATIVA SI SEAWRIGHT NO RESPONDE ####
setwd("/home/carolina/Documents/dataexterna/OBSREF")
data_externa <- readxl::read_xlsx("16.06.2024_ObservatorioReformas_BD_Incumbents.xlsx")
#data_externa <- read.csv("16.06.2024_ObservatorioReformas_BD_Incumbents.csv")
# no hay nicaragua ni panama, hay que completar
# hay que corregir algunos acentos para compatibilidad de nombres
# hay que corregir algunos formatos de numero que están como fecha y se desconfiguran
# datos de 1990 para algunos países, quizás se puede expandir ej Colombia

test <- base_year %>% 
  left_join(data_externa %>% 
              dplyr::rename(cat_pais = "País",
                            year = "Año de la elección presidencial") %>% 
              mutate(year = as.numeric(year)))

test <- test %>% subset(!is.na(prohibicion_propaganda))
table(test$cat_pais, test$year)

test <- test %>% subset(!is.na(acceso_gratuito))
table(test$cat_pais, test$year)


### VDEM ##################
# DATA EXTERNA  sobre medios, sirve! hay que fijarse momento inicial a primera reforma ####
#Local organizational strength (C) (v2paactcom) . . . . . . . . . . . . . . . . .
# 3.3.5 Affiliate organizations (C) (v2pasoctie) .
#3.1.9 Party continuation (C) (v2paelcont)
# 3.2.13 Economic left-right scale (C) (v2pariglef)
# 3.2.15 Clientelism (C) (v2paclient)

setwd("/home/carolina/Documents/dataexterna/VDEM")
data_externa <- read_csv("V-Dem-CPD-Party-V2.csv")

test <- base_year %>% 
  left_join(data_externa %>% dplyr::rename(eng_cat_pais = "country_name"#,
                                           #year = "año_reforma"
                                           ))

#test <- test %>% subset(!is.na(v2paactcom)) # activismo de base del partido
test <- test %>% subset(!is.na(v2pasoctie))

test <- test %>% subset(ncat_ronda!=0)
test_countryyear <- test %>% mutate(elecid = paste(cat_pais, year))
count <- test_countryyear$elecid %>% unique()
count <- test$v2pasoctie_osp %>% unique()
table(test$cat_pais, test$year)

test <- test %>% subset(!is.na(acceso_gratuito))
table(test$cat_pais, test$year)