# librerias

library(tidyverse)

setwd("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado/codebooksv2023")

# codebook general

codebookv2021 <-  readxl::read_xlsx("codebookv3.xlsx")
base <- readxl::read_xlsx("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado/datav2023/base_final3v2023.xlsx") 

# chequeamos que variables nos falta agregar al codebook de la version de maestria

variables <- colnames(base) %>%  as_tibble()

codebook_variables <- variables %>%
  dplyr::rename("Variable" = value) %>% 
  left_join(codebookv2021)

codebook_variables %>% 
  writexl::write_xlsx("codebookv2023.xlsx") # esto lo completaremos manualmente

# toca tambien complementar codebook de subtipos de organizadores

nombres_subtiposv2021 <- read.csv("nombres_subtipos.csv") 
base_organizadores <- readxl::read_xlsx("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado/datav2023/base_organizadoresv2023.xlsx")

cats_subtipos <- base_organizadores$ncat_subtipov2 %>% unique() %>%  as_tibble()

codebook_subtipos <- cats_subtipos %>% 
  dplyr::rename("ncat_subtipov2" = value) %>% 
  left_join(nombres_subtiposv2021)

codebook_subtipos %>% 
  writexl::write_xlsx("nombres_subtiposv2023.xlsx")