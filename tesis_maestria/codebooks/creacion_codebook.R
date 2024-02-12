# codebook general

codebookv1 <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebook.xlsx")

variables <- colnames(base) %>%  as_tibble()

codebook_variables <- variables %>%
  dplyr::rename("Variable" = value) %>% 
  left_join(codebookv1)

codebook_variables %>% 
  writexl::write_xlsx("codebookv3.xlsx")

codebookv3 <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebookv3.xlsx")

codebook <- codebookv3 %>% 
  mutate( Tipo = str_extract(string = Variable, pattern = ".+?(?=_)") )

tipos_variables1 <- codebook %>% distinct(Tipo) 

# tipos_variables1 %>% 
#   writexl::write_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/tipos_variables.xlsx")

tipos_variables <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/tipos_variables.xlsx")

codebook <- codebook %>% 
  left_join(tipos_variables) %>% 
  select(-Tipo) %>% 
  dplyr::rename( Tipo = "def_tipo")

codebook %>% 
  writexl::write_xlsx("codebookv3.xlsx")

# codebook de cluster

base_cluster_pais <- read.csv("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/base_cluster_pais.csv")
codebook_base_cluster_pais <- colnames(base_cluster_pais) %>% as_tibble()

codebook_base_cluster_pais %>% 
  writexl::write_xlsx("codebook_base_cluster_pais.xlsx")