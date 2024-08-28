# script de pocas lineas para crear una base basica, para evaluar fuentes externas de datos
# Carolina Franco, julio de 2024

library(tidyverse)

# DATA ###########
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")
base_elecciones <- read.csv("all_elections_full_dataset.csv") %>% select(-X)

c_elects <- base_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, dico_debates_eleccion)

trad_pais <- tibble(
  cat_pais = c("Brasil", "Chile", "Argentina", "Peru", "Paraguay", "Ecuador", "Uruguay", "Nicaragua", "Panama", "Venezuela", "Bolivia", "Mexico", "Guatemala", "Honduras", "Colombia", "Costa Rica", "Republica Dominicana", "El Salvador"),
  eng_cat_pais = c("Brazil", "Chile", "Argentina", "Peru", "Paraguay", "Ecuador", "Uruguay", "Nicaragua", "Panama", "Venezuela", "Bolivia", "Mexico", "Guatemala", "Honduras", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador")
)

c_elects <- left_join(c_elects, trad_pais)

min_year <- c_elects$ncat_eleccion %>% min()
max_year <- c_elects$ncat_eleccion %>% max()
all_years <- seq(min_year,max_year,1)
full_years <- rep(all_years, times= nrow(trad_pais))
full_countries <- rep(trad_pais$cat_pais, each = length(all_years))
c_years <- tibble(cat_pais = full_countries,
                  year = full_years)

c_years <- c_years %>% left_join(c_elects %>% dplyr::rename( "year" = "ncat_eleccion" ))

c_years <- c_years %>% 
  mutate(ncat_ronda = ifelse(is.na(ncat_ronda), 0, ncat_ronda))

c_years <- c_years %>% 
  select(-eng_cat_pais) %>% 
  left_join(trad_pais)

c_years %>% write_csv("base_base.csv")
c_elects %>% arrange(cat_pais,ncat_eleccion,ncat_ronda) %>% write_csv("base_base_elecs.csv")


# creacion de breve base que diferencia entre cat ciclo electoral y cat año electoral

electyears <- base_elecciones %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  mutate(ncat_election_year = ncat_eleccion) %>% 
  mutate(ncat_election_year = 
           ifelse(cat_pais=="Chile" & ncat_ronda==1 & ncat_eleccion>1999 & ncat_eleccion<2012,
                  ncat_election_year - 1,
                  ncat_election_year)) %>% 
  mutate(ncat_election_year = 
           ifelse(cat_pais=="Guatemala" & ncat_ronda==1 & ncat_eleccion==1991,
                  ncat_election_year - 1,
                  ncat_election_year)) 

electyears %>% write.csv("electyears.csv")