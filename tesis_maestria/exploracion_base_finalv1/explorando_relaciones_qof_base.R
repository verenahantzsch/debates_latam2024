
# previo #####

# librerias

library(tidyverse)


# datos
joined_base_qog_selection <- read.csv("joined_base_qog_selection.csv")

base_eventos <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/base_debates_limpia.xlsx", sheet = "base_eventos")

debates_año_pais <- base_eventos %>% 
  group_by(año_electoral, pais) %>% 
  summarise(n_debates_año_pais = n())

joined_base_qog_selection <- joined_base_qog_selection %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais))

joined_base_qog_selection$n_debates_año_pais <- joined_base_qog_selection$n_debates_año_pais %>% replace_na(0)


# Eective Number of Presidential Candidates (gol_enpres) // debates si no n ############

# descripciones
num_candidatos_descrip <- joined_base_qog_selection %>% 
  ggplot(aes(pais, gol_enpres)) +
  geom_boxplot() +
  theme_minimal()
num_candidatos_descrip2 <- joined_base_qog_selection %>% 
  ggplot(aes(año_electoral, gol_enpres)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ pais, ncol = 6) 

# pruebas descartadas 
num_cand_facet_pais_size <- joined_base_qog_selection %>% 
  ggplot(aes(año_electoral, gol_enpres, colour= debates_dico , size = n_debates_año_pais)) +
  geom_point() +
  facet_wrap(~ pais, ncol = 6) +
  theme_minimal()+
  theme(legend.position= "none")

num_cand_año <- joined_base_qog_selection %>% 
  ggplot(aes(año_electoral, gol_enpres, colour= debates_dico, size = n_debates_año_pais)) +
  geom_point()

num_cand_ndebates <- joined_base_qog_selection %>% 
  ggplot(aes(n_debates_año_pais, gol_enpres)) +
  geom_point()


# me quedo con estos

num_cand_facet_pais <- joined_base_qog_selection %>% 
  mutate(pais = str_replace(pais, "Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot(aes(año_electoral, gol_enpres, colour= debates_dico)) +
  geom_point() +
  facet_wrap(~ pais, ncol = 6) +
  theme_minimal()+
  theme(legend.position= "none",
        axis.text.x = element_text(angle = 90)) +
  ylab("Número Efectivo de Candidatos") +
  xlab("") +
  scale_y_continuous(breaks=seq(0,8,2)) +
  labs(title = "Realización de debates según Nr. efectivo de candidatos",
       subtitle = "Vista por país a lo largo del tiempo",
       caption = "Elaboración propia. *Nr efectivo de Candidatos es de Q. of Government") 

num_cand_debates <- joined_base_qog_selection %>% 
  ggplot(aes(debates_dico, gol_enpres, fill= debates_dico)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position= "none",
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=11) ) +
  ylab("Número Efectivo de Candidatos") +
  xlab("") +
  scale_x_discrete(labels = c("No hubo", "Hubo debates")) + 
  scale_y_continuous(breaks=seq(0,8,1)) +
  labs(title = "Realización de debates según Nr. efectivo de candidatos",
        subtitle = "América latina en su conjunto, todo el período bajo estudio",
       caption = "Elaboración propia. *Nr efectivo de Candidatos es de Q. of Government") 