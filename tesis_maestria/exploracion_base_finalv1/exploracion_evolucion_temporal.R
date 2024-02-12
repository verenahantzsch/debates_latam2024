require(tidyverse)
#require(readxl)
require(ggplot2)

base <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/latinoamerica debates.xlsx", sheet = 2)

debates_año <- base %>% 
  select(pais, año_electoral, debates_dicotomica)

plot_ev_anual_1 <- debates_año %>% 
  ggplot(aes(fill = debates_dicotomica, x = año_electoral)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  scale_x_continuous(breaks = 1955:2021) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank()) +
  xlab("Año") +
  ylab("") +
  scale_fill_manual(values = c("DUDOSO" = "gray", "SI" = "green", "NO" = "red"))

plot_ev_anual_2 <- debates_año %>% 
  subset(pais!="Haiti") %>% 
  ggplot(aes(fill = debates_dicotomica, x = año_electoral)) +
  geom_bar() +
  facet_wrap(~ pais, ncol = 6) +
  theme_void() +
  scale_x_continuous(breaks = 1955:2021) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank()) +
  xlab("Año") +
  ylab("") +
  scale_fill_manual(values = c("DUDOSO" = "gray", "SI" = "green", "NO" = "red"))



# con cantidad de debates al año 
n_debates_año <- base %>% 
  dplyr::mutate(n_debates_primera = as.numeric(n_debates_primera), 
                n_debates_segunda = as.numeric(n_debates_segunda)) %>% 
  dplyr::mutate(n_debates_año = n_debates_primera + n_debates_segunda) 

# plot_count_anual <- n_debates_año %>% 
#   ggplot(aes(año_electoral, pais, colour= pais, size= n_debates_año)) +
#    geom_count() +
#   scale_color_manual(n_debates_año,
#                      values = c(0 = "black")) +
#   theme_minimal() +
#   theme(legend.position = "none")


plot_point_anual <- n_debates_año %>% 
  ggplot(aes(año_electoral, pais, colour= pais, shape= debates_dicotomica, size= n_debates_año)) +
  scale_shape_manual(values=c("NO" = 4, "DUDOSO" = 8, "SI" = 19)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

# para ver organizadores

organizadores_dico <- base %>% 
  pivot_longer(cols = starts_with("org"),
               names_to = "tipo_org",
               values_to = "org_dico") %>% 
  #select(pais, año_electoral, tipo_org, org_dico, n_debates_primera, n_debates_segunda, vueltas) %>% 
   subset(org_dico=="TRUE") # %>% 
  # dplyr::mutate(n_debates_primera = as.numeric(n_debates_primera), 
  #               n_debates_segunda = as.numeric(n_debates_segunda)) %>% 
  # dplyr::mutate(n_debates_año = n_debates_primera + n_debates_segunda) 

plot_org_anual <- organizadores_dico %>% 
  ggplot(aes(año_electoral, 
             tipo_org, 
             colour= tipo_org, 
             #size= n_debates_año
             )) +
  geom_point() +
  facet_wrap( ~ pais, ncol = 5) +
  theme_minimal() +
  theme(legend.position = "none")

# con organizadores y cantidad de debates

organizadores_n <- base %>% 
  pivot_longer(cols = starts_with("n_org"),
               names_to = "tipo_org",
               values_to = "org_n") %>% 
  #select(pais, año_electoral, tipo_org, org_dico, n_debates_primera, n_debates_segunda, vueltas) %>% 
  mutate(org_n = as.numeric(org_n)) %>% 
  subset(org_n > 0 ) %>% 
  subset(org_n!="NA")  

# años_sin_debate <- base %>% 
#   pivot_longer(cols = starts_with("n_org"),
#                names_to = "tipo_org",
#                values_to = "org_n") %>% 
#   #select(pais, año_electoral, tipo_org, org_dico, n_debates_primera, n_debates_segunda, vueltas) %>% 
#   mutate(org_n = as.numeric(org_n)) %>% 
#   subset(org_n == 0 ) %>% 
#   subset(org_n == "NA")  

# dplyr::mutate(n_debates_primera = as.numeric(n_debates_primera), 
#               n_debates_segunda = as.numeric(n_debates_segunda)) %>% 
# dplyr::mutate(n_debates_año = n_debates_primera + n_debates_segunda) 

plot_n_org_anual <- organizadores_n %>% 
  ggplot(aes(año_electoral, 
             tipo_org, 
             colour= tipo_org, 
             size= org_n
  )) +
  geom_point() +
  facet_wrap( ~ pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none")
# falta: superponer años electorales con algun otro simbolo

# para separar en etapas, desconociendo paises

n_org_años <- organizadores_n %>%   
  group_by(año_electoral, tipo_org) %>%
  summarise(cantidad_debates = sum(org_n))

plot_org_años <- n_org_años %>% 
  ggplot(aes(año_electoral, 
             tipo_org,
             colour = tipo_org,
             size = cantidad_debates)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") 

## regulaciones vigentes
# nada = no encontramos ninguna mencion de ningun tipo

# agregado
plot_regulacion_años <- base %>% 
  subset(pais!="Haiti") %>% 
  ggplot(aes(año_electoral,
             regulacion,
             colour= pais,
             shape= regulacion)) +
  scale_shape_manual(values=c("NADA" = 4, 
                              "REGULACION" = 17,
                              "ORGANIZA" = 19,
                              "OBLIGATORIO" = 15,
                              "PROYECTO" = 8,
                              "INTERVIENE" = 18,
                              "NA" = 3))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Rol del Estado") +
  xlab("") +
  scale_x_continuous(breaks = seq(1955, 2025, by = 5))

# por pais
plot_regulacion_pais <- base %>% 
  subset(pais!="Haiti") %>% 
  ggplot(aes(año_electoral,
             regulacion,
             colour= pais,
             shape= regulacion)) +
  scale_shape_manual(values=c("NADA" = 4, 
                              "REGULACION" = 17,
                              "ORGANIZA" = 19,
                              "OBLIGATORIO" = 15,
                              "PROYECTO" = 8,
                              "INTERVIENE" = 18,
                              "NA" = 3))  +
  geom_point()+
  facet_wrap( ~ pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  theme(axis.text.x = element_text(angle= 90))
  