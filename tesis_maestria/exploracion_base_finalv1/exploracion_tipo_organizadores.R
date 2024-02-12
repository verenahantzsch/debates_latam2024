require(tidyverse)

base_eventos <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/base_debates_limpia.xlsx", sheet = "base_eventos")
base_años <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/base_debates_limpia.xlsx", sheet = "base_años")

debates_año_pais <- base_eventos %>% 
  group_by(año_electoral, pais) %>% 
  summarise(n_debates_año_pais = n())

base_años <- base_años %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais))

base_años$n_debates_año_pais <- base_años$n_debates_año_pais %>% replace_na(0)

## evolucion de realizacion global ####
ev_temporal <- ggplot(base_años , 
                      aes(año_electoral, n_debates_año_pais))  +
  geom_col() +
  theme_minimal()

# evolucion temporal de realizacion por paises ####
ev_temporal_pais <- ggplot(base_años, 
                           aes(año_electoral, n_debates_año_pais, fill = pais))  +
  geom_col() +
  facet_wrap(~ pais, ncol = 6) +
  theme_minimal() +
  #scale_x_continuous(breaks = 1955:2021) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none") +
  xlab("Año") +
  ylab("")

# ME GUSTA ESTE: 
cols_18 <- c("#00A5E3","#8DD7BF","#FF96C5","#FF5768", "#FFBF65",
             "#6C88C4","#E77577", "#F2D4CC", "#FFD872", "#FC6238",
             "#00CDAC","#FF6F68", "#FFEC59","#FF60A8","#CFF800",
             "#74737A", "#00B0BA", "#C05780")

plot_point_anual <- ggplot(base_años %>% 
                             mutate(pais = str_replace(pais,"Republica Dominicana", "Rep. Dom.")), 
                           aes(año_electoral, pais , size= n_debates_año_pais, colour = pais, shape = debates_dico))  +
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  geom_point() + 
  scale_colour_manual(values = cols_18 ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(colour= cols_18 )) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  labs(x = "", y = "",
       title = "Debates a través del tiempo",
       subtitle = "Cuántos y cuándo se hicieron",
       caption = "Elaboración propia. 
       El tamaño de los círculos representa la cantidad de debates hechos en una elección.
       Las x representan elecciones sin debates")


## TIPOS DE ORGS #####


tipos_organizadores <- base_eventos %>% 
  select(pais, año_electoral, fecha, vuelta, tipo_organizador) %>% 
  mutate( mmc = str_detect(tipo_organizador,"mmc") ,
          mmp = str_detect(tipo_organizador,"mmp") ,
          osc = str_detect(tipo_organizador,"osc") ,
          estado = str_detect(tipo_organizador,"estado") ,
          educ = str_detect(tipo_organizador,"educ") )

tipos_organizadores <- tipos_organizadores %>% 
  mutate(
        tot_tipo_orgs_debate = mmc + mmp + estado + educ + osc,
        pr_mmc = mmc/tot_tipo_orgs_debate  ,
        pr_mmp = mmp/tot_tipo_orgs_debate  ,
        pr_estado =  estado/tot_tipo_orgs_debate  ,
        pr_educ =  educ/tot_tipo_orgs_debate  ,
        pr_osc =  osc/tot_tipo_orgs_debate  )

tipos_organizadores_año_pais <- tipos_organizadores %>% 
  group_by(año_electoral, pais) %>% 
  summarise(n_mmc = sum(pr_mmc) ,
            n_mmp = sum(pr_mmp) ,
            n_estado = sum(pr_estado) ,
            n_educ = sum(pr_educ) ,
            n_osc = sum(pr_osc) ,
            tot_debates_año_pais = n()) 

tipos_organizadores_año_pais <-  tipos_organizadores_año_pais %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_org",
               values_to = "org_peso")  
  

# feo, me gusta mas el que sigue
tipos_organizadores_ev_anual_pais2 <- tipos_organizadores_año_pais %>% 
  subset( org_peso > 0 ) %>% 
  ggplot(aes(año_electoral, org_peso, fill= tipo_org)) +
  geom_col()  +
  facet_wrap(~ pais, ncol = 6) +
  theme_void()

# me gusta este para comparar entre paises
tipos_organizadores_ev_anual_pais <- tipos_organizadores_año_pais %>% 
  mutate(pais = str_replace(pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  subset( org_peso > 0 ) %>% 
  ggplot(aes(año_electoral, tipo_org, colour = tipo_org, size= org_peso))  +
  geom_point() +
  facet_wrap( ~ pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# me gusta este para comparar dos etapas
tipos_organizadores_ev_anual <- tipos_organizadores_año_pais %>% 
  subset( org_peso > 0 ) %>% 
  ggplot(aes(año_electoral, tipo_org, colour = tipo_org, size= org_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
labs(x = "",
     y = "Tipo de organizador",
     title = "Tipo de organizador de los debates ",
     subtitle = "A través el tiempo",
     caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")  



 