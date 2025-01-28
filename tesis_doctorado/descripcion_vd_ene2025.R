
# PARA CAPITULO DE DESCRIPCION DE VD
# Retomo cosas de trabajo_capitulosv2024 y limpio un poco

# librerias

library(tidyverse)

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# DATA VD
base_vdependiente <- read.csv("./datav2023/variables_dependientes_elecciones.csv")
# para filtrar por democracias
base_controles <- read.csv("./datav2023/controles_elecciones.csv")

# DATA DESCRIPTIVA

# datos
base <- read_csv("./datav2023/base_final3v2023.csv") %>% select(-"...1", -"...2") %>% 
  subset(dico_certidumbre==1)
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")
base_anual_full <- readxl::read_xlsx("./datav2023/base_anual_fullv2023.xlsx")

nombres_subtipos <- readxl::read_xlsx("./codebooksv2023/nombres_subtiposv2023.xlsx")

# DATA AUXILIAR 
colorespais <- base %>% 
  distinct(cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 

plotnumber <- 0 # para guardar con numeracion ascendente

base <- base %>% mutate(id_elec = paste0(cat_pais,ncat_eleccion,ncat_ronda)) 
u_elec <- base$id_elec %>% unique()

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# data solo democracias

democracias_ronda_full <- base_vdependiente %>% 
  select(-X) %>% 
  left_join(base_controles %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, democraciavdempolyarchy)) %>% 
  subset(democraciavdempolyarchy>0.45) %>% 
  subset(ncat_eleccion!=2024)  

democracias_basedebates <-  base %>%
  left_join(base_controles %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, democraciavdempolyarchy)) %>% 
  subset(democraciavdempolyarchy>0.45) %>% 
  subset(ncat_eleccion!=2024) 

democracias_anual_full <- base_anual_full %>%
  left_join(base_controles %>% 
              group_by(cat_pais, ncat_eleccion) %>% 
              summarise(democraciavdempolyarchy = mean(democraciavdempolyarchy, na.rm=T))) %>% 
  subset(democraciavdempolyarchy>0.45) %>% 
  subset(ncat_eleccion!=2024)  

# saco año incompleto 
base_anual_full <- base_anual_full %>% 
  subset(ncat_eleccion!=2024)   

base <- base %>% 
  left_join(base_controles %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, democraciavdempolyarchy)) %>% 
  subset(ncat_eleccion!=2024)  


# NOTAS

# base_anual_full -> una fila = 1 año , sin diferenciar rondas, sin filtrar x democracia
# democracias_anual_full -> una fila = 1 año, filtrando x democraci

# base y democracias_base = 1 debate x fila

# democracias_rondas_full -> 1 ronda x fila 

# VARIABLE DEPENDIENTE. #######

# MEDIDA DICO: EXISTENCIA DE DEBATES ############
 

## cuenta simple ####
#VD <- democracias_ronda_full$dico_hubo_debates

descriptive_stats <- function(VD){
  summary_stats <- summary(VD)
  summary_df <-  enframe(summary_stats, name = "Name", value = "Value")
  sd <-  tibble("Value" = sd(VD, na.rm=T) ,
                   "Name" = "Std. Dev")
  n <- tibble("Value" = as.numeric( length(VD) ) %>% round(0),
                 "Name" = "Sample N")
  
  stats <- rbind(summary_df,
                 sd,
                 n)
  stats
  
}

# solo democracias
summary(democracias_ronda_full$dico_hubo_debates)
sd(democracias_ronda_full$dico_hubo_debates)
length(democracias_ronda_full$dico_hubo_debates)
descriptive_completas <- descriptive_stats(democracias_ronda_full$dico_hubo_debates)

descriptive_completas %>% write.csv("anexos/descriptiva_VD_ppal.csv")


# uniendo 1 y 2 ronda

descriptive_anexas <- descriptive_stats(democracias_anual_full$dico_hubo_debates)
descriptive_anexas <- descriptive_anexas %>% 
  mutate(Muestra = "Muestra democracias")

# comparando con toda la muestra

descriptive_anexas2 <- descriptive_stats(base_anual_full$dico_hubo_debates)
descriptive_anexas2 <- descriptive_anexas2 %>% 
  mutate(Muestra = "Muestra completa")

descriptive_anexas3 <- descriptive_anexas %>% 
  rbind(descriptive_anexas2) %>% 
  pivot_wider(names_from = "Muestra",
              values_from = "Value")

descriptive_anexas3 %>% write.csv("anexos/descriptiva_VD_anexo.csv")


## between - within ######


ytsum <- xtsum::xtsum(
  democracias_ronda_full,
  variables = "dico_hubo_debates",
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)
ytsum

## evolucion temporal de medida dico: elecciones con y sin debates #####

base_elecciones_conysindebates_t <- democracias_ronda_full %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_elecciones = n(),
            n_elecciones_con_debates = sum(dico_hubo_debates),
            n_elecciones_sin_debates = n_elecciones - n_elecciones_con_debates,
            prop_elecciones_con_debates = n_elecciones_con_debates/n_elecciones)

base_plot_elecciones_conysindebates_t <- base_elecciones_conysindebates_t %>% 
  pivot_longer(cols= c(n_elecciones_con_debates, n_elecciones_sin_debates), 
               names_to = "dico_debates", values_to = "n_dico_debates")

plot_elecciones_conysindebates_t <- base_plot_elecciones_conysindebates_t %>% 
  ggplot() + 
  geom_col(aes(ncat_eleccion, n_dico_debates, fill = dico_debates), colour = "grey10", position = "dodge") +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elecciones con debates", "Elecciones sin debates"),
                    values = c("green", "grey80"),
                    name = "") +
  scale_x_continuous(breaks = seq(1960,2025,5)) +
  labs(title = "Cantidad de elecciones presidenciales con y sin debates",
         subtitle = "En las democracias de América Latina, 1960-2023",
       y = "Cantidad de elecciones",
       x = "Año",
       caption = "Elaboración propia.
       La primera y segunda ronda de las elecciones presidenciales se cuentan de manera separada cuando aplica.
       Se consideran únicamente elecciones ocurridas bajo regímenes democraticos (>0.45 en índice de poliarquía de V-Dem)") +
  theme(legend.position = "bottom")

plotnumber <- plotnumber + 1
plotname <- "ev_t"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_t$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_t$prop_elecciones_con_debates)

## distribucion espacial de medida dico: elecciones con y sin debates #####

base_elecciones_conysindebates_e <- democracias_ronda_full %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones = n(),
            n_elecciones_con_debates = sum(dico_hubo_debates),
            n_elecciones_sin_debates = n_elecciones - n_elecciones_con_debates,
            prop_elecciones_con_debates = n_elecciones_con_debates/n_elecciones)

base_plot_elecciones_conysindebates_e <- base_elecciones_conysindebates_e %>% 
  pivot_longer(cols= c(n_elecciones_con_debates, n_elecciones_sin_debates), 
               names_to = "dico_debates", values_to = "n_dico_debates")

plot_elecciones_conysindebates_e <- base_plot_elecciones_conysindebates_e %>% # reordenarn en f de elec sin debates
  ggplot() + 
  geom_col(aes(cat_pais, n_dico_debates, fill = dico_debates), colour = "grey10", position = "stack") +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elecciones con debates", "Elecciones sin debates"),
                    values = c("green", "grey80"),
                    name = "") +
  labs(title = "Cantidad de elecciones presidenciales con y sin debates",
       subtitle = "En las democracias de América Latina, por páis (1960-2023)",
       y = "Cantidad de elecciones",
       x = "País",
       caption = "Elaboración propia.
       La primera y segunda ronda de las elecciones presidenciales se cuentan de manera separada cuando aplica.
       Se consideran únicamente elecciones ocurridas bajo regímenes democraticos (>0.45 en índice de poliarquía de V-Dem)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, size = 12))

plotnumber <- plotnumber + 1
plotname <- "ev_e"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_e$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_e$prop_elecciones_con_debates)


# MEDIDA CUANTI: cantidad de debates por eleccion #####

## cuentas ###########

# POR AÑO; POR SI LAS DUDAS; NONO NO REPORTO # LA BASE LA VOY A USAR DESPUES
n_debates_año_pais <- base %>%
  group_by(ncat_eleccion, cat_pais) %>%
  summarise(n_debates_año_pais = n())

democracias_anual_full <- democracias_anual_full %>%
  left_join(n_debates_año_pais) %>%
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

summary(democracias_anual_full$n_debates_año_pais)
sd(democracias_anual_full$n_debates_año_pais)

# POR AÑO SEPARANDO EN RONDAS # SI REPORTO 
n_debates_año_pais_ronda <- base %>% 
  subset(democraciavdempolyarchy>0.45) %>% 
  group_by(ncat_eleccion, cat_pais, ncat_ronda) %>% 
  summarise(n_debates_año_pais_ronda = n()) 

democracias_ronda_full <- democracias_ronda_full %>% 
  left_join(n_debates_año_pais_ronda) %>% 
  mutate(n_debates_año_pais_ronda = ifelse(is.na(n_debates_año_pais_ronda),
                                           0, n_debates_año_pais_ronda))  


test <- democracias_ronda_full %>% 
  select(cat_pais, ncat_eleccion, ncat_ronda, dico_hubo_debates, n_debates_año_pais_ronda)

summary(democracias_ronda_full$n_debates_año_pais_ronda)
sd(democracias_ronda_full$n_debates_año_pais_ronda)

ytsum <- xtsum::xtsum(
  democracias_ronda_full,
  variables = "n_debates_año_pais_ronda",
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)

ytsum

## calculos de promedios en t y e ####

ev_t_mean_n_debatesxeleccion <- democracias_ronda_full %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_n_debates_año = mean(n_debates_año_pais_ronda, na.rm = T)) %>% 
  arrange(desc(mean_n_debates_año)) 

head(ev_t_mean_n_debatesxeleccion)

ev_decada_mean_n_debatesxeleccion <- democracias_ronda_full %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_debates_decada = mean(n_debates_año_pais_ronda, na.rm = T),
            n_elecciones = n())  

ev_decada_mean_dico_debatesxeleccion <- democracias_ronda_full %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_dico_debates_decada = mean(dico_hubo_debates, na.rm = T),
            n_elecciones = n())  

ev_decadas <- ev_decada_mean_n_debatesxeleccion %>% 
  left_join(ev_decada_mean_dico_debatesxeleccion)

ev_decadas %>% write.csv("anexos/evolucion_decadas.csv")

ev_e_mean_n_debatesxeleccion <- democracias_ronda_full %>% 
  group_by(cat_pais) %>% 
  summarise(mean_n_debates_pais = mean(n_debates_año_pais_ronda, na.rm = T),
            n_elecciones = n()) %>% 
  arrange(mean_n_debates_pais)  

ev_e_mean_dico_debatesxeleccion <- democracias_ronda_full %>% 
  group_by(cat_pais) %>%
  summarise(mean_dico_debates_pais = mean(dico_hubo_debates, na.rm = T),
            n_elecciones = n())  

ev_e <- ev_e_mean_n_debatesxeleccion %>% 
  left_join(ev_e_mean_dico_debatesxeleccion)

ev_e %>% write.csv("anexos/evolucion_paises.csv")


## ev anual, debates en t por pais ######

plot_point_anual2 <- democracias_anual_full %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(ncat_eleccion, 
             log(n_debates_año_pais),
             colour = as.factor(cat_pais) 
             # %>% 
             #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
             #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
             #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"))
  ))  +
  geom_line(lwd=2) + 
  geom_point(aes(ncat_eleccion,  
                 log(n_debates_año_pais),
                 #colour = as.factor(cat_pais),
    shape = as.factor(dico_hubo_debates)), colour = "black") + #  , alpha= as.factor(dico_hubo_debates))) + # corregir, no esta saliendo esto
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_shape_manual(breaks = c(0,1), values=c(4, 19)) +
  #scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  facet_wrap( ~ as.factor(cat_pais) 
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              , 
              ncol = 6) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 10), size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +  #,
  # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2025,10)) +
  scale_y_continuous(breaks = c(0,1,2,3) , labels = c(0, exp(1) %>% round(), exp(2) %>% round(), exp(3) %>% round())) +
  labs(x = "Año", y = "Cantidad de debates",
       title = "Debates a través del tiempo",
       subtitle = "Cuántos se hicieron y cuándo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       Las x representan elecciones sin debates.
       Para una mejor visualización, la cantidad de debates (eje vertical) está representada en su versión logarítmica,
       pero los valores señalados en el eje vertical indican la equivalencia traducida nuevamente a cantidad absoluta de debates.
       El máximo real se ubica en 24 debates anuales (Costa Rica), que equivale a un log de 3.17, el mínimo en 0.
       
       *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
plotname <- "ev_t_e"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# traduccion de log 

n_debates_año_pais <- base_anual_full$n_debates_año_pais %>% unique() 
log <- base_anual_full$n_debates_año_pais %>% unique() %>% log()
conversion_log <- tibble(n_debates_año_pais = n_debates_año_pais, 
                         log = log)



