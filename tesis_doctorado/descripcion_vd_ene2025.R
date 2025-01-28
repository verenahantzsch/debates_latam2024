
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
base <- read_csv("./datav2023/base_final3v2023.csv") %>% select(-"...1", -"...2")
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
 

# cuenta simple ####

# solo democracias
summary(democracias_ronda_full$dico_hubo_debates)
sd(democracias_ronda_full$dico_hubo_debates)
nrow(democracias_ronda_full)

# uniendo 1 y 2 ronda
summary(democracias_anual_full$dico_hubo_debates)
sd(democracias_anual_full$dico_hubo_debates)
nrow( democracias_anual_full)

# comparando con toda la muestra
summary(base_anual_full$dico_hubo_debates)
sd(base_anual_full$dico_hubo_debates)
nrow( base_anual_full)

# evolucion temporal de medida dico: elecciones con y sin debates #####

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
                    values = c("green", "grey90"),
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
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_t$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_t$prop_elecciones_con_debates)

# distribucion espacial de medida dico: elecciones con y sin debates #####

base_elecciones_conysindebates_e <- base_anual_full %>% 
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
  theme_minimal()  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_e$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_e$prop_elecciones_con_debates)


