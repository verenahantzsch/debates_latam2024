
# PARA CAPITULO DE DESCRIPCION DE VD
# Retomo cosas de trabajo_capitulosv2024 y limpio un poco

# librerias

library(tidyverse)

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")

# DATA VD
base_vdependiente <- read.csv("./datav2023/variables_dependientes_elecciones.csv")

# DATA VIS
# para filtrar por democracias
base_controles <- read.csv("./datav2023/controles_elecciones.csv")
# para extraer cantidad de inviados
base_indicadores <- read.csv("./datav2023/indicadores_elecciones.csv")

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
  left_join(base_indicadores %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, nec, ntc)) %>% 
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

# filtramos otras bases

#elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
democracias_organizadores <- base_organizadores %>% 
  subset(id_debate%in%democracias_basedebates$id_debate) %>% 
  subset(!is.na(nombres_organizadores))
democracias_formatos <- base_formatos %>% 
  subset(id_debate%in%democracias_basedebates$id_debate)
democracias_temas <- base_temas %>% 
  subset(id_debate%in%democracias_basedebates$id_debate)
democracias_normativa <- democracias_anual_full %>% 
  left_join(base_normativa)
#base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")

# NOTAS

# base_anual_full -> una fila = 1 año , sin diferenciar rondas, sin filtrar x democracia
# democracias_anual_full -> una fila = 1 año, filtrando x democraci

# base y democracias_base = 1 debate x fila

# democracias_rondas_full -> 1 ronda x fila 

# VARIABLE DEPENDIENTE. #######

# PPAL MEDIDA DICO: EXISTENCIA DE DEBATES ############
 

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

## diferencia de proporciones ###########


no_democracias_anual_full <- base_anual_full %>%
  left_join(base_controles %>% 
              group_by(cat_pais, ncat_eleccion) %>% 
              summarise(democraciavdempolyarchy = mean(democraciavdempolyarchy, na.rm=T))) %>% 
  subset(democraciavdempolyarchy<0.45) %>% 
  subset(ncat_eleccion!=2024)  

prop.test(x = c(sum(democracias_anual_full$dico_hubo_debates),
                sum(no_democracias_anual_full$dico_hubo_debates)),
          n = c(nrow(democracias_anual_full),
                nrow(no_democracias_anual_full)))
  
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
               names_to = "dico_debates", values_to = "n_dico_debates") %>% 
  mutate(porcentaje_elecciones_con_debates = paste(round(prop_elecciones_con_debates*100), "%"))

plot_elecciones_conysindebates_t <- base_plot_elecciones_conysindebates_t %>% 
  ggplot() + 
  geom_col(aes(ncat_eleccion, n_dico_debates, fill = dico_debates), colour = "grey80", position = "dodge") +
  #geom_text(aes(x = ncat_eleccion, y = n_dico_debates, label = porcentaje_elecciones_con_debates)) +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elecciones con debates", "Elecciones sin debates"),
                    values = c("green", "grey10"),
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
               names_to = "dico_debates", values_to = "n_dico_debates") %>% 
  mutate(porcentaje_elecciones_con_debates = paste(round(prop_elecciones_con_debates*100), "%")) %>% 
  group_by(cat_pais) %>% 
  mutate(y_porcentaje = sum(n_dico_debates)) %>% 
  ungroup() 

plot_elecciones_conysindebates_e <- base_plot_elecciones_conysindebates_e %>% # reordenarn en f de elec sin debates
  ggplot() + 
  geom_col(aes(cat_pais, n_dico_debates, fill = dico_debates), colour = "grey80", position = "stack") +
  geom_text(aes(x = cat_pais, y = y_porcentaje, label = porcentaje_elecciones_con_debates)) +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elecciones con debates", "Elecciones sin debates"),
                    values = c("green", "grey10"),
                    name = "") +
  labs(title = "Cantidad de elecciones presidenciales con y sin debates",
       subtitle = "En las democracias de América Latina, por páis (1960-2023)",
       y = "Cantidad de elecciones",
       x = "País",
       caption = "Elaboración propia.
       La primera y segunda ronda de las elecciones presidenciales se cuentan de manera separada cuando aplica.
       Se consideran únicamente elecciones ocurridas bajo regímenes democraticos (>0.45 en índice de poliarquía de V-Dem).
       Los porcentajes indican la cantidad de elecciones CON debates sobre el total") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, size = 12))

plotnumber <- plotnumber + 1
plotname <- "ev_e"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_e$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_e$prop_elecciones_con_debates)


# CANTIDAD DE DEBATES X ELECCION #####

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
n_debates_año_pais_ronda <- democracias_basedebates %>% 
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

# promedio sobre tot elecciones
ev_decada_mean_n_debatesxeleccion <- democracias_ronda_full %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_debates_decada = mean(n_debates_año_pais_ronda, na.rm = T),
            n_elecciones = n())  

# promedio sobre elec con debates
ev_decada_mean_n_debatesxeleccion_eleccdebates <- democracias_ronda_full %>% 
  subset(dico_hubo_debates ==1 ) %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_debates_decada_eleccdebates = mean(n_debates_año_pais_ronda, na.rm = T))

# prop elec con debates
ev_decada_mean_dico_debatesxeleccion <- democracias_ronda_full %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_dico_debates_decada = mean(dico_hubo_debates, na.rm = T))  


ev_decadas <- ev_decada_mean_n_debatesxeleccion %>% 
  left_join(ev_decada_mean_n_debatesxeleccion_eleccdebates) %>% 
  left_join(ev_decada_mean_dico_debatesxeleccion)

ev_decadas <- ev_decadas %>% 
  select(decada ,
         n_elecciones, 
         mean_n_debates_decada,
         mean_n_debates_decada_eleccdebates,
         mean_dico_debates_decada ) %>% 
  arrange(decada)  %>% 
  mutate(across(starts_with("mean"), ~round(., 2)))

ev_decadas %>% write.csv("anexos/evolucion_decadas.csv")

ev_e_mean_n_debatesxeleccion <- democracias_ronda_full %>% 
  group_by(cat_pais) %>% 
  summarise(mean_n_debates_pais = mean(n_debates_año_pais_ronda, na.rm = T),
            n_elecciones = n())   

ev_e_mean_n_debatesxeleccion_eleccdebates <- democracias_ronda_full %>% 
  subset(dico_hubo_debates ==1 ) %>% 
  group_by(cat_pais) %>% 
  summarise(mean_n_debates_pais_eleccdebates = mean(n_debates_año_pais_ronda, na.rm = T))  

ev_e_mean_dico_debatesxeleccion <- democracias_ronda_full %>% 
  group_by(cat_pais) %>%
  summarise(mean_dico_debates_pais = mean(dico_hubo_debates, na.rm = T),
            n_elecciones = n())  

ev_e <- ev_e_mean_n_debatesxeleccion  %>% 
  left_join(ev_e_mean_n_debatesxeleccion_eleccdebates) %>% 
  left_join(ev_e_mean_dico_debatesxeleccion)

ev_e <- ev_e %>% 
  select(cat_pais ,
         n_elecciones, 
         mean_n_debates_pais,
         mean_n_debates_pais_eleccdebates,
         mean_dico_debates_pais ) %>% 
  arrange(mean_dico_debates_pais)  %>% 
  mutate(across(starts_with("mean"), ~round(., 2)))
  
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
       title = "Gráfico Anexo 4.II. Debates a través del tiempo",
       subtitle = "Cuántos se hicieron y cuándo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       Las x indican elecciones sin debates.
       Para facilitar la visualización, el eje vertical se basa en el cálculo en escala logarítmica, 
       pero los valores de referencia corresponden a la cantidad absoluta de debates por elección.
       El máximo real corresponde a 24 debates anuales (Costa Rica), equivalentes a un log de 3.17; el mínimo es 0.
       
       *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
plotname <- "ev_t_e"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# traduccion de log 

n_debates_año_pais <- democracias_anual_full$n_debates_año_pais %>% unique() 
log <- democracias_anual_full$n_debates_año_pais %>% unique() %>% log()
conversion_log <- tibble(n_debates_año_pais = n_debates_año_pais, 
                         log = log)


 
######################################################################
# ORGANIZADORES #################

## aparte: check nombres internacionales #####
internacionales <- democracias_organizadores %>% 
  subset(ncat_subtipov2=="ong_internacional"|ncat_subtipov2=="publico_internacional"|ncat_subtipov2=="educ_internacional"|ncat_subtipov2=="tv_internacional"| str_detect(nombres_organizadores, "CNN"))
internacionales$nombres_organizadores %>% unique()
internacionales$id_debate %>% unique() %>% length()
####### PENDIENTE IMPORTANTE ALGUNOS CAMBIOS EN ROJO EN BASE DEBATES LIMPIA MAL ARRASTRADOS ###############
## incidencia de categorias ####

### cuenta de macro-orgs #############
democracias_organizadores <- democracias_organizadores %>% 
  mutate(macro_orgs =ifelse(cat_tipoorgv2%in%c("mmp","estado","osc","educ"), "no mediáticos",
                            ifelse(cat_tipoorgv2=="mmc", "mediáticos",
                                   NA)))


porcentaje_orgs_mediaticos <- nrow(democracias_organizadores[democracias_organizadores$macro_orgs=="mediáticos",]) / nrow(democracias_organizadores)
porcentaje_orgs_no_mediaticos <- nrow(democracias_organizadores[democracias_organizadores$macro_orgs=="no mediáticos",]) / nrow(democracias_organizadores)

n_debates <- length(unique(democracias_organizadores$id_debate))

ppaciones_mediaticos <- nrow( 
  unique(
    democracias_organizadores[democracias_organizadores$macro_orgs=="mediáticos","id_debate"]
  ))

ppaciones_no_mediaticos <- nrow( 
  unique(
    democracias_organizadores[democracias_organizadores$macro_orgs=="no mediáticos","id_debate"]
  ))

porcentaje_ppaciones_mediaticos <- ppaciones_mediaticos / n_debates
porcentaje_ppaciones_no_mediaticos <- ppaciones_no_mediaticos / n_debates
  
### cuentas de orgs #############

n_tot_individuos <- nrow(democracias_organizadores)
n_debates_c_data <- n_distinct(democracias_organizadores$id_debate)

cuenta_tipos <- democracias_organizadores %>% 
  group_by(cat_tipoorgv2) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  mutate(pr_n_individuos = n_individuos/n_tot_individuos*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_c_data*100) %>% 
  arrange(desc(pr_debates_en_participaron))

cuenta_tipos$n_individuos %>% sum(na.rm=T)

cuenta_tipos
n_tot_individuos
n_debates_c_data

### cuentas de subtipos por tipo ##########

# por tipo
cuenta_subtipos_mmc <- democracias_organizadores %>% 
  filter(cat_tipoorgv2=="mmc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_mmp <- democracias_organizadores %>% 
  filter(cat_tipoorgv2=="mmp") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_estado <- democracias_organizadores %>% 
  filter(cat_tipoorgv2=="estado") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))


cuenta_subtipos_educ <- democracias_organizadores %>% 
  filter(cat_tipoorgv2=="educ") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_osc <- democracias_organizadores %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))


### grafico ANEXO subtipos y tipos ####

cuenta_subtipos_gral_2 <- democracias_organizadores  %>%
  group_by(cat_tipoorgv2, ncat_subtipov2) %>% 
  summarise(n_subtipo = n()) %>% 
  arrange(desc(cat_tipoorgv2), desc(n_subtipo)) %>%  
  ungroup() %>% 
  mutate(ncat_subtipov2_2 = ifelse(n_subtipo > 3, ncat_subtipov2, paste("otros", cat_tipoorgv2))) %>%
  group_by(cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n_subtipo)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n_subtipo/n_cat*100) 

plot_cuenta_subtipos_gral_2 <- cuenta_subtipos_gral_2  %>% 
  left_join(nombres_subtipos %>% rename(ncat_subtipov2_2 = "ncat_subtipov2")) %>% 
  mutate(str_subtipo = ifelse(is.na(str_subtipo),  paste("Otros", cat_tipoorgv2), str_subtipo)) %>%  
  mutate(str_subtipo = ifelse(str_subtipo=="Otros mmc", "Otros medios privados",
                              ifelse(str_subtipo=="Otros mmp", "Otros medios públicos",
                                     ifelse(str_subtipo=="Otros educ", "Otros sector educativo",
                                            ifelse(str_subtipo=="Otros osc", "Otros OSCs",
                                                   ifelse(str_subtipo=="Otros estado", "Otros Estado",
                                                          str_subtipo)))))) %>% 
  na.omit() %>% 
  arrange(n_cat, n_subtipo) %>% 
  mutate(order = row_number()) %>% 
  mutate(str_subtipo = fct_reorder(str_subtipo, order))  %>%
 # mutate(str_subtipo = fct_relevel(str_subtipo, "Otros", after = Inf))  %>%
  ggplot() +
  geom_col(aes(str_subtipo, n_subtipo, fill = cat_tipoorgv2)) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(breaks=c("educ", "estado", "mmc","mmp", "osc", "NA"),
                    labels=c("Sector educativo", "Estado", "Medios comerciales","Medios públicos", "OSCs", "S/ Datos"),
                    values = c("#F2E947", "#2CD74F",  "#F42AF3","#23B0C2","#F1B61B","#C6C5C5")) + 
  labs(y = "n subtipo*", x = "Subtipo de organizador", fill = "Tipo de organizador",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un organizador-debate.
       Si un organizador participó de dos debates, se lo contabiliza dos veces.
       Hay 5 debates para los que desconcemos el carácter del organizador.
       
       La cuenta contempla los debates de todos los países estudiados que se hayan efectuado bajo regímenes mínimamente democráticos: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ),
       title = "Tipos y subtipos de organizadores",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto") +
  theme(legend.position = "bottom")


#plotnumber <- plotnumber + 1
plotname <- "_anexo_distrib_orgsysubtipos"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 12, height = 7)



## t evolucion temporal de tipo de organizador #####

# cuentas proporciones x decadas

cuenta_tipos_por_decada <- democracias_organizadores %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>%
  mutate(n_debates_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_tipoorgv2, decada) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate),
            n_debates_en_decada = mean(n_debates_en_decada)) %>% 
  ungroup() %>% 
  group_by(decada) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_en_decada*100) %>% 
  arrange(desc(pr_debates_en_participaron))

n_decadas_individuos <- cuenta_tipos_por_decada %>%
  group_by(decada) %>%
  summarise(n_entidades_total_decada = sum(n_individuos))  

# reporto
tabla_cuenta_tipos_por_decada_debates <- cuenta_tipos_por_decada %>% 
  select(cat_tipoorgv2, decada, pr_debates_en_participaron, n_debates_en_decada) %>% 
  left_join(n_decadas_individuos) %>% 
  arrange(decada) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(prom_entxdebate = round(n_entidades_total_decada/ n_debates_en_decada, 1)) %>% 
  select(   "decada",
            "n_debates_en_decada",
            "n_entidades_total_decada", 
            "prom_entxdebate",
            "mmc" , "estado",  "mmp", "osc", "educ") 
# se lee como: del total de debates registrados, en XX % participaron orgs de la categoria:

tabla_cuenta_tipos_por_decada_debates %>% write.csv("anexos/ev_t_organizadores.csv")


# no reporto
# tabla_cuenta_tipos_por_decada_individuos <- cuenta_tipos_por_decada %>% 
#   select(cat_tipoorgv2, decada, pr_n_individuos) %>% 
#   arrange(decada) %>% 
#   pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)
# # se lee como: del total de entidades organizadoras registradas, XX % son de la categoria:

## e distribucion por pais del tipo de organizador #####

cuenta_tipos_por_cat_pais <- democracias_organizadores %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>%
  group_by(cat_tipoorgv2, cat_pais) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate),
            n_debates_en_pais = mean(n_debates_en_pais)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_en_pais*100) %>% 
  arrange(desc(pr_debates_en_participaron))


n_por_pais_individuos <- cuenta_tipos_por_cat_pais %>%
  group_by(cat_pais) %>%
  summarise(n_entidades_total_pais = sum(n_individuos)) %>%
  arrange(cat_pais)


# reporto esta: 

tabla_cuenta_tipos_por_pais_debates <- cuenta_tipos_por_cat_pais %>% 
  select(cat_tipoorgv2, cat_pais, pr_debates_en_participaron, n_debates_en_pais) %>% 
  left_join(n_por_pais_individuos)  %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(prom_entxdebate = round(n_entidades_total_pais/ n_debates_en_pais, 1)) %>% 
  select(   "cat_pais",
            "n_debates_en_pais",
            "n_entidades_total_pais",
            "prom_entxdebate",
            "mmc" , "estado",  "mmp", "osc", "educ")

tabla_cuenta_tipos_por_pais_debates %>% write.csv("anexos/ev_e_organizadores.csv")

# organizadores del tipo XXX participaron en XXX% de los debates registrados en XXXX
# de los debates registrados en XXXX, en XX% participaron organizadores del tipo XXX.

# # no reporto: 
# tabla_cuenta_tipos_por_pais_individuos <- cuenta_tipos_por_cat_pais %>% 
#   select(cat_tipoorgv2, cat_pais, pr_n_individuos, n_debates_en_pais) %>% 
#   arrange(cat_pais) %>% 
#   pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)
# 
# 
# tabla_cuenta_tipos_por_pais_individuos <- tabla_cuenta_tipos_por_pais_individuos %>% 
#   left_join(n_por_pais_individuos)  

# se lee: del total de orgs de debate individuales (pos de que esten repe) recabados para el paisXX, 
# XX%% pertenecen a la categoria XXXX

# por subtipo SOLO PARA CHEQUEO NO REPORTO 


cuenta_SUSBtipos_por_cat_pais <- democracias_organizadores %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>%
  group_by(ncat_subtipov2, cat_pais) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate),
            n_debates_en_pais = mean(n_debates_en_pais)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_en_pais*100) %>% 
  arrange(desc(pr_debates_en_participaron))

  
tabla_cuenta_SUBtipos_por_pais_debates <- cuenta_SUSBtipos_por_cat_pais %>% 
  select(ncat_subtipov2, cat_pais, pr_debates_en_participaron, n_debates_en_pais) %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = ncat_subtipov2, values_from = pr_debates_en_participaron) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) 
 
## grafico ANEXO ev t e  ###########

#base no agrupada para hacer cuentas y graficos
base_tipos_ungrouped <- democracias_organizadores %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  mutate(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
         n_organizadores = n()) %>% 
  ungroup %>% 
  mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
         n_prppacorg = 1/n_organizadores) # porcentaje de participacion del organizador sobre la cantidad de organizadores del debate

# base agrupada por año y pais
base_tipos_countryear <- base_tipos_ungrouped %>% 
  group_by(ncat_eleccion, cat_pais, cat_tipoorgv2) %>% 
  summarise(n_prppaccatorg = sum(n_prppaccatorg, na.rm = T),
            n_prppacorg = sum(n_prppacorg, na.rm = T) )

tipos_organizadores_ev_anual_paisv2 <- base_tipos_countryear %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  mutate(cat_tipoorgv2 = ifelse(is.na(cat_tipoorgv2)|cat_tipoorgv2=="NA", "S/ Datos", cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc" ), 
             colour = cat_tipoorgv2, 
             size= n_prppaccatorg,
             shape= as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos")))  +
  geom_point() +
  facet_wrap( ~ as.factor(cat_pais) 
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              , ncol = 6) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_shape_manual(values=c("S/ Datos" = 4, 
                              "estado" = 19,
                              "mmp" = 19,
                              "educ" = 19,
                              "mmc" = 19,
                              "osc" = 19)) +
  scale_x_continuous(breaks = seq(1955,2025,10)) +
  scale_colour_manual(breaks = c("S/ Datos", "estado", "mmp","osc", "educ", "mmc"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "Estado","M.** Públicos","OSCs","Educativo","M.** Comerciales", "Sin Datos")) +
  labs(x = "Año",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
               El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado.
       Si un debate fue organizado por más de un tipo de organizador, se lo contabiliza dos veces.
       
       *Rep. Dom. = República Dominicana.
       **M. = Medios.")
#
plotnumber <- plotnumber + 1
plotname <- "anexo_ev_t_e_organizadores"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


##  between within NO REPORTO #####
 
orgstsum <- xtsum::xtsum(
  democracias_basedebates,
  variables = c("dico_org_educ",
                "dico_org_mmc",
                "dico_org_mmp",
                "dico_org_estado",
                "dico_org_osc"),
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)
orgstsum

## cuentas de alianzas #########

alianzas_cuenta <- democracias_organizadores %>%  
  group_by(id_debate, ncat_eleccion, cat_pais) %>% 
  summarise(n_orgs = n(),
            n_variedadorgs = n_distinct(cat_tipoorgv2),
            n_variedadsubtipos = n_distinct(ncat_subtipov2))


alianzas_cuenta_meandecada <- alianzas_cuenta %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_orgs_decada = mean(n_orgs, na.rm=T),
            mean_n_variedadorgs_decada = mean(n_variedadorgs, na.rm=T),
            mean_n_variedadsubtipos_decada = mean(n_variedadsubtipos, na.rm=T)) 

alianzas_cuenta_meandecada %>% 
  select(-mean_n_variedadsubtipos_decada) %>% write.csv("anexos/alianzas_variedad.csv")


# no reporto 
# alianzas_cuenta_meanpais <- alianzas_cuenta %>%  
#   group_by(cat_pais) %>% 
#   summarise(mean_n_orgs_pais = mean(n_orgs, na.rm=T),
#             mean_n_variedadorgs_pais = mean(n_variedadorgs, na.rm=T),
#             mean_n_variedadsubtipos_pais = mean(n_variedadsubtipos, na.rm=T))



## streaming (N AL PIE PENDIENTE) #######
# PENDIENTE CORREGIR , PARECEN ESTAR MAL LOS DATOS SOBRE STREAMING !
streaming <- democracias_basedebates %>% 
  filter(dico_streaming == TRUE) 

nrow(streaming)/nrow(democracias_basedebates)

# de los transmitidos streaming, X% contó con X actor
streaming_tipos <- streaming %>% 
  mutate(dico_cattipo_mmc = ifelse(n_cattipo_mmc==0,0,1),
         dico_cattipo_osc = ifelse(n_cattipo_osc==0,0,1),
         dico_cattipo_estado = ifelse(n_cattipo_estado==0,0,1),
         dico_cattipo_educ = ifelse(n_cattipo_educ==0,0,1),
         dico_cattipo_mmp = ifelse(n_cattipo_mmp==0,0,1),
         dico_cattipo_NA = ifelse(n_cattipo_NA==0,0,1)) %>% 
  select(starts_with("dico_cattipo"))

streamingtotals <- colSums(streaming_tipos) 
# revisar eventualmente si estas cuentas tienen sentido . Tienen sentido pero no son la cuenta pertinente. Porque no ponderan por cantidad de actores del tipo involucrado
streamingtotals["dico_cattipo_osc"]/sum(streamingtotals)*100   # osc  
streamingtotals["dico_cattipo_educ"]/sum(streamingtotals)*100  # educ  
streamingtotals["dico_cattipo_mmc"] /sum(streamingtotals)*100  # mmc  
streamingtotals["dico_cattipo_estado"]/sum(streamingtotals)*100 # estado
streamingtotals["dico_cattipo_NA"]/sum(streamingtotals)*100 # NA  
streamingtotals["dico_cattipo_mmp"]/sum(streamingtotals)*100 # NA
sum(streamingtotals)
length(streaming$id_debate)/length(democracias_basedebates$id_debate)*100 #  

# puesto al reves: de los que participaron... un XX fue transmitido solo x streaming 
oscsstreaming <- democracias_basedebates %>% 
  filter(n_cattipo_osc>0) %>% 
  select(dico_streaming, n_cattipo_osc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(oscsstreaming$dico_streaming, na.rm=T)/length(oscsstreaming$dico_streaming)*100

educstreaming <- democracias_basedebates %>% 
  filter(n_cattipo_educ>0) %>% 
  select(dico_streaming, n_cattipo_educ) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(educstreaming$dico_streaming, na.rm=T)/length(educstreaming$dico_streaming)*100


mmcstreaming <- democracias_basedebates %>% 
  filter(n_cattipo_mmc>0) %>% 
  select(dico_streaming, n_cattipo_mmc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(mmcstreaming$dico_streaming, na.rm=T)/length(mmcstreaming$dico_streaming)*100


##########################################################################################
# FORMATOS #############

# cuentas simples #

democracias_formatos$id_debate %>% unique() %>%  length()

## Patrones de interaccion ####
### cuentas simples #####

n <- democracias_basedebates %>% nrow()

democracias_basedebates %>% subset(dico_formato_duelo==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_duelo==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_libre==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_libre==1) %>% nrow()/ n



democracias_basedebates %>% subset(dico_formato_moderadores==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_moderadores==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_periodistas==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_periodistas==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_expertos==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_expertos==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_sectores ==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_sectores ==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_virtuales==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_virtuales==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_presentes==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_presentes==1) %>% nrow()/ n

democracias_basedebates %>% subset(dico_formato_expositivo==1) %>% nrow()
democracias_basedebates %>% subset(dico_formato_expositivo==1) %>% nrow()/ n

### ev t  por decada, para mostrar #########

tipos_formatos_decada <- democracias_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  mutate(n_debates_cregistro_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_tipo_formato, decada, n_debates_cregistro_en_decada) %>% 
  summarise(n_formato_en_decada = n()) %>% 
  ungroup() %>% 
  mutate(pr_formato_en_decada = n_formato_en_decada/ n_debates_cregistro_en_decada *100)  

tipos_formatos_decada_wide <- tipos_formatos_decada %>% 
  select(-n_formato_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_decada) %>% 
  arrange(decada) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  select("decada", "n_debates_cregistro_en_decada",
         "pr_formatoduelo", "pr_formatolibre",
         "pr_formatoexpositivo", "pr_formatomoderadores",
         "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
         "pr_formatopresentes","pr_formatovirtuales")

tipos_formatos_decada_wide %>% write_csv("anexos/tipos_formatos_decada_wide.csv")

# alternativa numerica, no report0
# tipos_formatos_decada_wide_n <- tipos_formatos_decada %>% 
#   select(-pr_formato_en_decada) %>% 
#   pivot_wider(names_from=cat_tipo_formato, values_from = n_formato_en_decada) %>% 
#   arrange(decada) 
# se lee: de los debates con datos para XX decada, XXX % incluyen al menos un segmento con XXX formato

### ev e patrones de interaccion por paises ########

tipos_formatos_pais <- democracias_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_cregistro_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_cregistro_en_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE),
            n_formato_en_pais = n()) %>% 
  mutate(pr_formato_en_pais = n_formato_en_pais/ n_debates_cregistro_en_pais )

 
tipos_formatos_pais_wide <- tipos_formatos_pais %>% 
  select(-c(n_peso_formato_xdebate, n_formato_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100)) %>% 
  #arrange(n_debates_cregistro_en_pais) %>% 
  select("cat_pais", "n_debates_cregistro_en_pais",
         "pr_formatoduelo", "pr_formatolibre",
         "pr_formatoexpositivo", "pr_formatomoderadores",
         "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
         "pr_formatopresentes","pr_formatovirtuales")

tipos_formatos_pais_wide %>% write_csv("anexos/tipos_formatos_pais_wide.csv")

### ANEXO ordinal patrones de interaccion PENDIENTE COMENTARIO Y PASAR A TABLA EN VEZ DE GRAFICO  ####


#### competencia ####

democracias_basedebates$ncat_competencia %>% summary()
democracias_basedebates$ncat_competencia %>% sd(na.rm=T)

##### por decadas #####

ev_ncat_competencia_decadas <- democracias_basedebates %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_ncat_competencia_decadas = mean(ncat_competencia, na.rm=T),
            sd_ncat_competencia_decadas = sd(ncat_competencia, na.rm=T),
            n_debates_año_pais = n()) %>% 
  mutate(
    SE = sd_ncat_competencia_decadas / sqrt(n_debates_año_pais),
    CI_Lower = mean_ncat_competencia_decadas - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_competencia_decadas + qt(0.975, n - 1) * SE
  )

plot_ncat_competencia_decadas <- ev_ncat_competencia_decadas %>% 
  ggplot(aes(x = decada, y = mean_ncat_competencia_decadas)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Promedio en el nivel de competencia por década",
       subtitle = "Junto a su intervalo de confianza (95%)",
       x = "Década",
       y = "Nivel de competencia promedio",
       caption = "Elaboración propia con base en los datos de Franco Häntzsch (2022).
       El nivel de competencia por debate se mide en una escala que va del 0 = nula interacción entre candidatos, al 4 = mucha") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1960,2020,10)) +
  scale_y_continuous(limits=c(min(democracias_basedebates$ncat_competencia, na.rm=T),max(democracias_basedebates$ncat_competencia, na.rm=T)), 
                     breaks=c(min(democracias_basedebates$ncat_competencia, na.rm=T):max(democracias_basedebates$ncat_competencia, na.rm=T)))


plotnumber <- plotnumber + 1
plotname <- "competencia_decadas"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

##### por pais  #####

democracias_basedebates$ncat_competencia %>% summary()
democracias_basedebates$ncat_competencia %>% sd(na.rm=T)

ncat_competencia_por_pais <- democracias_basedebates %>% 
  group_by(cat_pais) %>% 
  summarise(mean_ncat_competencia_pais = mean(ncat_competencia, na.rm=T),
            sd_ncat_competencia_pais = sd(ncat_competencia, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_competencia_pais / sqrt(n),
    CI_Lower = mean_ncat_competencia_pais - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_competencia_pais + qt(0.975, n - 1) * SE
  )

plot_ncat_competencia_por_pais <- ncat_competencia_por_pais %>% 
  ggplot(aes(x = cat_pais, y = mean_ncat_competencia_pais)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Promedio en el nivel de competencia por país",
       subtitle = "Junto a su intervalo de confianza (95%)",
       x = "País",
       y = "Nivel de competencia promedio",
       caption = "Elaboración propia con base en los datos de Franco Häntzsch (2022).
       El nivel de competencia por debate se mide en una escala que va del 0 = nula interacción entre candidatos, al 4 = mucha") +
  theme_classic() +
  scale_y_continuous(limits=c(min(democracias_basedebates$ncat_competencia, na.rm=T),max(democracias_basedebates$ncat_competencia, na.rm=T)), 
                     breaks=c(min(democracias_basedebates$ncat_competencia, na.rm=T):max(democracias_basedebates$ncat_competencia, na.rm=T))) +
  theme(axis.text.x = element_text(angle=90)) 


plotnumber <- plotnumber + 1
plotname <- "competencia_pais"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

#### participacion   ####


democracias_basedebates$ncat_ppac %>% summary()
democracias_basedebates$ncat_ppac %>% sd(na.rm=T)

##### decadas #####
ev_ncat_ppac_decadas <- base %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_ncat_ppac_decadas = mean(ncat_ppac, na.rm=T),
            sd_ncat_ppac_decadas = sd(ncat_ppac, na.rm=T),
            n = n()) 

ev_ncat_ppac_decadas <- ev_ncat_ppac_decadas %>% 
  mutate(
    SE = sd_ncat_ppac_decadas / sqrt(n),
    CI_Lower = mean_ncat_ppac_decadas - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_ppac_decadas + qt(0.975, n - 1) * SE
  )

plot_ev_ncat_ppac_decadas <- ev_ncat_ppac_decadas %>% 
  ggplot(aes(x = decada, y = mean_ncat_ppac_decadas)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Promedio en el nivel de participación por década",
       subtitle = "Junto a su intervalo de confianza (95%)",
       x = "Décadas",
       y = "Nivel de participación promedio",
       caption = "Elaboración propia con base en los datos de Franco Häntzsch (2022).
       El nivel de participación por debate se mide en una escala que va del 0 = nula participación del público, al 4 = mucha") +
  scale_x_continuous(breaks = seq(1960,2020,10)) +
  scale_y_continuous(limits=c(min(democracias_basedebates$ncat_ppac, na.rm=T),max(democracias_basedebates$ncat_ppac, na.rm=T)), 
                     breaks=c(min(democracias_basedebates$ncat_ppac, na.rm=T):max(democracias_basedebates$ncat_ppac, na.rm=T))) +
  theme_classic()


plotnumber <- plotnumber + 1
plotname <- "ppac_decadas"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

##### por pais   ######

ncat_ppac_por_pais <- democracias_basedebates %>% 
  group_by(cat_pais) %>% 
  summarise(mean_ncat_ppac_pais = mean(ncat_ppac, na.rm=T),
            sd_ncat_ppac_pais = sd(ncat_ppac, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_ppac_pais / sqrt(n),
    CI_Lower = mean_ncat_ppac_pais - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_ppac_pais + qt(0.975, n - 1) * SE
  )


plot_ncat_ppac_por_pais <- ncat_ppac_por_pais %>% 
  ggplot(aes(x = cat_pais, y = mean_ncat_ppac_pais)) +
  geom_point(color = "blue", size = 3) + # poner color pais
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Promedio en el nivel de participación por país",
       subtitle = "Junto a su intervalo de confianza (95%)",
       x = "País",
       y = "Nivel de participación promedio",
       caption = "Elaboración propia con base en los datos de Franco Häntzsch (2022).
       El nivel de participación por debate se mide en una escala que va del 0 = nula participación del público, al 4 = mucha") +
  theme_classic() +
  scale_y_continuous(limits=c(min(democracias_basedebates$ncat_ppac, na.rm=T),max(democracias_basedebates$ncat_ppac, na.rm=T)), 
                     breaks=c(min(democracias_basedebates$ncat_ppac, na.rm=T):max(democracias_basedebates$ncat_ppac, na.rm=T))) +
  theme(axis.text.x = element_text(angle=90)) 

plotnumber <- plotnumber + 1
plotname <- "ppac_pais"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

#### between within NO REPORTO #####

ordinalxtsum <- xtsum::xtsum(
  democracias_basedebates,
  variables = c("ncat_ppac",
                "ncat_competencia"),
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)
ordinalxtsum

### variedad patrones #####

patronesvariedad_t <- democracias_basedebates %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>%  
  mutate(n_patrones =    as.numeric(dico_formato_duelo)+
           as.numeric(dico_formato_libre)+
           as.numeric(dico_formato_moderadores)+
           as.numeric(dico_formato_periodistas)+
           as.numeric(dico_formato_expertos)+
           as.numeric(dico_formato_sectores)+
           as.numeric(dico_formato_virtuales)+
           as.numeric(dico_formato_presentes) +
           as.numeric(dico_formato_expositivo )) %>%
  group_by(decada) %>% 
  summarise(mean_n_patrones = mean(n_patrones, na.rm=T)) %>% 
  arrange(decada)

patronesvariedad_e <- democracias_basedebates %>%  
  mutate(n_patrones =    as.numeric(dico_formato_duelo)+
                              as.numeric(dico_formato_libre)+
                              as.numeric(dico_formato_moderadores)+
                              as.numeric(dico_formato_periodistas)+
                              as.numeric(dico_formato_expertos)+
                              as.numeric(dico_formato_sectores)+
                              as.numeric(dico_formato_virtuales)+
                              as.numeric(dico_formato_presentes) +
                              as.numeric(dico_formato_expositivo )) %>%
  group_by(cat_pais) %>% 
  summarise(mean_n_patrones = mean(n_patrones, na.rm=T),
            n_debates_pais = n())# %>% 
 # arrange(mean_n_patrones)
 
## Temas ############


### cuentas simples #####

n <- nrow(democracias_basedebates)
n - length(unique(democracias_temas$id_debate)) # missings
(n - length(unique(democracias_temas$id_debate)))/n

democracias_basedebates %>% subset(dico_temas_libre==1) %>% nrow()
democracias_basedebates %>% subset(dico_temas_libre==1) %>% nrow()/n

democracias_basedebates %>% subset(dico_temas_bloques==1) %>% nrow()
democracias_basedebates %>% subset(dico_temas_bloques==1) %>% nrow()/n

democracias_basedebates %>% subset(dico_temas_puntuales==1) %>% nrow()
democracias_basedebates %>% subset(dico_temas_puntuales==1) %>% nrow()/n

democracias_basedebates %>% subset(dico_temas_monotema==1) %>% nrow()
democracias_basedebates %>% subset(dico_temas_monotema==1) %>% nrow()/n


### ev t  temas x decada ####

tipos_temas_decada <- democracias_temas %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  mutate(n_debates_cregistro_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_tipo_tema, decada, n_debates_cregistro_en_decada) %>% 
  summarise(n_tema_en_decada = n()) %>% 
  ungroup() %>% 
  mutate(pr_tema_en_decada = n_tema_en_decada/ n_debates_cregistro_en_decada *100)  

tipos_temas_decada_wide <- tipos_temas_decada %>% 
  select(-n_tema_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_tema_en_decada) %>% 
  arrange(decada) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  select("decada", "n_debates_cregistro_en_decada",
         "pr_temalibre","pr_temabloques",
         "pr_temapuntuales","pr_temamonotema")

tipos_temas_decada_wide %>% write_csv("anexos/tipos_temas_decada_wide.csv")


### ev e temas x pais #####

# para calcular tabla

tipos_temas_pais <- democracias_temas %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_cregistro_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_cregistro_en_decada, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE),
            n_tema_en_pais = n()) %>% 
  mutate(pr_tema_en_pais = n_tema_en_pais/ n_debates_cregistro_en_decada )


# tabla de distribucion por pais, para calcular

tipos_temas_pais_wide <- tipos_temas_pais %>% 
  select(-c(n_peso_tema_xdebate, n_tema_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_tema_en_pais) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100)) %>% 
  #arrange(n_debates_cregistro_en_decada) %>% 
  select("cat_pais", "n_debates_cregistro_en_decada",
         "pr_temalibre","pr_temabloques",
         "pr_temapuntuales","pr_temamonotema")

tipos_temas_pais_wide %>% write_csv("anexos/tipos_temas_pais_wide.csv")

### betw- with no creo que reporte ####

temastsum <- xtsum::xtsum(
  democracias_basedebates,
  variables = c("dico_temas_libre",
                "dico_temas_bloques",
                "dico_temas_puntuales",
                "dico_temas_monotema"),
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)
temastsum

### variedad temas #####

esquemasvariedad_t <- democracias_basedebates %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>%  
  mutate(n_esquemas =    as.numeric(dico_temas_monotema)+
           as.numeric(dico_temas_libre)+
           as.numeric(dico_temas_puntuales)+
           as.numeric(dico_temas_bloques)) %>%
  group_by(decada) %>% 
  summarise(mean_n_esquemas = mean(n_esquemas, na.rm=T)) %>% 
  arrange(decada)

esquemasvariedad_e <- democracias_basedebates %>%  
  mutate(n_esquemas =    as.numeric(dico_temas_monotema)+
           as.numeric(dico_temas_libre)+
           as.numeric(dico_temas_puntuales)+
           as.numeric(dico_temas_bloques)) %>%
  group_by(cat_pais) %>% 
  summarise(mean_n_esquemas = mean(n_esquemas, na.rm=T),
            n_debates_pais = n())# %>% 
  #arrange(mean_n_esquemas)

### agrego variedad de temas y patrones ####

variedad_t <- patronesvariedad_t %>% 
  left_join(esquemasvariedad_t)

variedad_e <- patronesvariedad_e %>% 
  left_join(esquemasvariedad_e) %>% 
  select(cat_pais,
         n_debates_pais, 
         mean_n_patrones, 
         mean_n_esquemas)

variedad_t %>% write.csv("anexos/variedad_formatos_t.csv")
variedad_e %>% write.csv("anexos/variedad_formatos_e.csv")

## debates s ausentes ##
summary(democracias_basedebates$dico_ausencias)
debates_s_ausentes <- democracias_basedebates %>% 
  subset(n_ausentes==0)
debates_c_ausentes <- democracias_basedebates %>% 
  subset(n_ausentes>0)

nrow(debates_s_ausentes)/nrow(democracias_basedebates)

nrow(debates_c_ausentes)/nrow(democracias_basedebates)

## Invitaciones ####

 
hist(democracias_basedebates$n_invitados)
summary(democracias_basedebates$n_invitados)
max(democracias_basedebates$n_invitados, na.rm = T)
min(democracias_basedebates$n_invitados, na.rm = T)

#PENDIENTE: REVISAR MISSING DATA: #
n <- nrow(democracias_basedebates)
missing_n_invitados <- democracias_basedebates %>% subset(is.na(n_invitados))
nrow(missing_n_invitados)
 

base_n_candidatos_año_pais <- democracias_basedebates %>%  
  group_by(ncat_eleccion, cat_pais, ncat_ronda) %>% 
  summarise(n_debates_año_pais = n_distinct(id_debate),
            mean_n_invitados = mean(n_invitados, na.rm = T),
            mean_n_ausentes = mean(n_ausentes, na.rm=T),
            mean_n_presentes = mean(n_presentes, na.rm=T)) 


base_n_candidatos_año_pais <- democracias_ronda_full  %>% 
  left_join(base_n_candidatos_año_pais)  %>% 
  mutate(mean_prop_invitados = mean_n_invitados/ntc)   


cor(base_n_candidatos_año_pais$mean_n_invitados, base_n_candidatos_año_pais$ntc, method = "pearson", use ="complete.obs")
cor(base_n_candidatos_año_pais$mean_n_invitados, base_n_candidatos_año_pais$nec, method = "pearson", use ="complete.obs")


#### en el tiempo #####

ev_t_n_candidatos <- base_n_candidatos_año_pais %>% 
  group_by(ncat_eleccion, ncat_ronda) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais, na.rm = T),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T),
            mean_ntc = mean(ntc, na.rm=T),
            mean_nec = mean(nec, na.rm= T),
            mean_prop_invitados = mean(mean_prop_invitados, na.rm=T))   


ev_t_n_candidatos_decada <- ev_t_n_candidatos %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada, ncat_ronda) %>% 
  summarise(n_debates_decada = sum(n_debates_año),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T),
            mean_ntc = mean(mean_ntc, na.rm=T),
            mean_nec = mean(mean_nec, na.rm= T),
            mean_prop_invitados = mean(mean_prop_invitados, na.rm=T))

ev_t_n_candidatos_final <- ev_t_n_candidatos_decada %>% 
  subset(ncat_ronda==1) %>% 
  dplyr::rename("n_debates_ronda1_decada" = n_debates_decada) %>% 
  select(-ncat_ronda) %>% 
  left_join(ev_t_n_candidatos_decada %>% 
              subset(ncat_ronda==2) %>% 
              select(decada, n_debates_decada) %>% 
              dplyr::rename("n_debates_ronda2_decada" = n_debates_decada)) %>% 
  select("decada", "n_debates_ronda1_decada", "n_debates_ronda2_decada",
         "mean_ntc", "mean_nec", 
         "mean_n_invitados", "mean_n_presentes", "mean_n_ausentes", "mean_prop_invitados") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.),0,.))) 

ev_t_n_candidatos_final %>% write.csv("anexos/ev_t_n_candidatos_decada.csv")

# cor(ev_t_n_candidatos$mean_n_invitados, ev_t_n_candidatos$mean_ntc, method = "pearson", use ="complete.obs")
# cor(ev_t_n_candidatos$mean_n_invitados, ev_t_n_candidatos$mean_nec, method = "pearson", use ="complete.obs")


### por paises  ####


ev_e_n_candidatos <- base_n_candidatos_año_pais %>% 
  group_by(cat_pais, ncat_ronda) %>% 
  summarise(n_debates_pais = sum(n_debates_año_pais, na.rm = T),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T),
            mean_ntc = mean(ntc, na.rm=T),
            mean_nec = mean(nec, na.rm= T),
            mean_prop_invitados = mean(mean_prop_invitados, na.rm=T))   

ev_e_n_candidatos_final <- ev_e_n_candidatos %>% 
  subset(ncat_ronda==1) %>% 
  dplyr::rename("n_debates_ronda1_pais" = n_debates_pais) %>% 
  select(-ncat_ronda) %>% 
  left_join(ev_e_n_candidatos %>% 
              subset(ncat_ronda==2) %>% 
              select(cat_pais, n_debates_pais) %>% 
              dplyr::rename("n_debates_ronda2_pais" = n_debates_pais)) %>% 
  select("cat_pais", "n_debates_ronda1_pais", "n_debates_ronda2_pais",
                "mean_ntc", "mean_nec", 
                "mean_n_invitados", "mean_n_presentes", "mean_n_ausentes", "mean_prop_invitados") %>% 
  mutate(across(everything(), ~ ifelse(is.na(.),0,.))) 

ev_e_n_candidatos_final %>% write.csv("anexos/ev_e_n_candidatos_pais.csv")

# cor(ev_e_n_candidatos$mean_n_invitados, ev_e_n_candidatos$mean_ntc, method = "pearson", use ="complete.obs")
# cor(ev_e_n_candidatos$mean_n_invitados, ev_e_n_candidatos$mean_nec, method = "pearson", use ="complete.obs")

### grafico anexo #####


plot_prop_invitados_pais <- democracias_basedebates %>% 
  #subset(n_invitados!=42) %>%
  subset(ncat_ronda==1) %>% 
  left_join(democracias_ronda_full) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  mutate(prop_invitados =  as.numeric(n_invitados)/n_candidaturas) %>% 
  #select(cat_pais, ncat_eleccion, prop_invitados, cols_18, str_organizador) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais), prop_invitados, fill = cols_18)) +
  #facet_wrap(~ ncat_ronda) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Proporción de candidatos invitados",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       Se exponen solo los debates realizados de manera previa a la primera ronda electoral. 
       Los valores superiores a 1 se deben por excepcionalmente numerosas bajas de candidatos durante el curso de la campaña. 

       *Rep. Dom. = República Dominicana.",
       title = "Prop invitados",
       subtitle = "por país")

plotnumber <- plotnumber + 1
plotname <- "hist_propinvitados"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

### betw- with no creo que reporte ####

ncandtsum <- xtsum::xtsum(
  democracias_basedebates,
  variables = c("n_invitados"),
  id = "cat_pais",
  t = "ncat_eleccion",
  na.rm = T,
  return.data.frame = T,
  dec = 3
)
ncandtsum


 
######################################################################
# REGULACION #############


normativa_tabla <- democracias_normativa %>% 
  group_by(
    cat_pais, 
    cat_regestado,
    cat_regmedios,
    cat_regcandidatos) %>% 
  summarise(min_year = min(ncat_eleccion),
            max_year = max(ncat_eleccion)) %>% 
  mutate(elecciones_vigencia_all = paste(min_year, max_year, sep = " - ")) %>% 
  select(cat_pais,  
         cat_regestado,
         cat_regmedios,
         cat_regcandidatos,
         elecciones_vigencia_all) %>% 
  arrange(cat_pais , elecciones_vigencia_all)

normativa_tabla %>% write.csv("anexos/normativa_tabla.csv")


### cruce org - formato #####

cor(democracias_basedebates$dico_org_mmc, democracias_basedebates$dico_formato_periodistas, 
    use = "complete.obs",
    method = "kendall")


######################################################################
# NO USO CRUCES ENTRE VARIABLES ###############
# 
###### metodo propio ####
#   
# matriz_cor_all <-  democracias_basedebates %>% 
#   mutate(prop_invitados = n_invitados/n_candidaturas) %>%  
#   select(ncat_eleccion,
#          ncat_ronda,
#          prop_invitados,
#          n_presentes,
#          n_ausentes,
#          dico_org_mmc,
#          dico_org_mmp,
#          dico_org_estado,
#          dico_org_osc,
#          dico_org_educ,
#          dico_formato_libre,
#          dico_formato_duelo,
#          dico_formato_expositivo,
#          dico_formato_moderadores,
#          dico_formato_periodistas,
#          dico_formato_expertos,
#          dico_formato_sectores,
#          dico_formato_presentes,
#          dico_formato_virtuales,
#          dico_temas_libre,
#          dico_temas_bloques,
#          dico_temas_puntuales,
#          dico_temas_monotema,
#          ncat_regmedios,
#          ncat_regestado,
#          ncat_regcandidatos)
# 
# data_clean <- na.omit(matriz_cor_all)
# correlation_matrix <- cor(data_clean, method = "kendall")
# 
# # Visualize the correlation matrix
# # corrplot::corrplot(correlation_matrix, method = "square", type = "upper", 
# #                    tl.col = "black", tl.srt = 45, addCoef.col = "black")
# 
# 
# # otra funcion de grafico
# ggcorrplot::ggcorrplot(correlation_matrix, type = "lower", lab = T, show.legend = F)
# 
# # Generar el gráfico con coordenadas capturadas
# corr_plot <- corrplot::corrplot(
#   correlation_matrix, 
#   method = "circle",
#   col = colorRampPalette(c("red", "white", "green"))(8), 
#   diag = FALSE, 
#   plot = NULL # Capturar coordenadas
# )
# # Crear el gráfico de correlación (parte inferior)
# n <- ncol(correlation_matrix) # Número de columnas/filas
# 
# # Graficar la matriz de correlación
# corrplot::corrplot(
#   correlation_matrix, 
#   method = "circle",
#   col = colorRampPalette(c("red", "white", "green"))(8), 
#   type = "lower",   # Muestra solo la parte inferior
#   addgrid.col = "gray",
#   diag = FALSE      # Ocultar diagonal
# )
# 
# # Resaltar valores absolutos mayores a 0.25 con * y mayores a 0.5 con **
# for (j in 1:n) {
#   for (i in 1:n) {
#     if (i > j) { # Solo trabaja en el triángulo inferior (evita asteriscos en la parte oculta)
#       # Resaltar valores entre 0.25 y 0.5
#       if (abs(correlation_matrix[i, j]) > 0.25 && abs(correlation_matrix[i, j]) <= 0.5) {
#         text(j, n - i + 1, "*", col = "black", cex = 1.5)
#       }
#       # Resaltar valores mayores a 0.5
#       if (abs(correlation_matrix[i, j]) > 0.5) {
#         text(j, n - i + 1, "**", col = "black", cex = 1.5)
#       }
#     }
#   }
# }
# 
# 
##### metodo CHATGPT ####
# # 
# # library(Hmisc)
# # # Clasificación de variables
# # ordinales <- c("ncat_eleccion", "ncat_ronda", "ncat_regmedios", "ncat_regestado", "ncat_regcandidatos")
# # dicotomicas <- c("dico_org_mmc", "dico_org_mmp", "dico_org_estado", "dico_org_osc", 
# #                  "dico_org_educ", "dico_formato_libre", "dico_formato_duelo", 
# #                  "dico_formato_expositivo", "dico_formato_moderadores", "dico_formato_periodistas", 
# #                  "dico_formato_expertos", "dico_formato_sectores", "dico_formato_presentes", 
# #                  "dico_formato_virtuales", "dico_temas_libre", "dico_temas_bloques", 
# #                  "dico_temas_puntuales", "dico_temas_monotema")
# # continuas <- c("prop_invitados", "n_presentes", "n_ausentes")
# # 
# # # Filtrar y limpiar datos
# # data_clean <- democracias_basedebates %>%
# #   mutate(prop_invitados = n_invitados / n_candidaturas) %>%
# #   select(all_of(c(ordinales, dicotomicas, continuas))) %>%
# #   na.omit()
# # 
# # # Función para calcular correlaciones adecuadas
# # correlation_table <- function(data) {
# #   var_names <- colnames(data)
# #   results <- data.frame(Var1 = character(), Var2 = character(), Correlation = numeric(), Method = character())
# #   
# #   for (i in seq_along(var_names)) {
# #     for (j in i:length(var_names)) {
# #       var1 <- var_names[i]
# #       var2 <- var_names[j]
# #       if (i != j) {
# #         if (i %in% ordinales | j %in% ordinales) {
# #           cor_value <- cor(data[[var1]], data[[var2]], method = "kendall")
# #           method <- "Kendall"
# #         } else if (i %in% continuas | j %in% continuas) {
# #           cor_value <- cor(data[[var1]], data[[var2]], method = "pearson")
# #           method <- "Pearson"
# #         } else (i %in% dicotomicas | j %in% dicotomicas) 
# #           cor_value <- cor(data[[var1]], data[[var2]], method = "kendall") # Phi se calcula como Pearson para dicotómicas
# #           method <- "Kendall"
# #          
# #         results <- rbind(results, data.frame(Var1 = var1, Var2 = var2, Correlation = cor_value, Method = method))
# #       }
# #     }
# #   }
# #   return(results)
# # }
# # 
# # # Generar tabla de correlaciones
# # tabla_correlaciones <- correlation_table(data_clean)
# # 
# # # Convertir tabla de correlaciones en formato largo
# # tabla_correlaciones_larga <- tabla_correlaciones %>%
# #   mutate(AbsCorrelation = abs(Correlation)) %>%
# #   mutate(Significance = case_when(
# #     AbsCorrelation > 0.5 ~ "**",
# #     AbsCorrelation > 0.25 ~ "*",
# #     TRUE ~ ""
# #   ))
# # 
# # # Filtrar solo la mitad inferior (Var1 > Var2)
# # tabla_correlaciones_larga_filtrada <- tabla_correlaciones_larga %>%
# #   filter(Var1 > Var2)
# # 
# # # Crear el gráfico con ggplot2 (mitad inferior)
# # ggplot(tabla_correlaciones_larga_filtrada, aes(x = Var1, y = Var2, fill = Correlation)) +
# #   geom_tile(color = "white") +
# #   scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
# #   geom_text(aes(label = Significance), color = "black", size = 4) +
# #   theme_minimal() +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     axis.text.y = element_text(angle = 0, hjust = 1),
# #     axis.title.x = element_blank(),
# #     axis.title.y = element_blank()
# #   ) +
# #   labs(
# #     title = "Matriz de correlaciones (mitad inferior)",
# #     fill = "Correlación"
# #   )
 
