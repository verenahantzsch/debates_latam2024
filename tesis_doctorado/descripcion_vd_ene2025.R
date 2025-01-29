
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

# filtramos otras bases

#elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
democracias_organizadores <- base_organizadores %>% 
  subset(id_debate%in%democracias_basedebates$id_debate) %>% 
  subset(!is.na(nombres_organizadores))
democracias_formatos <- base_formatos %>% 
  subset(id_debate%in%democracias_basedebates$id_debate)
democracias_temas <- base_temas %>% 
  subset(id_debate%in%democracias_basedebates$id_debate)
#base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
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

n_debates_año_pais <- democracias_anual_full$n_debates_año_pais %>% unique() 
log <- democracias_anual_full$n_debates_año_pais %>% unique() %>% log()
conversion_log <- tibble(n_debates_año_pais = n_debates_año_pais, 
                         log = log)


########################################################################
######################################################################
# ORGANIZADORES #################

## incidencia de categorias ####
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
  mutate(ncat_subtipov2_2 = ifelse(n_subtipo > 3, ncat_subtipov2, "otros")) %>%
  group_by(cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n_subtipo)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n_subtipo/n_cat*100) 

plot_cuenta_subtipos_gral_2 <- cuenta_subtipos_gral_2  %>% 
  left_join(nombres_subtipos %>% rename(ncat_subtipov2_2 = "ncat_subtipov2")) %>% 
  mutate(str_subtipo = ifelse(is.na(str_subtipo), "Otros", str_subtipo)) %>%  
  na.omit() %>% 
  arrange(n_cat, n_subtipo) %>% 
  mutate(order = row_number()) %>% 
  mutate(str_subtipo = fct_reorder(str_subtipo, order))  %>%
  mutate(str_subtipo = fct_relevel(str_subtipo, "Otros", after = Inf))  %>%
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
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")


plotnumber <- plotnumber + 1
plotname <- "_anexo_distrib_orgsysubtipos"
filename <- paste("images/plot_", plotnumber, plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)



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
  mutate(prom_entxdebate = round(n_entidades_total_decada/ n_debates_en_decada, 2)) %>% 
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
  mutate(prom_entxdebate = round(n_entidades_total_pais/ n_debates_en_pais, 2)) %>% 
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

oscsstreaming <- democracias_basedebates %>% 
  filter(n_cattipo_osc>0) %>% 
  select(dico_streaming, n_cattipo_osc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(oscsstreaming$dico_streaming, na.rm=T)/length(oscsstreaming$dico_streaming)*100
# ojo, emprolijar NAs, en general equivalen a streaming == F. Creo que a los fines de na.rm el resultado seria el mismo

mmcstreaming <- democracias_basedebates %>% 
  filter(n_cattipo_mmc>0) %>% 
  select(dico_streaming, n_cattipo_mmc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(mmcstreaming$dico_streaming, na.rm=T)/length(mmcstreaming$dico_streaming)*100



####################################################################################
##########################################################################################
# VARIABLE CUALI: FORMATOS #############

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
  arrange(decada)%>% 
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

