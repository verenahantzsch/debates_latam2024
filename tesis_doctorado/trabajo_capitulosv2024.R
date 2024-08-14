# este trabajo crea los graficos usados originalmente en los capitulos individuales de la tesis de maestria ####################

## librerias
library(tidyverse)
#library(hrbrthemes) #
# library(RColorBrewer)
# library(amap)
# library(ape)
# library(dendextend)
# library(ggraph)
# library(igraph)
#library(xlsx) #

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/")

# datos
base <- read_csv("./datav2023/base_final3v2023.csv")
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")
base_anual_full <- readxl::read_xlsx("./datav2023/base_anual_fullv2023.xlsx")

nombres_subtipos <- readxl::read_xlsx("./codebooksv2023/nombres_subtiposv2023.xlsx")

colorespais <- base %>% 
  distinct(cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 

plotnumber <- 0 # para guardar con numeracion ascendente

base <- base %>% mutate(id_elec = paste0(cat_pais,ncat_eleccion,ncat_ronda)) 
u_elec <- base$id_elec %>% unique()

###############################################################################
######################################################################

# VARIABLE DEPENDIENTE. #######

# MEDIDA DICO: EXISTENCIA DE DEBATES ############
base_anual_full <- base_anual_full %>% 
  subset(ncat_eleccion!=2024)

# cuenta simple ####

summary(base_anual_full$dico_hubo_debates)
sd(base_anual_full$dico_hubo_debates)

# evolucion temporal de medida dico: elecciones con y sin debates #####

base_elecciones_conysindebates_t <- base_anual_full %>% 
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
  geom_col(aes(ncat_eleccion,n_dico_debates, fill = dico_debates), colour = "grey10", position = "dodge") +
  theme_minimal()  

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


# MEDIDA CUANTI: cantidad de debates por eleccion #####

# promedios ###########

n_debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n()) 

base_anual_full <- base_anual_full %>% 
  left_join(n_debates_año_pais) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))  

# cuentas
# summary(base_anual_full$n_debates_año_pais)
# sd(base_anual_full$n_debates_año_pais)

# calculos de promedios en t

ev_t_mean_n_debatesxeleccion <- base_anual_full %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_n_debates_año = mean(n_debates_año_pais, na.rm = T))

ev_decada_mean_n_debatesxeleccion <- base_anual_full %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_debates_decada = mean(n_debates_año_pais, na.rm = T))

ev_e_mean_n_debatesxeleccion <- base_anual_full %>% 
  group_by(cat_pais) %>% 
  summarise(mean_n_debates_pais = mean(n_debates_año_pais, na.rm = T)) %>% 
  arrange(mean_n_debates_pais)

# n debates en t ########

n_debates_año <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = n()) 

n_debates_decada <- base %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(n_debates_decada = n()) %>% 
  arrange(decada)

plot_n_debates_año <- n_debates_año %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_debates_año)) +
  geom_line(aes(ncat_eleccion, n_debates_año)) +
  theme_minimal()

# n debates por pais ############

n_debates_pais <- base %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_pais = n()) 

plot_n_debates_año <- n_debates_pais %>% 
  ggplot() +
  geom_col(aes(cat_pais, n_debates_pais)) +
  theme_minimal()


# ev anual, debates en t por pais ######

plot_point_anual2 <- base_anual_full %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(ncat_eleccion, 
             log(n_debates_año_pais),
             colour = as.factor(cat_pais) 
             # %>% 
             #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
             #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
             #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"))
  ))  +
  geom_line() + 
  geom_point(aes(#size= n_debates_año_pais, 
    shape = as.factor(dico_hubo_debates), alpha= as.factor(dico_hubo_debates))) + # corregir, no esta saliendo esto
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  facet_wrap( ~ as.factor(cat_pais) 
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              , 
              ncol = 6) +
  theme_minimal() +
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
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# traduccion de log 

n_debates_año_pais <- base_anual_full$n_debates_año_pais %>% unique() 
log <- base_anual_full$n_debates_año_pais %>% unique() %>% log()
conversion_log <- tibble(n_debates_año_pais = n_debates_año_pais, 
                         log = log)



########################################################################
######################################################################
# VARIABLE CUALI: TIPOS DE ORG #################

# cuentas de orgs #############

n_tot_individuos <- nrow(base_organizadores)
n_debates_c_data <- n_distinct(base_organizadores$id_debate)

cuenta_tipos <- base_organizadores %>% 
  group_by(cat_tipoorgv2) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  mutate(pr_n_individuos = n_individuos/n_tot_individuos*100,
         pr_debates_en_participaron = n_debates_en_participaron/n_debates_c_data*100) %>% 
  arrange(desc(pr_debates_en_participaron))

cuenta_tipos$n_individuos %>% sum(na.rm=T)


# cuentas de subtipos por tipo ##########

# general

cuenta_subtipos_gral <- base_organizadores %>% 
  group_by(cat_tipoorgv2, ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  arrange(desc(cat_tipoorgv2), desc(n)) %>%  
  ungroup() %>% 
  group_by(cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n/n_cat*100) 

#cuenta_subtipos_gral %>% write_excel_csv(file = "anexos/anexo1.xlsx")

# hacemos grafico

cuenta_subtipos_gral_2 <- base_organizadores  %>%
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
  ggplot() +
  geom_col(aes(str_subtipo, n_subtipo, fill = cat_tipoorgv2)) +
  coord_flip() +
  theme_minimal() +
  scale_fill_discrete(breaks=c("educ", "estado", "mmc","mmp", "osc", "NA"), 
                      labels=c("Sector educativo", "Estado", "Medios comerciales","Medios públicos", "OSCs", "S/ Datos")) +
  labs(y = "n subtipo*", x = "Subtipo de organizador", fill = "Tipo de organizador",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un organizador-debate.
       Si un organizador participó de dos debates, se lo contabiliza dos veces.
       Hay 16 debates para los que desconcemos el carácter del organizador.
       
       La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
                       ),
       title = "Tipos y subtipos de organizadores",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

  
plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# por tipo
cuenta_subtipos_mmc <- base_organizadores %>% 
  filter(cat_tipoorgv2=="mmc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_osc <- base_organizadores %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_mmp <- base_organizadores %>% 
  filter(cat_tipoorgv2=="mmp") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_educ <- base_organizadores %>% 
  filter(cat_tipoorgv2=="educ") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_estado <- base_organizadores %>% 
  filter(cat_tipoorgv2=="estado") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

sum(cuenta_tipos$n_individuos)
sum(cuenta_tipos$n_debates_en_participaron)


# cuentas de alianzas #########

alianzas_cuenta <- base_organizadores %>%  
  group_by(id_debate, ncat_eleccion, cat_pais) %>% 
  summarise(n_orgs = n(),
            n_variedadorgs = n_distinct(cat_tipoorgv2),
            n_variedadsubtipos = n_distinct(ncat_subtipov2))

alianzas_cuenta_meant <- alianzas_cuenta %>%  
  group_by(ncat_eleccion) %>% 
  summarise(mean_n_orgs_año = mean(n_orgs, na.rm=T),
            mean_n_variedadorgs_año = mean(n_variedadorgs, na.rm=T),
            mean_n_variedadsubtipos_año = mean(n_variedadsubtipos, na.rm=T))

alianzas_cuenta_meandecada <- alianzas_cuenta %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_n_orgs_decada = mean(n_orgs, na.rm=T),
            mean_n_variedadorgs_decada = mean(n_variedadorgs, na.rm=T),
            mean_n_variedadsubtipos_decada = mean(n_variedadsubtipos, na.rm=T))

alianzas_cuenta_meanpais <- alianzas_cuenta %>%  
  group_by(cat_pais) %>% 
  summarise(mean_n_orgs_pais = mean(n_orgs, na.rm=T),
            mean_n_variedadorgs_pais = mean(n_variedadorgs, na.rm=T),
            mean_n_variedadsubtipos_pais = mean(n_variedadsubtipos, na.rm=T))



# datos sobre streaming en relacion a orgs #######

streaming <- base %>% 
  filter(dico_streaming == TRUE) %>% 
  mutate(dico_cattipo_mmc = ifelse(n_cattipo_mmc==0,0,1),
         dico_cattipo_osc = ifelse(n_cattipo_osc==0,0,1),
         dico_cattipo_estado = ifelse(n_cattipo_estado==0,0,1),
         dico_cattipo_educ = ifelse(n_cattipo_educ==0,0,1),
         dico_attipo_mmp = ifelse(n_cattipo_mmp==0,0,1),
         dico_cattipo_NA = ifelse(n_cattipo_NA==0,0,1)) %>% 
  select(starts_with("dico_cattipo"))

streamingtotals <- colSums(streaming) 
# revisar eventualmente si estas cuentas tienen sentido . Tienen sentido pero no son la cuenta pertinente. Porque no ponderan por cantidad de actores del tipo involucrado
17/sum(streamingtotals)*100   # osc 43.58095 (viejo)--> 36.17021 (nuevo 20203)
9/sum(streamingtotals)*100  # educ 16.53333 . 60.11428 -->19.14894
19 /sum(streamingtotals)*100  # mmc 39.08572 --> 40.42553
0 # estado
2/sum(streamingtotals)*100 # NA -->4.255319
sum(streamingtotals)/length(base$id_debate)*100 #  (v2023:) 12.46684

oscsstreaming <- base %>% 
  filter(n_cattipo_osc>0) %>% 
  select(dico_streaming, n_cattipo_osc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(oscsstreaming$dico_streaming, na.rm=T)/length(oscsstreaming$dico_streaming)*100
# ojo, emprolijar NAs, en general equivalen a streaming == F. Creo que a los fines de na.rm el resultado seria el mismo


# t evolucion temporal de tipo de organizador #####

cuenta_tipos_por_decada <- base_organizadores %>% 
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

  
# 
tabla_cuenta_tipos_por_decada_individuos <- cuenta_tipos_por_decada %>% 
  select(cat_tipoorgv2, decada, pr_n_individuos) %>% 
  arrange(decada) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)
# se lee como: del total de entidades organizadoras registradas, XX % son de la categoria:

tabla_cuenta_tipos_por_decada_debates <- cuenta_tipos_por_decada %>% 
  select(cat_tipoorgv2, decada, pr_debates_en_participaron) %>% 
  arrange(decada) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron)
# se lee como: del total de debates registrados, en XX % participaron orgs de la categoria:



# e distribucion por pais del tipo de organizador #####

cuenta_tipos_por_cat_pais <- base_organizadores %>% 
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

# 
tabla_cuenta_tipos_por_pais_individuos <- cuenta_tipos_por_cat_pais %>% 
  select(cat_tipoorgv2, cat_pais, pr_n_individuos) %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_n_individuos)

n_por_pais_individuos <- cuenta_tipos_por_cat_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_entidades_total_pais = sum(n_individuos)) %>% 
  arrange(cat_pais)

tabla_cuenta_tipos_por_pais_individuos <- tabla_cuenta_tipos_por_pais_individuos %>% 
  left_join(n_por_pais_individuos) #%>% 
  #arrange(desc(mmc))
 # arrange(desc(estado))
  #arrange(desc(osc))
# se lee: del total de orgs de debate individuales (pos de que esten repe) recabados para el paisXX, 
# XX%% pertenecen a la categoria XXXX

tabla_cuenta_tipos_por_pais_debates <- cuenta_tipos_por_cat_pais %>% 
  select(cat_tipoorgv2, cat_pais, pr_debates_en_participaron) %>% 
  arrange(cat_pais) %>% 
  pivot_wider(names_from = cat_tipoorgv2, values_from = pr_debates_en_participaron)

n_por_pais_debates <- cuenta_tipos_por_cat_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_total_pais = sum(n_debates_en_participaron)) %>% 
  arrange(cat_pais)

tabla_cuenta_tipos_por_pais_debates <- tabla_cuenta_tipos_por_pais_debates %>% 
  left_join(n_por_pais_debates) 
 # %>% 
  #arrange(desc(n_debates_total_pais))
  #arrange(cat_pais)
  #arrange(desc(mmc))
  #arrange(desc(estado))
  #arrange(desc(osc))
# organizadores del tipo XXX participaron en XXX% de los debates registrados en XXXX
# de los debates registrados en XXXX, en XX% participaron organizadores del tipo XXX.
  

####################################################################################
##########################################################################################
# VARIABLE CUALI: FORMATOS #############

# cuentas simples #

base %>% subset(dico_formato_duelo==1) %>% nrow()
base %>% subset(dico_formato_duelo==1) %>% nrow()/389
base %>% subset(dico_formato_libre==1) %>% nrow()

base %>% subset(dico_formato_expositivo==1) %>% nrow()

base %>% subset(dico_formato_moderadores==1) %>% nrow()

base %>% subset(dico_formato_periodistas==1) %>% nrow()

base %>% subset(dico_formato_expertos==1) %>% nrow()

base %>% subset(dico_formato_sectores ==1) %>% nrow()

base %>% subset(dico_formato_presentes==1) %>% nrow()
base %>% subset(dico_formato_virtuales==1) %>% nrow()


# Patrones de interaccion en el tiempo ########
# tabla de distribucion por decada, para mostrar #########

tipos_formatos_decada <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  mutate(n_debates_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_tipo_formato, decada, n_debates_en_decada) %>% 
  summarise(n_formato_en_decada = n()) %>% 
  ungroup() %>% 
  mutate(pr_formato_en_decada = n_formato_en_decada/ n_debates_en_decada *100)  

tipos_formatos_decada_wide <- tipos_formatos_decada %>% 
  select(-n_formato_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_decada) %>% 
  arrange(decada)# %>% 
  # mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  # mutate(across(starts_with("pr_formato"), ~.*100))  
# Reorder columns

tipos_formatos_decada_wide <- tipos_formatos_decada_wide[, c("decada", "n_debates_en_decada",
                                                         "pr_formatoduelo", "pr_formatolibre",
                                                         "pr_formatoexpositivo", "pr_formatomoderadores",
                                                         "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
                                                         "pr_formatopresentes","pr_formatovirtuales")]

tipos_formatos_decada_wide %>% write_csv("anexos/tipos_formatos_decada_wide.csv")

tipos_formatos_decada_wide_n <- tipos_formatos_decada %>% 
  select(-pr_formato_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = n_formato_en_decada) %>% 
  arrange(decada)# %>% 
# mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
# mutate(across(starts_with("pr_formato"), ~.*100)) 
# se lee: de los debates con datos para XX decada, XXX % incluyen al menos un segmento con XXX formato

# graficos ordinales en el tiempo ###############################

# competencia ###################################

base$ncat_competencia %>% summary()
base$ncat_competencia %>% sd(na.rm=T)

ev_t_ncat_competencia <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_ncat_competencia_year = mean(ncat_competencia, na.rm=T),
            sd_ncat_competencia_year = sd(ncat_competencia, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_competencia_year / sqrt(n),
    CI_Lower = mean_ncat_competencia_year - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_competencia_year + qt(0.975, n - 1) * SE
  )

plot_ev_t_ncat_competencia <- ev_t_ncat_competencia %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, mean_ncat_competencia_year)) +
  geom_line(aes(ncat_eleccion, mean_ncat_competencia_year)) +
  geom_smooth(aes(ncat_eleccion, mean_ncat_competencia_year)) +
  theme_minimal()

plot_ev_t_ncat_competencia1 <- ev_t_ncat_competencia %>% 
  ggplot(aes(x = ncat_eleccion, y = mean_ncat_competencia_year)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2) +
  labs(title = "Evolving Mean with Confidence Intervals",
       x = "Time",
       y = "Mean Value") +
  theme_minimal()

# por decadas

ev_ncat_competencia_decadas <- base %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_ncat_competencia_decadas = mean(ncat_competencia, na.rm=T),
            sd_ncat_competencia_decadas = sd(ncat_competencia, na.rm=T),
            n = n()) 

ev_ncat_competencia_decadas <- ev_ncat_competencia_decadas %>% 
  mutate(
    SE = sd_ncat_competencia_decadas / sqrt(n),
    CI_Lower = mean_ncat_competencia_decadas - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_competencia_decadas + qt(0.975, n - 1) * SE
  )

plot_ev_decadas_ncat_competencia1 <- ev_ncat_competencia_decadas %>% 
  ggplot(aes(x = decada, y = mean_ncat_competencia_decadas)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2) +
  labs(title = "Evolving Mean with Confidence Intervals",
       x = "Time",
       y = "Mean Value") +
  theme_minimal()


# participacion #########################


base$ncat_ppac %>% summary()
base$ncat_ppac %>% sd(na.rm=T)

ev_t_ncat_ppac <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_ncat_ppac_year = mean(ncat_ppac, na.rm=T),
            sd_ncat_ppac_year = sd(ncat_ppac, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_ppac_year / sqrt(n),
    CI_Lower = mean_ncat_ppac_year - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_ppac_year + qt(0.975, n - 1) * SE
  )

plot_ev_t_ncat_ppac <- ev_t_ncat_ppac %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, mean_ncat_ppac_year)) +
  geom_line(aes(ncat_eleccion, mean_ncat_ppac_year)) +
  geom_smooth(aes(ncat_eleccion, mean_ncat_ppac_year)) +
  theme_minimal()

plot_ev_t_ncat_ppac1 <- ev_t_ncat_ppac %>% 
  ggplot(aes(x = ncat_eleccion, y = mean_ncat_ppac_year)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2) +
  labs(title = "Evolving Mean with Confidence Intervals",
       x = "Time",
       y = "Mean Value") +
  theme_minimal()

# por decadas

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

plot_ev_decadas_ncat_ppac1 <- ev_ncat_ppac_decadas %>% 
  ggplot(aes(x = decada, y = mean_ncat_ppac_decadas)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2) +
  labs(title = "Evolving Mean with Confidence Intervals",
       x = "Time",
       y = "Mean Value") +
  theme_minimal()

# graficos ordinales viejos . me gustan mas, pero estaria bueno sobreimprimirles el promedio #### 

plot_formatos_cuanti_decada <- base %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(decada) , 
                   as.numeric(ncat_competencia), fill ="grey")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de competencia",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por decada
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer a los candidatos entre sí.
       A mayor valor, más libre y directos los diálogos entre los candidatos.
       Para más detalle, consultar Codebook Anexo.",
       title = "Nivel de competencia  entre los candidatos",
       subtitle = "por decada")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

plot_formatos_cuanti2_decada <- base %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(decada) , 
                   as.numeric(ncat_ppac)), fill ="grey") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de participación",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por decada
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer al público.
       A mayor valor, mayor presencia del público en los debates. 
       Para más detalle, consultar Codebook Anexo.",
       title = "Nivel de participación del público en los debates",
       subtitle = "por decada")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

#  Temas en el tiempo ############


# cuentas simples #

base %>% subset(dico_temas_puntuales==1) %>% nrow()
base %>% subset(dico_temas_libre==1) %>% nrow()
base %>% subset(dico_temas_libre==1) %>% nrow()/389
base %>% subset(dico_temas_bloques==1) %>% nrow()

base %>% subset(dico_temas_monotema==1) %>% nrow()

# tabla de distribucion por decada, para mostrar ####

tipos_temas_decada <- base_temas %>%  
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  mutate(n_debates_en_decada = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_tipo_tema, decada, n_debates_en_decada) %>% 
  summarise(n_tema_en_decada = n()) %>% 
  ungroup() %>% 
  mutate(pr_tema_en_decada = n_tema_en_decada/ n_debates_en_decada *100)  

tipos_temas_decada_wide <- tipos_temas_decada %>% 
  select(-n_tema_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_tema_en_decada) %>% 
  arrange(decada)# %>% 
# mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
# mutate(across(starts_with("pr_formato"), ~.*100))  

tipos_temas_decada_wide <- tipos_temas_decada_wide[, c("decada", "n_debates_en_decada",
                                                       "pr_temalibre","pr_temabloques",
                                                       "pr_temapuntuales","pr_temamonotema")]

tipos_temas_decada_wide %>% write_csv("anexos/tipos_temas_decada_wide.csv")

tipos_formatos_decada_wide_n <- tipos_temas_decada %>% 
  select(-pr_tema_en_decada) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = n_tema_en_decada) %>% 
  arrange(decada)# %>% 
# mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
# mutate(across(starts_with("pr_formato"), ~.*100)) 
# se lee: de los debates de la decada XX de los que se tiene registro, 
# XX% tenian al menos un segmento con XXX disposicion tematica


# diversidad de formatos en el tiempo #####

diversidad_formato <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_ncatformatos = mean(n_catformatos, na.rm = TRUE),
            n_debates = n(),
            sum_catformatos = sum(n_catformatos, na.rm = TRUE))


diversidad_formato_decada <- diversidad_formato %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(mean_ncatformatos_decada = mean(mean_ncatformatos, na.rm = TRUE),
            n_debates_decada = sum(n_debates),
            sum_catformatos_decada = sum(sum_catformatos, na.rm = TRUE))
  
# Patrones de interaccion por paises ########

tipos_formatos_pais <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_en_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE),
            n_formato_en_pais = n()) %>% 
  mutate(pr_formato_en_pais = n_formato_en_pais/ n_debates_en_pais )

# tabla de distribucion por año, para calcular ###############

tipos_formatos_pais_wide <- tipos_formatos_pais %>% 
  select(-c(n_peso_formato_xdebate, n_formato_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = pr_formato_en_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_formato"), ~.*100)) %>% 
  arrange(n_debates_en_pais)

# Reorder columns
tipos_formatos_pais_wide <- tipos_formatos_pais_wide[, c("cat_pais", "n_debates_en_pais",
                                                         "pr_formatoduelo", "pr_formatolibre",
                                                         "pr_formatoexpositivo", "pr_formatomoderadores",
                                                         "pr_formatoperiodistas","pr_formatoexpertos","pr_formatosectores",
                                                         "pr_formatopresentes","pr_formatovirtuales")]

tipos_formatos_pais_wide %>% write_csv("anexos/tipos_formatos_pais_wide.csv")

tipos_formatos_pais_wide_n <- tipos_formatos_pais %>% 
  select(-c(n_peso_formato_xdebate, pr_formato_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_formato, values_from = n_formato_en_pais) %>% 
  mutate(across(starts_with("pr_formato"), ~ifelse(is.na(.),0,.)))# %>% 
 # mutate(across(starts_with("pr_formato"), ~.*100))  

# competencia por pais ###################################

base$ncat_competencia %>% summary()
base$ncat_competencia %>% sd(na.rm=T)

ncat_competencia_por_pais <- base %>% 
  group_by(cat_pais) %>% 
  summarise(mean_ncat_competencia_pais = mean(ncat_competencia, na.rm=T),
            sd_ncat_competencia_pais = sd(ncat_competencia, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_competencia_pais / sqrt(n),
    CI_Lower = mean_ncat_competencia_pais - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_competencia_pais + qt(0.975, n - 1) * SE
  )

plot_ncat_competencia_por_pais1 <- ncat_competencia_por_pais %>% 
  ggplot(aes(x = cat_pais, y = mean_ncat_competencia_pais)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Mean Values with Confidence Intervals by Country",
       x = "Country",
       y = "Mean Value") +
  theme_minimal()

# participacion por pais #########################

ncat_ppac_por_pais <- base %>% 
  group_by(cat_pais) %>% 
  summarise(mean_ncat_ppac_pais = mean(ncat_ppac, na.rm=T),
            sd_ncat_ppac_pais = sd(ncat_ppac, na.rm=T),
            n = n()) %>% 
  mutate(
    SE = sd_ncat_ppac_pais / sqrt(n),
    CI_Lower = mean_ncat_ppac_pais - qt(0.975, n - 1) * SE,
    CI_Upper = mean_ncat_ppac_pais + qt(0.975, n - 1) * SE
  )


plot_ncat_ppac_por_pais1 <- ncat_ppac_por_pais %>% 
  ggplot(aes(x = cat_pais, y = mean_ncat_ppac_pais)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +
  labs(title = "Mean Values with Confidence Intervals by Country",
       x = "Country",
       y = "Mean Value") +
  theme_minimal()

# graficos ordinales viejos . me gustan mas, pero estaria bueno sobreimprimirles el promedio #### 

plot_formatos_cuanti <- base %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais) 
                   # %>% 
                   #   fct_relevel(
                   #     "Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                   #      "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                   #      "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
                   , 
                   as.numeric(ncat_competencia), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de competencia",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por país.
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer a los candidatos entre sí.
       A mayor valor, más libre y directos los diálogos entre los candidatos.
       Para más detalle, consultar Codebook Anexo.
       
       *Rep. Dom. = República Dominicana.",
       title = "Nivel de competencia  entre los candidatos",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

plot_formatos_cuanti2 <- base %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais) 
                   # %>% 
                   #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                   #               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                   #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
                   , 
                   as.numeric(ncat_ppac), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de participación",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por país.
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer al público.
       A mayor valor, mayor presencia del público en los debates. 
       Para más detalle, consultar Codebook Anexo.
       
       *Rep. Dom. = República Dominicana.",
       title = "Nivel de participación del público en los debates",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# distribucion tematica por paises ########

# para calcular tabla

tipos_temas_pais <- base_temas %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_en_pais = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  group_by(cat_pais, n_debates_en_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE),
            n_tema_en_pais = n()) %>% 
  mutate(pr_tema_en_pais = n_tema_en_pais/ n_debates_en_pais )

# tabla de distribucion por pais, para calcular

tipos_temas_pais_wide <- tipos_temas_pais %>% 
  select(-c(n_peso_tema_xdebate, n_tema_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = pr_tema_en_pais) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.))) %>% 
  mutate(across(starts_with("pr_tema"), ~.*100)) %>% 
  arrange(n_debates_en_pais)

tipos_temas_pais_wide_n <- tipos_temas_pais %>% 
  select(-c(n_peso_tema_xdebate, pr_tema_en_pais)) %>% 
  pivot_wider(names_from=cat_tipo_tema, values_from = n_tema_en_pais) %>% 
  mutate(across(starts_with("pr_tema"), ~ifelse(is.na(.),0,.)))# %>% 
# mutate(across(starts_with("pr_formato"), ~.*100))  

tipos_temas_pais_wide <- tipos_temas_pais_wide[, c("cat_pais", "n_debates_en_pais",
                                                       "pr_temalibre","pr_temabloques",
                                                       "pr_temapuntuales","pr_temamonotema")]

tipos_temas_pais_wide %>% write_csv("anexos/tipos_temas_pais_wide.csv")

# n candidatos, nueva variable tematica ###############################

hist(base$n_invitados)
summary(base$n_invitados)
max(base$n_invitados, na.rm = T)
min(base$n_invitados, na.rm = T)

#PENDIENTE: REVISAR MISSING DATA: #

missing_n_invitados <- base %>% subset(is.na(n_invitados))
# aclarar que cuenta considera PRESENCIAS Y AUSENCIAS, para armar invitados
# tb podemos repe para presentes / ausentes

base_n_candidatos_año_pais <- base %>% 
  subset(n_invitados!=42) %>%  # descartamos un outlier
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n_distinct(id_debate),
            mean_n_invitados = mean(n_invitados, na.rm = T),
            mean_n_ausentes = mean(n_ausentes, na.rm=T),
            mean_n_presentes = mean(n_presentes, na.rm=T))

# base_n_candidatos_año_pais <- base_anual_full %>% 
#   left_join(base_n_candidatos_año_pais) %>% 
#   mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))
# en realidad da igual esta alternativa y la que sigue si no llenamos ni consideramos los missing

base_n_candidatos_año_pais <- base_n_candidatos_año_pais  %>% 
  left_join(base_anual_full) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

base_n_candidatos_año_pais <- base_anual_full  %>% 
  left_join(base_n_candidatos_año_pais) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

ev_t_n_candidatos <- base_n_candidatos_año_pais %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T))

ev_t_n_candidatos_decada <- ev_t_n_candidatos %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(n_debates_decada = sum(n_debates_año),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T))
  
ev_e_n_candidatos <- base_n_candidatos_año_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_pais = sum(n_debates_año_pais),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T)) %>% 
  arrange(n_debates_pais)


plot_ev_t_n_candidatos <- ev_t_n_candidatos %>% 
  select(ncat_eleccion, mean_n_invitados, mean_n_presentes) %>% 
  pivot_longer( cols = c(mean_n_invitados, mean_n_presentes) , 
                names_to = "categoria_asistencia", values_to =  "mean_n") %>% 
  ggplot() +
  geom_line(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  theme_minimal()


# REPITO PERO SACANDO BALLOTAGES

missing_n_invitados <- base %>% subset(is.na(n_invitados))
# aclarar que cuenta considera PRESENCIAS Y AUSENCIAS, para armar invitados
# tb podemos repe para presentes / ausentes

base_n_candidatos_año_pais_solo1 <- base %>% 
  subset(n_invitados!=42) %>%  # descartamos un outlier
  subset(ncat_ronda==1) %>%  # descartamos ballotages
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n_distinct(id_debate),
            mean_n_invitados = mean(n_invitados, na.rm = T),
            mean_n_ausentes = mean(n_ausentes, na.rm=T),
            mean_n_presentes = mean(n_presentes, na.rm=T))

# base_n_candidatos_año_pais_solo1 <- base_anual_full %>% 
#   left_join(base_n_candidatos_año_pais_solo1) %>% 
#   mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

base_n_candidatos_año_pais_solo1 <- base_n_candidatos_año_pais_solo1  %>% 
  left_join(base_anual_full) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

ev_t_n_candidatos_solo1 <- base_n_candidatos_año_pais_solo1 %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T))

ev_t_n_candidatos_decada_solo1 <- ev_t_n_candidatos_solo1 %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(n_debates_decada = sum(n_debates_año),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T))

ev_e_n_candidatos_solo1 <- base_n_candidatos_año_pais_solo1 %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_pais = sum(n_debates_año_pais),
            mean_n_invitados = mean(mean_n_invitados, na.rm = T),
            mean_n_ausentes = mean(mean_n_ausentes, na.rm=T),
            mean_n_presentes = mean(mean_n_presentes, na.rm=T)) %>% 
  arrange(mean_n_invitados)


plot_ev_t_n_candidatos_solo1 <- ev_t_n_candidatos_solo1 %>% 
  select(ncat_eleccion, mean_n_invitados, mean_n_presentes) %>% 
  pivot_longer( cols = c(mean_n_invitados, mean_n_presentes) , 
                names_to = "categoria_asistencia", values_to =  "mean_n") %>% 
  ggplot() +
  geom_line(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  theme_minimal()


# considerando N CANDIDATURAS, importante chequear data

missing_n_candidaturas <- base %>% subset(is.na(n_candidaturas))
# son casos para los que de todas formas no tenemos datos de invitados
# igual pendiente chequear la calidad del dato si conseguimos data

base_prop_candidatos_año_pais <- base %>% 
  subset(n_invitados!=42) %>%  # descartamos un outlier
 # subset(ncat_ronda==1) %>%  # descartamos ballotages
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n_distinct(id_debate),
            n_candidaturas_año_pais = mean(n_candidaturas, na.rm=T),
            mean_n_invitados = mean(n_invitados, na.rm = T),
            prop_invitados = mean_n_invitados / n_candidaturas_año_pais,
            mean_n_presentes = mean(n_presentes, na.rm=T),
            prop_presentes = mean_n_presentes / n_candidaturas_año_pais)

base_prop_candidatos_año_pais <- base_prop_candidatos_año_pais  %>% 
  left_join(base_anual_full) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

ev_t_prop_candidaturas <- base_prop_candidatos_año_pais %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_t_prop_candidaturas_decada <- base_prop_candidatos_año_pais %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(n_debates_decada = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_t_prop_candidaturas <- base_prop_candidatos_año_pais %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_e_prop_candidaturas <- base_prop_candidatos_año_pais %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_año_pais = sum(n_debates_año_pais),
            mean_candidaturas = mean(n_candidaturas_año_pais, na.rm = T),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T)) %>% 
  arrange(n_debates_año_pais)


plot_ev_t_ev_t_prop_candidaturas <- ev_t_prop_candidaturas %>% 
  select(ncat_eleccion, mean_prop_invitados, mean_prop_presentes) %>% 
  pivot_longer( cols = c(mean_prop_invitados, mean_prop_presentes) , 
                names_to = "categoria_asistencia", values_to =  "mean_prop") %>% 
  ggplot() +
  geom_line(aes(ncat_eleccion, mean_prop, colour = categoria_asistencia)) +
  #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  theme_minimal()

plot_ev_t_candidaturas <- ev_t_candidaturas %>% 
  select(ncat_eleccion, mean_candidaturas, mean_invitados, mean_presentes) %>% 
  pivot_longer( cols = c(mean_candidaturas, mean_invitados, mean_presentes) , 
                names_to = "categoria_asistencia", values_to =  "mean") %>% 
  ggplot() +
  geom_line(aes(ncat_eleccion, mean, colour = categoria_asistencia)) +
  #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  theme_minimal()

# chequeo data que parece estar mal

check <- base %>% 
  subset(n_candidaturas < n_invitados) %>% 
  select(id_debate, cat_pais, ncat_eleccion, ncat_ronda, 
         str_organizador, n_candidaturas, n_invitados, str_presentes)



# considerando N CANDIDATURAS, importante chequear data

 
base_prop_candidatos_año_pais1 <- base %>% 
  subset(n_invitados!=42) %>%  # descartamos un outlier
  subset(ncat_ronda==1) %>%  # descartamos ballotages
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n_distinct(id_debate),
            n_candidaturas_año_pais = mean(n_candidaturas, na.rm=T),
            mean_n_invitados = mean(n_invitados, na.rm = T),
            prop_invitados = mean_n_invitados / n_candidaturas_año_pais,
            mean_n_presentes = mean(n_presentes, na.rm=T),
            prop_presentes = mean_n_presentes / n_candidaturas_año_pais)

base_prop_candidatos_año_pais1 <- base_prop_candidatos_año_pais1  %>% 
  left_join(base_anual_full) %>% 
  mutate(n_debates_año_pais = ifelse(is.na(n_debates_año_pais),0,n_debates_año_pais))

ev_t_prop_candidaturas1 <- base_prop_candidatos_año_pais1 %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_t_prop_candidaturas_decada1 <- base_prop_candidatos_año_pais1 %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>% 
  summarise(n_debates_decada = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_t_prop_candidaturas1 <- base_prop_candidatos_año_pais1 %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T))

ev_e_prop_candidaturas1 <- base_prop_candidatos_año_pais1 %>% 
  group_by(cat_pais) %>% 
  summarise(n_debates_año = sum(n_debates_año_pais),
            mean_candidaturas = mean(n_candidaturas_año_pais, na.rm = T),
            mean_prop_invitados = mean(prop_invitados, na.rm = T),
            mean_prop_presentes = mean(prop_presentes, na.rm = T)) %>% 
  arrange(n_debates_año)


plot_ev_t_ev_t_prop_candidaturas1 <- ev_t_prop_candidaturas1 %>% 
  select(ncat_eleccion, mean_prop_invitados, mean_prop_presentes) %>% 
  pivot_longer( cols = c(mean_prop_invitados, mean_prop_presentes) , 
                names_to = "categoria_asistencia", values_to =  "mean_prop") %>% 
  ggplot() +
  geom_line(aes(ncat_eleccion, mean_prop, colour = categoria_asistencia)) +
  #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
  theme_minimal()

# plot_ev_t_candidaturas1 <- ev_t_prop_candidaturas1 %>% 
#   select(ncat_eleccion, mean_candidaturas, mean_invitados, mean_presentes) %>% 
#   pivot_longer( cols = c(mean_candidaturas, mean_invitados, mean_presentes) , 
#                 names_to = "categoria_asistencia", values_to =  "mean") %>% 
#   ggplot() +
#   geom_line(aes(ncat_eleccion, mean, colour = categoria_asistencia)) +
#   #geom_smooth(aes(ncat_eleccion, mean_n, colour = categoria_asistencia)) +
#   theme_minimal()


plot_n_invitados_pais <- base %>% 
  subset(n_invitados!=42) %>%
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais),  as.numeric(n_invitados), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Cantidad de candidatos invitados",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.

       *Rep. Dom. = República Dominicana.",
       title = "N invitados",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


plot_prop_invitados_pais <- base %>% 
  subset(n_invitados!=42) %>%
  left_join(base_anual_full) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais),  as.numeric(n_invitados)/n_candidaturas, fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Proporción de candidatos invitados",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.

       *Rep. Dom. = República Dominicana.",
       title = "Prop invitados",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)
 
######################################################################
######################################################################
# CRUCES ENTRE VARIABLES ###############

# formatos por organizadores

matriz_cor_patrones_interaccion <-  base %>% 
  left_join(base_anual_full %>% 
              group_by(cat_pais) %>%
              arrange(ncat_eleccion, .by_group=T) %>% 
              mutate(lagged_n_elecs_debates = cumsum(dico_hubo_debates))) %>% 
  mutate(prop_invitados = n_invitados/n_candidaturas) %>%  
  select(ncat_eleccion,
         ncat_ronda,
         lagged_n_elecs_debates,
         prop_invitados,
         n_presentes,
         n_ausentes,
         dico_org_mmc,
         dico_org_mmp,
         dico_org_estado,
         dico_org_osc,
         dico_org_educ,
         dico_formato_libre,
         dico_formato_duelo,
         dico_formato_expositivo,
         dico_formato_moderadores,
         dico_formato_periodistas,
         dico_formato_expertos,
         dico_formato_sectores,
         dico_formato_presentes,
         dico_formato_virtuales)

data_clean <- na.omit(matriz_cor_patrones_interaccion)
cor_matrix <- cor(data_clean)
# Visualize the correlation matrix
corrplot::corrplot(cor_matrix, method = "square", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

matriz_cor_temas <-  base %>% 
  left_join(base_anual_full %>% 
              group_by(cat_pais) %>%
              arrange(ncat_eleccion, .by_group=T) %>% 
              mutate(lagged_n_elecs_debates = cumsum(dico_hubo_debates))) %>% 
  mutate(prop_invitados = n_invitados/n_candidaturas) %>% 
  select(ncat_eleccion,
         ncat_ronda,
         lagged_n_elecs_debates,
         prop_invitados,
         n_presentes,
         n_ausentes,
         dico_org_mmc,
         dico_org_mmp,
         dico_org_estado,
         dico_org_osc,
         dico_org_educ,
         dico_temas_libre,
         dico_temas_bloques,
         dico_temas_puntuales,
         dico_temas_monotema)

data_clean <- na.omit(matriz_cor_temas)
cor_matrix <- cor(data_clean)
# Visualize the correlation matrix
corrplot::corrplot(cor_matrix, method = "square", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


matriz_cor_todo <- base %>% 
  left_join(base_anual_full %>% 
              group_by(cat_pais) %>%
              arrange(ncat_eleccion, .by_group=T) %>% 
              mutate(lagged_n_elecs_debates = cumsum(dico_hubo_debates))) %>% 
  mutate(prop_invitados = n_invitados/n_candidaturas) %>% 
  ungroup() %>% 
  select(ncat_eleccion,
         ncat_ronda,
         prop_invitados,
         lagged_n_elecs_debates,
         # n_presentes,
         # n_ausentes,
         # dico_org_mmc,
         # dico_org_mmp,
         # dico_org_estado,
         # dico_org_osc,
         # dico_org_educ,
         dico_temas_libre,
         dico_temas_bloques,
         dico_temas_puntuales,
         dico_temas_monotema,
         dico_formato_libre,
         dico_formato_duelo,
         dico_formato_expositivo,
         dico_formato_moderadores,
         dico_formato_periodistas,
         dico_formato_expertos,
         dico_formato_sectores,
         dico_formato_presentes,
         dico_formato_virtuales)

data_clean <- na.omit(matriz_cor_todo)
cor_matrix <- cor(data_clean)
corrplot::corrplot(cor_matrix, method = "square", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


######################################################################
######################################################################
######################################################################
######################################################################
###### APUNTES VIEJOS DE POTENCIAL UTILIDAD ##############
######################################################################
######################################################################

# CAPITULO 1 GR DE PENETRACION ###############

# grafico viejo de evolucion temporal #####


debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n()) 

colores <- base %>%
  distinct(cat_pais, cols_18) 

base_años <- elecciones %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais),
          n_debates_año_pais = replace_na(n_debates_año_pais, 0) ) %>% 
  left_join(colores)

plot_point_anual <- base_años %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot()  +
  geom_point(aes(ncat_eleccion,as.factor(cat_pais) 
                 # %>% 
                 #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                 #               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                 #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
                 , 
                 size= n_debates_año_pais, colour = cols_18, shape = debates_dico, alpha= debates_dico)) + 
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  #theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) +  #,
  # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  labs(x = "", y = "",
       title = "Debates a través del tiempo",
       subtitle = "Cuántos y cuándo se hicieron",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
      
       El tamaño de los círculos representa la cantidad de debates hechos en una elección.
       Las x representan elecciones sin debates")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# grafico de n debates versus index ausentes #####

# quiero revisar la cantidad de debates con alguna ausencia
# en relacion a la cantidad de debates por eleccion

base_ausencias_primera <- base %>% 
  filter(ncat_ronda== 1) %>% 
  group_by(ncat_eleccion, cat_pais, cols_18) %>% 
  summarise(cantidad_debates_ronda = n(),
            cantidad_debates_ausencias = sum(dico_ausencias, na.rm = TRUE),
            proporcion_debates_ausencias = cantidad_debates_ausencias/cantidad_debates_ronda,
            suma_indexausentes = sum(n_indexausentes, na.rm = TRUE),
            mean_indexausentes = mean(n_indexausentes, na.rm = TRUE),
            sd_indexausentes = sd(n_indexausentes, na.rm = TRUE))

# ANEXO mean versus sd ausencias

plotausencias_sd_versus_mean <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Desvío estándar en el índice de ausencias", y = "Promedio en el índice de ausencias",
       title = "Índice de ausencias ",
       subtitle = "promedio versus desvío estándar",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación
       Calculado para los debates realizados para la primera ronda.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# ANEXO proporcion de debates con ausencias versus proprocion de debates realizados , para el anexo

plotausencias_debates_versus_proporcion <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Proporción de debates con ausencias", y = "N debates primera ronda",
       title = "Debates con ausencias ",
       subtitle = "en relación al total de debates realizados para
       para la primera ronda",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# cantidad de debates versus promedio indice ausencias ESTE SI

plotausencias_anuales_meanindex <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = cat_pais, vjust = 0), alpha = 0.6) +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = ncat_eleccion, vjust = 1), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Índice de ausencias", y = "n debates primera ronda",
       title = "Promedio en el índice de ausencias",
       subtitle = "Por elección, en relación al los debates realizados
       para la primera ronda",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El índice de ausencias multiplica la proporcion de candidatos que faltaron a un debate, 
       por el porcentaje de votos que estos obtuvieron en las elecciones.
       
       El gráfico expone promedios por año-país, para los debates realizados antes de la primera vuelta electoral
       (antes del ballotage, por definición, no puede haber debates con ausencias).")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# summary de index ausentes ########

summary(base$n_indexausentes)
cuenta <- base %>%  
  select(str_ausentes, n_ausentes,
         n_indexausentes, 
         n_proporcionausentes,
         n_porcentajeausentes 
  ) %>% 
  filter(is.na(n_indexausentes))

# CAPITULO 2 TIPO DE ORGANIZADOR #########################

# grafico de alianzas en el tiempo #########
plot_alianzas_cuenta <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_variedadorgs)) +
  geom_smooth(alpha = 0.1, colour= "khaki") +
  geom_point(alpha = 0.1) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,15,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año de elección", y = "n tipos de orgs.**")


plot_alianzas_cuenta1 <-ggExtra::ggMarginal(plot_alianzas_cuenta,
                                            type="histogram",
                                            margins = "y",               # specify histograms
                                            fill = "lightblue",    # other parameters for x-axis marginal
                                            yparams = list(binwidth = 1))     # other parameters for y-axis marginal) 

plot_alianzas_cuenta2 <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_orgs) ) +
  geom_smooth(alpha = 0.1, colour= "khaki") +
  geom_point(alpha = 0.1 #aes(alpha = as.numeric(ncat_eleccion))
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,15,3)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "", y = "n organizadores*")


plot_alianzas_cuenta21 <-ggExtra::ggMarginal(plot_alianzas_cuenta2,
                                             type="histogram",               # specify histograms
                                             margins = "y",
                                             fill = "lightblue",               # bar fill
                                             xparams = list(binwidth = 4),    # other parameters for x-axis marginal
                                             yparams = list(binwidth = 1))     # other parameters for y-axis marginal) 

#patchwork_alianzas <-  patchwork::wrap_plots(plot_alianzas_cuenta21, plot_alianzas_cuenta1) + patchwork::plot_layout(ncol=1)
patchwork_alianzas <-  patchwork::wrap_plots(plot_alianzas_cuenta2, plot_alianzas_cuenta) + 
  patchwork::plot_layout(ncol=1) + 
  patchwork::plot_annotation(
    title = 'Cantidad y variedad de organizadores',
    subtitle = 'por debate, a lo largo del tiempo',
    caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       Los datos no diferencian entre países. Todos los casos estudiados fueron tenidos en consideración:  
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString(), "
       
       Cada círculo representa un debate. 
       Los círculos fueron graficados con cierta transparencia para visualizar las superposiciones entre ellos 
       (que ocurren cuando en una elección se realizaron varios debates con la misma cantidad o diversidad de organizadores).
       
       La línea amarilla grafica la fórmula y ~ x.
       
       *n organizadores = cantidad absoluta de entidades que organizan de manera conjunta un encuentro.
       **n tipos de orgs. = cantidad de tipos de organizadores diferentes que participan de la organización de un encuentro."
    ))  + 
  patchwork::plot_annotation(tag_levels = c('1'), tag_prefix = 'Fig. ', tag_sep = '.', 
                             tag_suffix = ':') & 
  theme(#plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 9, hjust = -0.5, vjust = -0.5))

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# otra prueba, cumulativa

alianzas_cuenta_anual_cantidad <- alianzas_cuenta %>% 
  mutate(n_orgs = as.factor(n_orgs)) %>% 
  group_by(ncat_eleccion, n_orgs) %>% 
  summarise(cantidad = n()) 

plot_alianzas_cuenta_anual_cantidad <- alianzas_cuenta_anual_cantidad %>% 
  ggplot() +
  geom_col(aes(ncat_eleccion, cantidad, alpha=n_orgs))  +
  theme_minimal() +
  #  scale_y_continuous(breaks = seq(0,30,3)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "", y = "",
       title = "",
       subtitle = "",
       caption = "")


alianzas_cuenta_anual_variedad <- alianzas_cuenta %>% 
  mutate(n_orgs = as.factor(n_variedadorgs)) %>% 
  group_by(ncat_eleccion, n_variedadorgs) %>% 
  summarise(cantidad = n()) 

plot_alianzas_cuenta_anual_variedad <- alianzas_cuenta_anual_variedad %>% 
  ggplot() +
  geom_col(aes(ncat_eleccion, cantidad, alpha=n_variedadorgs))  +
  theme_minimal() +
  # scale_y_continuous(breaks = seq(0,30,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año de elección", y = "Cantidad de debates",
       caption = "",
       title = "")


patchwork_alianzas_cum <-  patchwork::wrap_plots(plot_alianzas_cuenta_anual_cantidad, plot_alianzas_cuenta_anual_variedad ) + 
  patchwork::plot_layout(ncol=1) + 
  patchwork::plot_annotation(
    title = 'Cantidad y variedad de organizadores',
    subtitle = 'por debate, a lo largo del tiempo',
    caption = 'Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       *n organizadores = cantidad absoluta de entidades que organizan de manera conjunta un encuentro.
       **n tipos de organizadores = cantidad de tipos de organizadores diferentes que participan de la organización de un encuentro.'
  )

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# otra prueba, no use

alianzas_cuenta_promedio <- alianzas_cuenta %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_orgs = mean(n_orgs),
            n_variedadorgs = mean(n_variedadorgs),
            n_variedadsubtipos = mean(n_variedadsubtipos))

plot_alianzas_cuenta_promedio  <- alianzas_cuenta_promedio  %>% 
  ggplot(aes(ncat_eleccion, n_variedadorgs, size= n_orgs)) +
  geom_point() +
  theme_minimal() +
  #scale_y_continuous(breaks = seq(0,15,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "n tipos de organizadores",
       title = "Cantidad y variedad de organizadores",
       subtitle = "en promedio por año",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       El tamaño de los círculos representa la cantidad de organizadores promedio.
       El eje y, la variedad, es decir, los tipos de organizadores promedio.
       Los valores no discriminan entre países.
       Téngase en cuenta que la cantidad de debates total aumenta a lo largo del tiempo.")

# grafico original de distribucion temporal  ########

# preparamos datos. Calcularemos la "proporción" de organizadores que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de organizador sobre el total de diferentes tipos de organiadores por debates
# NO calcula esta proporción con base en el carácter de cada una de las organizaciones que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate
# esta base agrupada me sirve para comparar con la version vieja, eventualmente
base_tipos_grouped <- base_organizadores %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  summarise(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
            n_organizadores = n())

#base no agrupada para hacer cuentas y graficos
base_tipos_ungrouped <- base_organizadores %>% 
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

# me gusta este para comparar dos etapas
tipos_organizadores_ev_anualv2 <- base_tipos_countryear %>% 
  mutate(cat_tipoorgv2 = ifelse(is.na(cat_tipoorgv2)|cat_tipoorgv2=="NA", "S/ Datos", cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, 
             as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos"), 
             colour = cat_tipoorgv2, 
             size= n_prppacorg,
             shape= as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos")))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = -65),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_shape_manual(values=c("S/ Datos" = 4, 
                              "estado" = 19,
                              "mmp" = 19,
                              "educ" = 19,
                              "mmc" = 19,
                              "osc" = 19)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  scale_colour_manual(breaks = c("S/ Datos", "estado", "mmp","osc", "educ", "mmc"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "Estado","Medios
                               Públicos","OSCs","Educativo","Medios 
                               Comerciales", "Sin Datos")) +
  labs(x = "Año",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, para la región en su conjunto",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado.
       Si un debate fue organizado por más de un tipo de organizador, se lo contabiliza dos veces.
                       
                       La cuenta contempla a todos los casos bajo estudio: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# evolucion en el tiempo y por pais, me gusta para algun anexo ######

# 
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
  theme_minimal() +
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
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# CAPITULO 3 FORMATOS #############################


coloresformato <- base_formatos %>% 
  distinct(cat_tipo_formato, colores_formato)

length(base$id_debate)-length(unique(base_temas$id_debate))

# graficos de distribucion simple originales ###############

cuenta_temas_2 <-base_temas %>%
  group_by(cat_tipo_tema) %>% 
  summarise(n_tema = n()) %>% 
  mutate(pr_debates_con_tema = n_tema / length(base$id_debate) * 100)

plot_cuenta_temas_2 <- cuenta_temas_2 %>% 
  # na.omit() %>% 
  arrange(n_tema) %>% 
  mutate(order = row_number()) %>% 
  mutate(cat_tipo_tema = fct_reorder(cat_tipo_tema, order)) %>% 
  ggplot() +
  geom_col(aes(cat_tipo_tema, n_tema, fill = cat_tipo_tema)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "Monotemático",
    "Libre",
    "En bloques",
    "Preguntas puntuales"
  )) +
  labs(y = "n tema*", x = "Tipo de estructuración temática",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un formato temático-debate.
       Si un debate se realizó conforme a más de un patrón temático, se lo contabiliza dos veces.
       Hay 57 debates para los que no disponemos de información.
                       
                              La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ),
       title = "Tipos de estructuración temática de los debates",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# para formatos


length(base$id_debate)-length(unique(base_formatos$id_debate))

cuenta_formatos_2 <-base_formatos %>%
  group_by(cat_tipo_formato) %>% 
  summarise(n_formato = n()) %>% 
  mutate(pr_debates_con_formato = n_formato / length(base$id_debate) * 100)

plot_cuenta_formatos_2 <- cuenta_formatos_2 %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  # na.omit() %>% 
  arrange(n_formato) %>% 
  mutate(order = row_number()) %>% 
  mutate(cat_tipo_formato = fct_reorder(cat_tipo_formato, order)) %>% 
  ggplot() +
  geom_col(aes(cat_tipo_formato, n_formato, fill = cat_tipo_formato)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_fill_manual(breaks= coloresformato$cat_tipo_formato,
                    values=coloresformato$colores_formato) +
  scale_x_discrete(labels = c("Público-presente",
                              "Expositivo",
                              "Público-virtual",
                              "Panel-Expertos",
                              "Panel-Sectores", 
                              "Libre", 
                              "Periodistas", 
                              "Duelo",
                              "Moderadores"
  )) +
  labs(y = "n tema*", x = "Tipo de esquema de interacción",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un esquema de interacción-debate.
       Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
       Hay 51 debates para los que no disponemos de información.
       
              La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ),
       
       title = "Tipos de esquemas de interacción de los debates",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# grafico original evolucion temporal ##################

# para formatos #

tipos_formatos_ev_anual <-  tipos_formatos_año %>% 
  ggplot(aes(ncat_eleccion, fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                                        "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", 
                                        "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
             colour = cat_tipo_formato, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  scale_color_manual(breaks= coloresformato$cat_tipo_formato,
                     values=coloresformato$colores_formato) +
  scale_y_discrete(labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Expositivo", "Duelo", "Libre")) +
  labs(x = "Año",
       y = "Tipo de intercambio",
       title = "Patrones de interacción de los debates ",
       subtitle = "Formatos preferidos a través el tiempo, para la región en su conjunto",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con cada patrón de interacción en un año dado.
              Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
              
              La cuenta contempla a todos los casos bajo estudio: 
         ", colorespais$cat_pais[1:10] %>% toString(), "
       ", colorespais$cat_pais[11:18] %>% toString()))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# # para temas, evolucion temporal original  #
# grafico original 

tipos_temas_año <- base_temas %>% 
  group_by(ncat_eleccion, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

tipos_temas_ev_anual <- tipos_temas_año %>% 
  ggplot(aes(ncat_eleccion, cat_tipo_tema, 
             colour = cat_tipo_tema, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  scale_x_continuous(breaks = seq(1955,2025,5))  +
  scale_y_discrete(labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
  labs(x = "Año",
       y = "Tipo de esquema temático",
       title = "Estructuración temática de los debates",
       subtitle = "A través el tiempo",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
      
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con el esquema temático respectivo en un año dado. 
              Si un debate se realizó conforme a más de un esquema temático, se lo contabiliza dos veces.
              
              La cuenta contempla a todos los casos bajo estudio: 
         ", colorespais$cat_pais[1:10] %>% toString(), "
       ", colorespais$cat_pais[11:18] %>% toString()))   

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# por pais , grafico original ######

tipos_formatos_pais <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  #fct_collapse(cat_tipo_formato,
  #             pr_formatomoderadores = "pr_formatomoderadores",
  #                                             "pr_formatoapertura") %>% 
  group_by(cat_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )

plottipos_formatos_pais <- tipos_formatos_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(as.factor(cat_pais) 
             # %>% 
             #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
             #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
             #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
             ,
             #  fct_relevel(
             #    "Brasil", "Colombia", "Costa Rica", "Mexico", "Peru", "Chile",
             #    "Guatemala", "Panama", "Ecuador", "Uruguay", "Argentina", "Bolivia", 
             #    "Paraguay", "Honduras", "El Salvador", "Nicaragua", "Rep. Dom.",  "Venezuela"
             #  ), 
             fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                         "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", 
                         "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
             colour = cat_pais, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_y_discrete(labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Expositivo", "Duelo", "Libre")) +
  labs(x = "País",
       y = "Tipo de intercambio promovido",
       title = "Patrones de interacción de los debates ",
       subtitle = "Formatos preferidos por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con cada patrón de interacción en un año dado.
       Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
      
        *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# grafico original de temas por pais

tipos_temas_pais <- base_temas %>% 
  group_by(cat_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

plottipos_tema_pais <- tipos_temas_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(as.factor(cat_pais)
             # %>% 
             #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
             #               "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
             #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
             , cat_tipo_tema, 
             colour = cat_pais, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_y_discrete(labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
  labs(y = "Tipo de esquema temático",
       x = "País",
       title = "Estructuración temática de los debates ",
       subtitle = "Formatos preferidos por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
        El tamaño de los círculos es proporcional a la cantidad de debates hechos con el esquema temático respectivo en un año dado. 
              Si un debate se realizó conforme a más de un esquema temático, se lo contabiliza dos veces.
      
        *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)




# cuentas de cruces ####

base_tipoorganizadores_linea <- base %>% 
  select(c(id_debate, str_organizador,
           starts_with("n_cattipo")))

base_participativos <- base_formatos %>% 
  filter(cat_tipo_formato %in% c("pr_formatopresentes", 
                                 "pr_formatovirtuales",
                                 "pr_formatosectores")) %>% 
  left_join(base_tipoorganizadores_linea)

base_participativos_tabla <- base_participativos %>% 
  group_by(cat_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_estado, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_mmp, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_mmc, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

base_monotematicos <- base_temas %>% 
  filter(cat_tipo_tema %in% c("pr_temamonotema")) %>% 
  left_join(base_tipoorganizadores_linea)

base_monotematicos_tabla <- base_monotematicos  %>% 
  group_by(cat_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_estado, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_mmp, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_mmc, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

#  Diversidad de formatos en el tiempo. para el anexo  ######

# preferido
plot_diversidad_formatos2 <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_ncatformatos = mean(n_catformatos, na.rm = TRUE),
            n_debates = n(),
            sum_catformatos = sum(n_catformatos, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point((aes(ncat_eleccion,
                  mean_ncatformatos,
                  size= n_debates,
                  #size= sum_catformatos
  ))) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,2.5,0.5)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Variedad de formatos promedio",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       Promedio de esquemas de intercambio dentro de un mismo debate,
       calculado anualmente, sin discriminar entre países.
       Téngase en cuenta que en elecciones más recientes se realizan más debates,
       medida representada en el tamaño de los círculos respectivos.",
       title = "Variedad de formatos en el tiempo",
       subtitle = "promedio por año")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# otra opcion

plot_variedad_formatos4 <- base%>% 
  ggplot(aes(ncat_eleccion, n_catformatos) ) +
  geom_point(alpha = 0.1 #aes(alpha = as.numeric(ncat_eleccion))
  ) +
  geom_smooth(alpha = 0.1, colour= "khaki" ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,5,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año", y = "n formatos*",
       title = "Variedad de formatos",
       subtitle = "por debate, a lo largo del tiempo",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       Los datos no diferencian entre países.
       Todos los casos estudiados fueron tenidos en consideración:  
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString(), "
       
       Cada círculo representa un debate. 
       Los círculos fueron graficados con cierta transparencia para visualizar las superposiciones entre ellos 
       (estas ocurren cuando en una elección se realizaron varios debates con la misma cantidad de formatos).
       
       La línea amarilla grafica la fórmula y ~ x.
       
       *n formatos. = cantidad de tipos de esquemas de interacción diferentes en los que se desarrolla un encuentro."
       ))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# me gusto menos
plot_diversidad_formatos <- base %>% 
  group_by(ncat_eleccion, n_catformatos) %>% 
  summarise(n_n_catformatos = n()) %>% 
  ggplot() +
  geom_point((aes(ncat_eleccion,
                  n_catformatos ,
                  size = n_n_catformatos)))

# CAPITULO 4 REGULACION #############

# 41. punto de vista de los candidatos a través del tiempo ######

plot_normativa1 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regcandidatos,
                             "NADA", "POSIBILIDAD", "GARANTÍAS", "OBLIGACION"),
                 colour = cat_regcandidatos)) +
  facet_wrap( ~ as.factor(cat_pais) 
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              , 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista de los candidatos,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 4.2 punto de vista del Estado a través del tiempo #####

plot_normativa2 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regestado, "NADA", "POSIBILIDAD", "FISCALIZAR", "GARANTIZAR", "ORGANIZAR"),
                 colour = cat_regestado)) +
  facet_wrap( ~ as.factor(cat_pais) 
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              , 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista del Estado,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 4.3 punto de vista de los medios a través del tiempo ##########

plot_normativa3 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regmedios,
                             "NADA", "POSIBILIDAD", "OPORTUNIDAD", "LIMITACIONES", "OBLIGACIONES"),
                 colour = cat_regmedios)) +
  facet_wrap( ~ as.factor(cat_pais)
              # %>% 
              #   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
              #               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
              #               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")
              ,
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista de medios y organizadores,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# alternativa. tablita#### 

base_normativa_tabla <-
  base_normativa %>% 
  distinct(
    cat_pais, 
    cat_regmedios,
    cat_regestado,
    cat_regcandidatos) 

base_normativa_tabla %>%  
  gt::gt()

base_normativa_tabla %>% write_excel_csv("anexos/anexo2.xlsx")


