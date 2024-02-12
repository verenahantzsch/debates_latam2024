# LIBRERIAS #####
library(tidyverse)
library(ggforce)


# Importación de datos #####

base <- readxl::read_xlsx("./exploracion_base_finalv1/base_finalv1.xlsx") # base con datos por debate
elecciones <-  readxl::read_xlsx("./exploracion_base_finalv1/base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país

# SUBTIPOS DE ORGANIZADORES  #####
#
# creo base para completar manualmente ####

base_id <- base %>% rowid_to_column("id_debate")
base_organizadores <- base_id %>% 
  select(id_debate,
         ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, cat_organizador, cat_subtipoorg, 
         n_strorganizador, n_catorganizador, 
         dico_org_educ, dico_org_estado, dico_org_mmc, dico_org_mmp, dico_org_osc) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  unnest(nombres_organizadores)

#base_organizadores_distinct <- base_organizadores %>% 
#  distinct(nombres_organizadores, ncat_eleccion, cat_pais)

# las guardo

#base_organizadores %>% 
#writexl::write_xlsx("base_organizadores.xlsx")

#base_organizadores_distinct %>% 
# writexl::write_xlsx("base_organizadores_distinct.xlsx")

# las leo: ######
# 
# base_organizadores_distinct <- readxl::read_xlsx("base_organizadores_distinct.xlsx")
# 
# base_organizadores_distinct <- base_organizadores_distinct %>% 
#   left_join(readxl::read_xlsx("distinct_cat_subtipoorg.xlsx"))
# 
# # pruebo hacer wide, no se si esto funciona o tiene sentido
# #base_subtiposdistinct_wide <- base_organizadores_distinct  %>% 
# #  mutate(values = 1) %>% 
# #  pivot_wider(names_from = "ncat_subtipov2", 
# #              names_prefix = "dico_catsubtipo_",
# #              names_sep = ";", 
# #              names_sort = TRUE,
# #              names_repair = "check_unique",
# #              values_from = values) 
# #
# #base_organizadores_wide <- base_organizadores %>% 
# #  left_join(base_subtiposdistinct_wide)
# 
# # version no wide
# 
# base_subtipos <- base_organizadores %>%  
#   mutate(nombres_organizadores = str_trim(nombres_organizadores)) %>% 
#   # con el str trim se resuelven algunos problemas pero OJO que hay registros duplicados
#   left_join(base_organizadores_distinct %>% 
#               distinct(nombres_organizadores, ncat_eleccion, cat_pais,
#                        ncat_subtipov2, cat_tipoorgv2,
#                        ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
#                        cat_areaorg))
# # aplicando de nuevo distinct se resolvio la duplicacion

# vuelvo a guardar y a leer

# base_subtipos %>%  write.csv("base_subtipos_limpia.csv")

base_subtipos <-read.csv("./exploracion_base_finalv1/base_subtipos_lista.csv")


# varios borradores de trabajo #####

# a ver algunas cuentitas
# NOTA AL 23 de julio: pendiente corregir algunos detalles en base final, espacios antes de punto y coma
# revisar cuando tengas acceso a un excel decente

base_subtipos_groupedxdebate <- base_subtipos %>% 
  group_by(ncat_eleccion, cat_pais, t_fecha, str_organizador) %>% 
  summarise(n_subtipoorgs = n_distinct(ncat_subtipov2),
            n_orgs = n(),
            n_tiposorgs = n_distinct(cat_tipoorgv2))

# me gusta , para contar subtipos de organizadores
cuenta_subtipos <- forcats::fct_count(as.factor(base_subtipos$ncat_subtipov2)) %>% 
  arrange(desc(n))

cuenta_tipos <- forcats::fct_count(as.factor(base_subtipos$cat_tipoorgv2)) %>% 
  arrange(desc(n))

cuenta_subtipo_aniopais <- base_subtipos %>% 
  group_by(ncat_eleccion, cat_pais, ncat_subtipov2) %>% 
  summarise(n_incidenciasubtipos = n()) %>%  
  arrange(cat_pais, ncat_eleccion, desc(n_incidenciasubtipos)) %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  mutate(n_diversidadsubtipos = n_distinct(ncat_subtipov2))

cuenta_subtipos_pais <- base_subtipos %>% 
  group_by(cat_pais, ncat_subtipov2) %>% 
  mutate(n_incidenciasubtipos = n()) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(n_totalorganizadores = n(),
         n_incidenciarelativasubtipos = n_incidenciasubtipos/n_totalorganizadores) %>%   
  arrange(cat_pais, desc(n_incidenciasubtipos)) #%>% 
#group_by(cat_pais) %>% 
## mutate(n_diversidadsubtipos = n_distinct(ncat_subtipov2),

# algunas cuentas numericas  por anio pais 
subtipos_anio_pais <- base_subtipos %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarize(n_subtipoorgs = n_distinct(ncat_subtipov2),
            n_tiposorgs = n_distinct(cat_tipoorgv2),
            n_debates = n_distinct(str_organizador),
            n_organizadores = n(),
            n_promedioorgsxdebate = n_organizadores/n_debates)

# cuentas con base con un organizador por fila

base_subtipos_ungrouped <- base_subtipos %>% 
  group_by(ncat_eleccion, cat_pais, t_fecha, str_organizador) %>% 
  mutate(n_subtipoorgsxdebate = n_distinct(ncat_subtipov2),
         n_orgsxdebate = n(),
         n_tiposorgsxdebate = n_distinct(cat_tipoorgv2)) %>% 
  ungroup() %>% 
  mutate(n_pesoorg = 1/n_orgsxdebate)

# ahora agrupo por pais

base_suptipos_countryear <- base_subtipos_ungrouped %>% 
  group_by(ncat_eleccion, cat_pais, ncat_subtipov2) %>% 
  summarise(n_pesosubtipo = sum(n_pesoorg))

# grafico
# version facet y version no facet. ta bueno pero hay que mejorarle la claridad, no se entienden del todo

plot_subtipos_countryear <- base_suptipos_countryear %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_pesosubtipo, colour= cat_pais)) +
  geom_label(aes(ncat_eleccion, n_pesosubtipo, label = ncat_subtipov2, vjust = -0.5, colour = cat_pais), size = 3) +
  geom_label(aes(ncat_eleccion, n_pesosubtipo, label = cat_pais, vjust = 0.5, colour = cat_pais), size = 3) +
  theme_minimal() +
  theme(legend.position = "none") 

plot_subtipos_countryear_facet <- base_suptipos_countryear %>% 
  ggplot() +
  # geom_point(aes(ncat_eleccion, n_pesosubtipo, colour= cat_pais)) +
  geom_label(aes(ncat_eleccion, n_pesosubtipo, label = ncat_subtipov2, vjust = 0.5, colour = cat_pais), size = 3) +
  facet_wrap(~ cat_pais) +
  theme_minimal() +
  theme(legend.position = "none") 

# version facet pero barritas
# no es claro tampoco. estos datos quizas mejor hablar de ellos que mostrarlos
plot_subtipos_countryear_facet_bar <- base_suptipos_countryear %>% 
  ggplot() +
  geom_col(position = "stack", aes(ncat_eleccion, n_pesosubtipo, colour = ncat_subtipov2)) +
  facet_wrap(~ cat_pais) +
  theme_minimal() +
  theme(legend.position = "none") 

plot_subtipos_countryear_facet_bar2 <- base_suptipos_countryear %>% 
  ggplot() +
  geom_col(position = "fill", aes(ncat_eleccion, n_pesosubtipo, fill = ncat_subtipov2)) +
  facet_wrap(~ cat_pais) +
  theme_minimal() +
  theme(legend.position = "none") 

plot_subtipos_year_bar2 <- base_suptipos_countryear %>% 
  ggplot() +
  geom_col(position = "fill", aes(ncat_eleccion, n_pesosubtipo, fill = ncat_subtipov2)) +
  #facet_wrap(~ cat_pais) +
  theme_minimal() +
  theme(legend.position = "none") 


graficosubtipos <- base_suptipos_countryear %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_subtipov2) %>% 
  mutate(n_pesosubtipoacumulado = cumsum(n_pesosubtipo)) %>% 
  ungroup()

# NO FUNCA AGREGAR LABELS
plot_subtipos_country_bar2 <- graficosubtipos %>% 
  ggplot() +
  geom_col(aes(cat_pais, n_pesosubtipo, fill = ncat_subtipov2), position = "stack") +
  #geom_text(data = graficosubtipos %>% 
  #           group_by(cat_pais, ncat_subtipov2) %>% 
  #          summarise(promediopeso = mean(n_pesosubtipoacumulado),
  #                   aes(
  #cat_pais, promediopeso , label = ncat_subtipov2))) +
  theme_minimal() +
  theme(legend.position = "none") 

# vamos a probar lo anterior pero en version filtrada

dfplot_cuenta_subtipos_pais <- cuenta_subtipos_pais %>% 
  filter(n_incidenciarelativasubtipos > 0.1) %>% 
  distinct(cat_pais, n_incidenciarelativasubtipos, ncat_subtipov2) %>% 
  filter(!c("PENDIENTE") %in% ncat_subtipov2 ) %>% 
  filter(!is.na(ncat_subtipov2)) %>%  
  arrange(cat_pais, n_incidenciarelativasubtipos) %>% 
  group_by(cat_pais) %>% 
  mutate(n_acumulacion = cumsum(n_incidenciarelativasubtipos)) %>% 
  ungroup()


plot_cuenta_subtipos_pais <- dfplot_cuenta_subtipos_pais %>% 
  ggplot() +
  geom_col(aes(n_incidenciarelativasubtipos, cat_pais, fill=ncat_subtipov2), , position = "stack") +
  geom_text(aes(n_acumulacion, cat_pais, label=ncat_subtipov2), hjust = 1, size = 2) +
  theme_minimal() +
  theme(legend.position = "none") # VIAJA PERO OJO QUE NO SE CORRESPONDEN LAS LABELS LPM


# intentos mejores #####

base_subtipos_ungrouped <- base_subtipos %>% 
  group_by(ncat_eleccion, cat_pais, t_fecha, str_organizador) %>% 
  mutate(n_subtipoorgsxdebate = n_distinct(ncat_subtipov2),
         n_orgsxdebate = n(),
         n_tiposorgsxdebate = n_distinct(cat_tipoorgv2)) %>% 
  ungroup() %>% 
  mutate(n_pesoorg = 1/n_orgsxdebate) %>% 
  group_by(cat_pais, ncat_subtipov2) %>% 
  mutate(n_subtipoenpais = n(),
         n_pesosubtipoenpais = sum(n_pesoorg) ) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(n_totalorganizadoresxpais = n(),
         n_incidenciarelativasubtiposxpais = n_pesosubtipoenpais/n_totalorganizadoresxpais) %>%   
  ungroup() %>% 
  arrange(cat_pais, desc(n_incidenciarelativasubtiposxpais))


# grafico 

dfplot_cuenta_subtipos_pais <- base_subtipos_ungrouped %>% 
  filter(n_incidenciarelativasubtiposxpais > 0.05) %>% 
  distinct(cat_pais, n_incidenciarelativasubtiposxpais, ncat_subtipov2) %>% 
  filter(!c("PENDIENTE") %in% ncat_subtipov2 ) %>% 
  filter(!is.na(ncat_subtipov2)) %>%  
  arrange(cat_pais, n_incidenciarelativasubtiposxpais) %>% 
  group_by(cat_pais) %>% 
  mutate(n_acumulacion = cumsum(n_incidenciarelativasubtiposxpais)) %>% 
  ungroup() %>%  
  arrange(cat_pais, n_acumulacion)

plot_cuenta_subtipos_pais <- dfplot_cuenta_subtipos_pais %>% 
  ggplot() +
  geom_col(aes(n_incidenciarelativasubtiposxpais, cat_pais, fill=fct_reorder(ncat_subtipov2, desc(n_acumulacion) ) ), position = "stack") +
  geom_text(aes(n_acumulacion, cat_pais, label=fct_reorder(ncat_subtipov2, desc(n_acumulacion))),  hjust = 1, size = 2)  +
  theme_minimal() +
  theme(legend.position = "none") # 
#VIAJA PERO OJO QUE NO SE CORRESPONDEN LAS LABELS LPM
# NO SE POR QUE MIERDA ME TIENE LAS PELOTAS LLENAS

# grafico inverso. pparticipacion de subtipos de organizadores en paises

dfplot_cuenta_pais_subtipos <- base_subtipos_ungrouped %>% 
  filter(n_incidenciarelativasubtiposxpais > 0.05) %>% 
  distinct(cat_pais, n_incidenciarelativasubtiposxpais, n_pesosubtipoenpais, ncat_subtipov2) %>% 
  filter(!c("PENDIENTE") %in% ncat_subtipov2 ) %>% 
  filter(!is.na(ncat_subtipov2))  # %>%  
arrange(cat_pais, n_incidenciarelativasubtiposxpais) %>% 
  group_by(cat_pais) %>% 
  mutate(n_acumulacion = cumsum(n_incidenciarelativasubtiposxpais)) %>% 
  ungroup() %>%  
  arrange(cat_pais, n_acumulacion)

# pendiente corregir cosas

plot_cuenta_pais_subtpos <- dfplot_cuenta_pais_subtipos %>% 
  ggplot() +
  geom_col(aes(n_pesosubtipoenpais, fct_reorder(ncat_subtipov2, n_pesosubtipoenpais), fill=cat_pais) , position = "stack") +
  # geom_text(aes(n_incidenciarelativasubtiposxpais, ncat_subtipov2, label=cat_pais), position = position_stack(), size = 2) +
  theme_minimal() +
  theme(legend.position = "none")

# prueba sets paralelos


# v1 tres variables
# algo asi me conforma. hay que refinar un poquito. 
# este sumado al de 
dfparalel_subtipos <- base_subtipos_ungrouped %>% 
  select(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plotparalel_subtipos <- ggplot(dfparalel_subtipos,
                               aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_tipoorgv2), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# v2 dos variables

dfparalel_subtipos2 <- base_subtipos_ungrouped %>% 
  select(ncat_subtipov2, cat_tipoorgv2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  dplyr::count(ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:2)

plotparalel_subtipos2 <- ggplot(dfparalel_subtipos2,
                                aes(x, id = id, split = fct_reorder(y, cat_tipoorgv2), value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = ncat_subtipov2), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# v3 tres variables reformulada
# HATA AHORA ES EL QUE MAS ME GUSTA!

dfparalel_subtipos3 <- base_subtipos_ungrouped %>% 
  select(cat_pais, cat_areaorg, cat_tipoorgv2) %>% 
  filter(!is.na(cat_areaorg)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  dplyr::count(cat_pais, cat_areaorg, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plotparalel_subtipos <- ggplot(dfparalel_subtipos3,
                               aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))



# TIPOS DE ORGANIZADORES ####
#
# Tipos de organizadores con variable original vieja #####

# Nota: esto también fue trabajado previamente en "exploracion_tipo_organizadores.R"

# preparamos datos. Calcularemos la "proporción" de organizadores que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de organizador sobre el total de diferentes tipos de organiadores por debates
# NO calcula esta proporción con base en el carácter de cada una de las organizaciones que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate
tipos_organizadores <- base %>% 
  mutate(
    pr_mmc = as.logical(dico_org_mmc)/n_catorganizador  ,
    pr_mmp = as.logical(dico_org_mmp)/n_catorganizador  ,
    pr_estado =  as.logical(dico_org_estado)/n_catorganizador  ,
    pr_educ =  as.logical(dico_org_educ)/n_catorganizador ,
    pr_osc =  as.logical(dico_org_osc)/n_catorganizador  )

# cálculo sumarizado por país y elección
tipos_organizadores_año_pais <- tipos_organizadores %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_mmc = sum(pr_mmc) ,
            n_mmp = sum(pr_mmp) ,
            n_estado = sum(pr_estado) ,
            n_educ = sum(pr_educ) ,
            n_osc = sum(pr_osc) ,
            tot_debates_año_pais = n()) 

# pasamos data a long
tipos_organizadores_año_pais <-  tipos_organizadores_año_pais %>% 
  dplyr::rename(cat_eleccion = ncat_eleccion) %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_org",
               values_to = "org_peso")  

# graficamos

# me gusta este para comparar entre paises
tipos_organizadores_ev_anual_pais <- tipos_organizadores_año_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  subset( org_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_org, colour = tipo_org, size= org_peso))  +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
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

# Qué vemos? 
# 1- En el grupo de países que se hacen más debates, predominan claramente los medios de comunicaicón
# 2- Hay un grupo de países en el que parecen tener más peso las OSC. 
# Se trata en su mayoría (sabemos) de nuestro trabajo cualitativo) de organizaciones empresariales
# 3- El estado aparece más recientemente y sólo en algunos países. 
# Esto lo veremos con más claridad en el gráfico siguiente.
# 4- Es notoria la baja presencia de medios públicos, en línea con demandas históricas desde los estudios en comunicación.
# nota/ pendiente: estaría bueno reordenar las categorías de modo que medios públicos aparezca junto a Estado

# me gusta este para comparar dos etapas
tipos_organizadores_ev_anual <- tipos_organizadores_año_pais %>% 
  subset( org_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_org, colour = tipo_org, size= org_peso))  +
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

# qué vemos?
# 1- distinto "timing de entrada" para los diferentes tipos de organizadores
# que siguen el siguiente orden:
# medios privados - osc y centros educativos - Estado y medios públicos
# 2- algunas hipótesis:
# a- reintervencionismo estatal
# b- crisis de legitimidad del periodismo / batallas mediáticas
# c- posibilidad de tranmisión vía streaming para entidades no mediáticas
# nota/pendiente: reordenar ascendente o descendentemente de las categorías 
# "más antiguas" a las más recientes




# Tipos de organizadores nuevo ####

# preparamos datos. Calcularemos la "proporción" de organizadores que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de organizador sobre el total de diferentes tipos de organiadores por debates
# NO calcula esta proporción con base en el carácter de cada una de las organizaciones que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate
# esta base agrupada me sirve para comparar con la version vieja, eventualmente
base_tipos_grouped <- base_subtipos %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha) %>% 
  summarise(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
            n_organizadores = n())

#base no agrupada para hacer cuentas y graficos
base_tipos_ungrouped <- base_subtipos %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha) %>% 
  mutate(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
            n_organizadores = n()) %>% 
  ungroup %>% 
  mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
         n_prppacorg = 1/n_organizadores) # porcentaje de participacion del organizador sobre la cantidad de organizadores del debate

# base agrupada por año y pais
base_tipos_countryear <- base_tipos_ungrouped %>% 
  group_by(ncat_eleccion, cat_pais, cat_tipoorgv2) %>% 
  summarise(n_prppaccatorg = sum(n_prppaccatorg),
            n_prppacorg = sum(n_prppacorg) )

# grafico tipo de organizador nuevas versiones ME GUSTA #####
#
# para comparar paises. ###############
#PROPORCION SOBRE CATEGORIAS
# IEII Y HASTA ME GUSTA MAS QUE EL ANTERIOR IEIII
tipos_organizadores_ev_anual_paisv2 <- base_tipos_countryear %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  #subset( n_prppaccatorg > 0 ) %>% 
  ggplot(aes(ncat_eleccion, cat_tipoorgv2, colour = cat_tipoorgv2, size= n_prppaccatorg))  +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Elaboración propia. 
              El tamaño de los círculos es proporcional a la cantidad de debates 
       que involucraron a cada tipo de organnizador
       en un año dado")


# PROPORCION SOBRE TOTAL ORGS
# para comparar paises. 
# bien. no se ve tan diferente
tipos_organizadores_ev_anual_paisv3 <- base_tipos_countryear %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  #subset( n_prppaccatorg > 0 ) %>% 
  ggplot(aes(ncat_eleccion, cat_tipoorgv2, colour = cat_tipoorgv2, size= n_prppacorg))  +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates 
       en los que participaron los diferentes tipos de organnizador
       en un año dado")

# para comparar etapas ##############
# me gusta este para comparar dos etapas
tipos_organizadores_ev_anualv2 <- base_tipos_countryear %>% 
  ggplot(aes(ncat_eleccion, cat_tipoorgv2, colour = cat_tipoorgv2, size= n_prppacorg))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
 # scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")  


# ANALISIS COMBINADO TIPO SUBTIPO #####

# set paralelo tipo subtipo sin ponderar #############
# ta bueno pero ojo habria que ponderarlo por debate no? ###

# OSC 
base_osc_pais <- base_subtipos %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "osc") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plot_osc_pais  <- base_osc_pais %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# MMC 


base_mmc_pais <- base_subtipos %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "mmc") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plot_mmc_pais  <- base_mmc_pais %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# Estado
base_estado_pais <- base_subtipos %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "estado") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plot_estado_pais  <- base_estado_pais %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# EDUC
base_educ_pais <- base_subtipos %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "educ") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3)

plot_educ_pais  <- base_educ_pais %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# set paralelo tipo subtipo prueba version ponderada. ME GUSTA MAS #########

base_subtipos_tipos <- base_subtipos %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha) %>% 
  mutate(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
         n_organizadores = n(),
         n_ncatsubtipov2 = n_distinct(ncat_subtipov2)) %>% 
  ungroup %>% 
  mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
         n_prppacorg = 1/n_organizadores,
         n_prppacsubtipo = 1/n_ncatsubtipov2)

base_subtipos_tipos_country <- base_subtipos_tipos %>% 
  group_by(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  summarise(n_prppacsubtipo = sum(n_prppacsubtipo))

# OSC 

base_osc_pais2 <- base_subtipos_tipos_country %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "osc") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3) %>% 
  left_join(base_subtipos_tipos_country) %>% 
  mutate(n = n*n_prppacsubtipo) %>% 
  select(-c(n_prppacsubtipo))

plot_osc_pais  <- base_osc_pais2 %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# MMC

base_mmc_pais2 <- base_subtipos_tipos_country %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "mmc") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3) %>% 
  left_join(base_subtipos_tipos_country) %>% 
  mutate(n = n*n_prppacsubtipo) %>% 
  select(-c(n_prppacsubtipo))

plot_mmc_pais  <- base_mmc_pais2 %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# MMP y ESTADO

base_estadommp_pais2 <- base_subtipos_tipos_country %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "estado" | cat_tipoorgv2 == "mmp") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3) %>% 
  left_join(base_subtipos_tipos_country) %>% 
  mutate(n = n*n_prppacsubtipo) %>% 
  select(-c(n_prppacsubtipo))

plot_estadommp_pais  <- base_estadommp_pais2 %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# EDUC

base_educ_pais2 <- base_subtipos_tipos_country %>% 
  select(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_tipoorgv2)) %>% 
  filter(cat_tipoorgv2 == "educ") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_tipoorgv2) %>% 
  gather_set_data(1:3) %>% 
  left_join(base_subtipos_tipos_country) %>% 
  mutate(n = n*n_prppacsubtipo) %>% 
  select(-c(n_prppacsubtipo))

plot_educ_pais  <- base_educ_pais2 %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# por ambito. iglesia. 

base_iglesia <- base_subtipos %>% 
  select(cat_pais, cat_areaorg, ncat_subtipov2) %>% 
  filter(!is.na(ncat_subtipov2)) %>% 
  filter(!is.na(cat_areaorg)) %>% 
  filter(cat_areaorg == "iglesia") %>% 
  dplyr::count(cat_pais, ncat_subtipov2, cat_areaorg) %>% 
  gather_set_data(1:3) 

plot_iglesia  <- base_iglesia %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# CUENTAS MANUALES #####

# cuentas relacionadas a los subtipos ##### 
cuenta_subtipos_mmc <- base_subtipos %>% 
  filter(cat_tipoorgv2=="mmc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_osc <- base_subtipos %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_mmp <- base_subtipos %>% 
  filter(cat_tipoorgv2=="mmp") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_educ <- base_subtipos %>% 
  filter(cat_tipoorgv2=="educ") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_estado <- base_subtipos %>% 
  filter(cat_tipoorgv2=="estado") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_tipos <- base_subtipos %>% 
  group_by(cat_tipoorgv2) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/sum(n_debates_en_participaron)*100) %>% 
  arrange(desc(pr_debates_en_participaron))

sum(cuenta_tipos$n_individuos)
sum(cuenta_tipos$n_debates_en_participaron)

# cuentitas genero ######
cuenta_ong_genero <- base_subtipos %>% 
  filter(ncat_subtipov2 == "ong_genero") %>% 
  group_by(id_debate, ncat_eleccion) %>% 
  summarise(n = n())

# cuentitas impugnados #####

cuenta_impugnado <- base_subtipos %>% 
  left_join(base_id %>% 
              select(id_debate, dico_impugnado)) %>% 
  filter(dico_impugnado==TRUE) %>% 
  group_by(id_debate, cat_tipoorgv2) %>% 
  summarise(n= 1) %>% 
  group_by(cat_tipoorgv2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100)

sum(if_else(base$dico_impugnado=="TRUE", 1, 0) %>% na.omit())

# cuentas relacionadas al streaming #### 
cuenta_tipos_streaming <- base_subtipos %>% 
  left_join(base_id %>% 
              select(id_debate, dico_streaming)) %>% 
  filter(dico_streaming==TRUE) %>% 
  group_by(id_debate, cat_tipoorgv2) %>% 
  summarise(n= 1) %>% 
  group_by(cat_tipoorgv2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100)

sum(if_else(base$dico_streaming=="TRUE", 1, 0) %>% na.omit()) 

cuenta_streaming <- base_id %>% 
  group_by(dico_streaming) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100)

cuenta_streaming_osc <- base_subtipos %>% 
  left_join(base_id %>% 
              select(id_debate, dico_streaming)) %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(id_debate, dico_streaming) %>%
  summarise(n= 1) %>% 
  group_by(dico_streaming) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100)

# cuenta cnn ######### 

cuenta_cnn <- base_id %>% 
  filter(str_detect(str_organizador, "CNN")) %>% 
  select(str_organizador, cat_pais, ncat_eleccion)

# cuenta subtipos osc pais hay diferencia #####


cuenta_subtipos_osc_paises <- base_subtipos %>% 
  group_by(cat_pais) %>% 
  mutate(cantidad_debates_pais_total = n_distinct(id_debate)) %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(id_debate) %>% 
  mutate( undebate = 1/n() ) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  mutate(n_osc_pais = n(),
         n_debatesconosc_pais = sum(undebate)) %>% 
  ungroup() %>% 
  group_by(ncat_subtipov2, cat_pais) %>% 
  summarise(n = n(),
            cantidad_debates_pais_total = mean(cantidad_debates_pais_total),
            n_debatesconsubtipoosc = sum(undebate),
            pr_deoscpais = n/n_osc_pais*100,
            pr_dedebateoscspais = n_debatesconsubtipoosc/n_debatesconosc_pais*100,
            pr_dedebatespais = n_debatesconsubtipoosc/cantidad_debates_pais_total*100) %>% 
  distinct(cat_pais, ncat_subtipov2, n, pr_deoscpais, pr_dedebatespais, pr_dedebateoscspais, n_debatesconsubtipoosc )

# alianzas en el tiempo

alianzas_cuenta <- base_subtipos %>%  
  group_by(id_debate, ncat_eleccion, cat_pais) %>% 
  summarise(n_orgs = n(),
            n_variedadorgs = n_distinct(cat_tipoorgv2),
            n_variedadsubtipos = n_distinct(ncat_subtipov2))

plot_alianzas_cuenta <- alianzas_cuenta %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_variedadorgs, colour= cat_pais)) +
  theme_minimal() +
  theme(legend.position = "none")

plot_alianzas_cuenta1 <-ggExtra::ggMarginal(plot_alianzas_cuenta,
                                             type="histogram",
                                             margins = "y") 

plot_alianzas_cuenta2 <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_orgs)) +
  geom_point(aes(alpha = as.numeric(ncat_eleccion))) +
  theme_minimal() +
  theme(legend.position = "none")

plot_alianzas_cuenta21 <-ggExtra::ggMarginal(plot_alianzas_cuenta2,
                                            type="histogram") 

patchwork_alianzas <-  patchwork::wrap_plots(plot_alianzas_cuenta21, plot_alianzas_cuenta1) + patchwork::plot_layout(ncol=1)

plot_alianzas_cuenta3 <- alianzas_cuenta %>% 
  ggplot() +
  geom_point(aes(n_variedadorgs, n_orgs, alpha = ncat_eleccion)) +
  theme_minimal() +
  theme(legend.position = "none")
