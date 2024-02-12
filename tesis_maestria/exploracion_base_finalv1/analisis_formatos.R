# LIBRERIAS #####
library(tidyverse)
library(ggforce)


# Importación de datos #####

base <- readxl::read_xlsx("base_finalv1.xlsx") # base con datos por debate
base_id <- base %>% rowid_to_column("id_debate")

# carga de datos de elecciones

elecciones <-  readxl::read_xlsx("base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país

# carga de datos de subtipos 

base_organizadores <- base_id %>% 
  select(id_debate,
         ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  unnest(nombres_organizadores)

base_organizadores_distinct <- readxl::read_xlsx("base_organizadores_distinct.xlsx")

base_organizadores_distinct <- base_organizadores_distinct %>% 
  left_join(readxl::read_xlsx("distinct_cat_subtipoorg.xlsx"))

base_subtipos <- base_organizadores %>%  
  mutate(nombres_organizadores = str_trim(nombres_organizadores)) %>% 
  left_join(base_organizadores_distinct %>% 
              distinct(nombres_organizadores, ncat_eleccion, cat_pais,
                       ncat_subtipov2, cat_tipoorgv2,
                       ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
                       cat_areaorg))

# Formatos #####

# procedemos de manera parecida a lo anterior 
# (y lo mismo haremos con categorías siguientes)
# preparamos datos. Calcularemos la "proporción" de formatos que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de formato sobre el total de diferentes tipos de formatos por debate
# NO calcula esta proporción con base en el carácter de cada uno de los bloques o segmentos que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate

tipos_formatos <- base_id %>% # primero algunas transformaciones de los datos
  # no nos salió hacerlo automáticamente, procedemos por categoría
  # mutate_at(vars(contains('dico_foramto')), as.logical)
  mutate( dico_formato_apertura = as.logical(dico_formato_apertura),
          dico_formato_duelo = as.logical(dico_formato_duelo),
          dico_formato_expertos = as.logical(dico_formato_expertos),
          dico_formato_libre = as.logical(dico_formato_libre),
          dico_formato_moderadores = as.logical(dico_formato_moderadores),
          dico_formato_periodistas = as.logical(dico_formato_periodistas),
          dico_formato_presentes = as.logical(dico_formato_presentes),
          dico_formato_sectores = as.logical(dico_formato_sectores),
          dico_formato_virtuales = as.logical(dico_formato_virtuales),
          dico_formato_expositivo = str_detect(cat_formato, "expositivo"), # ups me había faltado contabilizar
          n_catformatos = dico_formato_apertura +
            dico_formato_duelo +
            dico_formato_expertos +
            dico_formato_libre +
            dico_formato_moderadores + 
            dico_formato_periodistas + 
            dico_formato_presentes +
            dico_formato_sectores + 
            dico_formato_virtuales +
            dico_formato_expositivo)  %>% 
  mutate(  # ahora sí calculamos proporciones
    pr_apertura = dico_formato_apertura/n_catformatos,
    pr_duelo = dico_formato_duelo/n_catformatos,
    pr_expertos =   dico_formato_expertos/n_catformatos,
    pr_libre =   dico_formato_libre/n_catformatos,
    pr_moderadores =   dico_formato_moderadores/n_catformatos,
    pr_periodistas =   dico_formato_periodistas/n_catformatos, 
    pr_presentes =   dico_formato_presentes/n_catformatos,
    pr_sectores =   dico_formato_sectores/n_catformatos, 
    pr_virtuales =   dico_formato_virtuales/n_catformatos,
    pr_expositivo =   dico_formato_expositivo/n_catformatos )
# nota/ pendiente: ver manera de hacer esto más automático

# cálculo sumarizado por país y elección
tipos_formatos_año_pais <- tipos_formatos %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_apertura = sum(pr_apertura),
            n_duelo = sum(pr_duelo),
            n_expertos = sum(pr_expertos),
            n_libre = sum(pr_libre),
            n_moderadores = sum(pr_moderadores),
            n_periodistas = sum(pr_periodistas), 
            n_presentes = sum(pr_presentes),
            n_sectores = sum(pr_sectores),
            n_virtuales = sum(pr_virtuales),
            n_expositivo = sum(pr_expositivo),
            tot_debates_año_pais = n())

# pasamos data a long
tipos_formatos_año_pais <-  tipos_formatos_año_pais %>% 
  dplyr::rename(cat_eleccion = ncat_eleccion) %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_formato",
               values_to = "formato_peso")

# graficamos. de nuevo repetimos anteriores

# graficamos
# para cargar colores

tipo_formato <-  c("n_presentes", "n_virtuales", "n_sectores",
                   "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                   "n_expositivo", "n_duelo", "n_libre")
colors <- c("#ff3452", "#FF6846", "#FF9044","#FFD667", "#F6E46E", "#ECF179", "#c8fd81","#ADFD7F", "#8CFC7C","#5FFB7F")
colores_formatos <- tibble(tipo_formato, colors)

# me gusta este para comparar entre paises
tipos_formatos_ev_anual_pais <- tipos_formatos_año_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  left_join(colores_formatos) %>% 
  subset( formato_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion,  fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                        "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                        "n_expositivo", "n_duelo", "n_libre"), 
             colour = fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                  "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                  "n_expositivo", "n_duelo", "n_libre"), size= formato_peso))  +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  scale_color_manual(values=colors) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de formato",
       title = "Tipo de formato de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# Qué vemos? 
# Hay muchos missing, pero
# 1- Los países con más trayectoria de debates tienen más diversidad de formatos
# 2- También parecen ser los que dan más lugar a la participación del público 
# (se bajo la forma de "presentes", de "virtuales" o de "sectores")
# nota/ pendiente: reordenar categorías para más claridad
# nota / pendiente: contrastar esto con categorías ordinales elaboradas

# comparacion entre paises pero sin el tiempo
# cálculo sumarizado por país 
tipos_formatos_pais <- tipos_formatos %>% 
  group_by(cat_pais) %>% 
  summarise(n_apertura = sum(pr_apertura %>%  na.omit()),
            n_duelo = sum(pr_duelo%>%  na.omit()),
            n_expertos = sum(pr_expertos%>%  na.omit()),
            n_libre = sum(pr_libre%>%  na.omit()),
            n_moderadores = sum(pr_moderadores%>%  na.omit()),
            n_periodistas = sum(pr_periodistas%>%  na.omit()), 
            n_presentes = sum(pr_presentes%>%  na.omit()),
            n_sectores = sum(pr_sectores%>%  na.omit()),
            n_virtuales = sum(pr_virtuales%>%  na.omit()),
            n_expositivo = sum(pr_expositivo%>%  na.omit()),
            tot_debates_año_pais = n())  %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_formato",
               values_to = "formato_peso") %>% 
  subset( formato_peso > 0 ) 

plottipos_formatos_pais <- tipos_formatos_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  left_join(colores_formatos) %>% 
  ggplot(aes(fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                        "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                        "n_expositivo", "n_duelo", "n_libre"),
             cat_pais, 
             colour = fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                  "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                  "n_expositivo", "n_duelo", "n_libre"),,
             size = formato_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  #scale_x_continuous(breaks = seq(1950,2021,10)) +
  scale_color_manual(values=colors) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de intercambio",
       title = "Tipo de intercambio durante los debates ",
       subtitle = " por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# me gusta este para comparar dos etapas#E0FE88
tipos_formatos_ev_anual <- tipos_formatos_año_pais %>% 
  left_join(colores_formatos) %>% 
  subset( formato_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                       "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                       "n_expositivo", "n_duelo", "n_libre"), 
             colour = fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
                                  "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
                                  "n_expositivo", "n_duelo", "n_libre"), size= formato_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  scale_color_manual(values=colors) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de formato",
       title = "Tipo de formato de los debates ",
       subtitle = "A través el tiempo",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")  

# qué vemos? 
# 1- Más recientemente (pos 2010) hay más diversidad de formatos
# ¿podría deberse a la falta de datos?
# 2- Los primeros debates eran de periodistas o de duelo
# 3- público aparece sobre todo post-2010

# competencia gr #####
# Evolución de grado de competencia entre candidatos a través del tiempo

competencia_candidatos <- base %>% 
  mutate(ncat_competencia = as.numeric(ncat_competencia)) %>% 
  select(ncat_eleccion, cat_pais, ncat_competencia) %>% 
  group_by(cat_pais) %>% 
  mutate(n_totdebatesxpais = n()) %>% 
  ungroup() %>% 
  mutate(n_pesorelativodebate = 1/n_totdebatesxpais)

competencia_candidatos_grouped <- competencia_candidatos %>% 
  group_by(cat_pais, ncat_competencia) %>% 
  summarise(n_pesorelativocompetencia = sum(n_pesorelativodebate))

competencia_candidatos_annualmean <- competencia_candidatos %>% 
  na.omit() %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(promedio_competencia = mean(ncat_competencia))

# este ca va 
plot_competencia_candidatosxdebate <- competencia_candidatos  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_eleccion, ncat_competencia, colour = cat_pais)) +
  geom_point() +
  # geom_line() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Nivel de competencia",
       title = "Grado de interacción entre candidatos que promueven los debates",
       subtitle = "Por debate, a través el tiempo, por países",
       caption = "Elaboración propia")  

# este tb me gusta
plot_competencia_candidatosxdebate2 <- competencia_candidatos  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  #filter(cat_pais!="Brasil") %>% 
  ggplot(aes(ncat_competencia, colour = cat_pais)) +
  geom_histogram() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  #gghighlight::gghighlight() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "n debates",
       title = "Grado de interacción entre candidatos que promueven los debates",
       subtitle = "Distribución de la variable por países",
       caption = "Elaboración propia") 


# cproporciones
# creo que en este caso puede ser más interesante el de valores absolutos. Para ver
plot_competencia_candidatosxdebate3 <- competencia_candidatos_grouped  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_competencia, n_pesorelativocompetencia, fill = cat_pais)) +
  geom_col() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  #gghighlight::gghighlight() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "Grado de interacción",
       y = "proporción de debates",
       title = "Grado de interacción entre candidatos que promueven los debates",
       subtitle = "Distribución de la variable por países",
       caption = "Elaboración propia") 

# no me gusta tanto
plot_competencia_candidatos <- competencia_candidatos_annualmean %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_eleccion, promedio_competencia, colour = cat_pais)) +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Nivel de competencia",
       title = "Grado de interacción entre candidatos que promueven los debates",
       subtitle = "Promedio por elección, a través el tiempo, por países",
       caption = "Elaboración propia")  

#horrible
plot_competencia_candidatos2 <- competencia_candidatos_annualmean %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_eleccion, promedio_competencia, colour = cat_pais)) +
  geom_point() +
 # geom_line() +
 # facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Nivel de competencia",
       title = "Grado de interacción entre candidatos que promueven los debates",
       subtitle = "Promedio por elección, a través el tiempo, por países",
       caption = "Elaboración propia")  

# presencia publico gr #####
# Evolución de grado de presencia del público a través del tiempo

presencia_publico <- base %>% 
  mutate(ncat_ppac = as.numeric(ncat_ppac)) %>% 
  select(ncat_eleccion, cat_pais, ncat_ppac) %>% 
  group_by(cat_pais) %>% 
  mutate(n_totdebatesxpais = n()) %>% 
  ungroup() %>% 
  mutate(n_pesorelativodebate = 1/n_totdebatesxpais)

presencia_publico_grouped <- presencia_publico %>% 
  group_by(cat_pais, ncat_ppac) %>% 
  summarise(n_pesorelativoppac = sum(n_pesorelativodebate))

presencia_publico_annualmean <- presencia_publico %>% 
  na.omit() %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(promedio_presencia = mean(ncat_ppac))


# replico los de la variable anterior

# este ca va 
plot_ppacxdebate <- presencia_publico   %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_eleccion,ncat_ppac, colour = cat_pais)) +
  geom_point() +
  # geom_line() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Nivel de presencia",
       title = "Grado de participación ciudadana que promueven los debates",
       subtitle = "Por debate, a través el tiempo, por países",
       caption = "Elaboración propia")  

# este tb me gusta
plot_ppacxdebate2 <- presencia_publico  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_ppac, colour = cat_pais)) +
  geom_histogram() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  #gghighlight::gghighlight() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,4,1)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "n debates",
       title = "Grado de participación ciudadana que promueven los debates",
       subtitle = "Distribución de la variable por países",
       caption = "Elaboración propia") 

# cproporciones

plot_ppacxdebate3 <- presencia_publico_grouped  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_ppac, n_pesorelativoppac, fill = cat_pais)) +
  geom_col() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  #gghighlight::gghighlight() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,4,1)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "Grado de participacion",
       y = "proporción de debates debates",
       title = "Grado de participación ciudadana que promueven los debates",
       subtitle = "Distribución de la variable por países",
       caption = "Elaboración propia") 

# con promedios por eleccion. pierdo info al pedo
plot_presencia_publico <- presencia_publico_annualmean %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>%
  ggplot(aes(ncat_eleccion, promedio_presencia, colour = cat_pais)) +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  labs(x = "",
       y = "Nivel de presencia del público",
       title = "Grado de interacción entre el público y los candidatos que promueven los debates",
       subtitle = "Promedio por elección, a través el tiempo, por países",
       caption = "Elaboración propia")  

# qué vemos?
# tendencia ascendente a presencia del público, 
# pero solo en países de larga tradición
# ídem con tendencia a mayor competencia entre candidatos
# nota/ pendiente = hacer más lindos estos gráficos. 
# quizás haya otra medida más pertinente
# u otra forma de mostrar esta info de manera más relevante


# Temas ######

# procedemos de manera parecida a lo anterior 
# (y lo mismo haremos con categorías siguientes)
# preparamos datos. Calcularemos la "proporción" de tipos de organización temática que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de tema sobre el total de diferentes tipos de temas por debate
# NO calcula esta proporción con base en el carácter de cada uno de los bloques o segmentos que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate

tipos_temas <- base %>% # primero algunas transformaciones de los datos
  # no nos salió hacerlo automáticamente, procedemos por categoría
  # mutate_at(vars(contains('dico_foramto')), as.logical)
  mutate( dico_temas_puntuales = as.logical(dico_temas_puntuales),
          dico_temas_bloques = as.logical(dico_temas_bloques),
          dico_temas_libre = as.logical(dico_temas_libre),
          dico_temas_monotema = as.logical(dico_temas_monotema),
          n_cattemas =  
            dico_temas_puntuales +
            dico_temas_bloques +
            dico_temas_libre +
            dico_temas_monotema)  %>% 
  mutate(  # ahora sí calculamos proporciones
    pr_puntuales = dico_temas_puntuales/n_cattemas,
    pr_bloques = dico_temas_bloques/n_cattemas,
    pr_libre = dico_temas_libre/n_cattemas,
    pr_monotema = dico_temas_monotema/n_cattemas
  )
# nota/ pendiente: ver manera de hacer esto más automático

# cálculo sumarizado por país y elección
tipos_temas_año_pais <- tipos_temas %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_puntuales = sum(pr_puntuales),
            n_bloques = sum(pr_bloques),
            n_libre = sum(pr_libre),
            n_monotema = sum(pr_monotema),
            tot_debates_año_pais = n())

# pasamos data a long
tipos_temas_año_pais <-  tipos_temas_año_pais %>% 
  dplyr::rename(cat_eleccion = ncat_eleccion) %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_tema",
               values_to = "tema_peso")

# graficamos, ídem antes

# para comparar entre paises
tipos_temas_ev_anual_pais <- tipos_temas_año_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  subset( tema_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_tema, colour = tipo_tema, size= tema_peso))  +
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
       y = "Tipo de tema",
       title = "Tipo de tema de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# comparacion entre paises pero sin el tiempo
# cálculo sumarizado por país 
tipos_temas_pais <- tipos_temas %>%
  group_by(cat_pais) %>% 
  summarise(n_puntuales = sum(pr_puntuales %>%  na.omit()),
            n_bloques = sum(pr_bloques %>%  na.omit()),
            n_libre = sum(pr_libre %>%  na.omit()),
            n_monotema = sum(pr_monotema %>%  na.omit()),
            tot_debates_año_pais = n()) %>% 
  pivot_longer(cols = starts_with("n"),
               names_to = "tipo_tema",
               values_to = "tema_peso") %>% 
  subset( tema_peso > 0 ) 

plottipos_temas_pais <- tipos_temas_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot(aes(tipo_tema,
             cat_pais, 
             colour = tipo_tema,
             size = tema_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  #scale_x_continuous(breaks = seq(1950,2021,10)) +
  #scale_color_manual(values=colors) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de disposición temática",
       title = "Tipo de disposición temátoca de los debates ",
       subtitle = "por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")


# para comparar dos etapas
tipos_tema_ev_anual <- tipos_temas_año_pais %>% 
  subset( tema_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_tema, colour = tipo_tema, size= tema_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
  labs(x = "",
       y = "Tipo de tema",
       title = "Tipo de tema de los debates ",
       subtitle = "A través el tiempo",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")  


# Organizador versus formato. vieja categorizacion de organizador #######

organizadores_formatos <- tipos_organizadores %>% 
  select( ncat_eleccion, cat_pais, t_fecha, str_organizador, 
          pr_mmc, pr_educ, pr_mmp, pr_estado, pr_osc) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_org",
               values_to = "org_peso") %>% 
  left_join( tipos_formatos %>%  
               select( ncat_eleccion, cat_pais, t_fecha, str_organizador, 
                       pr_apertura, pr_moderadores, pr_periodistas, 
                       pr_expertos, pr_sectores, pr_virtuales, pr_presentes,
                       pr_duelo, pr_libre, pr_expositivo) ) %>% 
  subset(org_peso!=0) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_formato",
               values_to = "formato_peso") %>% 
  subset(formato_peso!=0) 

organizadores_formatos <-  organizadores_formatos %>% 
  mutate( tipo_formato = as.factor(tipo_formato),
          tipo_formato =
            fct_relevel(tipo_formato, "pr_expositivo", "pr_libre", "pr_duelo", 
                        "pr_apertura", "pr_moderadores",
                        "pr_periodistas", 
                        "pr_expertos", 
                        "pr_sectores", "pr_virtuales", "pr_presentes") )


# distintos gráficos

# de tipo count
# este no
organizadores_formatos_plot <- organizadores_formatos %>% 
  group_by(cat_pais, tipo_org, tipo_formato) %>% 
  summarise( total_combinacion = n()) %>% 
  ggplot( aes( tipo_org, tipo_formato , size = total_combinacion, colour = tipo_org)) +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) 

# prefiero este
organizadores_formatos_plot2 <- organizadores_formatos %>% 
  ggplot() +
  geom_count(aes(tipo_formato, tipo_org, colour = tipo_formato, shape = tipo_org)) +
  facet_wrap( ~ cat_pais, ncol= 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Formato",
       y = "Organizador de los debates",
       title = "Formatos preferidos por tipo de organizador",
       subtitle = "por país",
       caption = "Elaboración propia. 
       El tamaño del símbolo es proporcional a la cantidad de debates hechos")

# probamos visualizar con sets paralelos

# v1 dos variables
organizadores_formatos_subset <- organizadores_formatos  %>% 
  select(tipo_org, tipo_formato) %>%
  dplyr::count(tipo_org, tipo_formato) %>% 
  gather_set_data(1:2)

organizadores_formatos_parallel <- ggplot(organizadores_formatos_subset,
                                          aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = tipo_org), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# segunda version sets paralelos. Incluimos variable país
organizadores_formatos_subset2 <- organizadores_formatos  %>% 
  select(cat_pais, tipo_org, tipo_formato) %>%
  dplyr::count(cat_pais, tipo_org, tipo_formato) %>% 
  gather_set_data(1:3)

organizadores_formatos_parallel2 <- ggplot(organizadores_formatos_subset2,
                                           aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = tipo_formato), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))



# relaciones entre organizadores y disposición temática de los debates ##########


organizadores_temas <- tipos_organizadores %>% 
  select( ncat_eleccion, cat_pais, t_fecha, str_organizador, 
          pr_mmc, pr_educ, pr_mmp, pr_estado, pr_osc) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_org",
               values_to = "org_peso") %>% 
  left_join( tipos_temas %>%  
               select( ncat_eleccion, cat_pais, t_fecha, str_organizador, 
                       pr_puntuales, pr_bloques, pr_monotema, pr_libre) ) %>% 
  subset(org_peso!=0) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_tema",
               values_to = "tema_peso") %>% 
  subset(tema_peso!=0)


# gráfico count
# este me gustó
organizadores_temas_plot <- organizadores_temas %>% 
  ggplot() +
  geom_count(aes(cat_pais, tipo_tema, colour = cat_pais)) +
  facet_wrap( ~ tipo_org, ncol= 5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "País",
       y = "Tipo de organización temática",
       title = "Estructuración temática preferida por tipo de organizador",
       subtitle = "por país",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# que vemos?
# 1- Las Osc son las que mas han propuesto debates monotemáticos.
# Esto lo han hecho en colaboración con istituciones educativas y medios
# Por otra parte, las osc también han interrogado con temas puntuales.
# Muy rara vez han dejado el tema librado a la voluntad de los candidatos.
# 2- Quienes más han dejado el tema librado a la voluntad de los candidatos han sido los medios,
# pero sólo en ciertos países, en los que también han interrogado mucho con temas puntuales. 
# 3- Los Estados, sobre todo en algunos países, han preferido la organización por bloques. 
# 4- No hay un patrón claro para las instituciones educativas, 
# aunque si se observan diferencias entre dos grupos de países: 
# aquéllos en los que el debate se divide por bloques, y otros en los que hay diversidad de formatos
# esto parece replicarse para todos los tipos de formato, de hecho, de modo que:
# 5- En algunos países hay más variedad de organización temática que en otros.

# gráfico de sets paralelos, para probar

# v1 dos variables / set temas-organziadores
organizadores_temas_subset <- organizadores_temas %>% 
  select(tipo_tema, tipo_org) %>%
  dplyr::count(tipo_org, tipo_tema) %>% 
  gather_set_data(1:2)

organizadores_temas_parallel <- ggplot(organizadores_temas_subset,
                                       aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = tipo_org), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# segunda version sets paralelos. Incluimos variable país-temas-org
organizadores_temas_subset2 <- organizadores_temas  %>% 
  select(cat_pais, tipo_org, tipo_tema) %>%
  dplyr::count(cat_pais, tipo_org, tipo_tema) %>% 
  gather_set_data(1:3)

organizadores_temas_parallel2 <- ggplot(organizadores_temas_subset2,
                                        aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# pruebas con nueva base subtipos. borradores ####

# esto que sigue no me gusto
# cuenta_formatos_osc_grouped <- formatos_osc %>% 
#               select(-c(ncat_subtipov2,nombres_organizadores,ncat_tipoorg_ambito,
#                         ncat_tipoorg_visibilidad,cat_areaorg)) %>% 
#   distinct()
# 
# cuenta_formatos_osc_grouped2 <- cuenta_formatos_osc_grouped  %>% # primero algunas transformaciones de los datos
#   # no nos salió hacerlo automáticamente, procedemos por categoría
#   # mutate_at(vars(contains('dico_foramto')), as.logical)
#   mutate( dico_formato_apertura = as.logical(dico_formato_apertura),
#           dico_formato_duelo = as.logical(dico_formato_duelo),
#           dico_formato_expertos = as.logical(dico_formato_expertos),
#           dico_formato_libre = as.logical(dico_formato_libre),
#           dico_formato_moderadores = as.logical(dico_formato_moderadores),
#           dico_formato_periodistas = as.logical(dico_formato_periodistas),
#           dico_formato_presentes = as.logical(dico_formato_presentes),
#           dico_formato_sectores = as.logical(dico_formato_sectores),
#           dico_formato_virtuales = as.logical(dico_formato_virtuales),
#           dico_formato_expositivo = str_detect(cat_formato, "expositivo"), # ups me había faltado contabilizar
#           n_catformatos = dico_formato_apertura +
#             dico_formato_duelo +
#             dico_formato_expertos +
#             dico_formato_libre +
#             dico_formato_moderadores + 
#             dico_formato_periodistas + 
#             dico_formato_presentes +
#             dico_formato_sectores + 
#             dico_formato_virtuales +
#             dico_formato_expositivo)  %>% 
#   mutate(  # ahora sí calculamos proporciones
#     pr_apertura = dico_formato_apertura/n_catformatos,
#     pr_duelo = dico_formato_duelo/n_catformatos,
#     pr_expertos =   dico_formato_expertos/n_catformatos,
#     pr_libre =   dico_formato_libre/n_catformatos,
#     pr_moderadores =   dico_formato_moderadores/n_catformatos,
#     pr_periodistas =   dico_formato_periodistas/n_catformatos, 
#     pr_presentes =   dico_formato_presentes/n_catformatos,
#     pr_sectores =   dico_formato_sectores/n_catformatos, 
#     pr_virtuales =   dico_formato_virtuales/n_catformatos,
#     pr_expositivo =   dico_formato_expositivo/n_catformatos ) %>% 
#   pivot_longer(cols = starts_with("pr_"),
#                names_to = "tipo_formato",
#                values_to = "formato_peso")  %>%   
#   subset( formato_peso > 0.0 )
# 
# cuenta_formatos_osc_grouped3 <- cuenta_formatos_osc_grouped2  %>%   
#     subset( formato_peso > 0.0 )

# # grafico por pais
# tipos_formatos_ev_anual_pais <- cuenta_formatos_osc_grouped3  %>% 
#   mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom."))  %>% 
#   ggplot(aes(ncat_eleccion,  fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
#                                         "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
#                                         "n_expositivo", "n_duelo", "n_libre"), 
#              colour = fct_relevel(tipo_formato, "n_presentes", "n_virtuales", "n_sectores",
#                                   "n_expertos", "n_moderadores","n_periodistas", "n_apertura",
#                                   "n_expositivo", "n_duelo", "n_libre"), size= formato_peso))  +
#   geom_point() +
#   facet_wrap( ~ cat_pais, ncol = 6) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 90)) +
#   scale_x_continuous(breaks = seq(1950,2021,10)) +
#   scale_color_manual(values=colors) +
#   #scale_y_discrete(labels = c("Educativo","Estado", "M. Comerciales", "M. Públicos","OSCs" )) +
#   labs(x = "",
#        y = "Tipo de formato",
#        title = "Tipo de formato de los debates ",
#        subtitle = "A través el tiempo, por país",
#        caption = "Elaboración propia. 
#        El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")
# 
# pruebas cons subtipos. vigentes ####


base_subtipos_formatos <- base_subtipos %>% 
  left_join(base_id %>% 
              select(id_debate, cat_temas, cat_formato,
                     ncat_ppac, ncat_competencia,
                     dico_analisis,	dico_analytics,	dico_ddreplica,	
                     dico_comentarios,	dico_inserts,	cat_panel,	ncat_panel,
                     dico_formato_apertura,	dico_formato_libre,	dico_formato_duelo,	dico_formato_moderadores,	dico_formato_periodistas,	dico_formato_sectores,	dico_formato_expertos,	dico_formato_virtuales,	dico_formato_presentes,
                     dico_temas_puntuales,	dico_temas_libre,	dico_temas_monotema,	dico_temas_bloques
                     
              )) %>% # primero algunas transformaciones de los datos
  # no nos salió hacerlo automáticamente, procedemos por categoría
  # mutate_at(vars(contains('dico_foramto')), as.logical)
  mutate( dico_formato_apertura = as.logical(dico_formato_apertura),
          dico_formato_duelo = as.logical(dico_formato_duelo),
          dico_formato_expertos = as.logical(dico_formato_expertos),
          dico_formato_libre = as.logical(dico_formato_libre),
          dico_formato_moderadores = as.logical(dico_formato_moderadores),
          dico_formato_periodistas = as.logical(dico_formato_periodistas),
          dico_formato_presentes = as.logical(dico_formato_presentes),
          dico_formato_sectores = as.logical(dico_formato_sectores),
          dico_formato_virtuales = as.logical(dico_formato_virtuales),
          dico_formato_expositivo = str_detect(cat_formato, "expositivo"), # ups me había faltado contabilizar
          n_catformatos = dico_formato_apertura +
            dico_formato_duelo +
            dico_formato_expertos +
            dico_formato_libre +
            dico_formato_moderadores + 
            dico_formato_periodistas + 
            dico_formato_presentes +
            dico_formato_sectores + 
            dico_formato_virtuales +
            dico_formato_expositivo)  %>% 
  mutate(  # ahora sí calculamos proporciones
    pr_apertura = dico_formato_apertura/n_catformatos,
    pr_duelo = dico_formato_duelo/n_catformatos,
    pr_expertos =   dico_formato_expertos/n_catformatos,
    pr_libre =   dico_formato_libre/n_catformatos,
    pr_moderadores =   dico_formato_moderadores/n_catformatos,
    pr_periodistas =   dico_formato_periodistas/n_catformatos, 
    pr_presentes =   dico_formato_presentes/n_catformatos,
    pr_sectores =   dico_formato_sectores/n_catformatos, 
    pr_virtuales =   dico_formato_virtuales/n_catformatos,
    pr_expositivo =   dico_formato_expositivo/n_catformatos ) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_formato",
               values_to = "formato_peso")  %>%   
  subset( formato_peso > 0.0 )

# solo osc

formatos_osc <- base_subtipos_formatos %>% 
  filter(cat_tipoorgv2 == "osc")

formatos_osc_grouped <- formatos_osc %>% 
  filter(cat_tipoorgv2 == "osc") %>% 
  select(-c(ncat_subtipov2,nombres_organizadores,ncat_tipoorg_ambito,
                                       ncat_tipoorg_visibilidad,cat_areaorg)) %>% 
  distinct()

paralel_osc <- formatos_osc_grouped %>% 
  group_by(cat_pais, tipo_formato) %>% 
  summarise(n =sum(formato_peso)) %>% 
  gather_set_data(1:2)

formatos_paralel_osc <- paralel_osc %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# paralel de estado


formatos_estado <- base_subtipos_formatos %>% 
  filter(cat_tipoorgv2 == "estado")

formatos_estado_grouped <- formatos_estado %>% 
  filter(cat_tipoorgv2 == "estado") %>% 
  select(-c(ncat_subtipov2,nombres_organizadores,ncat_tipoorg_ambito,
            ncat_tipoorg_visibilidad,cat_areaorg)) %>% 
  distinct()

paralel_estado <- formatos_estado_grouped %>% 
  group_by(cat_pais, tipo_formato) %>% 
  summarise(n =sum(formato_peso)) %>% 
  gather_set_data(1:2)

formatos_paralel_estado <- paralel_estado %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))




# simple grafico de barras. es el que mas me cierra POR AHORA

dfbar_formato_tipo <- base_subtipos_formatos %>% 
  group_by(cat_tipoorgv2, tipo_formato) %>% 
  summarise(n =sum(formato_peso)) 

#intento de hacer df ponderado. revisar
dfbar_formato_tipo_ponderado <- base_subtipos_formatos %>% 
  group_by(cat_tipoorgv2, tipo_formato) %>% 
  summarise(pr_formatos =sum(formato_peso), 
            n_debates = n_distinct(id_debate),
            n = pr_formatos/n_debates) 

bar_formato_tipo <- dfbar_formato_tipo %>% 
  ggplot() +
  geom_col(aes(tipo_formato, n, fill=cat_tipoorgv2 ),
           position = "dodge") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) 
# efectivamente, de los debates con sectores, la gran mayoria son con participacion de osc.
# ojo que ni este grafico ni el q sigue estan ponderados en el tipo de organizador por debate. o si??

# replicamos para temas

base_subtipos_temas <- base_subtipos %>% 
  left_join(base_id %>% 
              select(id_debate, cat_temas, cat_formato,
                     ncat_ppac, ncat_competencia,
                     dico_analisis,	dico_analytics,	dico_ddreplica,	
                     dico_comentarios,	dico_inserts,	cat_panel,	ncat_panel,
                     dico_formato_apertura,	dico_formato_libre,	dico_formato_duelo,	dico_formato_moderadores,	dico_formato_periodistas,	dico_formato_sectores,	dico_formato_expertos,	dico_formato_virtuales,	dico_formato_presentes,
                     dico_temas_puntuales,	dico_temas_libre,	dico_temas_monotema,	dico_temas_bloques
                     
              )) %>%
 # primero algunas transformaciones de los datos
  # no nos salió hacerlo automáticamente, procedemos por categoría
  # mutate_at(vars(contains('dico_foramto')), as.logical)
  mutate( dico_temas_puntuales = as.logical(dico_temas_puntuales),
          dico_temas_bloques = as.logical(dico_temas_bloques),
          dico_temas_libre = as.logical(dico_temas_libre),
          dico_temas_monotema = as.logical(dico_temas_monotema),
          n_cattemas =  
            dico_temas_puntuales +
            dico_temas_bloques +
            dico_temas_libre +
            dico_temas_monotema)  %>% 
  mutate(  # ahora sí calculamos proporciones
    pr_puntuales = dico_temas_puntuales/n_cattemas,
    pr_bloques = dico_temas_bloques/n_cattemas,
    pr_libre = dico_temas_libre/n_cattemas,
    pr_monotema = dico_temas_monotema/n_cattemas
  ) %>% 
# nota/ pendiente: ver manera de hacer esto más automático
# pasamos data a long  
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_tema",
               values_to = "tema_peso")  %>%   
  subset( tema_peso > 0.0 )

# simple grafico de barras

dfbar_tema_tipo <- base_subtipos_temas %>% 
  group_by(cat_tipoorgv2, tipo_tema) %>% 
  summarise(n =sum(tema_peso)) 

bar_tema_tipo <- dfbar_tema_tipo %>% 
  ggplot() +
  geom_col(aes(tipo_tema, n, fill=cat_tipoorgv2 ),
           position = "dodge") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) 
# y aca se ve lo del monotema :)
# ojo que ni este grafico ni el anterior estan ponderados en el tipo de organizador por debate. o si??


# solo monotemas

base_subtipos_monotemas <- base_subtipos_temas %>% 
  filter(tipo_tema=="pr_monotema")

cuenta_base_subtipos_monotemas <- base_subtipos_monotemas %>% 
  group_by(id_debate) %>% 
  mutate(n_organizadoresxdebate = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  mutate(n_contribucionorganizadorxdebate = 1/n_organizadoresxdebate) 

dfparalel_monotemas <- cuenta_base_subtipos_monotemas %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  summarise(n =sum(n_contribucionorganizadorxdebate)) %>% 
  gather_set_data(1:2)

plot_paralel_monotema <- dfparalel_monotemas %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# # solo bloques Y osc

base_subtipos_bloques <- base_subtipos_temas %>% 
  filter(tipo_tema=="pr_bloques")

cuenta_base_subtipos_bloques <- base_subtipos_bloques %>% 
  group_by(id_debate) %>% 
  mutate(n_organizadoresxdebate = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  mutate(n_contribucionorganizadorxdebate = 1/n_organizadoresxdebate) 


cuenta_base_subtipos_bloques_osc <- cuenta_base_subtipos_bloques %>% 
  filter(cat_tipoorgv2=="osc")

dfparalel_bloques_osc <- cuenta_base_subtipos_bloques_osc %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  summarise(n =sum(n_contribucionorganizadorxdebate)) %>% 
  gather_set_data(1:2)

plot_paralel_bloques_osc <- dfparalel_bloques_osc %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# # solo sectores

base_subtipos_sectores <- base_subtipos_formatos %>% 
  filter(tipo_formato=="pr_sectores")

cuenta_base_subtipos_sectores <- base_subtipos_sectores %>% 
  group_by(id_debate) %>% 
  mutate(n_organizadoresxdebate = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  mutate(n_contribucionorganizadorxdebate = 1/n_organizadoresxdebate) 

dfparalel_sectores <- cuenta_base_subtipos_sectores %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  summarise(n =sum(n_contribucionorganizadorxdebate)) %>% 
  gather_set_data(1:2)

plot_paralel_sectores <- dfparalel_sectores %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# solo publico 


base_subtipos_publico <- base_subtipos_formatos %>% 
  filter(tipo_formato=="pr_presentes")

cuenta_base_subtipos_publico <- base_subtipos_publico %>% 
  group_by(id_debate) %>% 
  mutate(n_organizadoresxdebate = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  mutate(n_contribucionorganizadorxdebate = 1/n_organizadoresxdebate) 

dfparalel_publico <- cuenta_base_subtipos_publico %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  summarise(n =sum(n_contribucionorganizadorxdebate)) %>% 
  gather_set_data(1:2)

plot_paralel_publico <- dfparalel_publico %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


base_subtipos_publico2 <- base_subtipos_formatos %>% 
  filter(tipo_formato=="pr_virtuales")

cuenta_base_subtipos_publico2 <- base_subtipos_publico2 %>% 
  group_by(id_debate) %>% 
  mutate(n_organizadoresxdebate = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  mutate(n_contribucionorganizadorxdebate = 1/n_organizadoresxdebate) 

dfparalel_publico2 <- cuenta_base_subtipos_publico2 %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  summarise(n =sum(n_contribucionorganizadorxdebate)) %>% 
  gather_set_data(1:2)

plot_paralel_publico2 <- dfparalel_publico2 %>% 
  ggplot(aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = cat_pais), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# Cuenta cantidad de formatos por debate a través del tiempo######

plot_formatos_tiempo <- tipos_formatos %>% 
  ggplot(aes(ncat_eleccion, n_catformatos)) +
  geom_point(aes(colour= cat_pais)) +
  theme_minimal() +
  theme(legend.position = "none") 

