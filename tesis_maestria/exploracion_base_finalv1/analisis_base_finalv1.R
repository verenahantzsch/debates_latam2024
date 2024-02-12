# LIBRERIAS #####
library(tidyverse)
library(ggforce)
library(data.table)

# Importación de datos #####

base <- readxl::read_xlsx("base_finalv1.xlsx") # base con datos por debate
elecciones <-  readxl::read_xlsx("base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país

# Evolución temporal simple ####

# nota: trabajado previamente en "exploracion_tipo_organizadores.R"

debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n())

base_años <- elecciones %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais),
          n_debates_año_pais = replace_na(n_debates_año_pais, 0) )

## evolucion de realizacion global 
ev_temporal <- ggplot(base_años , 
                      aes(ncat_eleccion, n_debates_año_pais))  +
  geom_col() +
  theme_minimal()

# evolucion temporal de realizacion por paises 
cols_18 <- c("#00A5E3","#8DD7BF","#FF96C5","#FF5768", "#FFBF65",
             "#6C88C4","#E77577", "#F2D4CC", "#FFD872", "#FC6238",
             "#00CDAC","#FF6F68", "#FFEC59","#FF60A8","#CFF800",
             "#74737A", "#00B0BA", "#C05780")

plot_point_anual <- ggplot(base_años %>% 
                             mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")), 
                           aes(ncat_eleccion, cat_pais , size= n_debates_año_pais, colour = cat_pais, shape = debates_dico))  +
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

# qué vemos? 
# 1- A medida que pasa el tiempo se hacen más debates en la región
# 2- Se identifica claramente un grupo de países que en la actualidad hace muchos debates
# y que en general hizo más debates que el resto
# 3- Se ven algunos paises con trayectorias interrumpidas y/o irregulares

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


# subtipos de organizadores #####
# TRABAJADO EN ARCHIVO APARTE

# Formatos. copio a nuevo archivo analisis_formatos #####

# procedemos de manera parecida a lo anterior 
# (y lo mismo haremos con categorías siguientes)
# preparamos datos. Calcularemos la "proporción" de formatos que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de formato sobre el total de diferentes tipos de formatos por debate
# NO calcula esta proporción con base en el carácter de cada uno de los bloques o segmentos que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate

tipos_formatos <- base %>% # primero algunas transformaciones de los datos
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

# me gusta este para comparar entre paises
tipos_formatos_ev_anual_pais <- tipos_formatos_año_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  subset( formato_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_formato, colour = tipo_formato, size= formato_peso))  +
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

# me gusta este para comparar dos etapas
tipos_formatos_ev_anual <- tipos_formatos_año_pais %>% 
  subset( formato_peso > 0 ) %>% 
  ggplot(aes(cat_eleccion, tipo_formato, colour = tipo_formato, size= formato_peso))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
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

# Evolución de grado de competencia entre candidatos a través del tiempo

competencia_candidatos <- base %>% 
  mutate(ncat_competencia = as.numeric(ncat_competencia)) %>% 
  select(ncat_eleccion, cat_pais, ncat_competencia)

competencia_candidatos_annualmean <- competencia_candidatos %>% 
  na.omit() %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(promedio_competencia = mean(ncat_competencia))


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


# Evolución de grado de presencia del público a través del tiempo

presencia_publico <- base %>% 
  mutate(ncat_ppac = as.numeric(ncat_ppac)) %>% 
  select(ncat_eleccion, cat_pais, ncat_ppac)

presencia_publico_annualmean <- presencia_publico %>% 
  na.omit() %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(promedio_presencia = mean(ncat_ppac))


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


# Temas. copio a nuevo archivo analisis_formatos ######

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


# Normativa #####


# en este caso el cálculo es más fácil dado que las categorías son exlcuyentes, 
# y dado que la unidad de análisis es la elección

# cargamos data pre-preparada en base aparte, que tiene los mismos años que base_elecciones
# y las mismas variables sobre normativa que en la base_final están computados por debate
data_normativa <- readxl::read_xlsx("base_normativa.xlsx")

# agregamos conteo de cantidad de debates hechos en un año dado
data_normativa <- data_normativa %>% 
  left_join( base_id %>%  
               group_by(ncat_eleccion, cat_pais) %>% 
               summarise(cantidad_debates_realizados = n()) ) %>% 
  mutate( cantidad_debates_realizados = cantidad_debates_realizados %>% replace_na(0))

# graficamos, ídem antes

# desde el punto de vista de los candidatos
# para comparar entre paises
tipos_norma_candidatos_ev_anual_pais <- data_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot()  +
  geom_point(aes(ncat_eleccion, fct_relevel(cat_regcandidatos,
                                            "NADA", "POSIBILIDAD", "GARANTÍAS", "OBLIGACION"),
                 colour = cat_regcandidatos)) +
    facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Espíritu de la norma",
       title = "Regulación de los debates",
       subtitle = "A través el tiempo, por país, 
      desde el punto de vista de los candidatos",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado
       De todos modos, se muestran también los años sin debates (el círculo es más pequeño)")

# qué vemos?
# 1- Las regulaciones aparecen sólo a partir de los 2000, aprox
# 2- Los países con obligatoriedad par alos candidatos son muy recientes
# (Uruguay, Argentina, Ecuador)
# 3- Los países de larga tradición de debates, en general, han tendido a regular
# con la salvedad de Guatemala y Chile

# replicamos el ejercicio para la regulación desde el punto de vista del Estado
tipos_norma_estado_ev_anual_pais <- data_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot(aes(ncat_eleccion, fct_relevel(cat_regestado, "NADA", "POSIBILIDAD", "FISCALIZAR", "GARANTIZAR", "ORGANIZAR"),
                                           colour = cat_regestado, size = cantidad_debates_realizados))  +
  geom_point() +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Espíritu de la norma",
       title = "Regulación de los debates",
       subtitle = "A través el tiempo, por país, 
       desde el punto de vista del propio Estado",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado.
       El color indica la normativa desde el punto de vista de los candidatos")

# qué vemos? 
# 1- con este gráfico se distinguen dos subgrupos dentro de los debates que ofrecen garantías a los candidatos
# En concreto, México y Costa Rica hacen la misma evolución de fiscalizar-> a organizar

# desde el punto de vista de los medios
# para comparar entre paises
tipos_norma_medios_ev_anual_pais <- data_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot()  +
  geom_point(aes(ncat_eleccion, fct_relevel(cat_regmedios,
                                            "NADA", "POSIBILIDAD", "OPORTUNIDAD", "LIMITACIONES", "OBLIGACIONES"),
                 colour = cat_regmedios)) +
  facet_wrap( ~ cat_pais, ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Espíritu de la norma",
       title = "Regulación de los debates",
       subtitle = "A través el tiempo, por país, 
       desde el punto de vista de los medios de comunicación",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado
       De todos modos, se muestran también los años sin debates (el círculo es más pequeño)")


# prueba de normativa con label y sin facet. no me gusta ######

plot_normativa1b <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_label(aes(ncat_eleccion,
                 fct_relevel(cat_regcandidatos,
                             "NADA", "POSIBILIDAD", "GARANTÍAS", "OBLIGACION"),
                 colour = cat_pais,
                 label = cat_pais)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  scale_color_manual(breaks = colorespais$cat_pais,
                     values= colorespais$cols_18) +
  labs(x = "",
       y = "",
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista de los candidatos,
       através el tiempo, por país",
       caption = "Elaboración propia")

# Algunos cruces visuales entre variables #### 

# Organizador versus formato. copio a nuevo archivo analisis_formatos #######

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



# relaciones entre organizadores y disposición temática de los debates. copio a nuevo archivo analisis_formato ##########


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

# Tres variables #####


organizadores_temas_formatos <- tipos_organizadores %>% 
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
  subset(tema_peso!=0) %>% 
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

# tercera version sets paralelos. Incluimos variable temas
organizadores_temas_formatos_subset3 <- organizadores_temas_formatos  %>% 
  select(tipo_org, tipo_formato, tipo_tema) %>%
  dplyr::count(tipo_org, tipo_formato, tipo_tema) %>% 
  gather_set_data(1:3)  #%>% 
#  mutate( tipo_formato = as.factor(tipo_formato), # no funca esto
#          tipo_formato =
#            fct_relevel(tipo_formato, "pr_expositivo", "pr_libre", "pr_duelo", 
#                        "pr_apertura", "pr_moderadores",
#                        "pr_periodistas", 
#                        "pr_expertos", 
#                        "pr_sectores", "pr_virtuales", "pr_presentes") )
#

organizadores_formatos_parallel3 <- ggplot(organizadores_temas_formatos_subset3,
                                           aes(x, id = id, split = y, value = n)) + #  INICIA GRAFICO
  geom_parallel_sets(aes(fill = tipo_org), alpha = 0.7, axis.width = 0.1, show.legend = FALSE) +
  theme_minimal() +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgray", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', angle= 0) +
  theme_no_axes() +
  theme(panel.background = element_rect(fill = "white", colour = "white"))


# cómo se comportan las osc? vieja cat ######

oscs <- organizadores_temas_formatos %>%  
  subset(tipo_org == "pr_osc")

plot_ocs <- oscs %>% 
  ggplot(aes(tipo_tema, tipo_formato)) +
  geom_count(aes(colour=cat_pais)) +
  facet_wrap( ~ cat_pais, ncol=5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Organización temática",
       y = "Formato",
       title = "Debates organizados por las Osc",
       subtitle = "por país, según clasificación temática y de tipos de interacción",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")


# cómo se comporta el Estado? vieja cat ######

estado <- organizadores_temas_formatos %>% 
  subset(tipo_org == "pr_estado") %>% 
  left_join(data_normativa) 

plot_estado <- estado %>% 
  ggplot(aes(tipo_tema, tipo_formato)) +
  geom_count(aes(colour=cat_regcandidatos)) +
  facet_wrap( ~ cat_pais, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Organización temática",
       y = "Formato",
       title = "Debates organizados por el Estado ",
       subtitle = "por país, según normativa, clasificación temática y de tipos de interacción",
       caption = "Elaboración propia. 
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado")

# cantidad de candidatos invitados  #########

debates_primeraronda <- base %>% 
  subset(ncat_ronda == 1)

plot_ncandidatos_t <- debates_primeraronda %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_proporcioninvitados, colour= cat_pais)) +
  theme_minimal() +
  theme(legend.position = "none")

# en Perú 2016 hubo 9 retiros. En Bolivia 2020 al menos uno. Colombia ídem 

datos_erroneos_n_proporcioncandidatos <- base %>% 
  subset(n_proporcioninvitados>1) %>%  
  select(cat_pais, ncat_eleccion, t_fecha, n_invitados, n_candidaturas, n_proporcioninvitados)

# para considerar según tipo de organizador 

debates_primeraronda_org <- tipos_organizadores  %>% 
  subset(ncat_ronda==1)  %>% 
  select( ncat_eleccion, cat_pais, t_fecha, str_organizador, 
          pr_mmc, pr_educ, pr_mmp, pr_estado, pr_osc, n_proporcioninvitados) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_org",
               values_to = "org_peso") %>% 
  mutate(tipo_org = as.factor(tipo_org)) %>% 
  subset(org_peso!=0)

# scatter temporal # no muy claro pero se ve la dispersión desde los mmc
plot_ncandidatos_torg <- debates_primeraronda_org  %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_proporcioninvitados, colour = tipo_org, shape = tipo_org)) +
  theme_minimal() +
  theme(#legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Proporción de candidatos invitados",
       title = "Proporción de candidatos invitados",
       subtitle = "Por tipo de organizador, a través del tiempo",
       caption = "Elaboración propia")

# me gustó
plot_ncandidatos_torg <- debates_primeraronda_org  %>% 
  ggplot() +
  geom_boxplot(aes(tipo_org, n_proporcioninvitados, fill = tipo_org)) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "",
       y = "Proporción de candidatos invitados",
       title = "Proporción de candidatos invitados",
       subtitle = "Por tipo de organizador",
       caption = "Elaboración propia")

# cantidad en primera versus cantidad en segunda ronda #####

primera_segunda_comparacion <- base %>% 
  dplyr::count(cat_pais, ncat_eleccion, ncat_ronda) %>% 
  subset(!is.na(ncat_ronda)) %>% 
  mutate(ncat_ronda = as.factor(ncat_ronda))

# me gustó, pero es engañoso: 
primera_segunda_comparacion_plot <- primera_segunda_comparacion  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(ncat_ronda, n, fill= ncat_ronda)) +
  facet_wrap( ~ cat_pais , ncol = 6)  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Ronda",
       y = "Cantidad de debates",
       title = "Cantidad de debates realizados por ronda",
       subtitle = "Por país",
       caption = "Elaboración propia")

# hechos por ronda en el tiempo

primera_segunda_comparacion_plot2 <- primera_segunda_comparacion  %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n, colour= ncat_ronda)) +
  #geom_smooth(aes(ncat_eleccion, n, colour= ncat_ronda)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Año",
       y = "Cantidad de debates",
       title = "Cantidad de debates realizados por ronda",
       subtitle = "En el tiempo",
       caption = "Elaboración propia")

# considerando cuantas rondas hubo

primera_segunda_comparacion <- primera_segunda_comparacion %>% 
  left_join(elecciones) %>% 
  mutate(cat_ballotage = as.factor(cat_ballotage))

# plot muy confuso
mosaicplot(
  table(primera_segunda_comparacion$ncat_ronda, 
                 primera_segunda_comparacion$cat_ballotage),
           color = TRUE,
           xlab = "¿Hubo debates en..?", # label for x-axis
           ylab = "¿Cuántas rondas hubo en la elección?" # label for y-axis
)

prop.table(  table(primera_segunda_comparacion$ncat_ronda, 
                   primera_segunda_comparacion$cat_ballotage))


# otra prueba

primera_segunda_comparacion_plot3 <- primera_segunda_comparacion  %>% 
  ggplot() +
  geom_boxplot(aes(ncat_ronda, n, fill= ncat_ronda)) +
  facet_wrap( ~ cat_ballotage , ncol = 2)  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Ronda",
       y = "Cantidad de debates",
       title = "Cantidad de debates realizados por ronda",
       subtitle = "En elecciones con una única versus con dos rondas",
       caption = "Elaboración propia")
# conclusión: en promedio se hacen más debates en primera ronda,
# aún en elecciones con dos rondas
# consideremos que la data parte de la cantidad de debates hechos, 
# y no de las rondas electorales per se

# debates sólo por streaming #####

solo_streaming_orgs <- tipos_organizadores  %>% 
  select(dico_streaming, ncat_eleccion, cat_pais, t_fecha, str_organizador, 
          pr_mmc, pr_educ, pr_mmp, pr_estado, pr_osc, n_proporcioninvitados, n_efcandidatos) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_org",
               values_to = "org_peso") %>% 
  mutate(tipo_org = as.factor(tipo_org)) %>% 
  subset(org_peso!=0) 

# cuantos son 

table(base$dico_streaming) # 27 TRUE, 6 en DUDA

# en términos absolutos / streaming

solo_straming_orgs_plot <- solo_streaming_orgs %>%  
  subset(dico_streaming==TRUE) %>% 
  ggplot() +
  geom_bar(aes(tipo_org))
# el de mmp corresponde al de Telecafé de Colombia que se canceló por disturbios

# urge ver esto en términos relativos / streaming

solo_streaming_cuenta <- solo_streaming_orgs %>% 
  count(dico_streaming, tipo_org) %>% 
  mutate(dico_streaming = replace_na(dico_streaming, FALSE)) %>% 
  subset(!str_detect(dico_streaming, "DUDA")) 

# ambas cosas absolutas / streaming
solo_streaming_cuenta_plot <- solo_streaming_cuenta %>% 
  ggplot() +
  geom_col(aes( n, tipo_org, fill= dico_streaming ), position = "stack")

# / streaming  relativa con fill (creo que puede ser más claro el siguiente, aunque se ve más lindo este)
solo_streaming_cuenta_plot2 <- solo_streaming_cuenta %>% 
  ggplot() +
  geom_col(aes( n, tipo_org, fill= dico_streaming ), position = "fill")

# / streaming solo medida relativa  (creo que puede ser más claro este, aunque se ve más lindo el anterior)
solo_streaming_cuenta_pivot <- solo_streaming_cuenta %>% 
  pivot_wider(names_from= dico_streaming, values_from=n ) %>% 
  rename( Si = "TRUE", No = "FALSE") %>% 
  mutate(proporcion_streaming = Si/No)

solo_streaming_cuenta_pivot_plot <- solo_streaming_cuenta_pivot %>% 
  arrange(proporcion_streaming) %>% 
  ggplot() +
  geom_col(aes(tipo_org, proporcion_streaming, fill= tipo_org))


# debates impugnados #############

impugnado_orgs <- tipos_organizadores  %>% 
  select(dico_impugnado, ncat_eleccion, cat_pais, t_fecha, str_organizador, 
         pr_mmc, pr_educ, pr_mmp, pr_estado, pr_osc, 
         n_proporcioninvitados, n_efcandidatos,
         cat_regcandidatos) %>% 
  pivot_longer(cols = starts_with("pr_"),
               names_to = "tipo_org",
               values_to = "org_peso") %>% 
  mutate(tipo_org = as.factor(tipo_org)) %>% 
  subset(org_peso!=0)  %>% 
  mutate(dico_impugnado = replace_na(dico_impugnado, FALSE)) 


# en términos absolutos / impugnados

impugnado_orgs_plot <- impugnado_orgs  %>%  
  subset(dico_impugnado==TRUE) %>% 
  ggplot() +
  geom_bar(aes(tipo_org, fill=tipo_org)) +
  theme_minimal() +
  theme(legend.position = "none")

# urge ver esto en términos relativos / impugnados

# preparacion de datos impugnado relativos

impugnado_orgs_cuenta <- impugnado_orgs %>% 
  count(dico_impugnado, tipo_org)

# graficos impugnados relativos

# ambas cosas absolutas /impugnado 
impugnado_orgs_plot <- impugnado_orgs_cuenta %>% 
  ggplot() +
  geom_col(aes( n, tipo_org, fill= dico_impugnado), position = "stack") +
  theme_minimal()
# vemos que igual la proporcion de debates impugnados de la que tenemos conocimiento es infima

# / impugnado   relativa con fill (creo que puede ser más claro el siguiente, aunque se ve más lindo este)

impugnado_orgs_plot2 <- impugnado_orgs_cuenta %>% 
  ggplot() +
  geom_col(aes( n, tipo_org, fill= dico_impugnado ), position = "fill") +
  theme_minimal()

# / impugnado  solo medida relativa  (creo que puede ser más claro este, aunque se ve más lindo el anterior)

impugnado_orgs_pivot <- impugnado_orgs_cuenta %>% 
  pivot_wider(names_from= dico_impugnado, values_from=n ) %>% 
  rename( Si = "TRUE", No = "FALSE") %>% 
  mutate(proporcion_impugnado = Si/No)

impugnado_orgs_pivot_plot <- impugnado_orgs_pivot %>% 
  arrange(proporcion_impugnado) %>% 
  ggplot() +
  geom_col(aes(tipo_org, proporcion_impugnado, fill= tipo_org)) +
  theme_minimal()
# interesante lo que se ve para mmp. revisar. 
# es uruguayo de primera vuelta 2019, solo dos candidatos, varios medios en colaboración

# impugnados en relación a proporcion de candidatos invitados

impugnado_orgs_npropcandidatos_plot <- impugnado_orgs %>% 
  ggplot() +
  geom_boxplot(aes(dico_impugnado, n_proporcioninvitados, fill= dico_impugnado)) +
  theme_minimal()
# vemos clara relación entre debates impugnados y menos de mitad de candidatos invitados

# impugnados en relación a normativa

impugnado_true <- impugnado_orgs %>% 
  subset(dico_impugnado==TRUE) 
 
impugnado_paises <- fct_count(as.factor(impugnado_true$cat_pais))

normativa_hoy <- data_normativa %>% 
  group_by(cat_pais) %>% 
  summarise(ncat_eleccion = max(ncat_eleccion)) %>% 
  left_join(data_normativa)

impugnado_paises_normativa <- impugnado_paises %>% 
  rename(cat_pais = f) %>% 
  left_join(normativa_hoy)

# no se ve clara relación aunque sí se ve que país más impugnado
# también es el que más extensamente reguló
# Se podría explorar esto mejor en relación a la variable temporal
# es decir al timing la normativa-las quejas y viceversa
  
# proporcion de invitados, impugnados, nef candidatos #########

impugnado_orgs_npropcandidatos_plot <- impugnado_orgs %>% 
  ggplot() +
  geom_point(aes(n_efcandidatos, n_proporcioninvitados, colour= dico_impugnado)) +
  theme_minimal()
# el 1.0 candidatos y TRUE imppugnado es un caso de la UCR de 2014 
# en el que los candidatos criticaron el orden de las jornadas 

# para saber total de debates impugnados de la base: 17
table(base$dico_impugnado)

# NEF  ##############

# feo
nef_pais <- base %>% 
  ggplot() +
  geom_point(aes(n_efcandidatos, n_proporcioninvitados, colour=cat_pais)) +
  theme_minimal()

# bastante feo tambien
nef_org <- impugnado_orgs %>% 
  ggplot() +
  geom_point(aes(n_efcandidatos, n_proporcioninvitados, colour=tipo_org, shape=dico_impugnado)) +
  theme_minimal()

# extraido de "explorando_relaciones_qof_base.R", revisado

# me quedo con estos

cantidad_debates_anual <- elecciones %>% 
  left_join( base %>% 
  count(cat_pais, ncat_eleccion) ) %>% 
    rename(cantidad_debates_anual = n) %>% 
    mutate( cantidad_debates_anual = replace_na(cantidad_debates_anual, 0),
            debate_dico = ifelse(cantidad_debates_anual==0, "No", "Si"))

qof_data <- read.csv("joined_base_qog_selection.csv") %>% 
  select(gol_enpres, pais, año_electoral) %>% 
  rename(cat_pais = pais, ncat_eleccion= año_electoral, n_efcandidatos = gol_enpres)

# este grafico esta lindo, se le pude sacar el parámetro size
num_cand_facet_pais <- cantidad_debates_anual  %>% 
  left_join(qof_data) %>% 
  mutate(pais = str_replace(cat_pais, "Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot(aes(ncat_eleccion, n_efcandidatos, size= cantidad_debates_anual, colour= debate_dico)) +
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
       caption = "Elaboración propia. 
       Nr efectivo de Candidatos es de Q. of Government") 

# version boxplot simple, me gusta más el anterior . (nef/ hubo o no debates)
num_cand_debates <- cantidad_debates_anual  %>% 
  left_join(qof_data) %>% 
  ggplot(aes(debate_dico, n_efcandidatos, fill= debate_dico)) +
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
       caption = "Elaboración propia. 
       Nr efectivo de Candidatos es de Q. of Government") 

# nef versus cantidad de debates

plot_nef_cantidad_debates <- cantidad_debates_anual  %>% 
  left_join(qof_data) %>% 
  ggplot(aes(cantidad_debates_anual, n_efcandidatos, fill= cat_pais)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position= "none",
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=11) ) +
  ylab("Número Efectivo de Candidatos") +
  xlab("") +
  labs(title = "Cantidad de debates por elección según Nr. efectivo de candidatos",
       subtitle = "América latina en su conjunto, todo el período bajo estudio",
       caption = "Elaboración propia. 
       Nr efectivo de Candidatos es de Q. of Government") 
# la relación es muy debil


# redes de relaciones incompleto, ppendiente empezar #######

# cantidad de organizadores en el tiempo, ppendiente empezar ####

# con este y próximo gráfico vemos claramente 
# que a medida que pasa el tiempo hay más alianzas y estas son más diversas
diversidad_org_t <- base %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_strorganizador, colour= n_catorganizador)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Cantidad de organizadores por debate",
       title = "Formación de alianzas para la organización de debates",
       subtitle = "A través el tiempo",
       caption = "Elaboración propia")


# diversidad de alianzas en el tiempo, ppendiente empezar ######


# con este y el gráfico anterior vemos claramente 
# que a medida que pasa el tiempo hay más alianzas y estas son más diversas
diversidad_org_t <- base %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion, n_catorganizador, colour= n_catorganizador)) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "Cantidad de tipos de organizadores por debate",
       title = "Diversidad en la organización de debates",
       subtitle = "A través el tiempo",
       caption = "Elaboración propia")


# ausencias ########

# creo base con un nombre de ausente por fila

base_ausentes <- base %>% 
  select(ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, 
         str_ausentes, n_ausentes, n_invitados, n_presentes, str_presentes) %>%
  mutate(nombres_ausentes = strsplit(str_ausentes, ";")) %>% 
  unnest(nombres_ausentes)

# guardo base en excel
#base_ausentes %>% 
 # writexl::write_xlsx("base_ausentes.xlsx")

# creo base para completar manualmente

#base_datosausentes <- base_ausentes %>% 
#  distinct(nombres_ausentes, ncat_eleccion, cat_pais)

# guardo base en excel, para su manipulacion manual
#base_datosausentes %>% 
#  writexl::write_xlsx("base_datosausentes.xlsx")

# leo base ya modificada

base_datosausentes <- readxl::read_xlsx("base_datosausentes.xlsx")

# uno datos a base de todos los ausentes

base_ausentes_added <- base_ausentes %>% 
  left_join(base_datosausentes)  # FALTA CHEQUEAR BOLIVIA 2002

# voy a necesitar cantidad de debates anuales, ademas, copio del primer apartado

base_años <- elecciones %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais))

# sumariso
base_ausencias <- base_ausentes_added %>% 
  group_by(ncat_eleccion, cat_pais, t_fecha, str_organizador) %>% 
  summarise( n_porcentajeausentes = sum(as.numeric(n_porcentajevotos)))

# uno a base general
base_ausencias <- base %>% 
  left_join(base_ausencias) %>% 
  mutate( n_proporcionausentes = as.numeric(n_ausentes)/as.numeric(n_invitados)) %>% 
  mutate( n_indexausentes = n_proporcionausentes*n_porcentajeausentes) %>% 
  left_join(base_años)

# quiero revisar la cantidad de debates con alguna ausencia
# en relacion a la cantidad de debates por eleccion

base_ausencias_primera <- base_ausencias %>% 
  filter(ncat_ronda== 1) %>% 
  mutate(dico_ausencias = ifelse(is.na(str_ausentes), 0, 1),
         n_indexausentes = ifelse(is.na(n_indexausentes), 0, n_indexausentes) ) %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  mutate(cantidad_debates_ronda = n(),
            cantidad_debates_ausencias = sum(dico_ausencias),
            proporcion_debates_ausencias = cantidad_debates_ausencias/cantidad_debates_ronda,
            suma_indexausentes = sum(n_indexausentes),
            mean_indexausentes = mean(n_indexausentes))

# graficamos los datos de nuestro indice.
grafico_indexausentes <- base_ausencias %>% 
  ggplot() +
  geom_label(aes(n_proporcionausentes, n_porcentajeausentes, label = cat_pais, vjust = -0.5, colour = cat_pais), size = 3) +
  geom_label(aes(n_proporcionausentes, n_porcentajeausentes, label = ncat_eleccion, vjust = 0.5, colour = cat_pais), size = 3) 

# VAMOS A PROBAR VARIAS VERSIONES DEL SIG GRAFICO
# CANTIDAD DE DEBATES VERSUS INDEX. 
#CANTIDAD DE DEBATES SE PUEDE PROBAR CON log(n_debates_año_pais), lo prefiero sin
grafico_ausentes2 <- base_ausencias %>% # PARA GRAFICAR EN RELACION AL TOTAL DE DEBATES DE ELECCION
  ggplot() +
  geom_label(aes(n_indexausentes, n_debates_año_pais, label = cat_pais, vjust = -0.5, colour = cat_pais), size = 3) +
  geom_label(aes(n_indexausentes, n_debates_año_pais, label = ncat_eleccion, vjust = 0.5, colour = cat_pais), size = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(0,80,10)) +
  scale_y_continuous(breaks = seq(0,30,2)) +
  labs(x = "", y = "",
       title = "Índice de ausencias",
       subtitle = "En relación a la cantidad de debates por elección",
       caption = "Elaboración propia.
       El índice de ausencias contempla el procentaje obtenido por los ausentes, 
       multiplicado por la proporcion de ausentes a los debates") 

# CANTIDAD DE DEBATES VERSUS VOTOS de ausentes # prefiero el anterior
grafico_ausentes3 <- base_ausencias %>% # PARA GRAFICAR EN RELACION AL TOTAL DE DEBATES DE ELECCION
  ggplot() +
  geom_label(aes(n_porcentajeausentes, n_debates_año_pais, label = cat_pais, vjust = -0.5, colour = cat_pais), size = 3) +
  geom_label(aes(n_porcentajeausentes, n_debates_año_pais, label = ncat_eleccion, vjust = 0.5, colour = cat_pais), size = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(0,80,10)) +
  scale_y_continuous(breaks = seq(0,30,2)) +
  labs(x = "", y = "",
       title = "Porcentaje de votos acumulados por ausencias",
       subtitle = "En relación a la cantidad de debates por elección",
       caption = "Elaboración propia.
       El índice de ausencias contempla el procentaje obtenido por los ausentes, 
       multiplicado por la proporcion de ausentes a los debates") 

# MAS GRAFICOS
# debates por anio , cantidad de debates con ausentes

plot_ausencias_anuales <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(cantidad_debates_ronda, proporcion_debates_ausencias, colour = cat_pais, label = cat_pais, vjust = -0.5)) +
  geom_label(aes(cantidad_debates_ronda, proporcion_debates_ausencias, colour = cat_pais, label = ncat_eleccion, vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "none")

# proporcion de debates con ausencias versus promedio de indice de ausencias

plot_ausencias_anuales_meanindex <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(mean_indexausentes, proporcion_debates_ausencias, colour = cat_pais, label = cat_pais, vjust = 0.2)) +
  geom_label(aes(mean_indexausentes, proporcion_debates_ausencias, colour = cat_pais, label = ncat_eleccion, vjust = 1.2)) +
  theme_minimal() +
  theme(legend.position = "none")

# cantidad de debates versus promedio indice ausencias
# ESTE ME GUSTO!!! # AGREGAR TITULOS!!!!!
plot_cantidad_anuales_meanindex <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cat_pais, label = cat_pais, vjust = 0)) +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cat_pais, label = ncat_eleccion, vjust = 1)) +
  theme_minimal() +
  theme(legend.position = "none")

# face wrap
# CREACION DE VARIABLES ORDINALES ####
# vamos a crear tablas para asignar manualmente un numero a una categoria. 
# son tablas de categorias distinct, digamos. de uniqe factors
# luego usaremos joins con las tablas ya cargadas. 

# normativa (tres variables diferentes)

#1
distinct_cat_regcandidatos <- base %>% 
  select(cat_regcandidatos) %>% 
  distinct()

distinct_cat_regcandidatos %>% 
  writexl::write_xlsx("distinct_cat_regcandidatos.xlsx")

#2
distinct_cat_regestado <- base %>% 
  select(cat_regestado) %>% 
  distinct()

distinct_cat_regestado %>% 
  writexl::write_xlsx("distinct_cat_regestado.xlsx")

#3
distinct_cat_regmedios <- base %>% 
  select(cat_regmedios) %>% 
  distinct()

distinct_cat_regmedios %>% 
  writexl::write_xlsx("distinct_cat_regmedios.xlsx")


# tipo de organizador

distinct_cat_subtipoorg <- readxl::read_xlsx("base_organizadores_distinct.xlsx") %>% 
  select(ncat_subtipov2) %>% 
  distinct()

distinct_cat_subtipoorg  %>% 
  writexl::write_xlsx("distinct_cat_subtipoorg.xlsx")