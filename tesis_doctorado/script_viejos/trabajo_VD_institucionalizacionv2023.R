# librerias
library(tidyverse)

# wd

setwd("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado")

# datos
base <- readxl::read_xlsx("./datav2023/base_final3v2023.xlsx")
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")
base_anual_full <- readxl::read_xlsx("./datav2023/base_anual_fullv2023.xlsx") #esta  contiene casos negativos

colorespais <- base %>% 
  distinct(cat_pais, cols_18)

### RUTINIZACION #####
# Rutinización. 
# antigüedad de la práctica: períodos electorales transcurridos desde el primer debate en los que se hicieron debates
# N debates en el país: total de debates hechos en la historia del país en total
# promedio de debates por elección en el país (incluyendo elecciones con 0 debates, desde cuando?)
# constancia de la práctica / interrupciones: elecciones sin debates desde el primer debate

rutinizacion1 <- base_anual %>% 
  group_by(cat_pais) %>% 
  # antiguedad
  summarise(n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
  # constancia
            n_interrupciones = n()-n_elecciones_con_debates) 

rutinizacion2 <- base %>% 
  group_by(cat_pais, ncat_eleccion)  %>% 
  mutate(n_debates_en_eleccion = n()) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  # N debates 
  summarise(n_debates_total = n(),
  # Promedio debates en elecciones que hubieron debates
            mean_debates_eleccion_c_debates = mean(n_debates_en_eleccion))
 
rutinizacion3 <- base_anual_full %>% 
  left_join( base %>%  
               group_by(cat_pais, ncat_eleccion) %>% 
               summarise(n_debates_en_eleccion = n())) %>% 
  mutate(n_debates_en_eleccion = ifelse(dico_hubo_debates==0,0,n_debates_en_eleccion)) %>% 
  # Promedio debates en elecciones contando todas las elecciones 
  group_by(cat_pais) %>% 
  summarise(n_debates_total = sum(n_debates_en_eleccion),
            mean_debates_eleccion_todas_elecciones = mean(n_debates_en_eleccion),
            prop_elecciones_c_debates_todas_elecciones = sum(n_debates_en_eleccion > 0)/n())

rutinizacion <- rutinizacion1 %>% 
  left_join(rutinizacion2) %>% 
  left_join(rutinizacion3)

### DIVERSIFICACION ####################

# diversidad de tipos de actores por eleccion: N tipos de actores que hacen debates por elección, promedio por país. 
# diversidad de tipos de actores por pais
# distintos actores: cuenta ?ponderada? de la cantidad de actores diferentes que participaron por eleccion
# diversidad de formatos por eleccion: N tipos de formatos de debates por elección, promedio por país. Además se puede hacer un indicador para cada una de las subdimensiones de los formatos, “interacciones” y “temáticas”
# diversidad de formatos en total
# desvío estándar en los tipos de debates y desvío estándar en los tipos de actores. algo parecido usé para el ejercicio de clustering en la tesis de maestría y no me gustó mucho. requiere la transformación ordinal de las variables categóricas en ordinales y se pierde mucha información. 
# innovación en los formatos: ratio de formatos nuevos sobre el total (en una dicotomización entre “nuevos” y “clásicos” del indicador categórico de formatos, según lo que indica la lit. Riesgo de falta de validez conceptual no mide directamente la “diversidad” de formatos). 

diversificacion1 <- base_organizadores %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_organizadores_por_eleccion = n_distinct(cat_tipoorgv2)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  # diversidad de tipos de actores por eleccion
  summarise(mean_n_tipos_organizador_por_eleccion = mean(n_tipos_organizadores_por_eleccion))

diversificacion2 <- base_organizadores %>% 
  group_by(cat_pais) %>% 
  summarise(n_tipos_organizadores_total = n_distinct(cat_tipoorgv2)) # REVISAR hay algo raro, el valor no debería exceder 5

table(base_organizadores %>% subset(cat_pais=="Costa Rica") %>%  select(cat_tipoorgv2)) # RARO
base_organizadores$cat_tipoorgv2 %>%  table()

diversificacion3 <- base_organizadores %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_organizadores_por_eleccion = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  # diversidad de actores
  summarise(mean_n_organizadores_por_eleccion = mean(n_organizadores_por_eleccion))

diversificacion4 <- base_formatos %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_formatos_por_eleccion = n_distinct(cat_tipo_formato)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  # diversidad de tipos de formatos por eleccion
  summarise(mean_n_tipos_formatos_por_eleccion = mean(n_tipos_formatos_por_eleccion))

diversificacion5 <- base_formatos %>% 
  group_by(cat_pais) %>% 
  # diversidad de tipos de formatos total
  summarise(n_tipos_formatos_por_eleccion = n_distinct(cat_tipo_formato)) 

diversificacion6 <- base_temas %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_temas_por_eleccion = n_distinct(cat_tipo_tema)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  # diversidad de tipos de temas por eleccion
  summarise(mean_n_tipos_temas_por_eleccion = mean(n_tipos_temas_por_eleccion))

diversificacion7 <- base_temas %>% 
  group_by(cat_pais) %>% 
  # diversidad de tipos de temas total
  summarise(n_tipos_temas_total = n_distinct(cat_tipo_tema)) 

diversificacion8 <- base_formatos %>% 
  mutate(formato_innovador = ifelse(cat_tipo_formato%in%c("pr_formatoperiodistas","pr_formatoduelo","pr_formatomoderadores","pr_formatoexpositivo"),0,1)) %>% 
  group_by(cat_pais) %>% 
  # Ratio de formatos "innovadores" sobre el total, según literatura. DUDA si incluir "expositivo" o no
  summarise(prop_formatos_innovadores = sum(formato_innovador)/n())
# control table(diversificacion8$cat_tipo_formato,diversificacion8$formato_innovador)

diversificacion <- diversificacion1 %>% 
  left_join(diversificacion2) %>% 
  left_join(diversificacion3) %>% 
  left_join(diversificacion4) %>% 
  left_join(diversificacion5) %>% 
  left_join(diversificacion6) %>% 
  left_join(diversificacion7) %>% 
  left_join(diversificacion8) 
  
#### FORMALIZACION #####################################################

# Formalización:
#   Antigüedad de la legislación sobre debates: N años o períodos electorales en los cuales ha habido alguna regulación en vigencia. Pregunta es desde cuando contar. Una posibilidad parecida es 
# ratio debates en años electorales sin regulación / debates en años electorales con regulación
# Máximo nivel de regulación alcanzada: a partir de transformación ordinal del indicador categórico de la regulación. Esta transformación es más intuitiva y válida que la transformación ordinal de los indicadores categóricos de organizadores y de formatos.
# Ratio debates en los que interviene el Estado / total de debates
# N elecciones en las que el Estado organizó algún debate
# N debates organizados por el Estado en el país en total (para estos tres últimos: hay algún problema de validez? la formalización comprende todo lo que hace el Estado o todo lo sancionado legalmente? o se trata quizás de dos sub sub dimensiones de la formalización? y otra pregunta es: los medios públicos se consideran como organizadores estatales?)

formalizacion1 <- base_anual_full %>% 
  mutate(dico_alguna_regulacion = ifelse(ncat_totreg>0,1,0),
         dico_debates_y_alguna_regulacion = ifelse(ncat_totreg>0&dico_hubo_debates>0,1,0)) %>% 
  group_by(cat_pais) %>% 
  # antiguedad: cantidad de elecciones con alguna regulacion
  summarise(n_elecciones_con_regulacion = sum(dico_alguna_regulacion),
  # ratio elecciones con regulacion / sin
    prop_elecciones_con_regulacion = sum(dico_alguna_regulacion)/n(),
   # ratio elecciones con debates regulados / elecciones con debates irregulados
  prop_debates_con_regulacion = sum(dico_debates_y_alguna_regulacion)/sum(dico_hubo_debates),  # OJO ESTE NOMBRE PUEDE SER CONFUSO
  # maximo nivel regulatorio alcanzado 
  maximo_nivel_regulatorio = max(ncat_totreg))

formalizacion2 <- base_organizadores %>% 
  mutate(dico_organizador_es_estado = ifelse(cat_tipoorgv2=="estado",1,0)) %>%  # no estoy contando medios públicos
  group_by(cat_pais) %>% 
  # debates org por el E sobre total de debates 
  summarise(prop_debates_org_por_estado = sum(dico_organizador_es_estado, na.rm = T)/n_distinct(id_debate),
  # N debates org por el E en total
            n_debates_org_por_estado_total = sum(dico_organizador_es_estado, na.rm = T))

formalizacion3 <- base_organizadores %>% 
  mutate(dico_organizador_es_estado = ifelse(cat_tipoorgv2=="estado",1,0)) %>%  # no estoy contando medios públicos
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(dico_eleccion_c_debate_org_por_estado = ifelse(sum(dico_organizador_es_estado, na.rm = T)>0,1,0)) %>% 
  group_by(cat_pais) %>% 
  # N elecciones con debates org por el Estado
  summarise(n_elecciones_c_debates_org_por_estado = sum(dico_eleccion_c_debate_org_por_estado, na.rm = T),
  # elecciones con debates org por el E sobre total de elecciones con debates 
            prop_elecciones_c_debates_org_por_estado = n_elecciones_c_debates_org_por_estado/n_distinct(ncat_eleccion))
   
formalizacion <- formalizacion1 %>% 
  left_join(formalizacion2) %>% 
  left_join(formalizacion3)


#### CORRELACIONES ENTRE INDICADORES DE MISMAS DIMENSIONES

cor_rutinizacion <- rutinizacion %>% 
  select(-cat_pais) %>% 
  cor( method = "pearson")

print(cor_rutinizacion)

# interrupciones menos correlacionado con los demas. tiene que ver con como estan construidos (unos toman como input a los otros)
# puede significar que es req incluir dos dimensiones distintas, tipo "constancia" y "antiguedad" o algo asi

cor_diversificacion <- diversificacion %>% 
  select(-cat_pais) %>% 
  cor( method = "pearson")

print(cor_diversificacion)

# solo formatos parece tener menos corr (< 0.7 o cerquita). puede ser por problemas de validez señalados anteriormente
# demas indicadores pueden ser sustituibles, hasta ahi. distincion actores, formatos

cor_formalizacion <- formalizacion %>% 
  select(-cat_pais) %>% 
  cor( method = "pearson")

print(cor_formalizacion)

# aca parece que ppac estatal y normativa definitivamente son diferentes. ver si es valido incluirlas a las dos.
# dentro de normativa, ademas, tipo de normativa y antiguedad definitivamente son diferentes


#### BASE AGREGADA #############################################

tot_indicadores <- diversificacion %>% 
  left_join(rutinizacion) %>% 
  left_join(formalizacion)

cor_tot_indicadores <- tot_indicadores %>% 
  select(-cat_pais) %>% 
  cor( method = "pearson")

print(cor_tot_indicadores)

#(b)
cormat <- round(cor(tot_indicadores %>% 
                      select(-cat_pais)), 2) #matriz de correlaciones con 2 decimales
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat) #me quedo con una matriz triangular superior
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE) #convierte la matriz en un dataframe
#genero el mapa de calor:
library(ggplot2)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))+
  coord_fixed()

############################################################################################################
############################################################################################################
############### PRUEBO HACER BASE AÑO A AÑO (INDICADORES POR ELECCION) #############################################################################################
########################################################################################################################################################################################################################
############################################################################################################


### Y_RUTINIZACION #####

y_rutinizacion1 <- base_anual %>% 
  mutate(dico_no_hubo_debates = ifelse(dico_hubo_debates==1,0,1)) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  # antiguedad
  mutate(n_elecciones_con_debates = cumsum(dico_hubo_debates),
            # constancia
            n_interrupciones = cumsum(dico_no_hubo_debates))

y_rutinizacion2 <- base %>% 
  group_by(cat_pais, ncat_eleccion)  %>% 
  arrange(ncat_eleccion) %>% 
  summarise(n_debates_en_eleccion = n()) %>% 
  group_by(cat_pais) %>% 
  # n debates
  mutate(n_debates_total = cumsum(n_debates_en_eleccion),
  # Promedio debates en elecciones que hubieron debates
            mean_debates_eleccion_c_debates = n_debates_total/row_number())

y_rutinizacion3 <- base_anual_full %>% 
  left_join( base %>%  
               group_by(cat_pais, ncat_eleccion) %>% 
               summarise(n_debates_en_eleccion = n())) %>% 
  mutate(n_debates_en_eleccion = ifelse(dico_hubo_debates==0,0,n_debates_en_eleccion)) %>% 
  # Promedio debates en elecciones contando todas las elecciones 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(hubo_debate_anterior_eleccion = lag(dico_hubo_debates), # NUEVA
         n_elecciones_con_debates_todas_elecciones = cumsum(dico_hubo_debates),
         hubo_debate_alguna_eleccion_anterior = ifelse(lag(n_elecciones_con_debates_todas_elecciones)>0,1,0), # NUEVA
         n_debates_total = cumsum(n_debates_en_eleccion),
         mean_debates_eleccion_todas_elecciones = n_debates_total/row_number(),
         prop_elecciones_c_debates_todas_elecciones = n_elecciones_con_debates_todas_elecciones/row_number())#,
         prop_elecciones_s_debates_desde_primero = ifelse(cat_primer_debate<=ncat_eleccion,)) # NUEVA

y_rutinizacion <- y_rutinizacion1 %>% 
  left_join(y_rutinizacion2) %>% 
  left_join(y_rutinizacion3)

# nuevas 
y_rutinizacion4 <- base_anual_full %>% 
  left_join( base %>%  
               group_by(cat_pais, ncat_eleccion) %>% 
               summarise(n_debates_en_eleccion = n())) %>% 
  mutate(n_debates_en_eleccion = ifelse(dico_hubo_debates==0,0,n_debates_en_eleccion)) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(n_elecciones_totales = row_number(),
         n_elecciones_con_debates_todas_elecciones = cumsum(dico_hubo_debates),
         prop_elecciones_c_debates_todas_elecciones = n_elecciones_con_debates_todas_elecciones/ n_elecciones_totales)

y_rutinizacion5 <- base_anual_full %>% 
  left_join( base %>%  
               group_by(cat_pais, ncat_eleccion) %>% 
               summarise(n_debates_en_eleccion = n())) %>% 
  mutate(n_debates_en_eleccion = ifelse(dico_hubo_debates==0,0,n_debates_en_eleccion)) %>% 
  left_join(base_anual %>% 
              mutate(n_interrupciones = ifelse(dico_hubo_debates==1,0,1)) ) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  mutate(n_elecciones_con_debates_todas_elecciones = cumsum(dico_hubo_debates),
         hubo_debate_alguna_eleccion_anterior = ifelse(lag(n_elecciones_con_debates_todas_elecciones)>0,1,0),
         dico_interrupcion = ifelse(hubo_debate_alguna_eleccion_anterior!=0&dico_hubo_debates==0,1,0),
         cadena_de_interrupciones = ifelse(dico_interrupcion==1, cumsum(dico_interrupcion), 0 ),
         cadena_de_interrupciones = ifelse( dico_interrupcion==1&is.na(cadena_de_interrupciones), 1, cadena_de_interrupciones),
         total_interrupciones = cumsum(dico_interrupcion),
         n_elecciones_totales = row_number(),
         max_interrupciones_posibles = ifelse(ncat_eleccion>=cat_primer_debate, n_elecciones_totales - min(n_elecciones_totales[ncat_eleccion==cat_primer_debate]) + 1, 0),
         tasa_interrupciones = n_interrupciones / max_interrupciones_posibles,
         tasa_interrupciones = ifelse(is.na(tasa_interrupciones, 0, tasa_interrupciones)))
       

y_rutinizacion <- y_rutinizacion4 %>% 
  left_join(y_rutinizacion5) %>% 
  mutate(nivel_instit = prop_elecciones_c_debates_todas_elecciones*(1-tasa_interrupciones),
         nivel_instit = ifelse(is.na(nivel_instit),0,nivel_instit))

rutinizacion <- y_rutinizacion %>% 
  group_by(cat_pais) %>% 
  summarise(nivel_instit = mean(nivel_instit)) # problema: me pensaliza paises democraticos hace mas tiempo # pos ponderar el simetrico de n_interrupciones

### Y_DIVERSIFICACION ####################

y_diversificacion1 <- base_organizadores %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_organizadores_por_eleccion = n_distinct(cat_tipoorgv2)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  # diversidad de tipos de actores por eleccion
  mutate(mean_n_tipos_organizador_por_eleccion = cumsum(n_tipos_organizadores_por_eleccion)/row_number())

# y_diversificacion2 carece de sentido en este contexto (total diversidad de actores total, el "por eleccion" esta capturado arriba)

y_diversificacion3 <- base_organizadores %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_organizadores_por_eleccion = n_distinct(nombres_organizadores)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  # diversidad de actores
  mutate(mean_n_organizadores_por_eleccion = cumsum(n_organizadores_por_eleccion)/row_number())

y_diversificacion4 <- base_formatos %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_formatos_por_eleccion = n_distinct(cat_tipo_formato)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  # diversidad de tipos de formatos por eleccion
  mutate(mean_n_tipos_formatos_por_eleccion = cumsum(n_tipos_formatos_por_eleccion)/row_number())

# idem diversificacion2, carece de sentido
# y_diversificacion5 <- base_formatos %>%   
#   group_by(cat_pais) %>%   
#   # diversidad de tipos de formatos total   
#   summarise(n_tipos_formatos_por_eleccion = n_distinct(cat_tipo_formato))  # idem diversificacion2, carece de sentido

y_diversificacion6 <- base_temas %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_tipos_temas_por_eleccion = n_distinct(cat_tipo_tema)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  # diversidad de tipos de temas por eleccion
  mutate(mean_n_tipos_temas_por_eleccion = cumsum(n_tipos_temas_por_eleccion)/row_number())

# idem diversificacion2 y 5 , carece de sentido
# y_diversificacion7 <- base_temas %>% 
#   group_by(cat_pais) %>% 
#   # diversidad de tipos de temas total
#   summarise(n_tipos_temas_total = n_distinct(cat_tipo_tema))  # idem diversificacion2 y 5 , carece de sentido

y_diversificacion8 <- base_formatos %>% 
  mutate(formato_innovador = ifelse(cat_tipo_formato%in%c("pr_formatoperiodistas","pr_formatoduelo","pr_formatomoderadores","pr_formatoexpositivo"),0,1)) %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  # Ratio de formatos "innovadores" sobre el total, según literatura. DUDA si incluir "expositivo" o no
  summarise(prop_formatos_innovadores_en_eleccion = sum(formato_innovador)/n()) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  mutate(prop_formatos_innovadores_total_hasta_ahora = cumsum(prop_formatos_innovadores_en_eleccion)/row_number())

# control table(diversificacion8$cat_tipo_formato,diversificacion8$formato_innovador)

y_diversificacion <- y_diversificacion1 %>% 
 # left_join(y_diversificacion2) %>% 
  left_join(y_diversificacion3) %>% 
  left_join(y_diversificacion4) %>% 
  #left_join(y_diversificacion5) %>% 
  left_join(y_diversificacion6) %>% 
  #left_join(y_diversificacion7) %>% 
  left_join(y_diversificacion8) 



#### Y_FORMALIZACION #####################################################

y_formalizacion1 <- base_anual_full %>% 
  mutate(dico_alguna_regulacion = ifelse(ncat_totreg>0,1,0),
         dico_debates_y_alguna_regulacion = ifelse(ncat_totreg>0&dico_hubo_debates>0,1,0)) %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>%
  # antiguedad: cantidad de elecciones con alguna regulacion
  mutate(n_elecciones_con_regulacion = cumsum(dico_alguna_regulacion),
  # ratio elecciones con regulacion / sin
            prop_elecciones_con_regulacion = cumsum(dico_alguna_regulacion)/row_number(),
            # ratio elecciones con debates regulados / elecciones con debates irregulados
            prop_debates_con_regulacion = cumsum(dico_debates_y_alguna_regulacion)/cumsum(dico_hubo_debates),  # OJO ESTE NOMBRE PUEDE SER CONFUSO
            # maximo nivel regulatorio alcanzado 
            maximo_nivel_regulatorio = cummax(ncat_totreg))

y_formalizacion2 <- base_organizadores %>% 
  mutate(dico_organizador_es_estado = ifelse(cat_tipoorgv2=="estado",1,0)) %>%  # no estoy contando medios públicos
  group_by(cat_pais, ncat_eleccion) %>% 
  # debates org por el E sobre total de debates en la eleccion
  summarise(prop_debates_org_por_estado_en_eleccion = sum(dico_organizador_es_estado, na.rm = T)/n_distinct(id_debate),
            # N debates org por el E en total
            n_debates_org_por_estado_total_en_eleccion = sum(dico_organizador_es_estado, na.rm = T),
            dico_hubo_debates_org_por_estado_en_eleccion = ifelse(n_debates_org_por_estado_total_en_eleccion==0,0,1)) %>% 
  ungroup() %>% 
  group_by(cat_pais) %>% 
  arrange(ncat_eleccion) %>% 
  # debates org por el E sobre total de debates total
  mutate(prop_debates_org_por_estado_total = cumsum(prop_debates_org_por_estado_en_eleccion)/row_number(), #CHEQUEAR SI ESTA CUENTA ES VALIDA,CREO QUE SI
            # N debates org por el E en total
            n_debates_org_por_estado_total = cumsum(n_debates_org_por_estado_total_en_eleccion),
         # N elecciones c debates org por E
         n_elecciones_c_debates_org_por_estado = cumsum(dico_hubo_debates_org_por_estado_en_eleccion),
         # prop elecciones c debates org por E de las elecciones en las que hubo debates
         prop_elecciones_c_debates_org_por_estado = cumsum(dico_hubo_debates_org_por_estado_en_eleccion)/row_number() )   

# y_formalizacion3 <- base_organizadores %>%  # REPETIDO, convenia agregar a formalizacion2
#   mutate(dico_organizador_es_estado = ifelse(cat_tipoorgv2=="estado",1,0)) %>%  # no estoy contando medios públicos
#   group_by(cat_pais, ncat_eleccion) %>% 
#   summarise(dico_eleccion_c_debate_org_por_estado = ifelse(sum(dico_organizador_es_estado, na.rm = T)>0,1,0)) %>% 
#   group_by(cat_pais) %>% 
#   # N elecciones con debates org por el Estado
#   summarise(n_elecciones_c_debates_org_por_estado = sum(dico_eleccion_c_debate_org_por_estado, na.rm = T),
#             # elecciones con debates org por el E sobre total de elecciones con debates 
#             prop_elecciones_c_debates_org_por_estado = n_elecciones_c_debates_org_por_estado/n_distinct(ncat_eleccion))

y_formalizacion <- y_formalizacion1 %>% 
  left_join(y_formalizacion2) #%>% 
  #left_join(y_formalizacion3)




#### Y_ CORRELACIONES ENTRE INDICADORES DE MISMAS DIMENSIONES ##########

cor_y_rutinizacion <- y_rutinizacion %>% 
  ungroup() %>% 
  select(-c(cat_pais,cat_primer_debate)) %>% 
  cor( method = "pearson")

print(cor_y_rutinizacion)

cor_y_diversificacion <- y_diversificacion %>% 
  ungroup() %>% 
  select(-c(cat_pais)) %>% 
  cor( method = "pearson")

print(cor_y_diversificacion)

cor_y_formalizacion <- y_formalizacion %>% 
  ungroup() %>% 
  select(-c(cat_pais)) %>%  
  cor( method = "pearson")

print(cor_y_formalizacion)

#### BASE AGREGADA #############################################

y_tot_indicadores <- y_diversificacion %>% 
  left_join(y_rutinizacion) %>% 
  left_join(y_formalizacion)

cor_y_tot_indicadores <- y_tot_indicadores %>% 
  ungroup() %>% 
  select(-c(cat_pais,cat_primer_debate)) %>% 
  cor( method = "pearson")

print(cor_y_tot_indicadores)

#(b)
cormat <- round(cor(y_tot_indicadores  %>% 
                      ungroup() %>% 
                      select(-c(cat_pais,cat_primer_debate)),
                    use= "pairwise.complete.obs"), 2) #matriz de correlaciones con 2 decimales
   #OJO:  if use has the value "pairwise.complete.obs" then the correlation or covariance between each pair of variables is computed using all complete pairs of observations on those variables

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat) #me quedo con una matriz triangular superior

library(reshape2)

melted_cormat <- melt(upper_tri, na.rm = TRUE) #convierte la matriz en un dataframe

#genero el mapa de calor:

library(ggplot2)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))+
  coord_fixed()
