# librerias
library(tidyverse)
#library(hrbrthemes)
library(RColorBrewer)
library(tidyverse)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
library(patchwork)
library(factoextra)

# datos
base <- readxl::read_xlsx("./datav2023/base_final3v2023.xlsx")
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")

colorespais <- base %>% 
  distinct(cat_pais, cols_18)



# CAPITULO 5 AGREGACION ########

# metodo de clustering elegido: complete. especificacion con ordinales ######

elecciones_sin_debates_filtrado_grouped <- base_anual %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
            n_interrupciones = n()-n_elecciones_con_debates,
            n_log_interrupciones = log(n()-n_elecciones_con_debates),
            n_años_reg = sum(ncat_totreg > 3, na.rm=TRUE)) %>% 
  mutate(n_minmax_interrupciones = (n_interrupciones - min(n_interrupciones) )/ max(n_interrupciones) - min(n_interrupciones))

base_cluster_pais <- base %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  select(id_debate, ncat_eleccion, cat_pais, 
         ncat_ronda, 
         ncat_ppac, # las de formato
         ncat_competencia,
         ncat_regmedios, # las de regulacion
         ncat_regestado,
         ncat_regcandidatos,
         ncat_totreg,
         n_presentes, # invitados
         n_ausentes,
         n_proporcioninvitados,
         n_invitados,
         n_indexausentes,
         ncat_mean_tipoorg_ambito, # las numericas de organizadores
         ncat_mean_tipoorg_visibilidad,
         n_catformatos,
         n_cattemas,
         n_cattipo_educ, # las de la org
         n_cattipo_estado,
         n_cattipo_mmp,
         n_cattipo_osc,
         n_cattipo_mmc,
         n_cattipo_NA,
         dico_formato_periodistas,
         n_orgsxdebate
  ) %>% 
  group_by(cat_pais, ncat_eleccion) %>%
  mutate( n_debates_en_eleccion = n()
          ) %>% 
  ungroup() %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = n_distinct(as.factor(ncat_eleccion)),
            n_debates_total = n(),
            n_log_debates_total = log(n()),
            n_promedio_debates_eleccion = n_debates_total/n_elecciones_con_debates,
            n_sd_n_debates = sd(n_debates_en_eleccion, na.rm=TRUE),
            ncat_mean_tipoorg_ambito2 = mean(ncat_mean_tipoorg_ambito, na.rm=TRUE),
            n_sd_tipoorg_ambito = sd(ncat_mean_tipoorg_ambito, na.rm=TRUE), # las numericas de organizadores
            ncat_mean_tipoorg_visibilidad = mean(ncat_mean_tipoorg_visibilidad, na.rm=TRUE),
            ncat_meanppac = mean(as.numeric(ncat_ppac), na.rm=TRUE), # las de formato
            ncat_meancompetencia = mean(as.numeric(ncat_competencia), na.rm=TRUE),
            n_sd_competencia = sd(as.numeric(ncat_competencia), na.rm=TRUE),
            n_sd_ppac = sd(as.numeric(ncat_ppac), na.rm=TRUE),
            ncat_regmedios  = mean(ncat_regmedios, na.rm=TRUE), # las de regulacion
            ncat_regestado  = mean(ncat_regestado, na.rm=TRUE),
            ncat_regcandidatos  = mean(ncat_regcandidatos, na.rm=TRUE),
            ncat_mean_totreg  = mean(ncat_totreg, na.rm=TRUE),
            n_max_reg = max(ncat_totreg),
            n_sd_reg = sd(ncat_totreg, na.rm=TRUE),
            n_proporcioninvitados = mean(n_proporcioninvitados, na.rm=TRUE),
            n_indexausentes = mean(n_indexausentes, na.rm=TRUE),
            ncat_año_primer_debate = min(ncat_eleccion),
            n_cattipo_educ = sum(n_cattipo_educ), # las de la org
            n_cattipo_estado = sum(n_cattipo_estado)+sum(n_cattipo_mmp),
            n_cattipo_mmc  = sum(n_cattipo_mmc),
            n_cattipo_osc = sum(n_cattipo_osc),
            n_cattipo_NA = sum(n_cattipo_NA),
            n_tot_orgs = sum(n_orgsxdebate, na.rm=TRUE),
            pr_dico_formato_periodistas = sum(dico_formato_periodistas)/n_debates_total) %>% 
  mutate(n_edad_practica_absoluta = as.numeric(format(Sys.Date(), "%Y"))-ncat_año_primer_debate)%>% 
  replace(., is.na(.), 0)  %>% 
  mutate(
    pr_cattipo_educ  = n_cattipo_educ/n_tot_orgs, # convertimos a proporcion las org
    pr_cattipo_estado = n_cattipo_estado/n_tot_orgs,
    pr_cattipo_mmc = n_cattipo_mmc/n_tot_orgs,
    pr_cattipo_NA = n_cattipo_NA/n_tot_orgs,
    pr_cattipo_osc = n_cattipo_osc/n_tot_orgs
  ) 

base_cluster_pais <- base_cluster_pais %>% 
  left_join(elecciones_sin_debates_filtrado_grouped) %>% 
  replace(., is.na(.), 0)

#base_cluster_pais %>% write.csv("base_cluster_pais.csv")
#colnames(base_cluster_pais)
# CREAMOS BASE MINIMA, QUE NO CAMBIA ######

cols_base_cluster_minima <- c("n_indexausentes",
                              "n_interrupciones",
                              "ncat_meanppac", # las de formato
                              "ncat_meancompetencia",
                              "n_sd_competencia",
                              "n_sd_ppac",
                              "cat_pais")

#base_cluster_minima <- base_cluster_pais %>% 
#  select(all_of(cols_base_cluster_minima))

# los que vamos a variar, apuntes #################

#### temporales / de magnitud

#n_promedio_debates_eleccion
#n_debates_total,
#n_edad_practica_absoluta,
#n_sd_n_debates,
#n_elecciones_con_debates,

### de la regulacion 

#n_max_reg,
#n_sd_reg,
#ncat_regestado,

#### de las orgas

# ncat_mean_tipoorg_ambito2,
# n_sd_tipoorg_ambito,
# pr_cattipo_educ 
# pr_cattipo_estado 
# pr_cattipo_mmc 
# pr_cattipo_NA 
# pr_cattipo_osc

#### de formato

#pr_dico_formato_periodistas

###
###


# funcion de base ###############

fclusterxpais <- function(df, cols, id, metodolink){
  
  # recibe un df, y un conjunto de parametros aes
  # devuelve un grafico de puntos dispersos
  
  df_reducido <- df %>% 
    select(all_of(cols)) %>% 
    mutate_if(is.numeric, scale)
  
  row.names(df_reducido) <- df[[id]] 
  
  hclust1 <- hcluster(df_reducido, link= metodolink )
  dend1 <- as.dendrogram(hclust1)
  
  return(dend1)
  
}

fplotclusterxpais <- function(dend, titulo = "" , colorsnum = 9) {
  #variables_incorporadas <- colnames(df)
  plot_dend <- dend %>% 
    set("labels_col", value = c(brewer.pal(n = colorsnum, name = "Paired")), k=colorsnum) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value =  c(brewer.pal(n = colorsnum, name = "Paired")), k=colorsnum) %>% 
    plot(axes = F, main = titulo)#,
         #sub = paste("Variables incorporadas: ",variables_incorporadas)) 
  
  
}

# aplicaciones de funcion de base #############

cols_base_cluster_minima <- c("n_indexausentes",
                              "n_interrupciones",
                              "ncat_meanppac", 
                              "ncat_meancompetencia",
                              "n_sd_competencia",
                              "n_sd_ppac",
                              "cat_pais")



# CON AÑOS REG ES ESTEEEEEEE #####

cols <- c( #"n_indexausentes",
  "n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  #"n_sd_competencia",
  #"n_debates_total",
  "n_log_debates_total",
  #"n_promedio_debates_eleccion",
  #"n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_años_reg",
  #"n_mean_reg",
  #"n_sd_ppac",
  "cat_pais")

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )

#pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
#  labs(caption = paste("Variables utilizadas: ", cols, ".
 #                   Calculado con base en la distancia euclidiana, 
#                    usando el método de agregación por enlace completo. 
#                    Elaboración propia con datos recolectados para la presente investigación. "))

fviz_dend(dend_prueba, k = 6,
          k_colors = c("#FD0100", "#F76915", "#EEDE04", "#ccff33", "#2FA236", "#333ED4"),
          main = "Resultado de análisis de clusters
          ",
          xlab = "",
          ylab = "",
          sub = NULL,
          labels_track_height = 4,
          ggtheme = theme_void()) +
  labs(caption = paste("
  Variables utilizadas: 
  ", cols[1:4] %>% toString(), "
  ", cols[5:8] %>% toString(),".
                    
                    (Referencias:
 
n_log_debates_total: logaritmo natural de la cantidad de debates realizados en un país en total,
n_interrupciones: cantidad de elecciones sin debates desde el primer debate,
ncat_mean_tipoorg_ambito2: promedio del ámbito de inscripción de los organizadores,
n_sd_tipoorg_ambito: desvío estándar en el ámbito de inscripción de los organizadores,
ncat_meanppac: promedio del nivel de partticipación que promueven los debates,
ncat_meancompetencia: promedio del nivel de competencia que promueven los debates,
n_max_reg: máxima regulación alcanzada,
n_años_reg: cantidad de campañas en las que se organizaron debates regulados.). 

                    Calculado con base en la distancia euclidiana, usando el método de agregación por enlace completo. 
                    Para más detalle, consultar Anexo 10.

                    Fuente: elaboración propia con datos recolectados para la presente investigación. 
                       "))

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais)  %>% 
  mutate_if(is.numeric, scale)

# df_reducido <- df %>% 
#   select(all_of(cols)) %>% 
#   mutate_if(is.numeric, scale)
# 
# row.names(df_reducido) <- df[[id]] 
# 
# hclust1 <- hcluster(df_reducido, link= "complete")
# dend1 <- as.dendrogram(hclust1)
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
#cor_filtered1 %>%  write.csv("cor_filtered1.csv")

# asignacion de grupos para mas analisis 

paises_grupos_cluster1 <- cutree(dend_prueba, k=6) # primero probamos 5 grupos, luego 6
paises_grupos_cluster2 <- paises_grupos_cluster1 %>% as.list() %>% as.tibble() 
paises_grupos_cluster3 <- colnames(paises_grupos_cluster2) 
paises_grupos_cluster <- cbind(paises_grupos_cluster1, paises_grupos_cluster3 ) %>% 
  as.tibble() %>% 
  rename(cat_pais = "paises_grupos_cluster3")

base_cluster_pais_final <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  left_join(paises_grupos_cluster)

base_cluster_final_grouped_centroides <- base_cluster_pais_final %>% 
  select(-cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(across(everything(), mean))

base_cluster_final_grouped_paises <- base_cluster_pais_final %>% 
  #select(cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(Paises = str_c(cat_pais, collapse = ", "),
            Proporcion = paste(round(n()/18*100,2), "%"))

#base_cluster_final_grouped_centroides %>% write.csv("centroides.csv")
#base_cluster_final_grouped_paises %>% write.csv("paisesgruposcluster.csv")

# HACEMOS HISTOGRAMAS PARA MOSTRAR CELES ##########
for (i in cols[1]){
  print(i)
  nam <- paste("plot", cols[i], sep="")
  assign(nam, 
         base_cluster_pais %>% 
           select(all_of(i)) %>% 
           ggplot() +
           geom_histogram(aes(i)))
  
} # no funciono


hist1 <- base_cluster_pais %>% 
  select(n_interrupciones) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))

# a ver que tan terrible es n interrupciones. en la prueba con log da valores inf

hist1bis <- base_cluster_pais %>% 
  select(n_interrupciones) %>% 
  mutate(n_interrupciones = scale(n_interrupciones)) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))

hist1bis <- base_cluster_pais %>%  # este es lo mismo que el anterior cuak
  select(n_interrupciones) %>% 
  mutate(n_interrupciones = (n_interrupciones- mean(n_interrupciones))/sd(n_interrupciones)) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))


hist1bis <- base_cluster_pais %>% 
  select(n_interrupciones, n_minmax_interrupciones) %>% 
  mutate(n_interrupciones = (n_interrupciones - min(n_interrupciones) )/ max(n_interrupciones) - min(n_interrupciones)) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))

hist1bis <- base_cluster_pais %>% 
  select(n_interrupciones) %>% 
  mutate(n_interrupciones = n_interrupciones/ max(n_interrupciones)) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))

# seguimos con los histogramas

hist2 <- base_cluster_pais %>% 
  select(all_of(cols[2])) %>% 
  ggplot() +
  geom_histogram(aes(ncat_meanppac))

hist3 <- base_cluster_pais %>% 
  select(all_of(cols[3])) %>% 
  ggplot() +
  geom_histogram(aes(ncat_meancompetencia))

hist4 <- base_cluster_pais %>% 
  select(n_log_debates_total) %>% 
  ggplot() +
  geom_histogram(aes(n_log_debates_total)) +
  theme_minimal()

hist4bis <- base_cluster_pais %>% 
  select(n_debates_total) %>% 
  ggplot() +
  geom_histogram(aes(n_debates_total)) +
  theme_minimal()

hist5 <- base_cluster_pais %>% 
  select(all_of(cols[5])) %>% 
  ggplot() +
  geom_histogram(aes(ncat_mean_tipoorg_ambito2))

hist6 <- base_cluster_pais %>% 
  select(all_of(cols[6])) %>% 
  ggplot() +
  geom_histogram(aes(n_sd_tipoorg_ambito))

hist7 <- base_cluster_pais %>% 
  select(all_of(cols[7])) %>% 
  ggplot() +
  geom_histogram(aes(n_max_reg))


hist8 <- base_cluster_pais %>% 
  select(all_of(cols[8])) %>% 
  ggplot() +
  geom_histogram(aes(n_años_reg))


# tests de correlaciones entre variables #####

# todas #######


cols_variables2 <- c("n_debates_total",
                     "n_elecciones_con_debates",
                     "ncat_mean_tipoorg_ambito2",
                     "n_sd_tipoorg_ambito",
                     "n_max_reg",
                     "n_sd_reg")


cols_2 <- c(cols_base_cluster_minima, cols_variables2)

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols_2)) %>% 
  select(-cat_pais)

cor_filtered  <-cor(df_filtered, method = "pearson", use = "complete.obs") 

cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
#cor_filtered1 %>%  write.csv("cor_filtered1.csv")


# por dimension ##########
cols_dim1 <- c("n_promedio_debates_eleccion",
               "n_indexausentes",
               "n_interrupciones",
               "n_debates_total",
               "n_elecciones_con_debates")

cols_dim2 <- c(
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  "n_sd_competencia",
  "n_sd_ppac")

cols_dim3 <- c(
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito")

cols_dim4 <- c(                   
  "n_max_reg",
  "n_sd_reg")



df_filtereddim1 <-base_cluster_pais %>% 
  select(all_of(cols_dim1)) 
df_filtereddim2 <-base_cluster_pais %>% 
  select(all_of(cols_dim2))
df_filtereddim3 <-base_cluster_pais %>% 
  select(all_of(cols_dim3))
df_filtereddim4 <-base_cluster_pais %>% 
  select(all_of(cols_dim4))

cor_filtereddim1 <-cor(df_filtereddim1, method = "pearson", use = "complete.obs") 

cor_filtereddim11 <- cor_filtereddim1 %>% as_tibble() 
rownames(cor_filtereddim11) <- colnames(df_filtereddim1) 
cor_filtereddim11 %>%  write.csv("anexos/cor_filtereddim11.csv")

cor_filtereddim2 <- cor(df_filtereddim2, method = "pearson", use = "complete.obs")
cor_filtereddim21 <- cor_filtereddim2   %>% as_tibble() 
rownames(cor_filtereddim21) <- colnames(df_filtereddim2) 
cor_filtereddim21 %>%  write.csv("anexos/cor_filtereddim21.csv")

cor_filtereddim3 <-cor(df_filtereddim3, method = "pearson", use = "complete.obs")
cor_filtereddim31 <- cor_filtereddim3  %>% as_tibble() 
rownames(cor_filtereddim31) <- colnames(df_filtereddim3) 
cor_filtereddim31 %>%  write.csv("anexos/cor_filtereddim31.csv")

cor_filtereddim4 <-cor(df_filtereddim4, method = "pearson", use = "complete.obs")
cor_filtereddim41 <- cor_filtereddim4 %>% as_tibble() 
rownames(cor_filtereddim41) <- colnames(df_filtereddim4) 
cor_filtereddim41 %>%  write.csv("anexos/cor_filtereddim41.csv")


############################
###############################
################################
############ OTROS MODELOS #############################

# sin n interrupciones #####


cols <- c( #"n_indexausentes",
  #"n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  #"n_sd_competencia",
  #"n_debates_total",
  "n_log_debates_total",
  #"n_promedio_debates_eleccion",
  #"n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_años_reg",
  #"n_mean_reg",
  #"n_sd_ppac",
  "cat_pais")

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )
# 
# pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
#   labs(caption = paste("Variables utilizadas: ", cols, ".
#                     Calculado con base en la distancia euclidiana, 
#                     usando el método de agregación por enlace completo. 
#                     Elaboración propia con datos recolectados para la presente investigación. "))

fviz_dend(dend_prueba, k = 6,
          k_colors = c("#FD0100", "#F76915", "#EEDE04", "#ccff33", "#2FA236", "#333ED4"), 
          main = "Resultado de análisis de clusters",
          xlab = "",
          ylab = "",
          sub = NULL,
          labels_track_height = 4,
          ggtheme = theme_void()) +
  labs(caption = paste("
  Variables utilizadas: 
  ", cols[1:4] %>% toString(), "
  ", cols[5:8] %>% toString(),".
                    
                    (Referencias:
 
n_log_debates_total: logaritmo natural de la cantidad de debates realizados en un país en total,
n_interrupciones: cantidad de elecciones sin debates desde el primer debate,
ncat_mean_tipoorg_ambito2: promedio del ámbito de inscripción de los organizadores,
n_sd_tipoorg_ambito: desvío estándar en el ámbito de inscripción de los organizadores,
ncat_meanppac: promedio del nivel de partticipación que promueven los debates,
ncat_meancompetencia: promedio del nivel de competencia que promueven los debates,
n_max_reg: máxima regulación alcanzada,
n_años_reg: cantidad de campañas en las que se organizaron debates regulados.). 

                    Calculado con base en la distancia euclidiana, usando el método de agregación por enlace completo. 
                    Para más detalle, consultar Anexo 10.

                    Fuente: elaboración propia con datos recolectados para la presente investigación. "))

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais) 
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 

#cor_filtered1 %>%  write.csv("cor_filtered1.csv")

# asignacion de grupos para mas analisis 

paises_grupos_cluster1 <- cutree(dend_prueba, k=6) # primero probamos 5 grupos, luego 6
paises_grupos_cluster2 <- paises_grupos_cluster1 %>% as.list() %>% as.tibble() 
paises_grupos_cluster3 <- colnames(paises_grupos_cluster2) 
paises_grupos_cluster <- cbind(paises_grupos_cluster1, paises_grupos_cluster3 ) %>% 
  as.tibble() %>% 
  rename(cat_pais = "paises_grupos_cluster3")

base_cluster_pais_final <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  left_join(paises_grupos_cluster)

base_cluster_final_grouped_centroides <- base_cluster_pais_final %>% 
  select(-cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(across(everything(), mean))

base_cluster_final_grouped_paises <- base_cluster_pais_final %>% 
  #select(cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(Paises = str_c(cat_pais, collapse = ", "),
            Proporcion = paste(round(n()/18*100,2), "%"))


# pruebo con otros metodos de enlace ########

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )

# pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
#   labs(caption = paste("Variables utilizadas: ", cols, ".
#                     Calculado con base en la distancia euclidiana, 
#                     usando el método de agregación por enlace completo. 
#                     Elaboración propia con datos recolectados para la presente investigación. "))

fviz_dend(dend_prueba, k = 6,
          k_colors = c("#FD0100", "#F76915", "#EEDE04", "#ccff33", "#2FA236", "#333ED4"),
          main = "Resultado de análisis de clusters",
          xlab = "",
          ylab = "",
          sub = NULL,
          labels_track_height = 4,
          ggtheme = theme_void()) +
  labs(caption = paste("
  Variables utilizadas: 
  ", cols[1:4] %>% toString(), "
  ", cols[5:8] %>% toString(),".
                    
                    (Referencias:
 
n_log_debates_total: logaritmo natural de la cantidad de debates realizados en un país en total,
n_interrupciones: cantidad de elecciones sin debates desde el primer debate,
ncat_mean_tipoorg_ambito2: promedio del ámbito de inscripción de los organizadores,
n_sd_tipoorg_ambito: desvío estándar en el ámbito de inscripción de los organizadores,
ncat_meanppac: promedio del nivel de partticipación que promueven los debates,
ncat_meancompetencia: promedio del nivel de competencia que promueven los debates,
n_max_reg: máxima regulación alcanzada,
n_años_reg: cantidad de campañas en las que se organizaron debates regulados.). 

                    Calculado con base en la distancia euclidiana, usando el método de agregación por enlace completo. 
                    Para más detalle, consultar Anexo 10.

                    Fuente: elaboración propia con datos recolectados para la presente investigación. "))

# pruebo transformando interrupciones a distintas cosas ##############

# LOG NO SIRVE PORQUE TIENE VALORES 0, POR ENDE EL LOG DA INFINITO
# probamos minmax # NO CAMBIAN RESULTADOS
# solo dividiendo por max no probamos porque da idem que anterior. 


cols <- c( #"n_indexausentes",
  "n_minmax_interrupciones",
  #"n_log_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  #"n_sd_competencia",
  #"n_debates_total",
  "n_log_debates_total",
  #"n_promedio_debates_eleccion",
  #"n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_años_reg",
  #"n_mean_reg",
  #"n_sd_ppac",
  "cat_pais")

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )

#pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
#  labs(caption = paste("Variables utilizadas: ", cols, ".
#                    Calculado con base en la distancia euclidiana, 
#                    usando el método de agregación por enlace completo. 
#                    Elaboración propia con datos recolectados para la presente investigación. "))
#
fviz_dend(dend_prueba, k = 6,
          k_colors = c("#FD0100", "#F76915", "#EEDE04", "#ccff33", "#2FA236", "#333ED4"),
          main = "Resultado de análisis de clusters",
          xlab = "",
          ylab = "",
          sub = NULL,
          labels_track_height = 4,
          ggtheme = theme_void()) +
  labs(caption = paste("
  Variables utilizadas: 
  ", cols[1:4] %>% toString(), "
  ", cols[5:8] %>% toString(),".
                    
                    (Referencias:

n_log_debates_total: logaritmo natural de la cantidad de debates realizados en un país en total,
n_interrupciones: cantidad de elecciones sin debates desde el primer debate,
ncat_mean_tipoorg_ambito2: promedio del ámbito de inscripción de los organizadores,
n_sd_tipoorg_ambito: desvío estándar en el ámbito de inscripción de los organizadores,
ncat_meanppac: promedio del nivel de partticipación que promueven los debates,
ncat_meancompetencia: promedio del nivel de competencia que promueven los debates,
n_max_reg: máxima regulación alcanzada,
n_años_reg: cantidad de campañas en las que se organizaron debates regulados.). 

                    Calculado con base en la distancia euclidiana, usando el método de agregación por enlace completo. 
                    Para más detalle, consultar Anexo 10.

                    Fuente: elaboración propia con datos recolectados para la presente investigación. "))

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais)
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
#cor_filtered1 %>%  write.csv("cor_filtered1.csv")


# asignacion de grupos para mas analisis 

paises_grupos_cluster1 <- cutree(dend_prueba, k=5) 
paises_grupos_cluster2 <- paises_grupos_cluster1 %>% as.list() %>% as.tibble() 
paises_grupos_cluster3 <- colnames(paises_grupos_cluster2) 
paises_grupos_cluster <- cbind(paises_grupos_cluster1, paises_grupos_cluster3 ) %>% 
  as.tibble() %>% 
  rename(cat_pais = "paises_grupos_cluster3")

base_cluster_pais_final <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  left_join(paises_grupos_cluster)

base_cluster_final_grouped_centroides <- base_cluster_pais_final %>% 
  select(-cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(across(everything(), mean))

hist1 <- base_cluster_pais %>% 
  select(n_log_interrupciones, n_interrupciones) %>% 
  ggplot() +
  geom_histogram(aes(n_interrupciones))

# MODELO 1 prueba  #################
## ESTA ELEGIDA EN LA TESIS SIES
cols_variables2 <- c("n_debates_total",
                     "n_elecciones_con_debates",
                     "ncat_mean_tipoorg_ambito2",
                     "n_sd_tipoorg_ambito",
                     "n_max_reg",
                     "n_sd_reg")


cols_2 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba2 <- fclusterxpais(df= base_cluster_pais, cols= cols_2, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba2, "Modelo1")

# SACO INDEX AUSENTES ############
cols_base_cluster_minima <- c( #"n_indexausentes",
                               "n_interrupciones",
                               "ncat_meanppac", # las de formato
                               "ncat_meancompetencia",
                               "n_sd_competencia",
                               "n_sd_ppac",
                               "cat_pais")
cols_variables2 <- c(#"n_promedio_debates_eleccion",
  "n_debates_total",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_sd_reg"
)


cols_22 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba22 <- fclusterxpais(df= base_cluster_pais, cols= cols_22, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba22, "SACO INDEX AUSENTES")

# SACO TODAS COLINEALES #############

cols_base_cluster_minima <- c( "n_indexausentes",
  "n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  "n_sd_competencia",
  #"n_sd_ppac",
  "cat_pais")
cols_variables2 <- c(#"n_promedio_debates_eleccion",
  #"n_debates_total",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg"#,
  #"n_sd_reg"
)


cols_23 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba23 <- fclusterxpais(df= base_cluster_pais, cols= cols_23, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba23, "SACO TODAS COLINEALES")

# SACO INDEX AUSENTES + COLINEALES #############

cols_base_cluster_minima <- c(# "n_indexausentes",
  "n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  "n_sd_competencia",
  #"n_sd_ppac",
  "cat_pais")
cols_variables2 <- c(#"n_promedio_debates_eleccion",
  #"n_debates_total",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg"#,
  #"n_sd_reg"
)


cols_24 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba24 <- fclusterxpais(df= base_cluster_pais, cols= cols_24, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba24, "SACO INDEX AUSENTES + COLINEALES")

# SACO REG #####

cols_base_cluster_minima <- c( "n_indexausentes",
  "n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  "n_sd_competencia",
  "n_sd_ppac",
  "cat_pais")
cols_variables2 <- c(#"n_promedio_debates_eleccion",
  "n_debates_total",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg"#,
  #"n_sd_reg"
)


cols_25 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba25 <- fclusterxpais(df= base_cluster_pais, cols= cols_25, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba25, "SACO REG")

# SACO TOT DEBATES ##########

cols_base_cluster_minima <- c( "n_indexausentes",
                               "n_interrupciones",
                               "ncat_meanppac", # las de formato
                               "ncat_meancompetencia",
                               "n_sd_competencia",
                               "n_sd_ppac",
                               "cat_pais")
cols_variables2 <- c(#"n_promedio_debates_eleccion",
  #"n_debates_total",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_sd_reg"
)


cols_26 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba26 <- fclusterxpais(df= base_cluster_pais, cols= cols_26, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba26, "SACO TOT DEBATES")






# CON AÑOS REG  #####

cols <- c( #"n_indexausentes",
           "n_interrupciones",
           "ncat_meanppac", # las de formato
           "ncat_meancompetencia",
           #"n_sd_competencia",
           "n_debates_total",
           #"n_log_debates_total",
           #"n_promedio_debates_eleccion",
           #"n_elecciones_con_debates",
           "ncat_mean_tipoorg_ambito2",
           "n_sd_tipoorg_ambito",
           "n_max_reg",
           "n_años_reg",
           #"n_mean_reg",
           #"n_sd_ppac",
           "cat_pais")

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba, cols, 6)
df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais)
cor(df_filtered, method = "pearson", use = "complete.obs") 


# CON AÑOS REG ES vARIANTES #####

cols <- c( "n_indexausentes",
  "n_interrupciones",
  "ncat_meanppac", # las de formato
  "ncat_meancompetencia",
  "n_sd_competencia",
  #"n_debates_total",
  "n_log_debates_total",
  #"n_promedio_debates_eleccion",
  "n_elecciones_con_debates",
  "ncat_mean_tipoorg_ambito2",
  "n_sd_tipoorg_ambito",
  "n_max_reg",
  "n_años_reg",
  #"n_mean_reg",
  #"n_sd_ppac",
  "cat_pais")

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba, cols, 6)
df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais)
cor(df_filtered, method = "pearson", use = "complete.obs") 

