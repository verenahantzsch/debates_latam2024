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
library(NbClust)

setwd("/home/carolina/Documents/Proyectos R/debates_latam/papers_ingles")

# datos
base <- readxl::read_xlsx("./englishdatav4/base_finalv3.xlsx")
elecciones <-  readxl::read_xlsx("./englishdatav4/base_elecciones.xlsx") # base auxiliar: Years que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./englishdatav4/base_organizadoresv3.xlsx")
base_formatos <- readxl::read_xlsx("./englishdatav4/base_formatos_longv3.xlsx")
base_temas <- readxl::read_xlsx("./englishdatav4/base_temas_longv3.xlsx")
base_normativa <- readxl::read_xlsx("./englishdatav4/base_normativa.xlsx")
base_anual <- readxl::read_xlsx("./englishdatav4/base_anualv3.xlsx")



colorespais <- base %>% 
  distinct(english_cat_pais, cols_18)

colorescluster <- tibble(.rows = 18)

colorescluster$english_cat_pais <- c("Brazil", "Colombia", "Costa Rica",  
                                     "Argentina", "Peru", "Mexico",
                                     "Ecuador", "Uruguay", "Panama", 
                                     "Chile", "Bolivia", "Paraguay", "Guatemala", "Honduras",  "El Salvador",
                                     "Dominican Republic", "Nicaragua", "Venezuela")
colorescluster$colorescluster <- c("#FD0100","#FD0100","#FD0100", 
                                            "#F76915", "#F76915", "#F76915",
                                            "#EEDE04",   "#EEDE04", "#EEDE04", 
                                            "#ccff33","#ccff33", "#ccff33", "#ccff33","#ccff33","#ccff33",
                                            "#2FA236","#2FA236", "#333ED4")

# CAPITULO 5 AGREGACION ########

# metodo de clustering elegido: complete. especificacion con ordinales ######

# CREO BASE ##################
elecciones_sin_debates_filtrado_grouped <- base_anual %>% 
  group_by(english_cat_pais) %>% 
  summarise(suma_elecciones_registradas = n(),
            n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
            n_interruptions = suma_elecciones_registradas-n_elecciones_con_debates,
            n_years_reg = sum(ncat_totreg > 3, na.rm=TRUE))

base_cluster_pais <- base %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  select(id_debate, ncat_eleccion, english_cat_pais, 
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
         n_cattipo_state,
         n_cattipo_pmm,
         n_cattipo_cso,
         n_cattipo_cmm,
         n_cattipo_NA,
         dico_formato_periodistas,
         n_orgsxdebate
  ) %>% 
  group_by(english_cat_pais, ncat_eleccion) %>%
  mutate( n_debates_en_eleccion = n()
  ) %>% 
  ungroup() %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  group_by(english_cat_pais) %>% 
  summarise(n_elecciones_con_debates = n_distinct(as.factor(ncat_eleccion)),
            n_debates_total = n(),
            n_log_debates_total = log(n()),
            n_promedio_debates_eleccion = n_debates_total/n_elecciones_con_debates,
            n_sd_n_debates = sd(n_debates_en_eleccion, na.rm=TRUE),
            ncat_mean_typeorg = mean(ncat_mean_tipoorg_ambito, na.rm=TRUE),
            n_sd_typeorg = sd(ncat_mean_tipoorg_ambito, na.rm=TRUE), # las numericas de organizadores
            ncat_mean_tipoorg_visibilidad = mean(ncat_mean_tipoorg_visibilidad, na.rm=TRUE),
            ncat_meanppac = mean(as.numeric(replace_na(ncat_ppac, 0)), na.rm=TRUE), # las de formato
            ncat_meancompetence = mean(as.numeric(replace_na(ncat_competencia, 0)), na.rm=TRUE),
            n_sd_competencia = sd(as.numeric(ncat_competencia), na.rm=TRUE),
            n_sd_ppac = sd(as.numeric(ncat_ppac), na.rm=TRUE),
            ncat_regmedios  = mean(ncat_regmedios, na.rm=TRUE), # las de regulacion
            ncat_regestado  = mean(ncat_regestado, na.rm=TRUE),
            ncat_regcandidatos  = mean(ncat_regcandidatos, na.rm=TRUE),
            ncat_mean_totreg  = mean(ncat_totreg, na.rm=TRUE),
            n_max_reg = max(ncat_totreg),
            n_mean_reg = mean(ncat_totreg, na.rm=TRUE),
            n_sd_reg = sd(ncat_totreg, na.rm=TRUE),
            n_proporcioninvitados = mean(n_proporcioninvitados, na.rm=TRUE),
            n_indexausentes = mean(n_indexausentes, na.rm=TRUE),
            ncat_year_primer_debate = min(ncat_eleccion),
            n_cattipo_educ = sum(n_cattipo_educ), # las de la org
            pr_cattipo_state = sum(n_cattipo_state)+sum(n_cattipo_pmm),
            pr_cattipo_cmm  = sum(n_cattipo_cmm),
            pr_cattipo_cso = sum(n_cattipo_cso),
            n_cattipo_NA = sum(n_cattipo_NA),
            n_tot_orgs = sum(n_orgsxdebate, na.rm=TRUE),
            pr_dico_formato_periodistas = sum(dico_formato_periodistas)/n_debates_total) %>% 
  mutate(n_edad_practica_absoluta = 2021-ncat_year_primer_debate)%>% 
  replace(., is.na(.), 0)  %>% 
  mutate(
    pr_cattipo_educ  = n_cattipo_educ/n_tot_orgs, # convertimos a proporcion las org
    pr_cattipo_state = pr_cattipo_state/n_tot_orgs,
    pr_cattipo_cmm = pr_cattipo_cmm/n_tot_orgs,
    pr_cattipo_NA = n_cattipo_NA/n_tot_orgs,
    pr_cattipo_cso = pr_cattipo_cso/n_tot_orgs
  ) 

base_cluster_pais <- base_cluster_pais %>% 
  left_join(elecciones_sin_debates_filtrado_grouped) %>% 
  replace(., is.na(.), 0)

# funciones de base   #########################

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


fclusterxpaiskmeans <- function(df, cols, id, ncenters){
  
  # recibe un df, y un conjunto de parametros aes
  # devuelve un grafico de puntos dispersos
  
  df_reducido <- df %>% 
    select(all_of(cols)) %>% 
    mutate_if(is.numeric, scale)
  
  row.names(df_reducido) <- df[[id]] 
  
  hclust1 <- kmeans(df_reducido %>% select(-id), ncenters, iter.max = 10, nstart = 25)
  
  return(hclust1)
  
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


# selecciono cols de especificacion final ###########

cols <- c( #"n_indexausentes",
  "n_interruptions",
  "ncat_meanppac", # las de formato
  "ncat_meancompetence",
  #"n_sd_competencia",
  #"n_debates_total",
  "n_log_debates_total",
  #"n_promedio_debates_eleccion",
  #"n_elecciones_con_debates",
  "ncat_mean_typeorg",
  "n_sd_typeorg",
  "n_max_reg",
  "n_years_reg",
  #"n_mean_reg",
  #"n_sd_ppac",
  "english_cat_pais")

# guardo tabla de correlaciones entre variables ####### 
df_filtered <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-english_cat_pais)
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
#cor_filtered1 %>%  write.csv("anexos/english_cor_filtered1.csv")
#base_cluster_pais %>%  write.csv("anexos/english_control_base_cluster_pais.csv")

# aplico clustering ##################
dend_prueba <- fclusterxpais(df= base_cluster_pais, cols = cols, id= "english_cat_pais", metodolink = "complete" )

pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
  labs(caption = paste("Variables utilizadas: ", cols, ".
                    Distances was calculated according to the Euclidean distance, 
                    and the complete linkage aggregation method was used.  
                    Source: Author, with data collected for this research. "))

fviz_dend(dend_prueba, k = 6,
          k_colors = c("#FD0100", "#F76915", "#EEDE04", "#ccff33", "#2FA236", "#333ED4"),
          main = "Cluster analysis' final solution",
          xlab = "",
          ylab = "",
          sub = NULL,
          labels_track_height = 4,
          ggtheme = theme_void()) +
  labs(caption = paste("
  The variate included: 
  ", cols[1:4] %>% toString(), "
  ", cols[5:8] %>% toString(),".
                    
                    (References:
                    n_log_debates_total: natural logarithm of the number of debates held in a country in total. 
n_log_debates_total: natural logarithm of the number of debates held in a country in total,
n_interruptions: number of elections without debates since the first debate,
ncat_mean_typeorg: average of the organizers' registration scope,
n_sd_typeorg: standard deviation in the enrollment scope of the organizers,
ncat_meanppac: average of the level of participation promoted by the discussions,
ncat_meancompetence: average level of competence promoted by the discussions,
n_max_reg: maximum regulation achieved,
n_years_reg: number of campaigns in which regulated debates were organized). 

Calculous was based on the Euclidean distance, and used the complete-linkage aggregation method. 

                    Source: Author, with data collected for this research."))

# pruebo aplicando kmeans  #######################

df_reducido <- base_cluster_pais  %>% 
  select(all_of(cols)) %>% 
  mutate_if(is.numeric, scale)

# para testear la cantidad de clusters a pedir de imput ####
fviz_nbclust(df_reducido %>% select(-english_cat_pais),
             FUNcluster = kmeans,
             method = "wss") # este me da 5/6

fviz_nbclust(df_reducido %>% select(-english_cat_pais),
             FUNcluster = kmeans,
             method = "silhouette") # este me da 6 jaja pero no se que significa

#The Silhouette Coefficient is calculated using the mean intra-cluster distance ( a ) and the mean nearest-cluster distance ( b ) for each sample. The Silhouette Coefficient for a sample is (b - a) / max(a, b) . To clarify, b is the distance between a sample and the nearest cluster that the sample is not a part of.

fviz_nbclust(df_reducido %>% select(-english_cat_pais),
             FUNcluster = kmeans,
             method = "gap_stat")

# varios indicadores juntos
nb <- NbClust(df_reducido %>% select(-english_cat_pais), distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

# indicador de tendencia al clustering (este se hace antes del analisis) 
res <- get_clust_tendency(df_reducido %>% select(-english_cat_pais), n = nrow(df_reducido)-1, graph = FALSE)
res$hopkins_stat
#We can conduct the Hopkins Statistic test iteratively, using 0.5 as the threshold to reject the alternative hypothesis. That is, if H < 0.5, then it is unlikely that D has statistically significant clusters.
#Put in other words, If the value of Hopkins statistic is close to 1, then we can reject the null hypothesis and conclude that the dataset D is significantly a clusterable data.


## CON 6 CLUSTERS

dend_pruebakmeans6 <- fclusterxpaiskmeans(df= base_cluster_pais, cols = cols, id= "english_cat_pais", ncenters = 6)

df_reducido6 <- base_cluster_pais  %>% 
  select(all_of(cols)) %>% 
  mutate_if(is.numeric, scale) %>% 
  cbind(cluster = dend_pruebakmeans6$cluster) 

fviz_cluster(dend_pruebakmeans6, df_reducido6 %>% select(-c(english_cat_pais, cluster)))

# pruebo con 5 clusters . cambia VENEZUELA. consistente con obs de que su trayectoria pasada es similar a grupo Ecuador 
dend_pruebakmeans5 <- fclusterxpaiskmeans(df= base_cluster_pais, cols = cols, id= "english_cat_pais", ncenters = 5)

df_reducido <- base_cluster_pais  %>% 
  select(all_of(cols)) %>% 
  mutate_if(is.numeric, scale) %>% 
  cbind(cluster = dend_pruebakmeans5$cluster) # ieii ison lo mismos resultados

fviz_cluster(dend_pruebakmeans5, df_reducido %>% select(-c(english_cat_pais, cluster)))

# asignacion de grupos para mas analisis  ####################################

paises_grupos_cluster1 <- cutree(dend_prueba, k=6) # primero probamos 5 grupos, luego 6
paises_grupos_cluster2 <- paises_grupos_cluster1 %>% as.list() %>% as.tibble() 
paises_grupos_cluster3 <- colnames(paises_grupos_cluster2) 
paises_grupos_cluster <- cbind(paises_grupos_cluster1, paises_grupos_cluster3 ) %>% 
  as.tibble() %>% 
  rename(english_cat_pais = "paises_grupos_cluster3")

base_cluster_pais_final <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  left_join(paises_grupos_cluster)

base_cluster_final_grouped_centroides <- base_cluster_pais_final %>% 
  select(-english_cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(across(everything(), mean))

base_cluster_final_grouped_paises <- base_cluster_pais_final %>% 
  #select(cat_pais) %>% 
  group_by(paises_grupos_cluster1) %>% 
  summarise(Paises = str_c(english_cat_pais, collapse = ", "),
            Proporcion = paste(round(n()/18*100,2), "%"))

#base_cluster_final_grouped_centroides %>% write.csv("anexos/english_centroides.csv")
#base_cluster_final_grouped_paises %>% write.csv("anexos/english_paisesgruposcluster.csv")

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
  select(n_interruptions) %>% 
  ggplot() +
  geom_histogram(aes(n_interruptions))


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

### hago scatterplots #######################

# primera prueba
scatter1 <- base_cluster_pais %>% 
  ggplot() +
  geom_point(aes(english_cat_pais, n_interruptions, colour = english_cat_pais))

# paso base a long 

base_cluster_long <- base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  pivot_longer(cols = colnames(df_filtered), names_to = "variables", values_to = "values")

plot_all <- base_cluster_long %>% 
  ggplot() +
  geom_text(aes(variables, values, colour = english_cat_pais, label = english_cat_pais)) +
 theme_minimal() +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none")
  
plot_facet <- base_cluster_long %>% 
  ggplot() +
  geom_point(aes(english_cat_pais, values, colour = english_cat_pais)) +
  scale_colour_manual(breaks = colorescluster$english_cat_pais,
                      values = colorescluster$colorescluster) +
  facet_wrap(~variables, scales= "free", nrow =2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "grey1"),
        strip.text = element_text(colour = "white")) +
  labs(x = "", 
       title = "Summary of indicators",
       subtitle = "Country scores",
       caption = "Country colors are indicative of their final cluster adjudication, according to results obtained below.
       Source: Author, with data collected for this research")


#### summary statistics #########################################

sumari_statistics <- df_filtered %>% 
  summary()  %>% 
  unclass() %>% 
  as_tibble(check.names = F)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sumari_stats <- tibble(.rows= 8)
sumari_stats$variables <- colnames(df_filtered)
sumari_stats$min <- apply(df_filtered, 2, min)
sumari_stats$max <- apply(df_filtered, 2, max)
sumari_stats$mean <- apply(df_filtered, 2, mean)
sumari_stats$sd <- apply(df_filtered, 2, sd)
sumari_stats$mode <- apply(df_filtered, 2, getmode)
sumari_stats$median <- apply(df_filtered, 2, median)
sumari_stats$q0 <- apply(df_filtered, 2, quantile, probs = 0)
sumari_stats$q25 <- apply(df_filtered, 2, quantile, probs = 0.25)
sumari_stats$q50 <- apply(df_filtered, 2, quantile, probs = 0.5)
sumari_stats$q75 <- apply(df_filtered, 2, quantile, probs = 0.75)
sumari_stats$q100 <- apply(df_filtered, 2, quantile, probs = 1)


#sumari_stats %>% write.csv("anexos/english_sumari_stats.csv")


########################################################################################################################################################################
###########################################################################################################################################################################
################################################################################################################################################
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

pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
  labs(caption = paste("Variables utilizadas: ", cols, ".
                    Calculado con base en la distancia euclidiana, 
                    usando el método de agregación por enlace completo. 
                    Elaboración propia con datos recolectados para la presente investigación. "))

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

pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
  labs(caption = paste("Variables utilizadas: ", cols, ".
                    Calculado con base en la distancia euclidiana, 
                    usando el método de agregación por enlace completo. 
                    Elaboración propia con datos recolectados para la presente investigación. "))

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

pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
  labs(caption = paste("Variables utilizadas: ", cols, ".
                    Calculado con base en la distancia euclidiana, 
                    usando el método de agregación por enlace completo. 
                    Elaboración propia con datos recolectados para la presente investigación. "))

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


# NUEVO NUEVO 2022 prueba por si las

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
cor_filtereddim11 %>%  write.csv("cor_filtereddim11.csv")

cor_filtereddim2 <- cor(df_filtereddim2, method = "pearson", use = "complete.obs")
cor_filtereddim21 <- cor_filtereddim2   %>% as_tibble() 
rownames(cor_filtereddim21) <- colnames(df_filtereddim2) 
cor_filtereddim21 %>%  write.csv("cor_filtereddim21.csv")

cor_filtereddim3 <-cor(df_filtereddim3, method = "pearson", use = "complete.obs")
cor_filtereddim31 <- cor_filtereddim3  %>% as_tibble() 
rownames(cor_filtereddim31) <- colnames(df_filtereddim3) 
cor_filtereddim31 %>%  write.csv("cor_filtereddim31.csv")

cor_filtereddim4 <-cor(df_filtereddim4, method = "pearson", use = "complete.obs")
cor_filtereddim41 <- cor_filtereddim4 %>% as_tibble() 
rownames(cor_filtereddim41) <- colnames(df_filtereddim4) 
cor_filtereddim41 %>%  write.csv("cor_filtereddim41.csv")
