# librerias
library(tidyverse)
library(hrbrthemes)
library(RColorBrewer)
library(tidyverse)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
library(patchwork)

# datos
base <- readxl::read_xlsx("./datav3/base_finalv3.xlsx")
elecciones <-  readxl::read_xlsx("./datav3/base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav3/base_organizadoresv3.xlsx")
base_formatos <- readxl::read_xlsx("./datav3/base_formatos_longv3.xlsx")
base_temas <- readxl::read_xlsx("./datav3/base_temas_longv3.xlsx")
base_normativa <- readxl::read_xlsx("./datav3/base_normativa.xlsx")
base_anual <- readxl::read_xlsx("./datav3/base_anualv3.xlsx")

colorespais <- base %>% 
  distinct(cat_pais, cols_18)



# CAPITULO 5 AGREGACION ########

# metodo de clustering elegido: complete. especificacion con ordinales ######

elecciones_sin_debates_filtrado_grouped <- base_anual %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
            n_interrupciones = n()-n_elecciones_con_debates)

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
  mutate( n_debates_en_eleccion = n()) %>% 
  ungroup() %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = n_distinct(as.factor(ncat_eleccion)),
            n_debates_total = n(),
            n_promedio_debates_eleccion = n_debates_total/n_elecciones_con_debates,
            n_sd_n_debates = sd(n_debates_en_eleccion, na.rm=TRUE),
            ncat_mean_tipoorg_ambito2 = mean(ncat_mean_tipoorg_ambito, na.rm=TRUE),
            n_sd_tipoorg_ambito = sd(ncat_mean_tipoorg_ambito, na.rm=TRUE), # las numericas de organizadores
            ncat_mean_tipoorg_visibilidad = mean(ncat_mean_tipoorg_visibilidad, na.rm=TRUE),
            ncat_meanppac = mean(as.numeric(replace_na(ncat_ppac, 0)), na.rm=TRUE), # las de formato
            ncat_meancompetencia = mean(as.numeric(replace_na(ncat_competencia, 0)), na.rm=TRUE),
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
  mutate(n_edad_practica_absoluta = 2021-ncat_año_primer_debate)%>% 
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

fplotclusterxpais <- function(df, dend, titulo = "" ) {
  variables_incorporadas <- colnames(df)
  plot_dend <- dend %>% 
  set("labels_col", value = c(brewer.pal(n = 9, name = "Paired")), k=9) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value =  c(brewer.pal(n = 9, name = "Paired")), k=9) %>% 
    plot(axes = F, main = titulo,
         sub = paste("Variables incorporadas: ",variables_incorporadas))
  
  
}

# aplicaciones de funcion de base #############

cols_base_cluster_minima <- c("n_indexausentes",
                              "n_interrupciones",
                              "ncat_meanppac", 
                              "ncat_meancompetencia",
                              "n_sd_competencia",
                              "n_sd_ppac",
                              "cat_pais")

# prueba 1 #################

cols_variables <- c("n_promedio_debates_eleccion",
                    "n_elecciones_con_debates",
                    "ncat_mean_tipoorg_ambito2",
                    "n_sd_tipoorg_ambito",
                    "n_max_reg",
                    "n_sd_reg")


cols_ <- c(cols_base_cluster_minima, cols_variables)

dend_prueba1 <- fclusterxpais(df= base_cluster_pais, cols= cols_, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba1)


# prueba EN TESIS 2 #################
## ESTA ELEGIDA EN LA TESIS SIES
cols_variables2 <- c("n_debates_total",
                    "n_elecciones_con_debates",
                    "ncat_mean_tipoorg_ambito2",
                    "n_sd_tipoorg_ambito",
                    "n_max_reg",
                    "n_sd_reg")


cols_2 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba2 <- fclusterxpais(df= base_cluster_pais, cols= cols_2, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba2)

# prueba 3 #################

cols_variables3 <- c("n_debates_total",
                     "n_elecciones_con_debates",
                     #"n_promedio_debates_eleccion",
                     "pr_dico_formato_periodistas",
                     "ncat_mean_tipoorg_ambito2",
                     "n_sd_tipoorg_ambito",
                     "n_max_reg",
                     "n_sd_reg")


cols_3 <- c(cols_base_cluster_minima, cols_variables3)

dend_prueba3 <- fclusterxpais(df= base_cluster_pais, cols= cols_3, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba3)


# prueba 4 #################

cols_variables4 <- c("n_promedio_debates_eleccion",
                    "n_elecciones_con_debates"
                    ,"pr_cattipo_educ" 
                    ,"pr_cattipo_estado" 
                    ,"pr_cattipo_mmc" 
                    ,"pr_cattipo_NA" 
                    ,"pr_cattipo_osc",
                    "n_max_reg",
                    "n_sd_reg")


cols_4 <- c(cols_base_cluster_minima, cols_variables4)

dend_prueba4 <- fclusterxpais(df= base_cluster_pais, cols= cols_4, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba4)


# prueba 5 #################

cols_variables5 <- c("n_debates_total",
                     "n_elecciones_con_debates"
                     ,"pr_cattipo_educ" 
                     ,"pr_cattipo_estado" 
                     ,"pr_cattipo_mmc" 
                     ,"pr_cattipo_osc",
                     "n_max_reg",
                     "n_sd_reg")


cols_5 <- c(cols_base_cluster_minima, cols_variables5)

dend_prueba5 <- fclusterxpais(df= base_cluster_pais, cols= cols_5, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba5)


# prueba 6 #################

cols_variables6 <- c("n_promedio_debates_eleccion",
                    "n_elecciones_con_debates",
                    "ncat_mean_tipoorg_ambito2",
                    "n_sd_tipoorg_ambito",
                    "ncat_regestado")


cols_6 <- c(cols_base_cluster_minima, cols_variables6)

dend_prueba6 <- fclusterxpais(df= base_cluster_pais, cols= cols_6, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(dend_prueba6)

# par para ver


par(mfrow=c(nf=2, nc=3))
fplotclusterxpais(dend_prueba1, "Modelo 1") # promedios
fplotclusterxpais(dend_prueba2, "Modelo 2") # tot debates
fplotclusterxpais(dend_prueba3, "Modelo 3")
fplotclusterxpais(dend_prueba4, "Modelo 4")
fplotclusterxpais(dend_prueba5, "Modelo 5")
fplotclusterxpais(dend_prueba6, "Modelo 6")

# mismos 6 modelos pero con otro metodo de agregacion #################

dend2_prueba1 <- fclusterxpais(df= base_cluster_pais, cols= cols_, id= "cat_pais", metodolink = "ward" )
dend2_prueba2 <- fclusterxpais(df= base_cluster_pais, cols= cols_2, id= "cat_pais", metodolink = "ward" )
dend2_prueba3 <- fclusterxpais(df= base_cluster_pais, cols= cols_3, id= "cat_pais", metodolink = "ward" )
dend2_prueba4 <- fclusterxpais(df= base_cluster_pais, cols= cols_4, id= "cat_pais", metodolink = "ward" )
dend2_prueba5 <- fclusterxpais(df= base_cluster_pais, cols= cols_5, id= "cat_pais", metodolink = "ward" )
dend2_prueba6 <- fclusterxpais(df= base_cluster_pais, cols= cols_6, id= "cat_pais", metodolink = "ward" )

par(mfrow=c(nf=2, nc=3))
fplotclusterxpais(dend2_prueba1, "prueba2.1") # promedios
fplotclusterxpais(dend2_prueba2, "prueba2.2") # tot debates
fplotclusterxpais(dend2_prueba3, "prueba2.3")
fplotclusterxpais(dend2_prueba4, "prueba2.4")
fplotclusterxpais(dend2_prueba5, "prueba2.5")
fplotclusterxpais(dend2_prueba6, "prueba2.6")


#buenisima la funcion, los resultados una porong ajajaja



# VIEJO VIEJO VIEJO prueba 1 clustering ###########
# con variables ordinales

base_cluster_pais_reducida1 <-  base_cluster_pais %>% 
  select(n_debates_total,  
         n_sd_n_debates,
         #n_promedio_debates_eleccion,
         ncat_mean_tipoorg_ambito2,
         n_sd_tipoorg_ambito, # probar con la proporcionalidad o las cantidades por categoria de organizador
         n_sd_competencia,
         n_sd_ppac,
         ncat_meanppac,
         ncat_meancompetencia,
         n_max_reg,
         n_sd_reg,
         n_indexausentes,
         n_edad_practica_absoluta,
         cat_pais,
         n_interrupciones
  ) %>% 
  mutate_if(is.numeric, scale)  #VERY INTERESTING cuando estandarizamos variables

row.names(base_cluster_pais_reducida1) <- base_cluster_pais_reducida1$cat_pais  

prueba1pais1 <- hcluster( base_cluster_pais_reducida1, link="complete" )
dendprueba1pais1 <- as.dendrogram(prueba1pais1)

dendprueba1pais1 %>% 
  set("labels_col", value = c(brewer.pal(n = 9, name = "Paired")), k=9) %>%
  set("branches_lty", 1) %>%
  set("branches_k_color", value =  c(brewer.pal(n = 9, name = "Paired")), k=9) %>% 
  plot(axes=F, hang = -1 )


colores %>% arrange(cat_pais) %>%  select(cat_pais)
unicos_orgs <- base_tipos_ungrouped %>%  
  distinct(nombres_organizadores) 

# NUEVO NUEVO 2022 prueba por si las

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
                     "n_max_reg")#,
                     #"n_sd_reg"
)


cols_2 <- c(cols_base_cluster_minima, cols_variables2)

dend_prueba22 <- fclusterxpais(df= base_cluster_pais, cols= cols_2, id= "cat_pais", metodolink = "complete" )
fplotclusterxpais(df= base_cluster_pais, dend_prueba22)

# por si las dudas pruebo por afuera de funcion
#fclusterxpais <- function(df, cols, id, metodolink){
  
  # recibe un df, y un conjunto de parametros aes
  # devuelve un grafico de puntos dispersos
id <- "cat_pais"
  df_reducido <- base_cluster_pais %>% 
    select(all_of(cols_2)) %>% 
    mutate_if(is.numeric, scale) 
  
  row.names(df_reducido) <- df[[id]] 
  
  hclust1 <- hcluster(dist(df_reducido)^2, link= "complete" )
  dend1 <- as.dendrogram(hclust1)
  
 # return(dend1)
  
#}

# tests de correlaciones entre variables
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