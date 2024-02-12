# librerias ############
library(tidyverse)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)

# documentacion a mano ##########

# install.packages("amap")

# hcluster(x, method = "euclidean", diag = FALSE, upper = FALSE,
#          link = "complete", members = NULL, nbproc = 2,
#          doubleprecision = TRUE)
# method
# the distance measure to be used. 
# "euclidean", "maximum", "manhattan", "canberra", 
# "binary", "pearson", "abspearson", "correlation", 
# "abscorrelation", "spearman" or "kendall". 
# 
# link
# the agglomeration method to be used. 
# This should be (an unambiguous abbreviation of) one of
# "ward", "single", "complete", "average",
# "mcquitty", "median" or "centroid","centroid2".

# datos  ###########

base <- readxl::read_xlsx("./datav3/base_finalv3.xlsx")
elecciones <-  readxl::read_xlsx("./datav3/base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país
base_normativa <- readxl::read_xlsx("./datav3/base_normativa.xlsx")
base_anual <- readxl::read_xlsx("./datav3/base_anualv3.xlsx")


# POR AÑO PAIS #############################

# preparo datos ##################


base_cluster_año_pais <- base %>% 
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
         dico_formato_periodistas
  ) %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  summarise(n_debates_eleccion = n(),
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
            ncat_totreg  = mean(ncat_totreg, na.rm=TRUE),
            n_proporcioninvitados = mean(n_proporcioninvitados, na.rm=TRUE),
            n_indexausentes = mean(n_indexausentes, na.rm=TRUE),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm=TRUE), # las de la org
            n_cattipo_estado = sum(n_cattipo_estado, na.rm=TRUE)+sum(n_cattipo_mmp, na.rm=TRUE),
            n_cattipo_mmc  = sum(n_cattipo_mmc, na.rm=TRUE),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm=TRUE),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm=TRUE),
            pr_dico_formato_periodistas = sum(dico_formato_periodistas, na.rm=TRUE)/n_debates_eleccion) # %>% 


base_cluster_año_pais <- base_anual %>% 
  left_join(base_cluster_año_pais)  %>% 
  replace(., is.na(.), 0) 


# pruebo clustering año pais ##################

# prueba con todo, cualca ########

base_cluster_año_pais <- base_cluster_año_pais %>% 
  mutate(id = paste(cat_pais, ncat_eleccion))
row.names(base_cluster_año_pais) <- base_cluster_año_pais$id

prueba1añopais1 <- hcluster( base_cluster_año_pais, link="complete" )
plot(prueba1añopais1)

# Para interpretar
dendprueba1añopais1 <- as.dendrogram(prueba1añopais1)
dend_list2 <- get_subdendrograms(dendprueba1añopais1, 8)
plot(dend_list2[[1]])
plot(dend_list2[[2]])
plot(dend_list2[[3]]) 
plot(dend_list2[[4]])
plot(dend_list2[[5]])
plot(dend_list2[[6]])
plot(dend_list2[[7]])

#### prueba 2 reducida ##########

base_cluster_año_pais2 <- base_cluster_año_pais%>% 
  select(n_debates_eleccion,
       ncat_mean_tipoorg_ambito2,
       n_sd_tipoorg_ambito,  
       n_sd_competencia,
       n_sd_ppac,
       ncat_meanppac,
       ncat_meancompetencia,
       ncat_totreg,
       n_indexausentes,
       cat_pais,
       ncat_eleccion) %>% 
  mutate(ncat_eleccion = as.character(ncat_eleccion)) %>% 
  mutate_if(is.numeric, scale) 

base_cluster_año_pais2 <- base_cluster_año_pais2 %>% 
  mutate(id = paste(cat_pais, ncat_eleccion)) %>% 
  select(-ncat_eleccion, cat_pais)

row.names(base_cluster_año_pais2) <- base_cluster_año_pais2$id

prueba1añopais2 <- hcluster( base_cluster_año_pais2, link="complete" )
plot(prueba1añopais2)

# Para interpretar. very confusing
dendprueba1añopais2 <- as.dendrogram(prueba1añopais2)
list_dendprueba1añopais2 <- get_subdendrograms(dendprueba1añopais2, 8)
plot(dend_list2[[1]])
plot(dend_list2[[2]])
plot(dend_list2[[3]]) 
plot(dend_list2[[4]])
plot(dend_list2[[5]])
plot(dend_list2[[6]])
plot(dend_list2[[7]])
plot(dend_list2[[8]])

# POR PAIS ############
# preparo base por pais ###########
#colnames(base) 
elecciones_sin_debates_filtrado_grouped <- base_anual %>% 
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
            n_interrupciones = n()-n_elecciones_con_debates)

base_cluster_pais <- base %>% 
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
         n_orgsxdebate,
         dico_formato_periodistas
         ) %>% 
  group_by(cat_pais, ncat_eleccion) %>%
  mutate( n_debates_en_eleccion = n()) %>% 
  ungroup() %>% 
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
            n_cattipo_educ = sum(n_cattipo_educ, na.rm=TRUE), # las de la org
            n_cattipo_estado = sum(n_cattipo_estado)+sum(n_cattipo_mmp, na.rm=TRUE),
            n_cattipo_mmc  = sum(n_cattipo_mmc, na.rm=TRUE),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm=TRUE),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm=TRUE),
            n_tot_orgs = sum(n_orgsxdebate, na.rm=TRUE),
            pr_dico_formato_periodistas = sum(dico_formato_periodistas)/n_debates_total) %>% 
  mutate(n_edad_practica_absoluta = 2021-ncat_año_primer_debate)%>% 
  replace(., is.na(.), 0)

base_cluster_pais <- base_cluster_pais %>% 
  left_join(elecciones_sin_debates_filtrado_grouped) %>% 
  replace(., is.na(.), 0)
  
# prueba 1 clustering ###########
# con variables ordinales

base_cluster_pais_reducida1 <-  base_cluster_pais %>% 
  select(n_debates_total, # faltaria hacer algun calculo de las interrupciones 
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
plot(prueba1pais1)

dendprueba1pais1 <- as.dendrogram(prueba1pais1)

dendprueba1pais1 %>% 
  set("labels_col", value = c(distinct(base$cols_18)), k=18) %>%
  set("branches_lty", 1) %>%
  set("branches_k_color", value =  c(distinct(base$cols_18)), k=18) %>% 
  plot(axes=F, hang = -1 )
# dendprueba1pais1 %>%
#   set("labels_col", value = c("skyblue", "green", "red", "black", "orange", "grey"), k=5) %>%
#   set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
#   plot(horiz=TRUE, axes=FALSE)
# abline(v = 350, lty = 2)
# , hang = -1, cex = 0.6
# rep dom y arg por poca cantidad de debates
# este es hermoso

prueba2pais1 <- hcluster( base_cluster_pais_reducida1, link="ward" )
dendprueba2pais1 <- as.dendrogram(prueba2pais1)

plot(prueba2pais1)  # a niveles superiores de agregacion el resultado es algo distinto
# aca si a diferencia de en la proxima prueba cambian bastante los agrupamientos
prueba3pais1 <- hcluster( base_cluster_pais_reducida1, link="average")
plot(prueba3pais1) 

prueba4pais1 <- hcluster( base_cluster_pais_reducida1, link=  "centroid")                          
plot(prueba4pais1) 


# para plotear y comparar dos dendogramas

# Custom these kendo, and place them in a list
dl <- dendlist(
  dendprueba2pais1 %>% 
    set("labels_col", value = c("#DAF7A6", "#FFC300", "#FF5733", "#D2C3D3", "#C9EEEA"), k=5) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value =  c("#DAF7A6", "#FFC300", "#FF5733", "#D2C3D3", "#C9EEEA"), k=5),
  dendprueba1pais1 %>% 
    set("labels_col", value =  c("#DAF7A6", "#FFC300", "#FF5733", "#D2C3D3", "#C9EEEA"), k=5) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value =  c("#DAF7A6", "#FFC300", "#FF5733", "#D2C3D3", "#C9EEEA"), k=5)
)

# Plot them together
tanglegram(dl, 
           common_subtrees_color_lines = FALSE, highlight_distinct_edges  = TRUE, highlight_branches_lwd=FALSE, 
           margin_inner=7,
           lwd=2
)


# segundo intento por pais. con variables pr ########
#es mala prueba porque pr esta calculado en forma absoluta
# entonces tira mucho el n debates, que esta contabilizado varias veces
# ademas hay demasiadas variables de regulacion juntas

base_cluster_pais_reducida2 <-  base_cluster_pais %>% 
  select(-ncat_mean_tipoorg_visibilidad,
         ncat_mean_tipoorg_ambito2,
         pr_dico_formato_periodistas) %>% 
  mutate_if(is.numeric, scale) 

row.names(base_cluster_pais_reducida2) <- base_cluster_pais_reducida2$cat_pais  

prueba1pais2 <- hcluster( base_cluster_pais_reducida2, link="complete" )
plot(prueba1pais2)

prueba2pais2 <- hcluster( base_cluster_pais_reducida2, link="ward" )
plot(prueba2pais2)  # a niveles superiores de agregacion el resultado es algo distinto
# chile, peru y guatemala siguen apareciendo juntos por
# peso os. carencia de regulacion!!
# recordemos que aplanan el tiempo

# tercera prueba por pais ##################
# tratamos de solucionar el problema anterior

# sugeridos: todavia no hice los de tradicion.
# cargar la base "elecciones"
# indicadores formato	sd formatos
# promedio formatos
# indicadores tradicion	n eleccions SIN debates desde primero
# persistencia de actores en el tiempo
# indicadores org	pr mayor de tipo de org
# regulacion	regulacion

# colnames(base_cluster_pais)

base_cluster_pais_reducida3 <-  base_cluster_pais %>% 
      select(  cat_pais,
               #n_debates_total,
        n_promedio_debates_eleccion,
       # n_sd_n_debates, 
        ncat_meanppac,
        ncat_meancompetencia,# formato y desvio formato. se podria agregar una dummy period
        n_sd_competencia,
        n_sd_ppac,   
        n_max_reg, #entendemos que mas variables de regulacion son algo redundantes
        pr_dico_formato_periodistas,
        n_indexausentes,            
        n_cattipo_educ,
        n_cattipo_estado,
        n_cattipo_mmc,
        n_cattipo_osc,
        n_edad_practica_absoluta,
        n_tot_orgs,
        n_interrupciones)  %>% 
  mutate(
    pr_cattipo_educ  = n_cattipo_educ/n_tot_orgs, # convertimos a proporcion las org
    pr_cattipo_estado = n_cattipo_estado/n_tot_orgs,
    pr_cattipo_mmc = n_cattipo_mmc/n_tot_orgs,
    pr_cattipo_osc = n_cattipo_osc/n_tot_orgs
  ) %>% 
  select( -c(
    n_cattipo_educ,
    n_cattipo_estado,
    n_cattipo_mmc,
    n_cattipo_osc,
    n_tot_orgs
  )) %>% 
  mutate_if(is.numeric, scale) 

row.names(base_cluster_pais_reducida3) <- base_cluster_pais_reducida3$cat_pais  

prueba1pais3 <- hcluster( base_cluster_pais_reducida3, link="complete" )
plot(prueba1pais3)

prueba2pais3 <- hcluster( base_cluster_pais_reducida3, link="ward" )
plot(prueba2pais3) 

prueba3pais3 <- hcluster( base_cluster_pais_reducida3, link="average" )
plot(prueba3pais3) 

prueba4pais3 <- hcluster( base_cluster_pais_reducida3, link="centroid" )
plot(prueba4pais3) 

# prueba por debate #################

# preparo base por debate

base_cluster_debate_full <- base %>% 
  select(-starts_with("str")) %>% 
  select(-starts_with("longstr")) %>% 
  select(-starts_with("t")) %>% 
  select(-starts_with("comentarios"))

base_cluster_debate <- base_cluster_debate_full %>% 
  select( id_debate,
              cat_pais
             , ncat_eleccion                 
             , ncat_ronda
             , dico_streaming
             , n_jornadas                   
             , ncat_ppac
             , ncat_competencia            
             , dico_analytics
             , dico_temas_puntuales        
             , dico_temas_libre
             , dico_temas_monotema        
             , dico_temas_bloques
             , ncat_regmedios
             , ncat_regestado
             , ncat_regcandidatos
             , ncat_totreg #
             , n_strorganizador
             , n_presentes
             , n_ausentes
             , n_invitados #                
             , n_proporcioninvitados
             , dico_formato_expositivo      
             , n_catformatos
             , pr_formatoapertura           
             , pr_formatoduelo
             , pr_formatoexpertos
             , pr_formatolibre
             , pr_formatomoderadores
             , pr_formatoperiodistas
             , pr_formatopresentes
             , pr_formatosectores
             , pr_formatovirtuales
             , pr_formatoexpositivo
             , n_cattemas                   
             , pr_temapuntuales
             , pr_temabloques             
             , pr_temalibre
             , pr_temamonotema             
             , n_cattipo_educ
             , n_cattipo_estado          
             , n_cattipo_mmc
             , n_cattipo_mmp
             , n_cattipo_osc
             , n_cattipo_NA
             , n_orgsxdebate
             , n_variedadorgsxdebate
             , n_variedadsubtiposxdebate
             , ncat_mean_tipoorg_ambito
             , ncat_mean_tipoorg_visibilidad
             , n_porcentajeausentes       
             , n_proporcionausentes
             , n_indexausentes          
             , dico_ausencias )

base_cluster_debate <- base_cluster_debate %>% 
  group_by(cat_pais, ncat_eleccion) %>% 
  mutate( pr_debate_en_eleccion = 1/n()) %>% 
  ungroup()

base_cluster_debate_filtro1 <- base_cluster_debate %>% 
  mutate(id_debate2 = paste(cat_pais, ncat_eleccion, id_debate),
         id_pais = cat_pais) %>% 
  select(-starts_with("cat")) %>% 
  select(-starts_with("dico")) %>% 
  select(-c(ncat_eleccion, n_jornadas, ncat_ronda, id_debate)) %>% 
  mutate(ncat_ppac = as.numeric(ncat_ppac),
         ncat_competencia = as.numeric(ncat_competencia)) %>% 
  mutate_if(is.numeric, scale)

# prueba con primer filtro base enorme #####

row.names(base_cluster_debate_filtro1) <- base_cluster_debate_filtro1$id_debate2
prueba1debate <- hcluster( base_cluster_debate_filtro1, link="ward" )

# Para interpretar
dend_list <- get_subdendrograms(dend1debate, 9)

# Plotting the result
par(mfrow = c(2,3))
plot(dend1debate, main = "Original dendrogram")
sapply(dend_list, plot)
                      # si solo dividimos en 6 
plot(dend_list[[1]]) #nas. se mantiene
plot(dend_list[[2]]) # parecen mas nas pero un poco mas modernos.
plot(dend_list[[3]]) #  paises de trad y modernos # aca cambio. bolivia. chile. los de asoc de medios. 
plot(dend_list[[4]]) # paises trad y estatales # parecen los de osc empresariales
plot(dend_list[[5]]) # otra sere de paises trad. #medios publicos? no se cual diferencia
plot(dend_list[[6]]) # org por e
plot(dend_list[[7]]) # aca quedaron los estatales
plot(dend_list[[8]]) # brasil y costa rica
plot(dend_list[[9]]) # otros medios comerciales
# intentos no exitosos de interpretacion
ggraph(dend1debate) + 
  geom_edge_elbow() + 
  coord_fixed()

ggraph(dend1debate, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() + 
  geom_node_point(aes(filter = leaf)) + 
  coord_fixed()

orderhclust <- prueba1debate$order
labelshclust <- prueba1debate$labels

aber <- cutree(prueba1debate, k=3) %>%  tibble() 
aber$orden <-  orderhclust
aber$label <-  labelshclust
aber2 <- cutree(prueba1debate, k=5) %>%  tibble()
aber3 <- cutree(prueba1debate,k = 1:5)
table(aber32 = aber3[,"2"], aber34 = aber3[,"4"])

#identify(prueba1debate)

dend1debate <- as.dendrogram(prueba1debate) 

orderdend <- order.dendrogram(dend1debate)
labelsdend <- labels(dend1debate)

colorsbase <- base %>% 
  select(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_sub(cat_pais, end = 3))

colors <- tibble(orderdend, labelsdend) %>% 
  mutate(cat_pais = str_sub(labelsdend, end = 3)) %>% 
  left_join(colorsbase)

labels_colors(dend1debate) 

labels_colors(dend1debate) <- colors$cols_18

plot(dend1debate)
plot(as.phylo(prueba1debate), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)


## segundo filtrado

colnames(base_cluster_debate_filtro1)


base_cluster_debate_filtro2 <- base_cluster_debate  %>% 
  mutate(id_debate2 = paste(cat_pais, ncat_eleccion, id_debate)) %>% 
          select( n_proporcioninvitados     
          ,n_strorganizador
          ,ncat_ppac                 
          ,ncat_competencia
          ,n_catformatos          
          ,pr_formatoapertura       
          ,pr_formatoduelo       
          ,pr_formatoexpertos        
          ,pr_formatolibre         
          ,pr_formatomoderadores    
          ,pr_formatoperiodistas
          ,pr_formatopresentes       
          ,pr_formatosectores       
          ,pr_formatovirtuales     
          ,pr_formatoexpositivo     
          ,n_cattemas          
          ,pr_temapuntuales       
          ,pr_temabloques           
          ,pr_temalibre           
          ,pr_temamonotema           
          ,n_cattipo_educ          
          ,n_cattipo_estado        
          ,n_cattipo_mmc        
          ,n_cattipo_mmp          
          ,n_cattipo_osc       
          ,n_cattipo_NA           
          ,n_orgsxdebate
          ,n_indexausentes, id_debate2) %>%  
  mutate(ncat_ppac = as.numeric(ncat_ppac),
         ncat_competencia = as.numeric(ncat_competencia)) %>% 
  mutate_if(is.numeric, scale)

  
row.names(base_cluster_debate_filtro2) <- base_cluster_debate_filtro2$id_debate2
prueba1debate2 <- hcluster( base_cluster_debate_filtro2, link="ward" )
dend1debate2 <- as.dendrogram(prueba1debate2)

# Para interpretar
dend_list2 <- get_subdendrograms(dend1debate2, 4)
plot(dend_list2[[1]])#nas
plot(dend_list2[[2]])
plot(dend_list2[[3]]) #nas
plot(dend_list2[[4]])
plot(dend_list2[[5]])
plot(dend_list2[[6]])
plot(dend_list2[[7]])

# ta interesante. primer grupo mas estado y osc. segundo mas comercial (24, 44)
dend_list22 <- get_subdendrograms(dend_list2[[2]], 4)
plot(dend_list22[[1]])
plot(dend_list22[[2]])
plot(dend_list22[[3]])
plot(dend_list22[[4]])

dend_list24 <- get_subdendrograms(dend_list2[[4]], 4)
plot(dend_list24[[1]])
plot(dend_list24[[2]])
plot(dend_list24[[3]])
plot(dend_list24[[4]])