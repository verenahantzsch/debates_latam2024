# librerias
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)


base <- readxl::read_xlsx("./datav3/base_finalv3.xlsx")


# prueba MCA 
# me gustaron por ahora la 3 y la 4 para comparar 

base_cluster_debate_full <- base %>% 
  select(-starts_with("str")) %>% 
  select(-starts_with("longstr")) %>% 
  select(-starts_with("t")) %>% 
  select(-starts_with("comentarios"))

# horrible primer intento ################

# con de todo. es demasiado. inentendible 
df_mca_xdebate <- base_cluster_debate_full %>% 
  mutate(cat_eleccion = ncat_eleccion) %>% 
  select(starts_with(c("dico", "cat"))) %>% 
  select(-c(dico_programas, dico_obligatorio, dico_impugnado,
            cat_ballotage, cat_temas, cat_formato, cat_ballotage,
            cat_panel)) %>% 
  mutate_all(as.factor) %>% 
  na.omit() %>% 
  mutate(cat_eleccion = as.numeric(cat_eleccion),
         cat_pais = as.character(cat_pais))


mca1 <- MCA(df_mca_xdebate, quanti.sup=26, quali.sup=22, graph = FALSE)
which(colnames(df_mca_xdebate) == "cat_pais") 
plotmca1 <- fviz_mca_biplot(mca1, repel = TRUE,
                            ggtheme = theme_minimal())


# segundo intento ##################
# mucho mas interesting. aunque todavia confusa para visualizar
# con dico_formato, dico_tema y cat_reg. no hay cat de organizador

df_mca_xdebate2 <-  base_cluster_debate_full %>% 
  mutate(cat_eleccion = ncat_eleccion) %>% 
  select(starts_with(c("dico", "cat"))) %>% 
  select(-c(dico_programas, dico_obligatorio, dico_impugnado,
            cat_ballotage, cat_temas, cat_formato, cat_ballotage,
            cat_panel, dico_streaming, dico_analytics, dico_ddreplica,
            dico_comentarios, dico_inserts, dico_analisis, dico_ausencias)) %>% 
  mutate_all(as.factor) %>% 
  na.omit() %>% 
  mutate(cat_eleccion = as.numeric(cat_eleccion),
         cat_pais = as.character(cat_pais))


mca2 <- MCA(df_mca_xdebate2,
            quanti.sup=which(colnames(df_mca_xdebate2) == "cat_eleccion"), 
            quali.sup=which(colnames(df_mca_xdebate2) == "cat_pais"), 
            graph = FALSE)

plotmca2 <- fviz_mca_biplot(mca2, 
                            ggtheme = theme_minimal())

fviz_mca_var(mca2, 
             ggtheme= theme_minimal())

fviz_mca_var(mca2, choice = "quanti.sup",
             ggtheme = theme_minimal())

# tercer intento ################

# aca si incorporamos organizador
# interesante pero regulatorio nos tracciona los resultados un monton.
# organziador esta transformado a dummy

# la dimension 2 parece diferenciar a los paises con debates obligatorios


df_mca_xdebate3 <- base %>% 
  select(starts_with(c("dico_format", "n_cattipo", 
                       "cat_reg", 
                       "ncat_elecc", "cat_pai"))) %>% 
  mutate( dico_formato_publico = ifelse(
    dico_formato_virtuales == TRUE | dico_formato_presentes == TRUE,
    TRUE, FALSE)) %>% 
  mutate( dico_formato_panel = ifelse(
    dico_formato_expertos == TRUE | dico_formato_sectores == TRUE,
    TRUE, FALSE)) %>% 
  mutate( dico_formato_candidatos = ifelse(
    dico_formato_libre == TRUE | dico_formato_duelo == TRUE,
    TRUE, FALSE)) %>% 
  select(-c(dico_formato_expertos, dico_formato_sectores,
            dico_formato_duelo, dico_formato_libre,
            dico_formato_virtuales, dico_formato_presentes)) %>% 
  select(-dico_formato_apertura) %>% 
  mutate(dico_org_educ = ifelse(n_cattipo_educ>0, TRUE, FALSE),
         dico_org_estado = ifelse(n_cattipo_estado>0 | 
                                    n_cattipo_mmp>0, TRUE, FALSE),
         dico_org_mmc = ifelse(n_cattipo_mmc>0, TRUE, FALSE),
         dico_org_osc = ifelse(n_cattipo_osc>0, TRUE, FALSE)) %>% 
  select(-starts_with("n_cattipo"))  %>% 
  mutate_all(as.factor) %>% 
  na.omit() %>% 
  mutate(ncat_eleccion = as.numeric(ncat_eleccion),
         cat_pais = as.character(cat_pais)) %>% 
  mutate(
    cat_regmedios =  fct_collapse(cat_regmedios, NADA = c("POSIBILIDAD", "NADA")), 
    cat_regestado =  fct_collapse(cat_regestado, NADA = c("POSIBILIDAD", "NADA")),
    cat_regcandidatos =  fct_collapse(cat_regcandidatos, NADA = c("POSIBILIDAD", "NADA")) )

mca3 <- MCA(df_mca_xdebate3,
            quanti.sup=which(colnames(df_mca_xdebate3) == "ncat_eleccion"), 
            quali.sup=which(colnames(df_mca_xdebate3) == "cat_pais"), 
            graph = FALSE)

# DISTINTAS VISUALIZACIONES DE RESULTADOS
summary(df_mca_xdebate3)[, 1:17]
plotmca3 <- fviz_mca_biplot(mca3, 
                            ggtheme = theme_minimal())

fviz_mca_var(mca3, choice = "mca.cor",
             ggtheme= theme_minimal())
fviz_mca_var(mca3, choice = "mca.cor",
             ggtheme= theme_minimal(),
             axes = c(2, 3))
fviz_mca_var(mca3, choice = "mca.cor",
             ggtheme= theme_minimal(),
             axes = c(3, 4))

fviz_mca_var(mca3, 
             ggtheme= theme_minimal())

fviz_mca_var(mca3, choice = "quanti.sup",
             ggtheme = theme_minimal())


fviz_screeplot(mca3, addlabels = TRUE, ylim = c(0, 45))
fviz_cos2(mca3, choice = "var", axes = 1)
fviz_cos2(mca3, choice = "var", axes = 4)


# Contributions of rows to dimension 1
fviz_contrib(mca3, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca3, choice = "var", axes = 2, top = 15)
# Contributions of rows to dimension 3
fviz_contrib(mca3, choice = "var", axes = 3, top = 15)
# Contributions of rows to dimension 4
fviz_contrib(mca3, choice = "var", axes = 4, top = 15)

fviz_mca_var(mca3, col.var = "contrib",
             axes = c(3, 4),
             alpha.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             #repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

# cuarto intento ###############

# la regulacion esta explicando demasiado.
# ya que estamos explicando debates y no paises, vamosa ver
# que pasa sin esta variable
# la dimension dos es organizador estatal
# la primera es organizador mediatico versus osc y formato periodistas versus moderador o panel
# la tercera la representan los formatos expositivos verusus los candidatos. ojo. podria ser un outlier
# la cuarta son los debates educativos con panel 
# esta bueno el primer biplot de esta especificacion
# y el plot que muestra que con el tiempo mas no - mediatico y mas si-estado

df_mca_xdebate4 <- df_mca_xdebate3 %>% 
  select(-starts_with("cat_reg"))

mca4 <- MCA(df_mca_xdebate4,
            quanti.sup=which(colnames(df_mca_xdebate4) == "ncat_eleccion"), 
            quali.sup=which(colnames(df_mca_xdebate4) == "cat_pais"), 
            graph = FALSE)
plotmca4 <- fviz_mca_biplot(mca4, 
                            ggtheme = theme_minimal())

fviz_mca_var(mca4, choice = "mca.cor",
             ggtheme= theme_minimal())
fviz_mca_var(mca4, choice = "mca.cor",
             ggtheme= theme_minimal(),
             axes = c(2, 3))
fviz_mca_var(mca4, choice = "mca.cor",
             ggtheme= theme_minimal(),
             axes = c(3, 4))
fviz_mca_var(mca4,
             ggtheme= theme_minimal())
fviz_mca_var(mca4, choice = "quanti.sup",
             ggtheme = theme_minimal())

fviz_screeplot(mca4, addlabels = TRUE, ylim = c(0, 45))
fviz_cos2(mca4, choice = "var", axes = 1)
fviz_cos2(mca4, choice = "var", axes = 2)
fviz_cos2(mca4, choice = "var", axes = 3)
fviz_cos2(mca4, choice = "var", axes = 4)

# Contributions of rows to dimension 1
fviz_contrib(mca4, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca4, choice = "var", axes = 2, top = 15)
# Contributions of rows to dimension 3
fviz_contrib(mca4, choice = "var", axes = 3, top = 15)
# Contributions of rows to dimension 4
fviz_contrib(mca4, choice = "var", axes = 4, top = 15)

fviz_mca_var(mca4, col.var = "contrib",
            # axes = c(3, 4),
             alpha.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             #repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())



# quinto intento ##############
# dejamos solo una reg como proxy
# elegimos regestado

# las primeras dos dimensiones se parecen a la especificacion anterior
# en una es org o no el estado versus org o no los medios
# itneresante ver los casos que quedan en una posicion intermedia en este eje
# la tercera son los formatos mas panel versus expositivos
# la 4 trae el outlier de colombia en "garantizar" 
# la 5ta el outlier expositivo


df_mca_xdebate5 <- df_mca_xdebate3 %>% 
  select(-c(cat_regmedios, cat_regcandidatos))
         
# sexto intento  #############
# con variables ordinales

# los resultados son claramente peores. ej medida en que hay individuos suficientemente representados por las primeras cuatro categorias
# proobablemente en medidas agregadas hay demasiado outlier (cat con pocos count)

df_mca_xdebate6 <- base %>%
  select(cat_pais, ncat_eleccion, 
         ncat_ppac, ncat_competencia,
         ncat_totreg,
         ncat_mean_tipoorg_ambito) %>% 
  mutate_all(as.factor) %>% 
  na.omit() %>% 
  mutate(ncat_eleccion = as.numeric(ncat_eleccion),
         cat_pais = as.character(cat_pais))


# septimo intento ##########

# mismos problemas que anterior
# con mix de ordinales y nominales dummy
df_mca_xdebate7 <- base %>%
  select(cat_pais, ncat_eleccion, 
         ncat_ppac, ncat_competencia,
         ncat_totreg,
         n_cattipo_educ,
         n_cattipo_estado,
         n_cattipo_mmc,
         n_cattipo_mmp,
         n_cattipo_osc
         ) %>% 
  mutate(dico_org_educ = ifelse(n_cattipo_educ>0, TRUE, FALSE),
         dico_org_estado = ifelse(n_cattipo_estado>0 | 
                                    n_cattipo_mmp>0, TRUE, FALSE),
         dico_org_mmc = ifelse(n_cattipo_mmc>0, TRUE, FALSE),
         dico_org_osc = ifelse(n_cattipo_osc>0, TRUE, FALSE)) %>% 
  select(-starts_with("n_cattipo"))  %>% 
  mutate_all(as.factor) %>% 
  na.omit() %>% 
  mutate(ncat_eleccion = as.numeric(ncat_eleccion),
         cat_pais = as.character(cat_pais))




# octavo intento ######
# sigue teniendo como problema los NAs
# pero esperamos haber solucionado parcialmente los outliers
# primera dimension diferencia a los debates de los que no tenemos datos del formato de los que tenemos
# segunda a los que tienen mucha regulacion del resto
# tercera a los orgs por el estado con ppac. cuarta a los org por educ
# la verdad que es dificil de interpretar

df_mca_xdebate8 <- base %>%
  select(cat_pais, ncat_eleccion, 
         ncat_ppac, ncat_competencia,
         ncat_totreg,
         n_cattipo_educ,
         n_cattipo_estado,
         n_cattipo_mmc,
         n_cattipo_mmp,
         n_cattipo_osc
  ) %>% 
  mutate( ncat_totreg = ifelse(ncat_totreg <= 3, 
                                "nada_reg",
                                ifelse(ncat_totreg >= 10,
                                       "mucha_reg",
                                       "intermedia_reg"))) %>% 
  mutate(dico_org_educ = ifelse(n_cattipo_educ>0, TRUE, FALSE),
         dico_org_estado = ifelse(n_cattipo_estado>0 | 
                                    n_cattipo_mmp>0, TRUE, FALSE),
         dico_org_mmc = ifelse(n_cattipo_mmc>0, TRUE, FALSE),
         dico_org_osc = ifelse(n_cattipo_osc>0, TRUE, FALSE)) %>% 
  select(-starts_with("n_cattipo"))  %>% 
  mutate_all(as.factor) %>% 
  mutate(ncat_ppac = fct_collapse(ncat_ppac,
    "nada_ppac" = "0",
    "poca_ppac" = c("1", "2"),
    "mucha_ppac" = c("3", "4") ),
  ncat_competencia = fct_collapse(ncat_competencia,
    "nada_compet" = c("0", "1"),
    "poca_compet" = c("3", "2"),
    "mucha_compet" = c("5", "4") ) ) %>% 
  na.omit() %>% 
  mutate(ncat_eleccion = as.numeric(ncat_eleccion),
         cat_pais = as.character(cat_pais))

summary(df_mca_xdebate8)

