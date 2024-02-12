# LIBRERIAS ######
library(tidyverse)
library(ggforce)

#
# Importación de datos #####

base <- readxl::read_xlsx("base_finalv1.xlsx") # base con datos por debate
elecciones <-  readxl::read_xlsx("base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país

# SUBTIPOS DE ORGANIZADORES  ######
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

base_organizadores_distinct <- readxl::read_xlsx("base_organizadores_distinct.xlsx")

base_organizadores_distinct <- base_organizadores_distinct %>% 
  left_join(readxl::read_xlsx("distinct_cat_subtipoorg.xlsx"))

# pruebo hacer wide, no se si esto funciona o tiene sentido
#base_subtiposdistinct_wide <- base_organizadores_distinct  %>% 
#  mutate(values = 1) %>% 
#  pivot_wider(names_from = "ncat_subtipov2", 
#              names_prefix = "dico_catsubtipo_",
#              names_sep = ";", 
#              names_sort = TRUE,
#              names_repair = "check_unique",
#              values_from = values) 
#
#base_organizadores_wide <- base_organizadores %>% 
#  left_join(base_subtiposdistinct_wide)

# version no wide

base_subtipos <- base_organizadores %>%  
  mutate(nombres_organizadores = str_trim(nombres_organizadores)) %>% 
  # con el str trim se resuelven algunos problemas pero OJO que hay registros duplicados
  left_join(base_organizadores_distinct %>% 
              distinct(nombres_organizadores, ncat_eleccion, cat_pais,
                       ncat_subtipov2, cat_tipoorgv2,
                       ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
                       cat_areaorg))
# aplicando de nuevo distinct se resolvio la duplicacion

# GRAFO PRUEBAS ###########
# PREPARACION DATOS #####
# pruebas fallidas ######
# la encontre en internet, no me sirve
# as_edgelist.weighted_incidence_matrix <- function(x, drop_rownames = TRUE) {
#   melted <- do.call(cbind, lapply(list(row(x), col(x), x), as.vector)) # 3 col matrix of row index, col index, and `x`'s values
#   
#   filtered <- melted[melted[, 3] != 0, ] # drop rows where column 3 is 0
#   
#   # data frame where first 2 columns are...
#   df <- data.frame(mode1 = rownames(x)[filtered[, 1]], # `x`'s rownames, indexed by first column in `filtered``
#                    mode2 = colnames(x)[filtered[, 2]], # `x`'s colnames, indexed by the second column in `filtered`
#                    weight = filtered[, 3],             # the third column in `filtered`
#                    stringsAsFactors = FALSE)
#   
#   out <- df[order(df$mode1), ] # sort by first column
#   
#   if (!drop_rownames) {
#     return(out)
#   }
#   `rownames<-`(out, NULL)
# }
# 
# 
# 
# 
# library(igraph)
# prueba <- aggregate(id_debate~nombres_organizadores,edge_df)
# 
# ,function(x)combn(as.character(x),2,paste0,collapse=","))[2]
# 
# as.matrix(as_adj(graph_from_data_frame(read.csv(text=unique(unlist(prueba)),h=F),F)))
# 
# 
# my_f <-function(x)  do.call(rbind,combn(as.character(x),2,simplify = F))
# 
# a <-unique(do.call(rbind,aggregate(id_debate~nombres_organizadores,edge_df,my_f)[,2]))
# 
# as.matrix(as_adj(graph_from_edgelist(a)))
# 2 Counting co-occurrences
# The counting of the joint word occurrence is easily possible via a matrix multiplication (https://en.wikipedia.org/wiki/Matrix_multiplication) on the binary DTM. For this purpose, the transposed matrix (dimensions: nTypes x nDocs) is multiplied by the original matrix (nDocs x nTypes), which as a result encodes a term-term matrix (dimensions: nTypes x nTypes).
# 
# matrix1 <- edge_df %>%
#   mutate(n=1) %>% 
#   pivot_wider(names_from = nombres_organizadores, values_from = n) %>% 
#   as.matrix()
# 
# matrix2 <- edge_df %>%
#   mutate(n=1) %>% 
#   pivot_wider(names_from = id_debate, values_from = n) %>% 
#   as.matrix()
# 
# matrix3 <- matrix1 %*% matrix2
# matrixedge <- as.matrix(edge_df)
# graphedge <- graph_from_edgelist(matrixedge)
# 
# plotedge <- plot(graphedge)
# 
# matrixedge <- as.matrix(edge_v3)
# graphedge <- graph_from_edgelist(matrixedge)
# plotedge <- plot(graphedge) 
# ## queria probar si daba el mismo resultado y en efecto
# nodes2 <- base_subtipos %>%  
#   distinct(nombres_organizadores, cat_pais) %>% 
#   left_join(base_subtipos %>% 
#               select(cat_pais, nombres_organizadores, cat_tipoorgv2, ncat_subtipov2, cat_areaorg)) %>% 
#   mutate(cat_pais = ifelse(nombres_organizadores=="CNN en Español", "internacional", cat_pais)) %>% 
#   rowid_to_column("id_organizador")  %>%
#   distinct(nombres_organizadores, cat_tipoorgv2, ncat_subtipov2, cat_areaorg, cat_pais)


# pruebas current ######

edge_v1 <- base_subtipos %>% 
  filter(!is.na(nombres_organizadores)) %>% 
  mutate(cat_pais = ifelse(nombres_organizadores=="CNN en Español", "internacional", cat_pais)) %>% 
  mutate(id_str_org = paste(cat_pais, nombres_organizadores)) %>% 
  select(id_debate, id_str_org) 


adjacency_v1 <- edge_v1  %>%
  mutate(n=1) %>% 
  pivot_wider(names_from = id_str_org, values_from = n) %>%
  left_join(edge_v1) %>% 
  select(-c(id_debate)) 

edge_v2 <- adjacency_v1 %>% 
  pivot_longer(cols = -c(id_str_org), names_to = "nombres_2", values_to = "n") %>% 
  filter(!is.na(n)) 

edge_v3 <- edge_v2 %>% 
  filter(!id_str_org==nombres_2)

# excluyendo si mismos
edge_v4 <- edge_v3 %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(id_str_org, nombres_2) %>% 
  summarise(weight = sum(n))

# incluyendo si mismos
edge_v5 <-  edge_v2 %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(id_str_org, nombres_2) %>% 
  summarise(weight = sum(n))


# nodes list
nodes <- base_subtipos %>% 
  distinct(nombres_organizadores, cat_pais) %>% 
  mutate(cat_pais = ifelse(nombres_organizadores=="CNN en Español", "internacional", cat_pais)) %>% 
  mutate(id_str_org = paste(cat_pais, nombres_organizadores)) %>% 
  distinct(id_str_org) %>% 
  rowid_to_column("id_organizador")  %>% 
  left_join(base_subtipos %>% 
              group_by(nombres_organizadores) %>% 
              mutate(n_participaciones = n()) %>% 
              ungroup %>% 
              mutate(cat_pais = ifelse(nombres_organizadores=="CNN en Español", "internacional", cat_pais)) %>% 
              mutate(id_str_org = paste(cat_pais, nombres_organizadores)) %>% 
              select(id_str_org, cat_tipoorgv2, ncat_subtipov2, cat_areaorg, cat_pais, n_participaciones)) %>% 
  distinct(id_organizador, id_str_org, cat_tipoorgv2, ncat_subtipov2, cat_areaorg, cat_pais, n_participaciones)


# SIN CONTAR SI MISMOS
edges <- edge_v4 %>% 
  left_join(nodes %>% 
              select(id_str_org, id_organizador)) %>% 
  rename(from = id_organizador)

edges <- edges %>% 
  left_join(nodes %>% 
              select(id_str_org, id_organizador), 
            by = c("nombres_2" = "id_str_org")) %>% 
  rename(to = id_organizador)

edges <-  edges %>% 
  ungroup() %>% 
  select(from, to, weight) 

# CONTANDO SI MISMOS

edges2 <- edge_v5 %>% 
  left_join(nodes %>% 
              select(id_str_org, id_organizador)) %>% 
  rename(from = id_organizador)

edges2 <- edges2 %>% 
  left_join(nodes %>% 
              select(id_str_org, id_organizador), 
            by = c("nombres_2" = "id_str_org")) %>% 
  rename(to = id_organizador)

edges2 <-  edges2 %>% 
  ungroup() %>% 
  select(from, to, weight) 


# GRAFICOS ##############
# DOS PAQUETES POSIBLES
# Network ####


library(network)
network_o <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
plot(network_o, vertex.cex = 3)

# IGRAPH #######

detach(package:network)
rm(routes_network)
library(igraph)
igraph_o <- graph_from_data_frame(d = edges2, vertices = nodes, directed = TRUE)

plot(igraph_o, edge.arrow.size = 0.2)
plot(igraph_o, layout = layout_with_graphopt, edge.arrow.size = 0.2)


# TABLE GRAPH E GGRAPH

library(tidygraph)
library(ggraph)

go_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
go_tidy2 <- as_tbl_graph(igraph_o)

gggraphp <- go_tidy2 %>% 
  ggraph('fr') +  # mds me gusto pero no se que carajo hace. grid y cicrle tb. fr el primero que probe
  geom_edge_link() + 
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  geom_node_point(aes(colour = cat_tipoorgv2, size = n_participaciones)) + 
  #geom_edge_loop(aes(colour = year)) +
  theme_graph()

#  layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
# 'randomly', 'fr', 'kk', 'drl', 'lgl')
