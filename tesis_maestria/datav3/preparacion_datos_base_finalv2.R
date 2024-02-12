# LIBRERIAS #####
library(tidyverse)


# IMPORTO DATOS ###########
base <- readxl::read_xlsx("./datav3/base_finalv2.xlsx") # base con datos por debate


# TUNEO BASE BASE ############
# DE UN DEBATE POR FILA #######
# agrego id ###########

base <- base %>% 
  rowid_to_column("id_debate") #agrego id de debate


# creo data para añadir colores #########


cols_18 <- c("#00A5E3","#8DD7BF","#FF96C5","#FF5768", "#FFBF65",
             "#6C88C4","#E77577", "#F2D4CC", "#FFD872", "#FC6238",
             "#00CDAC","#FF6F68", "#FFEC59","#FF60A8","#CFF800",
             "#74737A", "#00B0BA", "#C05780")
cat_pais <- fct_unique(as.factor(base$cat_pais))
df_colores <- tibble(cols_18, cat_pais)

base <- base %>% 
  left_join(df_colores)
  

# añado datos ordinales ########

# sobre regulacion #######
base <- base %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regmedios.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regestado.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regcandidatos.xlsx")) %>% 
  mutate(ncat_totreg = ncat_regmedios + ncat_regestado + ncat_regcandidatos )


# sobre cantidad de candidatos ########


base <- base  %>% 
  mutate(n_strorganizador = str_count(str_organizador, ";")+1,
         n_presentes = str_count(str_presentes, ";")+1,
         n_ausentes = ifelse(str_ausentes=="NA","NA", ifelse(
           str_detect(str_ausentes, "sin ausencias"), 
           0, str_count(str_ausentes, ";")+1))) 

base<-  base %>% 
  mutate(n_ausentes = as.numeric(n_ausentes),
         n_invitados = n_presentes + n_ausentes)

base <-  base %>% 
  mutate(n_candidaturas = as.numeric(n_candidaturas),
         n_candidaturas = ifelse(ncat_ronda==2, 2, n_candidaturas),
         n_proporcioninvitados = n_invitados/n_candidaturas)


# mejoro dicotomicas del formato #######

base <- base %>% # primero algunas transformaciones de los datos
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
    pr_formatoapertura = dico_formato_apertura/n_catformatos,
    pr_formatoduelo = dico_formato_duelo/n_catformatos,
    pr_formatoexpertos =   dico_formato_expertos/n_catformatos,
    pr_formatolibre =   dico_formato_libre/n_catformatos,
    pr_formatomoderadores =   dico_formato_moderadores/n_catformatos,
    pr_formatoperiodistas =   dico_formato_periodistas/n_catformatos, 
    pr_formatopresentes =   dico_formato_presentes/n_catformatos,
    pr_formatosectores =   dico_formato_sectores/n_catformatos, 
    pr_formatovirtuales =   dico_formato_virtuales/n_catformatos,
    pr_formatoexpositivo =   dico_formato_expositivo/n_catformatos )
# nota/ pendiente: ver manera de hacer esto más automático

# mejoro dicotomicas de los temas #########


base <- base %>% # primero algunas transformaciones de los datos
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
    pr_temapuntuales = dico_temas_puntuales/n_cattemas,
    pr_temabloques = dico_temas_bloques/n_cattemas,
    pr_temalibre = dico_temas_libre/n_cattemas,
    pr_temamonotema = dico_temas_monotema/n_cattemas
  ) 


# agrego data de organizadores. OJO computar despues de lo de abajo ######

# paso data de base organizadores a wide

base_organizadores_wide <- base_organizadores %>% 
  select(-c(nombres_organizadores, cat_areaorg, ncat_subtipov2,
            ncat_tipoorg_ambito, ncat_tipoorg_visibilidad, t_fecha)) %>% 
     mutate(values = 1) %>% 
     pivot_wider(names_from = "cat_tipoorgv2", 
                 names_prefix = "n_cattipo_",
                 names_sep = ";",
                 names_sort = TRUE,
                 names_repair = "check_unique",
                 values_fill = 0,
                 values_from = values,
                 values_fn = sum)

base_organizadores_grouped <- base_organizadores %>% 
  group_by(id_debate) %>% 
  summarise(n_orgsxdebate = n(),
            n_variedadorgsxdebate = n_distinct(cat_tipoorgv2),
            n_variedadsubtiposxdebate = n_distinct(ncat_subtipov2),
            ncat_mean_tipoorg_ambito = mean(ncat_tipoorg_ambito),
            ncat_mean_tipoorg_visibilidad = mean(ncat_tipoorg_visibilidad))

base <- base %>% 
  left_join(base_organizadores_wide) %>% 
  left_join(base_organizadores_grouped)

# agrego indice de ausencias ########

# creo base con un nombre de ausente por fila 

base_ausentes <- base %>% 
  select(id_debate, 
         ncat_eleccion,
         cat_pais,
         str_ausentes,
         n_ausentes,
         n_invitados) %>%
  mutate(nombres_ausentes = strsplit(str_ausentes, ";")) %>% 
  unnest(nombres_ausentes)

# leo base ya modificada

base_datosausentes <- readxl::read_xlsx("./exploracion_base_finalv1/base_datosausentes.xlsx")

# uno datos a base de todos los ausentes

base_ausentes <- base_ausentes %>% 
  left_join(base_datosausentes)  

# sumariso
base_ausentes_grouped <- base_ausentes %>% 
  group_by(id_debate) %>% 
  summarise( n_porcentajeausentes = sum(as.numeric(n_porcentajevotos), na.rm = TRUE))

base <- base %>% 
  left_join(base_ausentes_grouped)   %>% 
  mutate(n_proporcionausentes = n_ausentes/n_invitados) %>% 
  mutate( n_indexausentes = n_proporcionausentes*n_porcentajeausentes,
          dico_ausencias = ifelse(n_ausentes==0, 0, 1)) %>% 
  mutate(n_indexausentes = ifelse(n_ausentes==0, 0, n_indexausentes),
         n_porcentajeausentes = ifelse(n_ausentes==0, 0, n_porcentajeausentes))

# guardo v3 ######

base %>%  writexl::write_xlsx("./datav3/base_finalv3.xlsx")

# CREO BASES AUXILIARES #############

# base de organizador por fila #################

base_organizadores <- base %>% # BASE CON UN ORG POR FILA 
  select(id_debate, ncat_eleccion, cat_pais, t_fecha, ncat_ronda, 
         str_organizador, n_strorganizador) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  unnest(nombres_organizadores) 

# UNO DATOS CARGADOS

base_organizadores_distinct <-  readxl::read_xlsx("./exploracion_base_finalv1/base_organizadores_distinct.xlsx") %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_subtipoorg.xlsx"))

base_organizadores <- base_organizadores %>%  
  mutate(nombres_organizadores = str_trim(nombres_organizadores)) %>% 
  # con el str trim se resuelven algunos problemas pero OJO que hay registros duplicados
  left_join(base_organizadores_distinct %>% 
              distinct(nombres_organizadores, ncat_eleccion, cat_pais,
                       ncat_subtipov2, cat_tipoorgv2,
                       ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
                       cat_areaorg))

base_organizadores %>%  writexl::write_xlsx("./datav3/base_organizadoresv3.xlsx")

# base de formato por fila ##############
cat_tipo_formato <-  c("pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                   "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                   "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre")
colors <- c("#ff3452", "#FF6846", "#FF9044","#FFD667", "#F6E46E", "#ECF179", "#c8fd81","#ADFD7F", "#8CFC7C","#5FFB7F")
colores_formatos <- tibble(cat_tipo_formato, colors)

base_formatos_long <- base %>% 
  pivot_longer(cols = starts_with("pr_formato"),
               names_to = "cat_tipo_formato",
               values_to = "n_peso_formato_xdebate") %>% 
  filter(n_peso_formato_xdebate>0)%>% 
  select(id_debate, cat_pais, ncat_eleccion,
         n_peso_formato_xdebate, cat_tipo_formato)  %>% 
  left_join(colores_formatos) %>% 
  rename(colores_formato= colors)


base_formatos_long %>%  writexl::write_xlsx("./datav3/base_formatos_longv3.xlsx")


# base de tema por fila ###############

base_temas_long <- base %>% 
  pivot_longer(cols = starts_with("pr_tema"),
               names_to = "cat_tipo_tema",
               values_to = "n_peso_tema_xdebate")%>% 
  filter(n_peso_tema_xdebate>0) %>% 
  select(id_debate, cat_pais, ncat_eleccion,
         n_peso_tema_xdebate, cat_tipo_tema)



base_temas_long %>%  writexl::write_xlsx("./datav3/base_temas_longv3.xlsx")


# base de eleccion por fila ###################

elecciones <-  readxl::read_xlsx("./datav3/base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país
base_normativa <- readxl::read_xlsx("./exploracion_base_finalv1/base_normativa.xlsx")

base_ordinales <- base_normativa %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regmedios.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regestado.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regcandidatos.xlsx")) %>% 
  mutate(ncat_totreg = ncat_regmedios + ncat_regestado + ncat_regcandidatos ) %>% 
  select(c(ncat_eleccion, cat_pais, starts_with("ncat_")))

elecciones_con_debates <- base %>% 
  distinct(cat_pais,ncat_eleccion) %>% 
  mutate(dico_hubo_debates = 1)

primeros_debates <- elecciones_con_debates %>% 
  group_by(cat_pais) %>% 
  mutate(cat_primer_debate = min(ncat_eleccion)) %>% 
  ungroup() %>% 
  distinct(cat_pais, cat_primer_debate)

elecciones_sin_debates_absoluto <- elecciones %>% 
  distinct(cat_pais,ncat_eleccion) %>% 
  left_join(elecciones_con_debates) %>% 
  mutate(dico_hubo_debates = replace_na(dico_hubo_debates,0)) %>% 
  left_join(primeros_debates)

elecciones_sin_debates_filtrado <- elecciones_sin_debates_absoluto %>% 
  filter(!(ncat_eleccion<cat_primer_debate))

base_anual <- elecciones_sin_debates_filtrado %>% 
  left_join(base_ordinales)

base_anual %>%  writexl::write_xlsx("./datav3/base_anualv3.xlsx")