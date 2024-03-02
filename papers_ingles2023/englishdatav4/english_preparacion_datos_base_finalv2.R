# LIBRERIAS #####
library(tidyverse)


# IMPORTO DATOS ###########
base <- readxl::read_xlsx("./englishdatav4/base_finalv2.xlsx") # base con datos por debate

# primera y principal trad: paises

trad_pais <- tibble(
  cat_pais = c("Brasil", "Chile", "Argentina", "Peru", "Paraguay", "Ecuador", "Uruguay", "Nicaragua", "Panama", "Venezuela", "Bolivia", "Mexico", "Guatemala", "Honduras", "Colombia", "Costa Rica", "Republica Dominicana", "El Salvador"),
  english_cat_pais = c("Brazil", "Chile", "Argentina", "Peru", "Paraguay", "Ecuador", "Uruguay", "Nicaragua", "Panama", "Venezuela", "Bolivia", "Mexico", "Guatemala", "Honduras", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador")
)

base <- base %>% 
  left_join(trad_pais)

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
  mutate(n_strorganizador = stringr::str_count(str_organizador, ";")+1,
         n_presentes = stringr::str_count(str_presentes, ";")+1,
         n_ausentes = ifelse(str_ausentes=="NA","NA", ifelse(
           stringr::str_detect(str_ausentes, "sin ausencias"), 
           0, stringr::str_count(str_ausentes, ";")+1))) 

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
          dico_formato_expositivo = stringr::str_detect(cat_formato, "expositivo"), # ups me había faltado contabilizar
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


# STOP PRIMERO CORRER BASE AUXILIAR ORGANIZADORES agrego data de organizadores. OJO computar despues de lo de abajo ######

# paso data de base organizadores a wide

base_organizadores_wide_translated <- base_organizadores_translated %>% 
  select(-c(nombres_organizadores, english_cat_areaorg, english_ncat_subtipov2,
            cat_areaorg, ncat_subtipov2, cat_tipoorgv2,
            ncat_tipoorg_ambito, ncat_tipoorg_visibilidad, t_fecha)) %>% 
     mutate(values = 1) %>% 
  tidyr::pivot_wider(names_from = "english_cat_tipoorgv2", 
                 names_prefix = "n_cattipo_",
                 names_sep = ";",
                 names_sort = TRUE,
                 names_repair = "check_unique",
                 values_fill = 0,
                 values_from = values,
                 values_fn = sum)

base_organizadores_grouped_translated <- base_organizadores_translated %>% 
  group_by(id_debate) %>% 
  summarise(n_orgsxdebate = n(),
            n_variedadorgsxdebate = n_distinct(english_cat_tipoorgv2),
            n_variedadsubtiposxdebate = n_distinct(english_ncat_subtipov2),
            ncat_mean_tipoorg_ambito = mean(ncat_tipoorg_ambito),
            ncat_mean_tipoorg_visibilidad = mean(ncat_tipoorg_visibilidad))

base <- base %>% 
  left_join(base_organizadores_wide_translated) %>% 
  left_join(base_organizadores_grouped_translated)

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
  tidyr::unnest(nombres_ausentes)

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

#### TRADUCCION 

variables <- base %>% select(where(is.character)) %>%  select(contains("cat")) %>% colnames()

for (name in variables){
   nam <- paste("english_", name, sep="")
   assign(nam, base[,name] %>% unique())
 } 

# TRADUCCION MANUAL

english_cat_panel %>% as.character()
english_cat_panel$english_cat_panel <- c(0, "Women", "Businessmen", "NA", NA, "Church", "Academy", "Professionals", "Students", "Trade unions", "Migrants", "Youth", "Retired", "Various", "Prisoners")

english_cat_regcandidatos %>% as.character()
english_cat_regcandidatos$english_cat_regcandidatos <- c("POSSIBILITY", "GUARANTEES", "NOTHING", "OBLIGATION")

english_cat_regestado %>% as.character()
english_cat_regestado$english_cat_regestado <- c("POSSIBILITY", "AUDIT", "NOTHING", "ORGANIZE", "GUARANTEE")

english_cat_regmedios %>% as.character()
english_cat_regmedios$english_cat_regmedios <- c("POSSIBILITY", "LIMITATIONS", "NOTHING", "OPPORTUNITY", "OBLIGATIONS")

base_translated <- base %>% 
  left_join(english_cat_pais) %>% 
  left_join(english_cat_panel) %>% 
  left_join(english_cat_regcandidatos) %>%
  left_join(english_cat_regestado) %>%
  left_join(english_cat_regmedios) 

to_remove <- ls() %>% as_tibble() %>% subset(stringr::str_detect(value, "english_"))
rm(list=to_remove$value)

base_translated %>%  writexl::write_xlsx("./englishdatav4/base_finalv3.xlsx")

# CREO BASES AUXILIARES #############

# PRIMERO ESTA base de organizador por fila #################

base_organizadores <- base %>% # BASE CON UN ORG POR FILA 
  select(id_debate, ncat_eleccion, english_cat_pais, t_fecha, ncat_ronda, 
         str_organizador, n_strorganizador) %>%
  mutate(nombres_organizadores = strsplit(str_organizador, ";")) %>% 
  tidyr::unnest(nombres_organizadores) 

# UNO DATOS CARGADOS

base_organizadores_distinct <-  readxl::read_xlsx("./exploracion_base_finalv1/base_organizadores_distinct.xlsx") %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_subtipoorg.xlsx")) %>% 
  left_join(trad_pais)

base_organizadores <- base_organizadores %>%  
  mutate(nombres_organizadores = stringr::str_trim(nombres_organizadores)) %>% 
  # con el str trim se resuelven algunos problemas pero OJO que hay registros duplicados
  left_join(base_organizadores_distinct %>% 
              distinct(nombres_organizadores, ncat_eleccion, english_cat_pais,
                       ncat_subtipov2, cat_tipoorgv2,
                       ncat_tipoorg_ambito, ncat_tipoorg_visibilidad,
                       cat_areaorg))

#### TRADUCCION 

variables <- base_organizadores %>% select(where(is.character)) %>%  select(contains("cat")) %>% colnames()

for (name in variables){
  nam <- paste("english_", name, sep="")
  assign(nam, base_organizadores[,name] %>% unique())
} 

# TRADUCCION MANUAL
# english_cat_pais %>% as.character()
# english_cat_pais$english_cat_pais <- c("Brazil", "Chile", "Argentina", "Peru", "Paraguay", "Ecuador", "Uruguay", "Nicaragua", "Panama", "Venezuela", "Bolivia", "Mexico", "Guatemala", "Honduras", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador")

english_ncat_subtipov2 %>% as.character()
english_ncat_subtipov2$english_ncat_subtipov2 <- c("open_private_tv", "autonomous_state", "business_association", "press_association", "private_graphics", "private_telecommunications", "open_church_tv", "subnational_state", "church_education", "open_public_tv", "private_education", "church_association", "church_association_education", "private_radio", "media_association", "public_education", "international_tv", "ngo_gender", "educ_privada", "tv_payed_private", "coordinates", "digital_privado", "ong_democracia", "electoral_state", "education", "thinkthank", "association_professional", "NA", NA, "public_radio", "public_subnational_tv", "private_cultural_space", "ngo_ddhh", "ngo_international", "student_movement", "migrant NGO", "open_university_tv", "private_production_tv", "private_program_tv", "state_media", "company", "public_cultural_space", "university_graphics", "trade_union_association", "public_education_association")
  
english_cat_tipoorgv2 %>% as.character()
english_cat_tipoorgv2$english_cat_tipoorgv2 <- c("cmm", "state", "cso", "educ", "pmm", NA)
  
english_cat_areaorg %>% as.character()
english_cat_areaorg$english_cat_areaorg <- c("tv", "state", "company", "media associations", "graphic", "other_media", "church", "public_media", "education", "radio", "international", "cso_hhrr", "other_cso", "cso_development", "professionals", NA)
  
base_organizadores_translated <- base_organizadores %>% 
  #left_join(english_cat_pais) %>% 
  left_join(english_ncat_subtipov2) %>% 
  left_join(english_cat_tipoorgv2) %>% 
  left_join(english_cat_areaorg) 

to_remove <- ls() %>% as_tibble() %>% subset(stringr::str_detect(value, "english_"))
rm(list=to_remove$value)

base_organizadores_translated %>%  writexl::write_xlsx("./englishdatav4/base_organizadoresv3.xlsx")

# traduccion de base de subtipos de organizadores  #####

nombres_subtipos <- read.csv("./datav3/nombres_subtipos.csv")

trad_subtipos <- base_organizadores_translated %>% 
  group_by(english_ncat_subtipov2, ncat_subtipov2) %>% 
  summarise(no_importa=n()) %>% 
  select(-no_importa)

nombres_subtipos <- nombres_subtipos %>% 
  left_join(trad_subtipos)

nombres_subtipos$str_subtipo %>% paste0(collapse=", ")
nombres_subtipos$english_str_subtipo <- c("Business association", "Religious association", "NGO for gender equality", "Think Thank", "Coordinator of several SCOs", "NGO for democracy", "International NGO", "Professional association", "Student movement", "NGO for immigrant rights", "NGO for human rights", "Trade union", "Company", "National public open TV channel", "Subnational public broadcast TV channel", "Public cultural space", "Public radio", "Private broadcast TV channel", "Private graphic media", "Media owners' association", "Private broadcast radio", "Journalists' association", "Private digital media", "International channel", "Church broadcast TV channel", "Private payed TV channel", "Telecommunications sector company", "Commercial TV production company", "Open university TV channel", "Private TV program", "Private cultural space", "University graphic media", "National electoral body", "Subnational state administration", "State agency of the communications sector", "Decentralized entity", "Public higher education institution", "Private higher education institution", "Association of educational institutions (variety of institutions)", "Church higher education institution", "Association of private educational institutions", "Association of religious educational institutions", "Association of public educational institutions", "No data")

nombres_subtipos %>% write.csv("./englishdatav4/nombres_subtipos.csv")

# LUEGO DE GUARDOv3 ESTAS QUE SIGUEN base de formato por fila ##############

cat_tipo_formato <-  c("pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                   "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                   "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre")
colors <- c("#ff3452", "#FF6846", "#FF9044","#FFD667", "#F6E46E", "#ECF179", "#c8fd81","#ADFD7F", "#8CFC7C","#5FFB7F")

# aca agrego la tradu aca arriba

english_cat_tipo_formato <- c("pr_formatpresent", "pr_formatvirtual", "pr_formatsectors", "pr_formatexperts", "pr_formatmoderators", "pr_formatjournalists", "pr_formatopening", "pr_formatmonologic", "pr_formatduel", "pr_formatfree")
  
colores_formatos <- tibble(cat_tipo_formato, colors, english_cat_tipo_formato)

base_formatos_long <- base_translated %>% 
  pivot_longer(cols = starts_with("pr_formato"),
               names_to = "cat_tipo_formato",
               values_to = "n_peso_formato_xdebate") %>% 
  filter(n_peso_formato_xdebate>0)%>% 
  select(id_debate, cat_pais, ncat_eleccion,
         n_peso_formato_xdebate, cat_tipo_formato)  %>% 
  left_join(colores_formatos) %>% 
  rename(colores_formato= colors)


base_formatos_long %>%  writexl::write_xlsx("./englishdatav4/base_formatos_longv3.xlsx")


# base de tema por fila ###############

base_temas_long <- base_translated %>% 
  pivot_longer(cols = starts_with("pr_tema"),
               names_to = "cat_tipo_tema",
               values_to = "n_peso_tema_xdebate")%>% 
  filter(n_peso_tema_xdebate>0) %>% 
  select(id_debate, cat_pais, ncat_eleccion,
         n_peso_tema_xdebate, cat_tipo_tema)

# traduccion

variables <- colnames(base_temas_long)

for (name in variables){
  nam <- paste("english_", name, sep="")
  assign(nam, base_temas_long[,name] %>% unique())
  
} 

english_cat_tipo_tema %>% as.character()
english_cat_tipo_tema$english_cat_tipo_tema <- c("pr_thematicquestions", "pr_freetheme", "pr_monothematic", "pr_thematictopics")

translated_base_temas_long <- base_temas_long %>% 
  left_join(english_cat_tipo_tema)

to_remove <- ls() %>% as_tibble() %>% subset(stringr::str_detect(value, "english_"))
rm(list=to_remove$value)

translated_base_temas_long %>%  writexl::write_xlsx("./englishdatav4/base_temas_longv3.xlsx")


# base de eleccion por fila ###################

elecciones <-  readxl::read_xlsx("./englishdatav4/base_elecciones.xlsx") # base auxiliar: años que hubo elecciones por país
base_normativa <- readxl::read_xlsx("./exploracion_base_finalv1/base_normativa.xlsx")

# agrego trad de cat pais
trad_pais <- base_translated %>% 
  distinct(cat_pais, english_cat_pais)

elecciones <- elecciones %>% 
  left_join(trad_pais)

base_normativa <- base_normativa %>% 
  left_join(trad_pais)
  
base_ordinales <- base_normativa %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regmedios.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regestado.xlsx")) %>% 
  left_join(readxl::read_xlsx("./exploracion_base_finalv1/distinct_cat_regcandidatos.xlsx")) %>% 
  mutate(ncat_totreg = ncat_regmedios + ncat_regestado + ncat_regcandidatos ) %>% 
  select(c(ncat_eleccion, cat_pais, starts_with("ncat_")))

elecciones_con_debates <- base_translated %>% 
  distinct(cat_pais,english_cat_pais,ncat_eleccion) %>% 
  mutate(dico_hubo_debates = 1)

primeros_debates <- elecciones_con_debates %>% 
  group_by(english_cat_pais) %>% 
  mutate(cat_primer_debate = min(ncat_eleccion)) %>% 
  ungroup() %>% 
  distinct(english_cat_pais, cat_primer_debate)

elecciones_sin_debates_absoluto <- elecciones %>% 
  distinct(english_cat_pais, cat_pais, ncat_eleccion) %>% 
  left_join(elecciones_con_debates) %>% 
  mutate(dico_hubo_debates = replace_na(dico_hubo_debates,0)) %>% 
  left_join(primeros_debates)

elecciones_sin_debates_filtrado <- elecciones_sin_debates_absoluto %>% 
  filter(!(ncat_eleccion<cat_primer_debate))

base_anual <- elecciones_sin_debates_filtrado %>% 
  left_join(base_ordinales)


base_anual %>%  writexl::write_xlsx("./englishdatav4/base_anualv3.xlsx")



