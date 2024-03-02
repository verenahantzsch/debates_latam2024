# librerias
library(tidyverse)
#library(hrbrthemes) #
library(RColorBrewer)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
#library(xlsx) #

setwd("/home/carolina/Documents/Proyectos R/debates_latam/papers_ingles")

# datos
base <- readxl::read_xlsx("./englishdatav4/base_finalv3.xlsx")
elecciones <-  readxl::read_xlsx("./englishdatav4/base_elecciones.xlsx") # base auxiliar: Years que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./englishdatav4/base_organizadoresv3.xlsx")
base_formatos <- readxl::read_xlsx("./englishdatav4/base_formatos_longv3.xlsx")
base_temas <- readxl::read_xlsx("./englishdatav4/base_temas_longv3.xlsx")
base_normativa <- readxl::read_xlsx("./englishdatav4/base_normativa.xlsx")
base_anual <- readxl::read_xlsx("./englishdatav4/base_anualv3.xlsx")

nombres_subtipos <- read.csv("./englishdatav4/nombres_subtipos.csv") 

plotnumber <- 0

# traducciones pendientes

trad0 <- base %>% 
  group_by(english_cat_pais, cat_pais) %>% 
  summarise(no_importa=n()) %>% 
  select(-no_importa)

elecciones <- elecciones %>% 
  left_join(trad0) 

base_formatos <- base_formatos %>% 
  left_join(trad0) 

base_temas <- base_temas %>% 
  left_join(trad0) 

# dfs de colores


colorespais <- base %>% 
  distinct(english_cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(english_cat_pais, cols_18) %>% 
  mutate(english_cat_pais = stringr::str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) 


###############################################################################
######################################################################
# CAPITULO 1 GR DE PENETRACION ###############

# 1.1 GRAFICO DE EVOLUCION TEMPORAL #####


debates_Year_pais <- base %>% 
  group_by(ncat_eleccion, english_cat_pais) %>% 
  summarise(n_debates_Year_pais = n()) 

colores <- base %>%
  distinct(english_cat_pais, cols_18) 

base_Years <- elecciones %>% 
  left_join(debates_Year_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_Year_pais),
          n_debates_Year_pais = replace_na(n_debates_Year_pais, 0) ) %>% 
  left_join(colores)

plot_point_anual <- base_Years %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot()  +
  geom_point(aes(ncat_eleccion,as.factor(english_cat_pais) %>% 
                   fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                               "Guatemala",  "Paraguay", "Dom. Rep.",  "Honduras", "El Salvador",  "Nicaragua", 
                               "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"), 
                 size= n_debates_Year_pais, colour = cols_18, shape = debates_dico, alpha= debates_dico)) + 
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  #theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) +  #,
       # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  labs(x = "", y = "",
       title = "Debates over time",
       subtitle = "How many and when they were made",
       caption = "Source: Author, with data collected for this research. 
      
       The size of the circles represents the number of debates held in an election.
       The x's represent elections without debates.")

# segunda prueba de plot anual

plot_point_anual2 <- base_Years %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais, "Dominican Republic", "Dom. Rep.ᵃ")) %>% 
  ggplot(aes(ncat_eleccion, 
             log(n_debates_Year_pais),
             colour = as.factor(english_cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Dom. Rep.ᵃ",  "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina")))  +
  geom_line() + 
  geom_point(aes(#size= n_debates_Year_pais, 
                 shape = debates_dico, alpha= debates_dico)) +
  scale_color_manual(breaks= colorespais2$english_cat_pais,
                     values=colorespais2$cols_18) +
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  facet_wrap( ~ as.factor(english_cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Dom. Rep.ᵃ", "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"), 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0),
        strip.background = element_rect(fill = "grey1"),
        strip.text = element_text(colour = "white", size = 12)) +  #,
  # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2021,10)) +
  #scale_y_continuous(breaks = seq(0,24,3)) +
  labs(x = "Year", y = "Number of debates, in log",
       title = "Debates over time",
       subtitle = "How many were made and when, by country",
       caption = "Source: Author, with data collected for this research.
       
       The x's represent elections without debates.
       For better visualization, the number of debates (vertical axis) is represented in its logarithmic version.
       The actual maximum is 24 debates per year (Costa Rica), the minimum is 0..
       
       ᵃDom. Rep. = Dominican Republic.")

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 1.2 GRAFICO DE CANTIDAD DE DEBATES / INDEX AUSENTES #####

# quiero revisar la cantidad de debates con alguna ausencia
# en relacion a la cantidad de debates por eleccion

base_ausencias_primera <- base %>% 
  filter(ncat_ronda== 1) %>% 
  group_by(ncat_eleccion, english_cat_pais, cols_18) %>% 
  summarise(cantidad_debates_ronda = n(),
         cantidad_debates_ausencias = sum(dico_ausencias, na.rm = TRUE),
         proporcion_debates_ausencias = cantidad_debates_ausencias/cantidad_debates_ronda,
         suma_indexausentes = sum(n_indexausentes, na.rm = TRUE),
         mean_indexausentes = mean(n_indexausentes, na.rm = TRUE),
         sd_indexausentes = sd(n_indexausentes, na.rm = TRUE))

# ANEXO mean versus sd ausencias

plotausencias_sd_versus_mean <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = english_cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Standard deviation in absences rate", y = "Average absences rate",
       title = "Absences rate ",
       subtitle = "average versus standard deviation",
       caption = "Source: Author, with data collected for this research.
       Calculous only takes into account debates held for the first round.")


# ANEXO proporcion de debates con ausencias versus proprocion de debates realizados , para el anexo

plotausencias_debates_versus_proporcion <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = english_cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Proportion of debates with absenteeism", y = "N first round debates",
       title = "Debates with absenteeism ",
       subtitle = "in relation to the total number of debates held for
       for the first round",
       caption = "Source: Author, with data collected for this research.")


# cantidad de debates versus promedio indice ausencias ESTE SI

plotausencias_anuales_meanindex <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = english_cat_pais, vjust = 0), alpha = 0.6) +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = ncat_eleccion, vjust = 1), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Absences rate", y = "n first round debates",
       title = "Average absences rate",
       subtitle = "By election, in relation to the debates held 
       for the first round",
       caption = "Source: Author, with data collected for this research..
       
       The absentee rate multiplies the proportion of candidates who missed a debate, 
       by the percentage of votes they obtained in the elections.
       
       The graph shows averages per year-country, for the debates held before the first round of the elections
       (before the ballotage, by definition, there can be no debates with absentees).")

# 1.3 SUMMARY DEL INDEX AUSENTES ########

summary(base$n_indexausentes)

cuenta <- base %>%  
         select(str_ausentes, n_ausentes,
                n_indexausentes, 
                n_proporcionausentes,
                n_porcentajeausentes 
                ) %>% 
         filter(is.na(n_indexausentes))

########################################################################
######################################################################
# CAPITULO 2 TIPOS DE ORG #################

# 2.1 CUENTAS DE ORGS #############

cuenta_tipos <- base_organizadores %>% 
  group_by(english_cat_tipoorgv2) %>% 
  summarise(n_individuos = n(),
            n_debates_en_participaron = n_distinct(id_debate)) %>% 
  ungroup() %>% 
  mutate(pr_n_individuos = n_individuos/sum(n_individuos)*100,
         pr_debates_en_participaron = n_debates_en_participaron/sum(n_debates_en_participaron)*100) %>% 
  arrange(desc(pr_debates_en_participaron))

n_distinct(base_organizadores$id_debate)

# 2.0 cuentas de subtipos, para anexo ##########

# general

cuenta_subtipos_gral <- base_organizadores %>% 
  group_by(english_cat_tipoorgv2, english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  arrange(desc(english_cat_tipoorgv2), desc(n)) %>%  
  ungroup() %>% 
  group_by(english_cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n/n_cat*100) 

#cuenta_subtipos_gral %>% write_excel_csv(file = "anexos/anexo1.xlsx")

# hacemos grafico

cuenta_subtipos_gral_2 <- base_organizadores  %>%
  group_by(english_cat_tipoorgv2, english_ncat_subtipov2) %>% 
  summarise(n_subtipo = n()) %>% 
  arrange(desc(english_cat_tipoorgv2), desc(n_subtipo)) %>%  
  ungroup() %>% 
  mutate(english_ncat_subtipov2_2 = ifelse(n_subtipo > 3, english_ncat_subtipov2, "Other")) %>%
  group_by(english_cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n_subtipo)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n_subtipo/n_cat*100) 

plot_cuenta_subtipos_gral_2 <- cuenta_subtipos_gral_2  %>% 
  left_join(nombres_subtipos %>% select(c(english_str_subtipo, english_ncat_subtipov2)) %>%  rename(english_ncat_subtipov2_2 = "english_ncat_subtipov2")) %>% 
  mutate(english_str_subtipo = ifelse(is.na(english_str_subtipo), "Other", english_str_subtipo)) %>%  
  na.omit() %>% 
  arrange(n_cat, n_subtipo) %>% 
  mutate(order = row_number()) %>% 
  mutate(english_str_subtipo = fct_reorder(english_str_subtipo, order))  %>%
  ggplot() +
  geom_col(aes(english_str_subtipo, n_subtipo, fill = english_cat_tipoorgv2)) +
  coord_flip() +
  theme_minimal() +
  scale_fill_discrete(breaks=c("educ", "state", "cmm","pmm", "cso", NA), 
                      labels=c("Academic sector", "State", "Commercial media", "Public media", "CSOs", "No Data")) +
  labs(y = "n subtypeᵃ", x = "Organizer subtype", fill = "Type of organizer",
       caption = paste("Source: Author, with data collected for this research.
       
       Each unit represents one organizer-debate.
       
       If an organizer participated in two debates, it is counted twice.
       
       There are 16 debates for which we do not know the nature of the organizer.
       
       The count includes the debates of all the studied countries: 
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString()
                       ),
       title = "Organizer types and subtypes",
       subtitle = "Absolute quantity, for the entire database")

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# por tipo
cuenta_subtipos_mmc <- base_organizadores %>% 
  filter(english_cat_tipoorgv2=="cmm") %>% 
  group_by(english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_osc <- base_organizadores %>% 
  filter(english_cat_tipoorgv2=="cso") %>% 
  group_by(english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_mmp <- base_organizadores %>% 
  filter(english_cat_tipoorgv2=="pmm") %>% 
  group_by(english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_educ <- base_organizadores %>% 
  filter(english_cat_tipoorgv2=="educ") %>% 
  group_by(english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_estado <- base_organizadores %>% 
  filter(english_cat_tipoorgv2=="estado") %>% 
  group_by(english_ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

sum(cuenta_tipos$n_individuos)
sum(cuenta_tipos$n_debates_en_participaron)


# 2.2 ANEXO ALIANZAS EN T #########

alianzas_cuenta <- base_organizadores %>%  
  group_by(id_debate, ncat_eleccion, english_cat_pais) %>% 
  summarise(n_orgs = n(),
            n_variedadorgs = n_distinct(english_cat_tipoorgv2),
            n_variedadsubtipos = n_distinct(english_ncat_subtipov2))

plot_alianzas_cuenta <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_variedadorgs)) +
  geom_smooth(alpha = 0.1, colour= "khaki") +
  geom_point(alpha = 0.1) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,15,1)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Election Year", y = "n types of orgs.ᵇ")


plot_alianzas_cuenta1 <-ggExtra::ggMarginal(plot_alianzas_cuenta,
                                            type="histogram",
                                            margins = "y",               # specify histograms
                                            fill = "lightblue",    # other parameters for x-axis marginal
                                            yparams = list(binwidth = 1))     # other parameters for y-axis marginal) 

plot_alianzas_cuenta2 <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_orgs) ) +
  geom_smooth(alpha = 0.1, colour= "khaki") +
  geom_point(alpha = 0.1 #aes(alpha = as.numeric(ncat_eleccion))
    ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,15,3)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "", y = "n organizersᵃ")


plot_alianzas_cuenta21 <-ggExtra::ggMarginal(plot_alianzas_cuenta2,
                                             type="histogram",               # specify histograms
                                             margins = "y",
                                             fill = "lightblue",               # bar fill
  xparams = list(binwidth = 4),    # other parameters for x-axis marginal
  yparams = list(binwidth = 1))     # other parameters for y-axis marginal) 

#patchwork_alianzas <-  patchwork::wrap_plots(plot_alianzas_cuenta21, plot_alianzas_cuenta1) + patchwork::plot_layout(ncol=1)
patchwork_alianzas <-  patchwork::wrap_plots(plot_alianzas_cuenta2, plot_alianzas_cuenta) + 
  patchwork::plot_layout(ncol=1) + 
  patchwork::plot_annotation(
  title = 'Number and variety of organizers',
  subtitle = 'by debate, over time',
  caption = paste("Source: Author, with data collected for this research.
  
       The count does not differentiate between countries. All studied cases were considered:  
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString(), "
       
       Each circle represents a debate. 
       The circles were plotted with some transparency to visualize the overlaps between them (which occur when several debates with the same number or diversity of organizers were held in an election). 
       (which occur when several debates with the same number or diversity of organizers were held in an election).
       
       The yellow line plots the formula y ~ x.
       
       ᵃn organizers = absolute number of entities that jointly organize an encounter.
       ᵇn types of orgs. = number of different types of organizers involved in organizing an encounter."
))  + 
  patchwork::plot_annotation(tag_levels = c('1'), tag_prefix = 'Fig. ', tag_sep = '.', 
                  tag_suffix = ':') & 
  theme(#plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 9, hjust = -0.5, vjust = -0.5))

# otra prueba, cumulativa

alianzas_cuenta_anual_cantidad <- alianzas_cuenta %>% 
  mutate(n_orgs = as.factor(n_orgs)) %>% 
  group_by(ncat_eleccion, n_orgs) %>% 
  summarise(cantidad = n()) 

plot_alianzas_cuenta_anual_cantidad <- alianzas_cuenta_anual_cantidad %>% 
  ggplot() +
  geom_col(aes(ncat_eleccion, cantidad, alpha=n_orgs))  +
  theme_minimal() +
#  scale_y_continuous(breaks = seq(0,30,3)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "", y = "",
       title = "",
       subtitle = "",
       caption = "")


alianzas_cuenta_anual_variedad <- alianzas_cuenta %>% 
  mutate(n_orgs = as.factor(n_variedadorgs)) %>% 
  group_by(ncat_eleccion, n_variedadorgs) %>% 
  summarise(cantidad = n()) 

plot_alianzas_cuenta_anual_variedad <- alianzas_cuenta_anual_variedad %>% 
  ggplot() +
  geom_col(aes(ncat_eleccion, cantidad, alpha=n_variedadorgs))  +
  theme_minimal() +
 # scale_y_continuous(breaks = seq(0,30,1)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Election Year", y = "Number of debates",
       caption = "",
       title = "")


patchwork_alianzas_cum <-  patchwork::wrap_plots(plot_alianzas_cuenta_anual_cantidad, plot_alianzas_cuenta_anual_variedad ) + 
  patchwork::plot_layout(ncol=1) + 
  patchwork::plot_annotation(
    title = 'Number and variety of organizers',
    subtitle = 'by debate, over time',
    caption = 'Source: Author, with data collected for this research.
  
       ᵃn organizers = absolute number of entities that jointly organize an encounter.
       ᵇn types of orgs. = number of different types of organizers involved in organizing an encounter.'
  )


# otra prueba, no use

alianzas_cuenta_promedio <- alianzas_cuenta %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_orgs = mean(n_orgs),
            n_variedadorgs = mean(n_variedadorgs),
            n_variedadsubtipos = mean(n_variedadsubtipos))

plot_alianzas_cuenta_promedio  <- alianzas_cuenta_promedio  %>% 
  ggplot(aes(ncat_eleccion, n_variedadorgs, size= n_orgs)) +
  geom_point() +
  theme_minimal() +
  #scale_y_continuous(breaks = seq(0,15,1)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "n types of organizers",
       title = "Number and variety of organizers",
       subtitle = "yearly averages",
       caption = "Source: Author, with data collected for this research.
       The size of the circles represents the average number of organizers.
       The y-axis, the variety, i.e., the types of average organizers.
       The values do not discriminate between countries.
       Note that the number of total debates increases over time.")

# 2.3 STREAMING #######

streaming <- base %>% 
  filter(dico_streaming == TRUE) %>% 
  mutate(dico_cattipo_mmc = ifelse(n_cattipo_cmm==0,0,1),
         dico_cattipo_osc = ifelse(n_cattipo_cso==0,0,1),
         dico_cattipo_estado = ifelse(n_cattipo_state==0,0,1),
         dico_cattipo_educ = ifelse(n_cattipo_educ==0,0,1),
         dico_attipo_mmp = ifelse(n_cattipo_pmm==0,0,1),
         dico_cattipo_NA = ifelse(n_cattipo_NA==0,0,1)) %>% 
  select(starts_with("dico_cattipo"))

streamingtotals <- colSums(streaming) 
15/25*100   # osc 43.58095
8/25*100  # educ 16.53333 . 60.11428
16 /25*100  # mmc 39.08572
0.200000/25*100
25/317*100

oscsstreaming <- base %>% 
  filter(n_cattipo_cso>0) %>% 
  select(dico_streaming, n_cattipo_cso) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(oscsstreaming$dico_streaming)/length(oscsstreaming$dico_streaming)*100


# 2.4 TIPOS DE ORG A LO LARGO DEL T ########

# preparamos datos. Calcularemos la "proporción" de organizadores que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de organizador sobre el total de diferentes tipos de organiadores por debates
# NO calcula esta proporción con base en el carácter de cada una de las organizaciones que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate
# esta base agrupada me sirve para comparar con la version vieja, eventualmente
base_tipos_grouped <- base_organizadores %>% 
  group_by(str_organizador, english_cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  summarise(n_catorganizadorv2 = n_distinct(english_cat_tipoorgv2),
            n_organizadores = n())

#base no agrupada para hacer cuentas y graficos
base_tipos_ungrouped <- base_organizadores %>% 
  group_by(str_organizador, english_cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  mutate(n_catorganizadorv2 = n_distinct(english_cat_tipoorgv2),
         n_organizadores = n()) %>% 
  ungroup %>% 
  mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
         n_prppacorg = 1/n_organizadores) # porcentaje de participacion del organizador sobre la cantidad de organizadores del debate

# base agrupada por Year y pais
base_tipos_countryear <- base_tipos_ungrouped %>% 
  group_by(ncat_eleccion, english_cat_pais, english_cat_tipoorgv2) %>% 
  summarise(n_prppaccatorg = sum(n_prppaccatorg, na.rm = T),
            n_prppacorg = sum(n_prppacorg, na.rm = T) )

# me gusta este para comparar dos etapas
tipos_organizadores_ev_anualv2 <- base_tipos_countryear %>% 
  mutate(english_cat_tipoorgv2 = ifelse(is.na(english_cat_tipoorgv2), "No Data", english_cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, 
             as.factor(english_cat_tipoorgv2) %>% 
               fct_relevel("state", "pmm","cso", "educ", "cmm", "No Data"), 
             colour = english_cat_tipoorgv2, 
             size= n_prppacorg,
             shape= as.factor(english_cat_tipoorgv2) %>% 
               fct_relevel("state", "pmm","cso", "educ", "cmm", "No Data")))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = -40), size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)) + 
  scale_shape_manual(values=c("No Data" = 4, 
                              "state" = 19,
                              "pmm" = 19,
                              "educ" = 19,
                              "cmm" = 19,
                              "cso" = 19)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  scale_colour_manual(breaks = c("No Data", "state", "pmm","cso", "educ", "cmm"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "State", "Public 
                               Media", "CSOs", "Academic", "Commercial 
                               Media", "No Data")) +
  labs(x = "Year",
       y = "Type of organizer",
       title = "Type of debates-organizers ",
       subtitle = "Over time, for the entire region",
       caption = paste("Source: Author, with data collected for this research. 
       
       The size of the circles is proportional to the number of debates held in a given year.
       If a debate was organized by more than one type of organizer, it is counted twice.
                       
                       The count includes all the cases under study: 
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString()
                       ))  

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 2.5 para comparar TIPOS DE ORG POR PAIS, FACET ######
#
tipos_organizadores_ev_anual_paisv2 <- base_tipos_countryear %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.ᵃ")) %>% 
  mutate(english_cat_tipoorgv2 = ifelse(is.na(english_cat_tipoorgv2), "No Data", english_cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, as.factor(english_cat_tipoorgv2) %>% 
               fct_relevel("state", "pmm","cso", "educ", "cmm" ), 
             colour = english_cat_tipoorgv2, 
             size= n_prppaccatorg,
             shape= as.factor(english_cat_tipoorgv2) %>% 
               fct_relevel("state", "pmm","cso", "educ", "cmm", "No Data")))  +
  geom_point() +
  facet_wrap( ~ as.factor(english_cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Dom. Rep.ᵃ",  "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0),
        strip.text=element_text(size=12)) + 
  scale_shape_manual(values=c("No Data" = 4, 
                              "state" = 19,
                              "pmm" = 19,
                              "educ" = 19,
                              "cmm" = 19,
                              "cso" = 19)) +
  scale_x_continuous(breaks = seq(1955,2021,10)) +
  scale_colour_manual(breaks = c("No Data", "state", "pmm","cso", "educ", "cmm"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "State","Public M.ᵇ","SCOs","Academic","Commercial M.ᵇ", "No Data")) +
  labs(x = "Year",
       y = "Type of organizer",
       title = "Type of debates-organizers ",
       subtitle = "Over time, by country",
       caption = "Source: Author, with data collected for this research.
       
               The size of the circles is proportional to the number of debates held in a given year.
       If a debate was organized by more than one type of organizer, it is counted twice.
       
       ᵃDom. Rep. = Dominican Republic.
       ᵇM. = Media.")


plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# 2.6 cuentas de osc , subtipos, por paises. no usado. mandar a anexo ##############

base_osc <- base_organizadores %>% 
  group_by(english_cat_pais) %>% 
  mutate(n_debates_pais = n_distinct(id_debate),
         n_organizadores_pais = n() ) %>% 
  ungroup() %>% 
  filter(english_cat_tipoorgv2=="cso") %>% 
  group_by(english_cat_pais) %>% 
  mutate(n_osc_pais = n(),
         n_debates_osc_pais = n_distinct(id_debate),
         pr_debates_osc_pais = n()/n_debates_pais) %>% 
  ungroup() %>% 
  group_by(english_ncat_subtipov2, english_cat_pais) %>% 
  summarise(n_subtipo_pais = n(),
            n_debates_con_subtipo_pais = n_distinct(id_debate),
            n_debates_pais = mean(n_debates_pais),
           # n_organizadores_pais = mean(n_organizadores_pais),
           # n_osc_pais = mean(n_osc_pais), 
            n_debates_osc_pais = mean(n_debates_osc_pais),
            pr_debates_osc_sobre_totdebates = round(n_debates_osc_pais/n_debates_pais, 3)*100,
           # pr_subtipo_pais_sobre_osc = round(n_subtipo_pais/n_osc_pais, 3)*100,
            #pr_subtipo_pais_sobre_totorgs = round(n_subtipo_pais/n_organizadores_pais, 3)*100,
            pr_debates_con_subtipo_pais_sobre_debatesosc = round(n_debates_con_subtipo_pais/n_debates_osc_pais, 3)*100,
            pr_debates_con_subtipo_pais_sobre_totdebates = round(n_debates_con_subtipo_pais/n_debates_pais, 3)*100) %>% 
  arrange(english_cat_pais, english_ncat_subtipov2) #%>% 
 # mutate(where(is.numeric(), round(2)))

#base_osc %>% write_excel_csv(file = "anexos/anexo3.xlsx")

# ups, eran todos los subtipos, . Ah no lo hice antes, pero no por pais

base_subtipos <- base_organizadores %>% 
  group_by(english_cat_pais) %>% 
  mutate(n_debates_pais = n_distinct(id_debate),
         n_organizadores_pais = n() ) %>% 
  ungroup() %>% 
  group_by(english_cat_pais, english_cat_tipoorgv2) %>% 
  mutate(n_tipo_pais = n(),
         n_debates_tipo_pais = n_distinct(id_debate),
         pr_debates_tipo_pais = n_debates_tipo_pais/n_debates_pais) %>% 
  ungroup() %>% 
  group_by(english_cat_tipoorgv2, english_ncat_subtipov2, english_cat_pais) %>% 
  summarise(n_subtipo_pais = n(),
            n_debates_con_subtipo_pais = n_distinct(id_debate),
            n_debates_pais = mean(n_debates_pais),
            # n_organizadores_pais = mean(n_organizadores_pais),
            # n_osc_pais = mean(n_osc_pais), 
            n_debates_tipo_pais = mean(n_debates_tipo_pais),
            pr_debates_tipo_sobre_totdebates = round(n_debates_tipo_pais/n_debates_pais, 3)*100,
            # pr_subtipo_pais_sobre_osc = round(n_subtipo_pais/n_osc_pais, 3)*100,
            #pr_subtipo_pais_sobre_totorgs = round(n_subtipo_pais/n_organizadores_pais, 3)*100,
            pr_debates_con_subtipo_pais_sobre_debatestipo = round(n_debates_con_subtipo_pais/n_debates_tipo_pais, 3)*100,
            pr_debates_con_subtipo_pais_sobre_totdebates = round(n_debates_con_subtipo_pais/n_debates_pais, 3)*100) %>% 
  arrange(english_cat_pais, english_cat_tipoorgv2, english_ncat_subtipov2) #%>% 

base_subtipos %>% write_excel_csv(file = "anexos/english_anexo3.2.xlsx")

# 2.7 cuenta de organizadores desde numerico. horrible. no usado #######

plot_orgs_violin <- base %>% 
  ggplot() +
  geom_violin(aes(english_cat_pais, ncat_mean_tipoorg_ambito,
                  colour=cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

#####################################################################################
######################################################################

# CAPITULO 3 FORMATOS #############

# para esta cap y el que viene tengo que agregar los nombres de pais en ingles, falto

base_formatos <- base_formatos %>% 
  left_join(trad0)

base_temas <- base_temas %>% 
  left_join(trad0)

# colores
coloresformato <- base_formatos %>% 
  distinct(english_cat_tipo_formato, colores_formato)

# 3.0 cuentas de formatos, para presentacion anexa #####
# hacemos grafico
# para temas

317-length(unique(base_temas$id_debate))

cuenta_temas_2 <-base_temas %>%
  group_by(english_cat_tipo_tema) %>% 
  summarise(n_tema = n())  

plot_cuenta_temas_2 <- cuenta_temas_2 %>% 
 # na.omit() %>% 
  arrange(n_tema) %>% 
  mutate(order = row_number()) %>% 
  mutate(english_cat_tipo_tema = fct_reorder(english_cat_tipo_tema, order)) %>% 
  ggplot() +
  geom_col(aes(english_cat_tipo_tema, n_tema, fill = english_cat_tipo_tema)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "Monothematic",
    "Free",
    "Topics",
    "Questions"
  )) +
  labs(y = "n themesᵃ", x = "Thematic layout",
       caption = paste("Source: Author, with data collected for this research.
       
       ᵃEach unit represents a single debate-thematic-layout.
       
       If a debate was conducted according to more than one thematic pattern, it is counted twice.
       There are 57 debates for which no information is available.
                       
                              The count includes the debates of all the countries studied.: 
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString()
       ),
       title = "Debates' thematic layouts",
       subtitle = "Absolute quantity, for the entire database")

# para formatos


317-length(unique(base_formatos$id_debate))

cuenta_formatos_2 <-base_formatos %>%
  group_by(english_cat_tipo_formato) %>% 
  summarise(n_formato = n())  

plot_cuenta_formatos_2 <- cuenta_formatos_2 %>% 
  subset(!str_detect(english_cat_tipo_formato,"open")) %>% 
  # na.omit() %>% 
  arrange(n_formato) %>% 
  mutate(order = row_number()) %>% 
  mutate(english_cat_tipo_formato = fct_reorder(english_cat_tipo_formato, order)) %>% 
  ggplot() +
  geom_col(aes(english_cat_tipo_formato, n_formato, fill = english_cat_tipo_formato)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_fill_manual(breaks= coloresformato$english_cat_tipo_formato,
                     values=coloresformato$colores_formato) +
  scale_x_discrete(labels = c("Public-present",
                              "Monologic",
                              "Public-remote",
                              "Panel-Experts",
                              "Panel-Sectors", 
                              "Free", 
                              "Journalists", 
                              "Duel",
                              "Moderators"
                              )) +
  labs(y = "n patternsᵃ", x = "Patterns of interaction",
       caption = paste("Source: Author, with data collected for this research.
       
       ᵃEach unit represents a single debate-pattern of interaction.
       
       If a debate was conducted according to more than one interaction pattern, it is counted twice.
       There are 51 debates for which no information is available.
       
              The count includes the debates of all the countries studied.: 
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString()
       ),
                       
       title = "Debates' patterns of interaction",
       subtitle = "Absolute quantity, for the entire database")


# 3.1 Formato en el tiempo ########

tipos_formatos_Year <- base_formatos %>% 
  subset(!str_detect(english_cat_tipo_formato,"open")) %>% 
 #fct_collapse(cat_tipo_formato,
 #             pr_formatomoderadores = "pr_formatomoderadores",
 #                                             "pr_formatoapertura") %>% 
  group_by(ncat_eleccion, english_cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )

#base_formatos$english_cat_tipo_formato %>% unique()

tipos_formatos_ev_anual <-  tipos_formatos_Year %>% 
  ggplot(aes(ncat_eleccion, fct_relevel(english_cat_tipo_formato, "pr_formatpresent", "pr_formatvirtual", "pr_formatsectors",
                                       "pr_formatexperts", "pr_formatmoderators","pr_formatjournalists", 
                                       "pr_formatmonologic", "pr_formatduel", "pr_formatfree"), 
             colour = english_cat_tipo_formato, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)) + 
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  scale_color_manual(breaks= coloresformato$english_cat_tipo_formato,
                     values=coloresformato$colores_formato) +
 scale_y_discrete(labels = c("Public-present", "Public-virtual", "Panel-Sectors", "Panel-Experts", "Moderators", "Journalists", "Monologic", "Duel", "Free")) +
  labs(x = "Year",
       y = "Patterns of interaction",
       title = "Debates' patterns of interaction ",
       subtitle = "Over time, for the entire region",
       caption = paste("Source: Author, with data collected for this research. 
       
       The size of the circles is proportional to the number of discussions made with each interaction pattern in a given year.
              If a debate was conducted according to more than one interaction pattern, it is counted twice.
              
              The count includes all sampled countries: 
         ", colorespais$english_cat_pais[1:10] %>% toString(), "
       ", colorespais$english_cat_pais[11:18] %>% toString()))  

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.2 Temas en el tiempo ############

tipos_temas_Year <- base_temas %>% 
  group_by(ncat_eleccion, english_cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

tipos_temas_Year$english_cat_tipo_tema %>% unique()

tipos_temas_ev_anual <- tipos_temas_Year %>% 
  ggplot(aes(ncat_eleccion, english_cat_tipo_tema, 
             colour = english_cat_tipo_tema, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)) +
  scale_x_continuous(breaks = seq(1955,2021,5))  +
  scale_y_discrete(breaks = c("pr_thematictopics", "pr_freetheme", "pr_monothematic", "pr_thematicquestions"), 
  labels = c("Broad Topics", "Free", "Monothematic", "Specific questions")) +
  labs(x = "Year",
       y = "Thematic layout",
       title = "Debates' thematic layouts",
       subtitle = "Over time",
       caption = paste("Source: Author, with data collected for this research.
      
       The size of the circles is proportional to the number of debates held under the respective thematic layout in a given year. 
              If a debate was conducted under more than one thematic layout, it is counted twice.
              
              The count includes all the cases studied: 
         ", colorespais$english_cat_pais[1:10] %>% toString(), "
       ", colorespais$english_cat_pais[11:18] %>% toString()))   

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.3 ANEXO Diversidad de formatos en el tiempo. para el anexo  ######

# preferido
plot_diversidad_formatos2 <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(mean_ncatformatos = mean(n_catformatos, na.rm = TRUE),
            n_debates = n(),
            sum_catformatos = sum(n_catformatos, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point((aes(ncat_eleccion,
                  mean_ncatformatos,
                  size= n_debates,
                  #size= sum_catformatos
                  ))) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,2.5,0.5)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Average format variety",
       caption = "Source: Author, with data collected for this research.
       Average patterns of interaction within a single debate,
       Calculated annually, without discriminating between countries.
       Note that more debates are held in more recent elections,
       as represented by the size of the respective circles.",
       title = "Variety of patterns of interaction over time",
       subtitle = "Yearly averages")

# otra opcion

plot_variedad_formatos4 <- base%>% 
  ggplot(aes(ncat_eleccion, n_catformatos) ) +
  geom_point(alpha = 0.1 #aes(alpha = as.numeric(ncat_eleccion))
  ) +
  geom_smooth(alpha = 0.1, colour= "khaki" ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,5,1)) +
  scale_x_continuous(breaks = seq(1955,2021,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Year", y = "n formatsᵃ",
       title = "Patterns of interaction variety",
       subtitle = "by debate, over time",
       caption = paste("Source: Author, with data collected for this research.
  
       Data does not differentiate between countries.
       All the studied cases were taken into account:  
                       ", colorespais$english_cat_pais[1:10] %>% toString(), "
                       ", colorespais$english_cat_pais[11:18] %>% toString(), "
       
       Each circle represents a debate.
       
       The circles were plotted with some transparency to visualize the overlaps between them  
       (these occur when several debates with the same number of formats were held in one election).
       
       The yellow line plots the y ~ x formula.
       
       ᵃn formats = number of different patterns of interaction according to which an encounter takes place."
       ))  


# me gusto menos
plot_diversidad_formatos <- base %>% 
  group_by(ncat_eleccion, n_catformatos) %>% 
  summarise(n_n_catformatos = n()) %>% 
  ggplot() +
  geom_point((aes(ncat_eleccion,
                  n_catformatos ,
                  size = n_n_catformatos)))

# 3.3 Formatos por paises ########

tipos_formatos_pais <- base_formatos %>% 
  subset(!str_detect(english_cat_tipo_formato,"open")) %>% 
  #fct_collapse(cat_tipo_formato,
  #             pr_formatomoderadores = "pr_formatomoderadores",
  #                                             "pr_formatoapertura") %>% 
  group_by(english_cat_pais, english_cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )

plottipos_formatos_pais <- tipos_formatos_pais %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.ᵃ")) %>% 
  ggplot(aes(as.factor(english_cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Dom. Rep.ᵃ", "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"),
             #  fct_relevel(
             #    "Brazil", "Colombia", "Costa Rica", "Mexico", "Peru", "Chile",
             #    "Guatemala", "Panama", "Ecuador", "Uruguay", "Argentina", "Bolivia", 
             #    "Paraguay", "Honduras", "El Salvador", "Nicaragua", "Dom. Rep.",  "Venezuela"
             #  ), 
             fct_relevel(english_cat_tipo_formato, "pr_formatpresent", "pr_formatvirtual", "pr_formatsectors",
                                        "pr_formatexperts", "pr_formatmoderators","pr_formatjournalists", 
                                        "pr_formatmonologic", "pr_formatduel", "pr_formatfree"), 
             colour = english_cat_pais, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$english_cat_pais,
                     values=colorespais2$cols_18) +
  scale_y_discrete(labels = c("Public-present", "Public-virtual", "Panel-Sectors", "Panel-Experts", "Moderators", "Journalists", "Monologic", "Duel", "Free")) +
  labs(x = "Country",
       y = "Patterns of interaction",
       title = "Debates'patterns of interaction ",
       subtitle = "By country",
       caption = "Source: Author, with data collected for this research. 
       
       The size of the circles is proportional to the number of discussions made with each interaction pattern in a given year.
       If a debate was made according to more than one interaction pattern, it is counted twice.
      
        ᵃDom. Rep. = Dominican Republic.")

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.3a cuentas ########

base_tipoorganizadores_linea <- base %>% 
  select(c(id_debate, str_organizador,
         starts_with("n_cattipo")))

base_participativos <- base_formatos %>% 
  filter(english_cat_tipo_formato %in% c("pr_formatpresent", 
                                 "pr_formatvirtual",
                                 "pr_formatsectors")) %>% 
  left_join(base_tipoorganizadores_linea)

base_participativos_tabla <- base_participativos %>% 
  group_by(english_cat_pais, english_cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_state, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_pmm, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_cmm, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_cso, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

base_monotematicos <- base_temas %>% 
  filter(english_cat_tipo_tema %in% c("pr_monothematic")) %>% 
  left_join(base_tipoorganizadores_linea)

base_monotematicos_tabla <- base_monotematicos  %>% 
  group_by(english_cat_pais, english_cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_state, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_pmm, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_cmm, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_cso, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

# 3.4 tEMAS por paises ########

tipos_temas_pais <- base_temas %>% 
  group_by(english_cat_pais, english_cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

#tipos_temas_pais$english_cat_tipo_tema %>% unique()
plottipos_tema_pais <- tipos_temas_pais %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.ᵃ")) %>% 
  ggplot(aes(as.factor(english_cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Dom. Rep.ᵃ", "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"), english_cat_tipo_tema, 
             colour = english_cat_pais, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$english_cat_pais,
                     values=colorespais2$cols_18) +
 scale_y_discrete(breaks = c("pr_thematictopics","pr_freetheme", "pr_monothematic", "pr_thematicquestions"), 
  labels = c("Broad topics", "Free", "Monothematic", "Specific questions")) +
  labs(y = "Thematic layouts",
       x = "Country",
       title = "Debates' thematic layouts ",
       subtitle = "By country",
       caption = "Source: Author, with data collected for this research.
       
        The size of the circles is proportional to the number of debates held under the respective thematic scheme in a given year. 
              If a debate was held under more than one thematic scheme, it is counted twice.
      
        ᵃDom. Rep. = Dominican Republic.")

plotnumber <- plotnumber + 1
filename <- paste("images/english_plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.5 ANEXO cuanti ################

plot_formatos_cuanti <- base %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(english_cat_pais) %>% 
                     fct_relevel(
                       "Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                        "Guatemala",  "Paraguay", "Dom. Rep.",  "Honduras", "El Salvador",  "Nicaragua", 
                        "Venezuela", "Bolivia", "Panama", "Ecuador","Uruguay", "Argentina"), 
                   as.numeric(ncat_competencia), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Competition levels",
       caption = "Source: Author, with data collected for this research.
       
       The graph shows the distribution of the level of competition of the debates by country.
       The variable is an ordinal transformation of the interaction patterns that candidates are encouraged to establish with each other.
       The higher the value, the freer and more direct the dialogues between candidates.
       For a more detailed account, see the Appendix.
       
       ᵃDom. Rep. = Dominican Republic.",
       title = "Level of competition among candidates",
       subtitle = "by country")
  
plot_formatos_cuanti2 <- base %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(english_cat_pais) %>% 
                     fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                                 "Guatemala",  "Paraguay", "Dom. Rep.ᵃ",  "Honduras", "El Salvador",  "Nicaragua", 
                                 "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"), 
                   as.numeric(ncat_ppac), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Participation level",
       caption = "Source: Author, with data collected for this research.
       
       The graph shows the distribution of the debates' level of participation by country.
       The variable is an ordinal transformation of the patterns of interaction that the candidates are encouraged to establish with the audience.
       The higher the value, the greater the presence of the public in the debates. 
       For a more detailed account, see the Appendix.
       
       ᵃDom. Rep. = Dominican Republic.",
       title = "Level of public participation in the debates",
       subtitle = "by country")


######################################################################
######################################################################
# CAPITULO 4 REGULACION #############

# esto me falto traducir asi que traduzco ahora

trad1 <- base %>% 
  group_by(english_cat_regcandidatos, cat_regcandidatos) %>% 
  summarise(no_importa=n()) %>% 
  select(-no_importa)

trad2 <- base %>% 
  group_by(english_cat_regmedios, cat_regmedios) %>% 
  summarise(no_importa=n()) %>% 
  select(-no_importa)

trad3 <- base %>% 
  group_by(english_cat_regestado, cat_regestado) %>% 
  summarise(no_importa=n()) %>% 
  select(-no_importa)

base_normativa <- base_normativa %>% 
  left_join(trad0) %>% 
  left_join(trad1) %>% 
  left_join(trad2) %>% 
  left_join(trad3) 


# 41. punto de vista de los candidatos a través del tiempo ######

#base_normativa$english_cat_regcandidatos %>% unique()

plot_normativa1 <- base_normativa %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(english_cat_regcandidatos,
                             "NOTHING", "POSSIBILITY", "GUARANTEES", "OBLIGATION"),
                 colour = english_cat_regcandidatos)) +
  facet_wrap( ~ as.factor(english_cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Dom. Rep.", "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador", "Uruguay", "Argentina"), 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolution of the regulatory frameworks",
       subtitle = "From the candidates' point of view,
       over time, by country",
       caption = "Source: Author, with data collected for this research.")

# 4.2 punto de vista del Estado a través del tiempo #####

#base_normativa$english_cat_regestado %>% unique()

plot_normativa2 <- base_normativa %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(english_cat_regestado, "NOTHING", "POSSIBILITY", "AUDIT", "GUARANTEE", "ORGANIZE"),
                 colour = english_cat_regestado)) +
  facet_wrap( ~ as.factor(english_cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Dom. Rep.",  "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolution of the regulatory frameworks",
       subtitle = "From the point of view of the State,
       over time, by country",
       caption = "Source: Author, with data collected for this research.")

# 4.3 punto de vista de los medios a través del tiempo ##########

#base_normativa$english_cat_regmedios %>% unique()

plot_normativa3 <- base_normativa %>% 
  mutate(english_cat_pais = str_replace(english_cat_pais,"Dominican Republic", "Dom. Rep.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(english_cat_regmedios,
                             "NOTHING", "POSSIBILITY", "OPPORTUNITY", "LIMITATIONS", "OBLIGATIONS"),
                 colour = english_cat_regmedios)) +
  facet_wrap( ~ as.factor(english_cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brazil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Dom. Rep.",  "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"),
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = seq(1950,2021,10)) +
  labs(x = "",
       y = "",
       title = "Evolution of the regulatory frameworks",
       subtitle = "From the point of view of media and organizers,
       over time, by country",
       caption = "Source: Author, with data collected for this research.")

# alternativa. tablita#### 

base_normativa_tabla <-
  base_normativa %>% 
  distinct(
  english_cat_pais, 
  english_cat_regmedios,
  english_cat_regestado,
  english_cat_regcandidatos) 
 

base_normativa_tabla %>%  
  gt::gt()

base_normativa_tabla %>% write_excel_csv("anexos/english_anexo2.xlsx")

######################################################################
######################################################################
# CAPITULO 5 AGREGACION COPIADO DE trabajo_capitulo_agregacion.R ########

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

# metodo de clustering elegido: complete. especificacion con ordinales 
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

# funcion de base  

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


# CON YearS REG ES ESTEEEEEEE  

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

dend_prueba <- fclusterxpais(df= base_cluster_pais, cols= cols, id= "english_cat_pais", metodolink = "complete" )

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

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-english_cat_pais)
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
cor_filtered1 %>%  write.csv("anexos/english_cor_filtered1.csv")
base_cluster_pais %>%  write.csv("anexos/english_control_base_cluster_pais.csv")