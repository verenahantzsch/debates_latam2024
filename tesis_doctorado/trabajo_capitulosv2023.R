# este trabajo crea los graficos usados originalmente en los capitulos individuales de la tesis de maestria ####################

## librerias
library(tidyverse)
#library(hrbrthemes) #
library(RColorBrewer)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
#library(xlsx) #

setwd("/home/carolina/Documents/Proyectos R/debates_latam/tesis_doctorado/")

# datos
base <- readxl::read_xlsx("./datav2023/base_final3v2023.xlsx")
elecciones <-  readxl::read_xlsx("./datav2023/base_eleccionesv2023.xlsx") # base auxiliar: años que hubo elecciones por país
base_organizadores <- readxl::read_xlsx("./datav2023/base_organizadoresv2023.xlsx")
base_formatos <- readxl::read_xlsx("./datav2023/base_formatos_longv2023.xlsx")
base_temas <- readxl::read_xlsx("./datav2023/base_temas_longv2023.xlsx")
base_normativa <- readxl::read_xlsx("./datav2023/base_debates_limpiav2023.xlsx", sheet = "debates_normativo")
base_anual <- readxl::read_xlsx("./datav2023/base_anualv2023.xlsx")

nombres_subtipos <- readxl::read_xlsx("./codebooksv2023/nombres_subtiposv2023.xlsx")

colorespais <- base %>% 
  distinct(cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 

plotnumber <- 0 # para guardar con numeracion ascendente

###############################################################################
######################################################################
# CAPITULO 1 GR DE PENETRACION ###############

# 1.1 GRAFICO DE EVOLUCION TEMPORAL #####


debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n()) 

colores <- base %>%
  distinct(cat_pais, cols_18) 

base_años <- elecciones %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais),
          n_debates_año_pais = replace_na(n_debates_año_pais, 0) ) %>% 
  left_join(colores)

plot_point_anual <- base_años %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot()  +
  geom_point(aes(ncat_eleccion,as.factor(cat_pais) %>% 
                   fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                               "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                               "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), 
                 size= n_debates_año_pais, colour = cols_18, shape = debates_dico, alpha= debates_dico)) + 
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  #theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90) ) +  #,
       # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  labs(x = "", y = "",
       title = "Debates a través del tiempo",
       subtitle = "Cuántos y cuándo se hicieron",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
      
       El tamaño de los círculos representa la cantidad de debates hechos en una elección.
       Las x representan elecciones sin debates")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# segunda prueba de plot anual

plot_point_anual2 <- base_años %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(ncat_eleccion, 
             log(n_debates_año_pais),
             colour = as.factor(cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina")))  +
  geom_line() + 
  geom_point(aes(#size= n_debates_año_pais, 
                 shape = debates_dico, alpha= debates_dico)) +
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
  scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
  facet_wrap( ~ as.factor(cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), 
              ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 10), size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +  #,
  # axis.text.y = element_text(colour= text_colour$cols_18) ) + 
  scale_x_continuous(breaks = seq(1955,2025,10)) +
  #scale_y_continuous(breaks = seq(0,24,3)) +
  labs(x = "Año", y = "Cantidad de debates, en log",
       title = "Debates a través del tiempo",
       subtitle = "Cuántos se hicieron y cuándo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       Las x representan elecciones sin debates.
       Para una mejor visualización, la cantidad de debates (eje vertical) está representada en su versión logarítmica.
       El máximo real se ubica en 24 debates anuales (Costa Rica), el mínimo en 0.
       
       *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 1.2 GRAFICO DE CANTIDAD DE DEBATES / INDEX AUSENTES #####

# quiero revisar la cantidad de debates con alguna ausencia
# en relacion a la cantidad de debates por eleccion

base_ausencias_primera <- base %>% 
  filter(ncat_ronda== 1) %>% 
  group_by(ncat_eleccion, cat_pais, cols_18) %>% 
  summarise(cantidad_debates_ronda = n(),
         cantidad_debates_ausencias = sum(dico_ausencias, na.rm = TRUE),
         proporcion_debates_ausencias = cantidad_debates_ausencias/cantidad_debates_ronda,
         suma_indexausentes = sum(n_indexausentes, na.rm = TRUE),
         mean_indexausentes = mean(n_indexausentes, na.rm = TRUE),
         sd_indexausentes = sd(n_indexausentes, na.rm = TRUE))

# ANEXO mean versus sd ausencias

plotausencias_sd_versus_mean <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = mean_indexausentes, x= sd_indexausentes, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Desvío estándar en el índice de ausencias", y = "Promedio en el índice de ausencias",
       title = "Índice de ausencias ",
       subtitle = "promedio versus desvío estándar",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación
       Calculado para los debates realizados para la primera ronda.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# ANEXO proporcion de debates con ausencias versus proprocion de debates realizados , para el anexo

plotausencias_debates_versus_proporcion <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = cat_pais, vjust = 0.2), alpha = 0.6) +
  geom_label(aes(y = cantidad_debates_ronda, x= proporcion_debates_ausencias, colour = cols_18, label = ncat_eleccion, vjust = 1.2), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Proporción de debates con ausencias", y = "N debates primera ronda",
       title = "Debates con ausencias ",
       subtitle = "en relación al total de debates realizados para
       para la primera ronda",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# cantidad de debates versus promedio indice ausencias ESTE SI

plotausencias_anuales_meanindex <- base_ausencias_primera %>% 
  ggplot() +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = cat_pais, vjust = 0), alpha = 0.6) +
  geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = ncat_eleccion, vjust = 1), alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,30,1)) +
  labs(x = "Índice de ausencias", y = "n debates primera ronda",
       title = "Promedio en el índice de ausencias",
       subtitle = "Por elección, en relación al los debates realizados
       para la primera ronda",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El índice de ausencias multiplica la proporcion de candidatos que faltaron a un debate, 
       por el porcentaje de votos que estos obtuvieron en las elecciones.
       
       El gráfico expone promedios por año-país, para los debates realizados antes de la primera vuelta electoral
       (antes del ballotage, por definición, no puede haber debates con ausencias).")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

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
  group_by(cat_tipoorgv2) %>% 
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
  group_by(cat_tipoorgv2, ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  arrange(desc(cat_tipoorgv2), desc(n)) %>%  
  ungroup() %>% 
  group_by(cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n/n_cat*100) 

#cuenta_subtipos_gral %>% write_excel_csv(file = "anexos/anexo1.xlsx")

# hacemos grafico

cuenta_subtipos_gral_2 <- base_organizadores  %>%
  group_by(cat_tipoorgv2, ncat_subtipov2) %>% 
  summarise(n_subtipo = n()) %>% 
  arrange(desc(cat_tipoorgv2), desc(n_subtipo)) %>%  
  ungroup() %>% 
  mutate(ncat_subtipov2_2 = ifelse(n_subtipo > 3, ncat_subtipov2, "otros")) %>%
  group_by(cat_tipoorgv2) %>% 
  mutate(n_cat = sum(n_subtipo)) %>% 
  ungroup() %>% 
  mutate(pr_cat = n_subtipo/n_cat*100) 

plot_cuenta_subtipos_gral_2 <- cuenta_subtipos_gral_2  %>% 
  left_join(nombres_subtipos %>% rename(ncat_subtipov2_2 = "ncat_subtipov2")) %>% 
  mutate(str_subtipo = ifelse(is.na(str_subtipo), "Otros", str_subtipo)) %>%  
  na.omit() %>% 
  arrange(n_cat, n_subtipo) %>% 
  mutate(order = row_number()) %>% 
  mutate(str_subtipo = fct_reorder(str_subtipo, order))  %>%
  ggplot() +
  geom_col(aes(str_subtipo, n_subtipo, fill = cat_tipoorgv2)) +
  coord_flip() +
  theme_minimal() +
  scale_fill_discrete(breaks=c("educ", "estado", "mmc","mmp", "osc", "NA"), 
                      labels=c("Sector educativo", "Estado", "Medios comerciales","Medios públicos", "OSCs", "S/ Datos")) +
  labs(y = "n subtipo*", x = "Subtipo de organizador", fill = "Tipo de organizador",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un organizador-debate.
       Si un organizador participó de dos debates, se lo contabiliza dos veces.
       Hay 16 debates para los que desconcemos el carácter del organizador.
       
       La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
                       ),
       title = "Tipos y subtipos de organizadores",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

  
plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# por tipo
cuenta_subtipos_mmc <- base_organizadores %>% 
  filter(cat_tipoorgv2=="mmc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_osc <- base_organizadores %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_mmp <- base_organizadores %>% 
  filter(cat_tipoorgv2=="mmp") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_educ <- base_organizadores %>% 
  filter(cat_tipoorgv2=="educ") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

cuenta_subtipos_estado <- base_organizadores %>% 
  filter(cat_tipoorgv2=="estado") %>% 
  group_by(ncat_subtipov2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pr = n/sum(n)*100) %>% 
  arrange(desc(pr))

sum(cuenta_tipos$n_individuos)
sum(cuenta_tipos$n_debates_en_participaron)


# 2.2 ANEXO ALIANZAS EN T #########

alianzas_cuenta <- base_organizadores %>%  
  group_by(id_debate, ncat_eleccion, cat_pais) %>% 
  summarise(n_orgs = n(),
            n_variedadorgs = n_distinct(cat_tipoorgv2),
            n_variedadsubtipos = n_distinct(ncat_subtipov2))

plot_alianzas_cuenta <- alianzas_cuenta %>% 
  ggplot(aes(ncat_eleccion, n_variedadorgs)) +
  geom_smooth(alpha = 0.1, colour= "khaki") +
  geom_point(alpha = 0.1) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,15,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año de elección", y = "n tipos de orgs.**")


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
  labs(x = "", y = "n organizadores*")


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
  title = 'Cantidad y variedad de organizadores',
  subtitle = 'por debate, a lo largo del tiempo',
  caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       Los datos no diferencian entre países. Todos los casos estudiados fueron tenidos en consideración:  
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString(), "
       
       Cada círculo representa un debate. 
       Los círculos fueron graficados con cierta transparencia para visualizar las superposiciones entre ellos 
       (que ocurren cuando en una elección se realizaron varios debates con la misma cantidad o diversidad de organizadores).
       
       La línea amarilla grafica la fórmula y ~ x.
       
       *n organizadores = cantidad absoluta de entidades que organizan de manera conjunta un encuentro.
       **n tipos de orgs. = cantidad de tipos de organizadores diferentes que participan de la organización de un encuentro."
))  + 
  patchwork::plot_annotation(tag_levels = c('1'), tag_prefix = 'Fig. ', tag_sep = '.', 
                  tag_suffix = ':') & 
  theme(#plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 9, hjust = -0.5, vjust = -0.5))

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

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
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año de elección", y = "Cantidad de debates",
       caption = "",
       title = "")


patchwork_alianzas_cum <-  patchwork::wrap_plots(plot_alianzas_cuenta_anual_cantidad, plot_alianzas_cuenta_anual_variedad ) + 
  patchwork::plot_layout(ncol=1) + 
  patchwork::plot_annotation(
    title = 'Cantidad y variedad de organizadores',
    subtitle = 'por debate, a lo largo del tiempo',
    caption = 'Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       *n organizadores = cantidad absoluta de entidades que organizan de manera conjunta un encuentro.
       **n tipos de organizadores = cantidad de tipos de organizadores diferentes que participan de la organización de un encuentro.'
  )

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

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
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "n tipos de organizadores",
       title = "Cantidad y variedad de organizadores",
       subtitle = "en promedio por año",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       El tamaño de los círculos representa la cantidad de organizadores promedio.
       El eje y, la variedad, es decir, los tipos de organizadores promedio.
       Los valores no discriminan entre países.
       Téngase en cuenta que la cantidad de debates total aumenta a lo largo del tiempo.")

# 2.3 STREAMING #######

streaming <- base %>% 
  filter(dico_streaming == TRUE) %>% 
  mutate(dico_cattipo_mmc = ifelse(n_cattipo_mmc==0,0,1),
         dico_cattipo_osc = ifelse(n_cattipo_osc==0,0,1),
         dico_cattipo_estado = ifelse(n_cattipo_estado==0,0,1),
         dico_cattipo_educ = ifelse(n_cattipo_educ==0,0,1),
         dico_attipo_mmp = ifelse(n_cattipo_mmp==0,0,1),
         dico_cattipo_NA = ifelse(n_cattipo_NA==0,0,1)) %>% 
  select(starts_with("dico_cattipo"))

streamingtotals <- colSums(streaming) 
# revisar eventualmente si estas cuentas tienen sentido . Tienen sentido pero no son la cuenta pertinente. Porque no ponderan por cantidad de actores del tipo involucrado
17/sum(streamingtotals)*100   # osc 43.58095 (viejo)--> 36.17021 (nuevo 20203)
9/sum(streamingtotals)*100  # educ 16.53333 . 60.11428 -->19.14894
19 /sum(streamingtotals)*100  # mmc 39.08572 --> 40.42553
0 # estado
2/sum(streamingtotals)*100 # NA -->4.255319
sum(streamingtotals)/length(base$id_debate)*100 #  (v2023:) 12.46684

oscsstreaming <- base %>% 
  filter(n_cattipo_osc>0) %>% 
  select(dico_streaming, n_cattipo_osc) %>% 
  mutate(dico_streaming = ifelse(dico_streaming==TRUE,1,0))

sum(oscsstreaming$dico_streaming, na.rm=T)/length(oscsstreaming$dico_streaming)*100
# ojo, emprolijar NAs, en general equivalen a streaming == F. Creo que a los fines de na.rm el resultado seria el mismo

# 2.4 TIPOS DE ORG A LO LARGO DEL T ########

# preparamos datos. Calcularemos la "proporción" de organizadores que corresponde a cada tipo por debate
# esta es calculada simplemente como la proporción de un tipo determinado de organizador sobre el total de diferentes tipos de organiadores por debates
# NO calcula esta proporción con base en el carácter de cada una de las organizaciones que hacen al evento.

# calculamos, como dijimos, el aporte de cada tipo de organizador a un debate
# esta base agrupada me sirve para comparar con la version vieja, eventualmente
base_tipos_grouped <- base_organizadores %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  summarise(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
            n_organizadores = n())

#base no agrupada para hacer cuentas y graficos
base_tipos_ungrouped <- base_organizadores %>% 
  group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
  mutate(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
         n_organizadores = n()) %>% 
  ungroup %>% 
  mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
         n_prppacorg = 1/n_organizadores) # porcentaje de participacion del organizador sobre la cantidad de organizadores del debate

# base agrupada por año y pais
base_tipos_countryear <- base_tipos_ungrouped %>% 
  group_by(ncat_eleccion, cat_pais, cat_tipoorgv2) %>% 
  summarise(n_prppaccatorg = sum(n_prppaccatorg, na.rm = T),
            n_prppacorg = sum(n_prppacorg, na.rm = T) )

# me gusta este para comparar dos etapas
tipos_organizadores_ev_anualv2 <- base_tipos_countryear %>% 
  mutate(cat_tipoorgv2 = ifelse(is.na(cat_tipoorgv2)|cat_tipoorgv2=="NA", "S/ Datos", cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, 
             as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos"), 
             colour = cat_tipoorgv2, 
             size= n_prppacorg,
             shape= as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos")))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = -65),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_shape_manual(values=c("S/ Datos" = 4, 
                              "estado" = 19,
                              "mmp" = 19,
                              "educ" = 19,
                              "mmc" = 19,
                              "osc" = 19)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  scale_colour_manual(breaks = c("S/ Datos", "estado", "mmp","osc", "educ", "mmc"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "Estado","Medios
                               Públicos","OSCs","Educativo","Medios 
                               Comerciales", "Sin Datos")) +
  labs(x = "Año",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, para la región en su conjunto",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado.
       Si un debate fue organizado por más de un tipo de organizador, se lo contabiliza dos veces.
                       
                       La cuenta contempla a todos los casos bajo estudio: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
                       ))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 2.5 para comparar TIPOS DE ORG POR PAIS, FACET ######
#
tipos_organizadores_ev_anual_paisv2 <- base_tipos_countryear %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  mutate(cat_tipoorgv2 = ifelse(is.na(cat_tipoorgv2)|cat_tipoorgv2=="NA", "S/ Datos", cat_tipoorgv2)) %>% 
  ggplot(aes(ncat_eleccion, as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc" ), 
             colour = cat_tipoorgv2, 
             size= n_prppaccatorg,
             shape= as.factor(cat_tipoorgv2) %>% 
               fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos")))  +
  geom_point() +
  facet_wrap( ~ as.factor(cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
                            "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), ncol = 6) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_shape_manual(values=c("S/ Datos" = 4, 
                              "estado" = 19,
                              "mmp" = 19,
                              "educ" = 19,
                              "mmc" = 19,
                              "osc" = 19)) +
  scale_x_continuous(breaks = seq(1955,2025,10)) +
  scale_colour_manual(breaks = c("S/ Datos", "estado", "mmp","osc", "educ", "mmc"),
                      values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
  scale_y_discrete(labels = c( "Estado","M.** Públicos","OSCs","Educativo","M.** Comerciales", "Sin Datos")) +
  labs(x = "Año",
       y = "Tipo de organizador",
       title = "Tipo de organizador de los debates ",
       subtitle = "A través el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
               El tamaño de los círculos es proporcional a la cantidad de debates hechos en un año dado.
       Si un debate fue organizado por más de un tipo de organizador, se lo contabiliza dos veces.
       
       *Rep. Dom. = República Dominicana.
       **M. = Medios.")
#
plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 2.6 cuentas de osc , subtipos, por paises. no usado. mandar a anexo ##############

base_osc <- base_organizadores %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_pais = n_distinct(id_debate),
         n_organizadores_pais = n() ) %>% 
  ungroup() %>% 
  filter(cat_tipoorgv2=="osc") %>% 
  group_by(cat_pais) %>% 
  mutate(n_osc_pais = n(),
         n_debates_osc_pais = n_distinct(id_debate),
         pr_debates_osc_pais = n()/n_debates_pais) %>% 
  ungroup() %>% 
  group_by(ncat_subtipov2, cat_pais) %>% 
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
  arrange(cat_pais, ncat_subtipov2) #%>% 
 # mutate(where(is.numeric(), round(2)))

#base_osc %>% write_excel_csv(file = "anexos/anexo3.xlsx")

# ups, eran todos los subtipos, . Ah no lo hice antes, pero no por pais

base_subtipos <- base_organizadores %>% 
  group_by(cat_pais) %>% 
  mutate(n_debates_pais = n_distinct(id_debate),
         n_organizadores_pais = n() ) %>% 
  ungroup() %>% 
  group_by(cat_pais, cat_tipoorgv2) %>% 
  mutate(n_tipo_pais = n(),
         n_debates_tipo_pais = n_distinct(id_debate),
         pr_debates_tipo_pais = n_debates_tipo_pais/n_debates_pais) %>% 
  ungroup() %>% 
  group_by(cat_tipoorgv2, ncat_subtipov2, cat_pais) %>% 
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
  arrange(cat_pais, cat_tipoorgv2, ncat_subtipov2) #%>% 

#base_subtipos %>% write_excel_csv(file = "anexos/anexo3.2.xlsx")

# 2.7 cuenta de organizadores desde numerico. horrible. no usado #######

plot_orgs_violin <- base %>% 
  ggplot() +
  geom_violin(aes(cat_pais, ncat_mean_tipoorg_ambito,
                  colour=cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

#####################################################################################
######################################################################

# CAPITULO 3 FORMATOS #############

coloresformato <- base_formatos %>% 
  distinct(cat_tipo_formato, colores_formato)

# 3.0 cuentas de formatos, para presentacion anexa #####
# hacemos grafico
# para temas

length(base$id_debate)-length(unique(base_temas$id_debate))

cuenta_temas_2 <-base_temas %>%
  group_by(cat_tipo_tema) %>% 
  summarise(n_tema = n())  

plot_cuenta_temas_2 <- cuenta_temas_2 %>% 
 # na.omit() %>% 
  arrange(n_tema) %>% 
  mutate(order = row_number()) %>% 
  mutate(cat_tipo_tema = fct_reorder(cat_tipo_tema, order)) %>% 
  ggplot() +
  geom_col(aes(cat_tipo_tema, n_tema, fill = cat_tipo_tema)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "Monotemático",
    "Libre",
    "En bloques",
    "Preguntas puntuales"
  )) +
  labs(y = "n tema*", x = "Tipo de estructuración temática",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un formato temático-debate.
       Si un debate se realizó conforme a más de un patrón temático, se lo contabiliza dos veces.
       Hay 57 debates para los que no disponemos de información.
                       
                              La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ),
       title = "Tipos de estructuración temática de los debates",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# para formatos


length(base$id_debate)-length(unique(base_formatos$id_debate))

cuenta_formatos_2 <-base_formatos %>%
  group_by(cat_tipo_formato) %>% 
  summarise(n_formato = n())  

plot_cuenta_formatos_2 <- cuenta_formatos_2 %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  # na.omit() %>% 
  arrange(n_formato) %>% 
  mutate(order = row_number()) %>% 
  mutate(cat_tipo_formato = fct_reorder(cat_tipo_formato, order)) %>% 
  ggplot() +
  geom_col(aes(cat_tipo_formato, n_formato, fill = cat_tipo_formato)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_fill_manual(breaks= coloresformato$cat_tipo_formato,
                     values=coloresformato$colores_formato) +
  scale_x_discrete(labels = c("Público-presente",
                              "Expositivo",
                              "Público-virtual",
                              "Panel-Expertos",
                              "Panel-Sectores", 
                              "Libre", 
                              "Periodistas", 
                              "Duelo",
                              "Moderadores"
                              )) +
  labs(y = "n tema*", x = "Tipo de esquema de interacción",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       *Cada unidad representa un esquema de interacción-debate.
       Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
       Hay 51 debates para los que no disponemos de información.
       
              La cuenta contempla los debates de todos los países estudiados: 
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString()
       ),
                       
       title = "Tipos de esquemas de interacción de los debates",
       subtitle = "Cantidad absoluta, para la base de datos en su conjunto")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# 3.1 Formato en el tiempo ########

tipos_formatos_año <- base_formatos %>% 
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
 #fct_collapse(cat_tipo_formato,
 #             pr_formatomoderadores = "pr_formatomoderadores",
 #                                             "pr_formatoapertura") %>% 
  group_by(ncat_eleccion, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )

tipos_formatos_ev_anual <-  tipos_formatos_año %>% 
  ggplot(aes(ncat_eleccion, fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                                       "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", 
                                       "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
             colour = cat_tipo_formato, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  scale_color_manual(breaks= coloresformato$cat_tipo_formato,
                     values=coloresformato$colores_formato) +
 scale_y_discrete(labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Expositivo", "Duelo", "Libre")) +
  labs(x = "Año",
       y = "Tipo de intercambio",
       title = "Tipo de formato de los debates ",
       subtitle = "A través el tiempo, para la región en su conjunto",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con cada patrón de interacción en un año dado.
              Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
              
              La cuenta contempla a todos los casos bajo estudio: 
         ", colorespais$cat_pais[1:10] %>% toString(), "
       ", colorespais$cat_pais[11:18] %>% toString()))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.2 Temas en el tiempo ############

tipos_temas_año <- base_temas %>% 
  group_by(ncat_eleccion, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

tipos_temas_ev_anual <- tipos_temas_año %>% 
  ggplot(aes(ncat_eleccion, cat_tipo_tema, 
             colour = cat_tipo_tema, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  scale_x_continuous(breaks = seq(1955,2025,5))  +
  scale_y_discrete(labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
  labs(x = "Año",
       y = "Tipo de esquema temático",
       title = "Estructuración temática de los debates",
       subtitle = "A través el tiempo",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación. 
      
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con el esquema temático respectivo en un año dado. 
              Si un debate se realizó conforme a más de un esquema temático, se lo contabiliza dos veces.
              
              La cuenta contempla a todos los casos bajo estudio: 
         ", colorespais$cat_pais[1:10] %>% toString(), "
       ", colorespais$cat_pais[11:18] %>% toString()))   

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
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
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Variedad de formatos promedio",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       Promedio de esquemas de intercambio dentro de un mismo debate,
       calculado anualmente, sin discriminar entre países.
       Téngase en cuenta que en elecciones más recientes se realizan más debates,
       medida representada en el tamaño de los círculos respectivos.",
       title = "Variedad de formatos en el tiempo",
       subtitle = "promedio por año")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# otra opcion

plot_variedad_formatos4 <- base%>% 
  ggplot(aes(ncat_eleccion, n_catformatos) ) +
  geom_point(alpha = 0.1 #aes(alpha = as.numeric(ncat_eleccion))
  ) +
  geom_smooth(alpha = 0.1, colour= "khaki" ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,5,1)) +
  scale_x_continuous(breaks = seq(1955,2025,5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90), 
        axis.title.y = element_text(margin = margin(r = 5),size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) +
  labs(x = "Año", y = "n formatos*",
       title = "Variedad de formatos",
       subtitle = "por debate, a lo largo del tiempo",
       caption = paste("Fuente: elaboración propia, con datos recopilados para la presente investigación.
  
       Los datos no diferencian entre países.
       Todos los casos estudiados fueron tenidos en consideración:  
                       ", colorespais$cat_pais[1:10] %>% toString(), "
                       ", colorespais$cat_pais[11:18] %>% toString(), "
       
       Cada círculo representa un debate. 
       Los círculos fueron graficados con cierta transparencia para visualizar las superposiciones entre ellos 
       (estas ocurren cuando en una elección se realizaron varios debates con la misma cantidad de formatos).
       
       La línea amarilla grafica la fórmula y ~ x.
       
       *n formatos. = cantidad de tipos de esquemas de interacción diferentes en los que se desarrolla un encuentro."
       ))  

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

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
  subset(!str_detect(cat_tipo_formato,"apertura")) %>% 
  #fct_collapse(cat_tipo_formato,
  #             pr_formatomoderadores = "pr_formatomoderadores",
  #                                             "pr_formatoapertura") %>% 
  group_by(cat_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )

plottipos_formatos_pais <- tipos_formatos_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(as.factor(cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"),
             #  fct_relevel(
             #    "Brasil", "Colombia", "Costa Rica", "Mexico", "Peru", "Chile",
             #    "Guatemala", "Panama", "Ecuador", "Uruguay", "Argentina", "Bolivia", 
             #    "Paraguay", "Honduras", "El Salvador", "Nicaragua", "Rep. Dom.",  "Venezuela"
             #  ), 
             fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                                        "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", 
                                        "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
             colour = cat_pais, size= n_peso_formato_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
  scale_y_discrete(labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Expositivo", "Duelo", "Libre")) +
  labs(x = "País",
       y = "Tipo de intercambio",
       title = "Tipo de intercambio durante los debates ",
       subtitle = "Formatos preferidos por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
       El tamaño de los círculos es proporcional a la cantidad de debates hechos con cada patrón de interacción en un año dado.
       Si un debate se realizó conforme a más de un patrón de interacción, se lo contabiliza dos veces.
      
        *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.3a cuentas ########

base_tipoorganizadores_linea <- base %>% 
  select(c(id_debate, str_organizador,
         starts_with("n_cattipo")))

base_participativos <- base_formatos %>% 
  filter(cat_tipo_formato %in% c("pr_formatopresentes", 
                                 "pr_formatovirtuales",
                                 "pr_formatosectores")) %>% 
  left_join(base_tipoorganizadores_linea)

base_participativos_tabla <- base_participativos %>% 
  group_by(cat_pais, cat_tipo_formato) %>% 
  summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_estado, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_mmp, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_mmc, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

base_monotematicos <- base_temas %>% 
  filter(cat_tipo_tema %in% c("pr_temamonotema")) %>% 
  left_join(base_tipoorganizadores_linea)

base_monotematicos_tabla <- base_monotematicos  %>% 
  group_by(cat_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm = T),
            n_cattipo_educ = sum(n_cattipo_educ, na.rm = T),
            n_cattipo_estado = sum(n_cattipo_estado, na.rm = T),
            n_cattipo_mmp = sum(n_cattipo_mmp, na.rm = T),
            n_cattipo_mmc = sum(n_cattipo_mmc, na.rm = T),
            n_cattipo_osc = sum(n_cattipo_osc, na.rm = T),
            n_cattipo_NA = sum(n_cattipo_NA, na.rm = T))

# 3.4 tEMAS por paises ########

tipos_temas_pais <- base_temas %>% 
  group_by(cat_pais, cat_tipo_tema) %>% 
  summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) )

plottipos_tema_pais <- tipos_temas_pais %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.*")) %>% 
  ggplot(aes(as.factor(cat_pais) %>% 
               fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                           "Guatemala",  "Paraguay", "Rep. Dom.*",  "Honduras", "El Salvador",  "Nicaragua", 
                           "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), cat_tipo_tema, 
             colour = cat_pais, size= n_peso_tema_xdebate))  +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10), size = 8, hjust = 0)) + 
  scale_color_manual(breaks= colorespais2$cat_pais,
                     values=colorespais2$cols_18) +
 scale_y_discrete(labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
  labs(y = "Tipo de esquema temático",
       x = "País",
       title = "Estructuración temática de los debates ",
       subtitle = "Formatos preferidos por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación. 
       
        El tamaño de los círculos es proporcional a la cantidad de debates hechos con el esquema temático respectivo en un año dado. 
              Si un debate se realizó conforme a más de un esquema temático, se lo contabiliza dos veces.
      
        *Rep. Dom. = República Dominicana.")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 3.5 ANEXO cuanti ################

plot_formatos_cuanti <- base %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais) %>% 
                     fct_relevel(
                       "Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                        "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                        "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), 
                   as.numeric(ncat_competencia), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de competencia",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por país.
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer a los candidatos entre sí.
       A mayor valor, más libre y directos los diálogos entre los candidatos.
       Para más detalle, consultar Codebook Anexo.
       
       *Rep. Dom. = República Dominicana.",
       title = "Nivel de competencia  entre los candidatos",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

plot_formatos_cuanti2 <- base %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(cat_pais) %>% 
                     fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                                 "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
                                 "Venezuela", "Bolivia", "Panama", "Ecuador",  "Uruguay", "Argentina"), 
                   as.numeric(ncat_ppac), fill = cols_18)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 5),size = 8))  +
  labs(x = "", y = "Nivel de participación",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación.
       
       El gráfico expone la distribución del nivel de participación de los debates por país.
       La variable consiste en una transformación ordinal de los tipos de intercambios que se le perimte establecer al público.
       A mayor valor, mayor presencia del público en los debates. 
       Para más detalle, consultar Codebook Anexo.
       
       *Rep. Dom. = República Dominicana.",
       title = "Nivel de participación del público en los debates",
       subtitle = "por país")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


######################################################################
######################################################################
# CAPITULO 4 REGULACION #############

# 41. punto de vista de los candidatos a través del tiempo ######

plot_normativa1 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regcandidatos,
                             "NADA", "POSIBILIDAD", "GARANTÍAS", "OBLIGACION"),
                 colour = cat_regcandidatos)) +
  facet_wrap( ~ as.factor(cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
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
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista de los candidatos,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 4.2 punto de vista del Estado a través del tiempo #####

plot_normativa2 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regestado, "NADA", "POSIBILIDAD", "FISCALIZAR", "GARANTIZAR", "ORGANIZAR"),
                 colour = cat_regestado)) +
  facet_wrap( ~ as.factor(cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
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
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista del Estado,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# 4.3 punto de vista de los medios a través del tiempo ##########

plot_normativa3 <- base_normativa %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) %>% 
  ggplot() +
  geom_point(aes(ncat_eleccion,
                 fct_relevel(cat_regmedios,
                             "NADA", "POSIBILIDAD", "OPORTUNIDAD", "LIMITACIONES", "OBLIGACIONES"),
                 colour = cat_regmedios)) +
  facet_wrap( ~ as.factor(cat_pais) %>% 
                fct_relevel("Peru", "Mexico", "Costa Rica", "Brasil", "Colombia", "Chile",
                            "Guatemala",  "Paraguay", "Rep. Dom.",  "Honduras", "El Salvador",  "Nicaragua", 
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
       title = "Evolución de la normativa",
       subtitle = "Desde el punto de vista de medios y organizadores,
       através el tiempo, por país",
       caption = "Fuente: elaboración propia, con datos recopilados para la presente investigación")

plotnumber <- plotnumber + 1
filename <- paste("images/plot_", plotnumber, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)

# alternativa. tablita#### 

base_normativa_tabla <-
  base_normativa %>% 
  distinct(
  cat_pais, 
  cat_regmedios,
  cat_regestado,
  cat_regcandidatos) 
 
base_normativa_tabla %>%  
  gt::gt()

base_normativa_tabla %>% write_excel_csv("anexos/anexo2.xlsx")

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
  group_by(cat_pais) %>% 
  summarise(n_elecciones_con_debates = sum(dico_hubo_debates, na.rm = T),
            n_interrupciones = n()-n_elecciones_con_debates,
            n_años_reg = sum(ncat_totreg > 3, na.rm=TRUE))

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
            n_mean_reg = mean(ncat_totreg, na.rm=TRUE),
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


# CON AÑOS REG ES ESTEEEEEEE  

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

pdend <- fplotclusterxpais(dend_prueba, "Resultado de análisis de clusters", 6) +
  labs(caption = paste("Variables utilizadas: ", cols, ".
                    Calculado con base en la distancia euclidiana, 
                    usando el método de agregación por enlace completo. 
                    Fuente: elaboración propia, con datos recopilados para la presente investigación con datos recolectados para la presente investigación. "))

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
                    n_log_debates_total: logaritmo natural de la cantidad de debates realizados en un país en total. 
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

                    Fuente: Fuente: elaboración propia, con datos recopilados para la presente investigación con datos recolectados para la presente investigación. "))

df_filtered <-base_cluster_pais %>% 
  select(all_of(cols)) %>% 
  select(-cat_pais)
cor_filtered <- cor(df_filtered, method = "pearson", use = "complete.obs") 
cor_filtered1 <- cor_filtered %>% as_tibble() 
rownames(cor_filtered1) <- colnames(cor_filtered) 
cor_filtered1 %>%  write.csv("anexos/cor_filtered1.csv")
base_cluster_pais %>%  write.csv("anexos/control_base_cluster_pais.csv")