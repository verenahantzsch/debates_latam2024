
# https://arcruz0.github.io/libroadp/logit.html#representaci%C3%B3n-visual-de-los-resultados
# primera version bastante avanzada de este codigo en test_modelos_dic2024.R
# librerias ########### 

library(tidyverse)
library(sandwich) # para clusterizar errores estandares
library(lmtest) # para clusterizar errores estandares

# seed #################
set.seed(111)

# carga de data y seteo de directorio ######

# data unificada, creada en creacion_base_elecciones_datoselectorales
setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado/datav2023")

base_vdependiente <- read.csv("variables_dependientes_elecciones.csv")
base_indicadores <- read.csv("indicadores_elecciones.csv")
base_controles <- read.csv("controles_elecciones.csv")
base_candidatos <-  read.csv("indicadores_candidatos.csv")
diccionario_indicadores <- read.csv("diccionario_indicadores_etiquetas.csv")
base_debates <- read_csv("base_final3v2023.csv") %>% select(-"...1", -"...2") %>% 
  subset(dico_certidumbre==1)

countrynames <- read.csv("countrynames.csv") %>% 
  mutate(cat_pais =  iconv(cat_pais, to = "ASCII//TRANSLIT") %>%  str_trim()) %>% 
  mutate(mayus_eng_cat_pais = toupper(eng_cat_pais)) %>% 
  mutate(mayus_eng_cat_pais = str_replace(mayus_eng_cat_pais, 
                                          "DOMINICAN REPUB", 
                                          "DOMINICAN REPUBLIC"))

setwd("/home/carolina/Documents/Proyectos R/debates_latam2024/tesis_doctorado")
# ELECCIONES ##############################
## PREPARO DATA #####
### data completa (al final no la uso) #### 
data <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-starts_with("X")) %>% 
  select(-eng_cat_pais, -dico_debates_eleccion) %>% 
  left_join(base_controles %>% 
              select(-starts_with("source_")) %>% 
              select(-X, -eng_cat_pais)) %>% 
  left_join(base_vdependiente %>% select(-X)) 

# creo variable para clusterizar SSEE
data <- data %>% 
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

### data sólo democracias #### 
democracias <- base_indicadores  %>% 
  select(-starts_with("source_")) %>% 
  select(-starts_with("X")) %>% 
  select(-eng_cat_pais, -dico_debates_eleccion) %>% 
  left_join(base_controles %>% 
              select(-starts_with("source_")) %>% 
              select(-X, -eng_cat_pais)) %>% 
  left_join(base_vdependiente %>% select(-X)) %>% 
    subset(democraciavdempolyarchy>0.45) 

# creo variable para clusterizar SSEE
democracias <- democracias %>% 
  mutate(elecid = paste(cat_pais, ncat_eleccion) %>% as.factor())

### defino id obs ####

democracias <- democracias %>% 
  mutate(obsid = paste(cat_pais, ncat_eleccion, ncat_ronda, sep = " "))

### creo data log y otras transformaciones de variables ####

democracias <- democracias %>% 
  mutate(marginvic = abs(marginvic)) %>% 
  mutate(lnpropindivinternet = log(propindivinternet +1),
         lngdp = log(gdpxcapita),
         lnmarginvic = log(marginvic),
         lnnec = log(nec),
         lnvoteshareincumbent = log(voteshareincumbent + 1),
         lncumsumpastciclos = log(cumsum_pastciclos + 1)) %>% 
  mutate(regulacionotradico = ifelse(regulaciongarantiasdico==1|regulacionposibilidaddico==1,
                                     1,0))


# democracias$marginvic

### data estandarizada (toda data/democracias) #### 
# data_scaled <- data %>%
#   mutate(across(-c(dico_hubo_debates,  # excluyo las variables dep dicotomicas de la escalada
#                    dico_debates_primerosdos,
#                    dico_hubo_debate_mediatico,
#                    dico_nueva_practica_elec,
#                    dico_practica_interrumpida_elec,
#                    dico_nueva_practica_ciclo,
#                    dico_practica_interrumpida_ciclo,
#                    cat_pais,
#                    elecid), scale))

democracias_reservanoescalada <- democracias

democracias <- democracias %>%
  mutate(across(-c(dico_hubo_debates,  # excluyo las variables dep dicotomicas de la escalada
                   dico_debates_primerosdos,
                   dico_hubo_debate_mediatico,
                   dico_nueva_practica_elec,
                   dico_practica_interrumpida_elec,
                   dico_nueva_practica_ciclo,
                   dico_practica_interrumpida_ciclo,
                   cat_pais,
                   elecid,
                   ncat_ronda, #excluyo algunas variables de id
                   ncat_eleccion,
                   obsid), scale))


### data debates  ####
democracias_basedebates <-  base_debates %>%
  left_join(base_controles %>% 
              select(cat_pais, ncat_eleccion, ncat_ronda, democraciavdempolyarchy)) %>% 
  subset(democraciavdempolyarchy>0.45) %>% 
  subset(ncat_eleccion!=2024) 

### mini exploracion ####

skimr::skim(democracias)
# problemas de missing en propindiv internet (seguro se puede completar)
# problemas de missing en acceso gratuito (tratar de completar)
# problemas de missing en gdp (una picardia total)

### defino funcion para exportar ###

exportedstats <- function(lista_modelos){
  zeros <- rep(0, length(lista_modelos))
  exportedstats <- tibble("aic" = zeros,
                          "bic"  = zeros,
                          "loglik"  = zeros,
                          "n_obs"  = zeros)
  for (j in seq_along(lista_modelos)) {
    exportedstats$aic[j] <- AIC(lista_modelos[[j]])
    exportedstats$bic[j] <- BIC(lista_modelos[[j]])
    exportedstats$loglik[j] <- logLik(lista_modelos[[j]])
    exportedstats$n_obs[j] <- nobs(lista_modelos[[j]])
  }
  exportedstats
}

# PPAL MEDIDA DICO: EXISTENCIA DE DEBATES ############

## evolucion temporal de medida dico: elecciones con y sin debates #####
  
base_elecciones_conysindebates_t <- democracias %>% 
  mutate( decada = (ncat_eleccion %/% 10) * 10 ) %>% 
  group_by(decada) %>%  
 # group_by(ncat_eleccion) %>% 
  summarise(n_elecciones = n(),
            n_elecciones_con_debates = sum(dico_hubo_debates),
            n_elecciones_sin_debates = n_elecciones - n_elecciones_con_debates,
            prop_elecciones_con_debates = n_elecciones_con_debates/n_elecciones)

base_plot_elecciones_conysindebates_t <- base_elecciones_conysindebates_t %>% 
  pivot_longer(cols= c(n_elecciones_con_debates, n_elecciones_sin_debates), 
               names_to = "dico_debates", values_to = "n_dico_debates") %>% 
  mutate(porcentaje_elecciones_con_debates = paste(round(prop_elecciones_con_debates*100), "%")) %>% 
   group_by(decada) %>% 
   mutate(y_porcentaje = sum(n_dico_debates)) %>% 
   ungroup() 

############### PENDIENTE ajustar CAPTION y test significance (caption p test)  ##########

plot_elecciones_conysindebates_t <- base_plot_elecciones_conysindebates_t %>% 
  ggplot() + 
  geom_col(aes(decada, n_dico_debates, fill = dico_debates), colour = "grey80", position = "stack") +
  geom_text(aes(x = decada, y = y_porcentaje, label = porcentaje_elecciones_con_debates)) +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elections with debates", "Elections without debates"),
                    values = c("green", "grey10"),
                    name = "") +
  scale_x_continuous(breaks = seq(1960,2025,10)) +
  scale_y_continuous(breaks = seq(0,70,10)) +
  labs(title = "Graph 1: Temporal Evolution of Debates",
       subtitle = "Number of Presidential Elections with and without Debates in Latin American Democracies, 1960–2023",
       y = "Count of Elections",
       x = "Decade",
       caption = "Source: Author. 
     The first and second rounds of presidential elections are counted separately when applicable. 
     Only elections under democratic regimes (V-Dem polyarchy index > 0.45) are considered.") +
  theme(legend.position = "bottom")

 
plotname <- "ev_t"
filename <- paste("images/ENG_plot_",  plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_t$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_t$prop_elecciones_con_debates)

## distribucion espacial de medida dico: elecciones con y sin debates #####

#democracias$cat_pais %>% unique()
#countrynames$cat_pais %>% unique()
base_elecciones_conysindebates_e <- democracias %>% 
  left_join(countrynames %>% 
              mutate(cat_pais = ifelse(cat_pais=="Rep. Dominicana", "Republica Dominicana", cat_pais))) %>% 
  group_by(eng_cat_pais) %>% 
  summarise(n_elecciones = n(),
            n_elecciones_con_debates = sum(dico_hubo_debates),
            n_elecciones_sin_debates = n_elecciones - n_elecciones_con_debates,
            prop_elecciones_con_debates = n_elecciones_con_debates/n_elecciones)

base_plot_elecciones_conysindebates_e <- base_elecciones_conysindebates_e %>% 
  pivot_longer(cols= c(n_elecciones_con_debates, n_elecciones_sin_debates), 
               names_to = "dico_debates", values_to = "n_dico_debates") %>% 
  mutate(porcentaje_elecciones_con_debates = paste(round(prop_elecciones_con_debates*100), "%")) %>% 
  group_by(eng_cat_pais) %>% 
  mutate(y_porcentaje = sum(n_dico_debates)) %>% 
  ungroup() 

x_labels <- base_plot_elecciones_conysindebates_e %>% 
  subset(dico_debates == "n_elecciones_con_debates") %>% 
  mutate(label = paste(eng_cat_pais, ": ", n_elecciones, " elections, ", n_dico_debates, " with debates", sep = "")) %>% 
  select(eng_cat_pais, label) %>% 
  arrange(eng_cat_pais)

############### PENDIENTE ajustar CAPTION y test significance (caption p test y para appendix)  ##########
plot_elecciones_conysindebates_e <- base_plot_elecciones_conysindebates_e %>% # reordenarn en f de elec sin debates
  ggplot() + 
  geom_col(aes(eng_cat_pais, n_dico_debates, fill = dico_debates), colour = "grey80", position = "stack") +
  geom_text(aes(x = eng_cat_pais, y = y_porcentaje, label = porcentaje_elecciones_con_debates)) +
  theme_classic() +
  scale_fill_manual(breaks = c("n_elecciones_con_debates", "n_elecciones_sin_debates"),
                    labels =c("Elections with debates", "Elections without debates"),
                    values = c("green", "grey10"),
                    name = "") +
  scale_x_discrete(breaks = x_labels$eng_cat_pais, label = x_labels$label) +
  labs(title = "Graph 2: Geographical Distribution of Debates",
       subtitle = "Number of Presidential Elections with and without Debates in Latin American Democracies, by Country (1960–2023)",
       y = "Number of Elections",
       x = "Country",
       caption = "Source: Author. 
     The first and second rounds of presidential elections are counted separately when applicable. 
     Only elections under democratic regimes (V-Dem polyarchy index > 0.45) are considered. 
     Percentages represent the number of elections with debates out of the total.") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, size = 9))

 
plotname <- "ev_e"
filename <- paste("images/ENG_plot_",  plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)


# cuentas
summary(base_elecciones_conysindebates_e$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_e$prop_elecciones_con_debates)

## Tabla anexa para Appendix #####
# The authors should provide a table in the Appendix in which it becomes transparent
# for each country 
# when there where elections and 
# how many debates have been broadcasted. “

# elections with debates
data_pais_debates_appendix1 <- democracias_basedebates %>% 
  mutate(ronda_year = paste(ncat_ronda, "° round ", ncat_eleccion, sep = "")) %>% 
  group_by(cat_pais, ronda_year) %>% 
  summarise(n_debates = n()) %>% 
  mutate(txt = paste(ronda_year, " (", n_debates, " debates)", sep = ""))

data_pais_debates_appendix1 <- data_pais_debates_appendix1 %>%
  group_by(cat_pais) %>%
  summarise(
    elections_with_debates = paste(txt, collapse = ", "),
    .groups = "drop"
  )

# elections without debates

data_pais_debates_appendix2 <- democracias %>% 
  mutate(ronda_year = paste(ncat_ronda, "° round ", ncat_eleccion, sep = "")) %>% 
  group_by(cat_pais) %>%
  summarise(
    elections_without_debates = paste(ronda_year, collapse = ", "),
    .groups = "drop"
  )

# join
############### PENDIENTE GUARDAR  ##########
data_pais_debates_appendix <- data_pais_debates_appendix1 %>% 
  left_join(data_pais_debates_appendix2) %>% 
  left_join(countrynames %>% 
              mutate(cat_pais = ifelse(cat_pais == "Rep. Dominicana",
                                       "Republica Dominicana",
                                       cat_pais))) %>% 
  select(eng_cat_pais, elections_without_debates, elections_with_debates)

## Formulas y estimaciones de base (toda la muestra de democracias - V.D = dico_hubo_debates) #####
### Modelo contingencia  ####

formula_modelo_contingencia <- "dico_hubo_debates ~ 
                                       #dico_debates_primerosdos ~ # variables V.D alternativas, por ahora no uso, serían secundarias 
                                       #dico_hubo_debate_mediatico ~ 
                              marginvic + 
                              nec +
                             #exapprovalnotsmoothed + # este indicador y el que sigue son alternativos 
                              voteshareincumbent +
                              dico_reeleccion + 
                              regulaciondico +
                              cumsum_pastciclos + # este indicador y el que sigue son alternativos 
                              #dico_debates_pastelection +
                              gdpxcapita +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem"
                               #edadregimenbmr#, #Sacar edad de régimen.  
                               #No tiene sentido incluirla si controlas por la calidad de la democracia 
                               # ademas CAMBIAN MUY POCO resultados relevantes, chequeado

modelo_contingencia <- glm(formula_modelo_contingencia, 
                           family = binomial(link = "logit"), 
                           #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                           data = democracias)
options(scipen=999)
summary(modelo_contingencia)

formula_modelo_contingencia_bis <- "dico_hubo_debates ~ 
                                       #dico_debates_primerosdos ~ # variables V.D alternativas, por ahora no uso, serían secundarias 
                                       #dico_hubo_debate_mediatico ~ 
                               lnmarginvic + # CAMBIE
                                lnnec +
                             #exapprovalnotsmoothed + # este indicador y el que sigue son alternativos 
                              voteshareincumbent +
                              #lnvoteshareincumbent +
                              dico_reeleccion + 
                              regulaciondico +
                              cumsum_pastciclos + # este indicador y el que sigue son alternativos 
                              #dico_debates_pastelection +
                              lngdp +
                              democraciavdemelectoralcomp +
                              mediaqualitycorruptvdem"

#edadregimenbmr#, #Sacar edad de régimen.  
#No tiene sentido incluirla si controlas por la calidad de la democracia 
# ademas CAMBIAN MUY POCO resultados relevantes, chequeado

modelo_contingencia_bis <- glm(formula_modelo_contingencia_bis, 
                           family = binomial(link = "logit"), 
                           #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                           data = democracias)
options(scipen=999)
summary(modelo_contingencia_bis)


### Modelo sistemico ####


formula_modelo_sistemico <- "dico_hubo_debates ~ 
                                    alineamiento + 
                                    #volatility + #
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    gdpxcapita +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem"

modelo_sistemico <- glm(formula_modelo_sistemico,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_sistemico) # 


formula_modelo_sistemico_bis <- "dico_hubo_debates ~ 
                                    alineamiento + 
                                    #volatility + #pierdo muchas observaciones! dejar para despues
                                    proptv +
                                    propindivinternet +
                                    regulaciondico +
                                    cumsum_pastciclos +
                                    #dico_debates_pastelection +
                                    lngdp +
                                    democraciavdemelectoralcomp +
                                    mediaqualitycorruptvdem"

modelo_sistemico_bis <- glm(formula_modelo_sistemico_bis,
                        family = binomial(link = "logit"), 
                        #data = data 
                        data = democracias)

summary(modelo_sistemico_bis) # 

### Modelo marco regulatorio ####

formula_modelo_regulatorio <- "dico_hubo_debates ~ 
                                prohibicionpropaganda +
                                accesogratuito +
                                regulaciondico + 
                                cumsum_pastciclos +
                                #dico_debates_pastelection +
                                gdpxcapita +
                                democraciavdemelectoralcomp +
                                mediaqualitycorruptvdem"
                                #edadregimenbmr"


modelo_regulatorio <- glm(formula_modelo_regulatorio,
                                  family = binomial(link = "logit"), 
                                  #data = data 
                                  data = democracias)

summary(modelo_regulatorio)


formula_modelo_regulatorio_bis <- "dico_hubo_debates ~ 
                                prohibicionpropaganda +
                                accesogratuito +
                                regulaciondico + 
                                cumsum_pastciclos +
                                #dico_debates_pastelection +
                                lngdp +
                                democraciavdemelectoralcomp +
                                mediaqualitycorruptvdem"
#edadregimenbmr"


modelo_regulatorio_bis <- glm(formula_modelo_regulatorio_bis,
                          family = binomial(link = "logit"), 
                          #data = data 
                          data = democracias)

summary(modelo_regulatorio_bis)

### Modelo geo/temp ####

formula_modelo_difusion <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         gdpxcapita +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_difusion <- glm(formula_modelo_difusion,
                         family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_difusion)

formula_modelo_difusion_bis <- "dico_hubo_debates ~ 
                         avgpropdebatesregionxciclo + 
                         prop_elec_usa_ciclo +
                         regulaciondico +
                         cumsum_pastciclos +
                         #dico_debates_pastelection +
                         lngdp +
                         democraciavdemelectoralcomp +
                         mediaqualitycorruptvdem"

modelo_difusion_bis <- glm(formula_modelo_difusion_bis,
                       family = binomial(link = "logit"), 
                       #data = data 
                       data = democracias)

summary(modelo_difusion_bis)

### Modelo final / sficativas ####

formula_modelo_sficativas <- "dico_hubo_debates ~ 
                        #dico_debates_primerosdos ~ 
                        #dico_hubo_debate_mediatico ~ 
                           marginvic + 
                           nec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           gdpxcapita +
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_sficativas <- glm(formula_modelo_sficativas,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas)


### Modelo final / sficativas variantes ####
# variantes modelo sficativas
formula_modelo_sficativas_variantes <- "dico_hubo_debates ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                          #lnvoteshareincumbent +
                           dico_reeleccion + 
                           #proptv +
                           #alineamiento +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           #lncumsumpastciclos +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_sficativas_variantes <- glm(formula_modelo_sficativas_variantes,
                         family = binomial(link = "logit"), 
                         #data = data 
                         data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes)



### Modelo all #####

formula_modelo_all <- "dico_hubo_debates ~ 
                           lnmarginvic + # CAMBIE
                           lnnec +
                           #exapprovalnotsmoothed + 
                           voteshareincumbent +
                           #lnvoteshareincumbent +
                           dico_reeleccion + 
                           alineamiento +
                           proptv +
                           propindivinternet +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo +
                           regulaciondico +
                           cumsum_pastciclos +
                           #dico_debates_pastelection +
                           lngdp + # CAMBIE
                           democraciavdemelectoralcomp +
                           mediaqualitycorruptvdem "


modelo_all <- glm(formula_modelo_all,
                                   family = binomial(link = "logit"), 
                                   #data = data 
                                   data = democracias)
options(scipen=999)
summary(modelo_all)
## Modelos multinivel ####
#https://m-clark.github.io/mixed-models-with-R/random_intercepts.html


#### Modelo empty #### 

# Varying-intercept model with no predictors. 
#   M0 <- lmer (y ~ 1 + (1 | county))
# This model simply includes a constant term (the predictor “1”) and allows it to vary by county

empty_model_paises <- lme4::glmer(dico_hubo_debates ~ 
                                    1 + (1 | cat_pais), 
                                  family=binomial("logit"), 
                                  data = democracias)
summary(empty_model_paises)

mlmhelpr::icc(empty_model_paises)
# The ICC represents the proportion of group-level variance to total variance. 
# (para modelo emtpty: ) An ICC of 0.33 means that 33% of the variation in the outcome variable can be accounted for by the clustering stucture of the data. This provides evidence that a multilevel model may make a difference to the model estimates, in comparison with a non-multilevel model. Therefore, the use of multilevel models is necessary and warrantied.
# https://en.wikipedia.org/wiki/Intraclass_correlation

empty_model_years <- lme4::glmer(dico_hubo_debates ~ 
                                   1 + (1 | ncat_eleccion), 
                                 family=binomial("logit"), 
                                 data = democracias)
summary(empty_model_years)

mlmhelpr::icc(empty_model_years)


empty_model_paisyears <- lme4::glmer(dico_hubo_debates ~ 
                                       1 + (1 | ncat_eleccion) + (1 | cat_pais), 
                                     family=binomial("logit"), 
                                     data = democracias)
summary(empty_model_paisyears)

mlmhelpr::icc(empty_model_paisyears)

####  Modelos multinivel #### 
# varying-intercept model
# Varying-intercept model with an individual-level predictor. 
# M1 <- lmer (y ~ x + (1 | county))
# This expression starts with the no-pooling model, “y ~ x,” and then adds “(1 | county),” 
# which allows the intercept (the coeﬃcient of the predictor “1,” which is  the column of ones—the constant term in the regression) to vary by county.

#cluster <- democracias$cat_pais %>% as.factor()
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

### Modelo contingencia  ####

contingencia_random_intercepts <- lme4::glmer(paste(formula_modelo_contingencia_bis, 
                                                    "(1 | cat_pais)", sep = "+"), 
                                              family=binomial("logit"), 
                                              data = democracias,
                                              # weights = NULL,
                                              control = control)
summary(contingencia_random_intercepts)


### Modelo sistemico  ####

sistemico_random_intercepts <- lme4::glmer(paste(formula_modelo_sistemico_bis, 
                                                 "(1 | cat_pais)", sep = "+"), 
                                           family=binomial("logit"), 
                                           data = democracias,
                                           control = control)
summary(sistemico_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(sistemico_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# sistemico_random_intercepts_robust <- coef_test(sistemico_random_intercepts, vcov = vcov_cluster)

### Modelo regulatorio  ####

regulatorio_random_intercepts <- lme4::glmer(paste(formula_modelo_regulatorio_bis, 
                                                   "(1 | cat_pais)", sep = "+"), 
                                             family=binomial("logit"), 
                                             data = democracias,
                                             control = control)
summary(regulatorio_random_intercepts)
# vcov_cluster <-  clubSandwich::vcovCR(regulatorio_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# regulatorio_random_intercepts_robust <- coef_test(regulatorio_random_intercepts, vcov = vcov_cluster)

### Modelo difusion  ####

difusion_random_intercepts <- lme4::glmer(paste(formula_modelo_difusion_bis, 
                                                "(1 | cat_pais)", sep = "+"), 
                                          family=binomial("logit"), 
                                          data = democracias,
                                          control = control)
summary(difusion_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(difusion_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# difusion_random_intercepts_robust <- coef_test(difusion_random_intercepts, vcov = vcov_cluster)

### Modelo final  ####

final_random_intercepts <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                             "(1 | cat_pais)", sep = "+"), 
                                       family=binomial("logit"), 
                                       data = democracias,
                                       control = control)
summary(final_random_intercepts)
# vcov_cluster <- clubSandwich::vcovCR(final_random_intercepts, cluster = democracias$cat_pais, type = "CR2")
# final_random_intercepts_robust <- coef_test(final_random_intercepts, vcov = vcov_cluster)

mlmhelpr::icc(final_random_intercepts)
# The ICC represents the proportion of group-level variance to total variance. 
# (para modelo emtpty: ) An ICC of 0.33 means that 33% of the variation in the outcome variable can be accounted for by the clustering stucture of the data. This provides evidence that a multilevel model may make a difference to the model estimates, in comparison with a non-multilevel model. Therefore, the use of multilevel models is necessary and warrantied.

modelo_random_intercepts <- final_random_intercepts
coef(modelo_random_intercepts)
lme4::fixef(modelo_random_intercepts)  # The estimated regression line in an average county
# The term “ﬁxed eﬀects” is used for the regression coeﬃcients that do not vary by group (such as the coeﬃcient for x in this example) or for group-level coeﬃcients or group averages (such as the average intercept, μα in (12.3)).
lme4::ranef(modelo_random_intercepts)  #  how much the intercept is shifted up or down in particular counties or county level errors
#lme4::confint(gpa_mixed)

## Modelos de control para Reviewer #######
#### Sin medios corruptos #######
control_sin_medios_corruptos <- lme4::glmer(paste(str_remove(formula_modelo_sficativas_variantes,
                                                        " mediaqualitycorruptvdem"), 
                                             "(1 | cat_pais)", sep = ""), 
                                       family=binomial("logit"), 
                                       data = democracias,
                                       control = control)
summary(final_random_intercepts)
summary(control_sin_medios_corruptos)

#### Operacionalizaciones alternativas de "institucionalizacion" ######

control_pasadolncumsum <-  lme4::glmer(paste(
                    str_replace(formula_modelo_sficativas_variantes,
                    "cumsum_pastciclos", "lncumsumpastciclos"),
                    " (1 | cat_pais)", sep = "+"), 
                                            family=binomial("logit"), 
                                            data = democracias,
                                            control = control)

control_pasadodico <-  lme4::glmer(paste(
  str_replace(formula_modelo_sficativas_variantes,
              "cumsum_pastciclos", "dico_debates_pastelection"),
  " (1 | cat_pais)", sep = "+"), 
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts)
summary(control_pasadolncumsum)
summary(control_pasadodico)

#### Con "Ronda" #####


control_ncatronda <-  lme4::glmer(paste(formula_modelo_sficativas_variantes,
                                          " ncat_ronda ", " (1 | cat_pais)", sep = "+"), 
  family=binomial("logit"), 
  data = democracias,
  control = control)

summary(final_random_intercepts)
summary(control_ncatronda)

#### Con "year" ########
control_year <-  lme4::glmer(paste(formula_modelo_sficativas_variantes,
                                        " ncat_eleccion ", " (1 | cat_pais)", sep = "+"), 
                                  family=binomial("logit"), 
                                  data = democracias,
                                  control = control)

summary(final_random_intercepts)
summary(control_year)

# MODEL FAILED TO CONVERGE 

#### Completo (todas las variables) ##########

control_all <- lme4::glmer(paste(formula_modelo_all, " (1 | cat_pais)", sep = "+"), 
                          family=binomial("logit"), 
                          data = democracias,
                          control = control)
options(scipen=999)
summary(control_all)
# 129 obs. nec persiste, regulacion desaparece

#### Modelo Interactivo ###############
control_interactivo <-  lme4::glmer(paste(formula_modelo_sficativas_variantes,
                                   "  regulaciondico*lnnec ", " (1 | cat_pais)", sep = "+"), 
                             family=binomial("logit"), 
                             data = democracias_reservanoescalada,
                             control = control)

summary(final_random_intercepts)
summary(control_interactivo)

## Exporto Modelos CONTROL ##### 
lista_controles <-  list(control_all,
                      control_interactivo,
                      control_year,
                      control_ncatronda,
                      control_pasadolncumsum,
                      control_pasadodico,
                      control_sin_medios_corruptos,
                      final_random_intercepts)

stats_to_export = exportedstats(lista_controles)

texreg::htmlreg(lista_controles,
                custom.model.names = c("control_all",
                      "control_interactivo",
                      "control_year",
                      "control_ncatronda",
                      "control_pasadolncumsum",
                      "control_pasadodico",
                      "control_sin_medios_corruptos",
                      "final_random_intercepts")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                # custom.coef.names = c("(Intercepto)",
                #                       "log Margen de victoria",
                #                       "log NEC",
                #                       "Votos oficialista",
                #                       "Presidente a reelección",
                #                       
                #                       "Debates regulados",
                #                       "Cant. elecc. pasadas c debates",
                #                       "log PBI per cápita",
                #                       "N° Democracia electoral",
                #                       "N° Corrupción de medios",
                #                       
                #                       "Alineamiento partidario",
                #                       "Prop. TV por hogar"		,
                #                       "Prop. individuos c internet",
                #                       "Prohibición propaganda"	 ,
                #                       "Acceso gratuito",
                #                       "Prop. debates en región",
                #                       "Prop. debates en USA"
                #                       
                # ),
                # reorder.coef =  c(1,
                #                   2,
                #                   3,
                #                   4,
                #                   5,
                #                   
                #                   11,
                #                   12,
                #                   13,
                #                   14,
                #                   15,
                #                   16,
                #                   17,
                #                   
                #                   6,
                #                   7,
                #                   9,
                #                   10,
                #                   8
                # ),
                include.aic = FALSE,
                include.bic = FALSE,
                include.loglik = FALSE,
                include.nobs = FALSE,
                custom.gof.rows = list("AIC" = stats_to_export$aic,
                                       "BIC" = stats_to_export$bic,
                                       "Log-verosimilitud" = stats_to_export$loglik,
                                       "n observations" = stats_to_export$n_obs),
                file="anexos/ENG_tabla_modelos_controles.html",
                caption = "<div style='text-align:left;'>
               <div style='font-weight:bold; font-size:110%; margin-bottom:4px;'>
                 Tabla XXXX Modelos de control multinivel
               </div>
               Se presentan los resultados de la serie de modelos estimados con regresión logística con efectos aleatorios por país. Los coeficientes corresponden a variables estandarizadas.  
             </div>",
                caption.above = T,
                center = T,
                bold = 0.1)

## Exporto resultados regresion multinivel #####
lista_random <-  list(contingencia_random_intercepts,
                   sistemico_random_intercepts,
                   regulatorio_random_intercepts,
                   difusion_random_intercepts,
                   final_random_intercepts)

stats_to_export = exportedstats(lista_random)

# https://www.rdocumentation.org/packages/texreg/versions/1.39.4/topics/htmlreg

texreg::htmlreg(lista_random,
                custom.model.names = c("Model #1",
                                       "Model #2",
                                       "Model #3",
                                       "Model #4",
                                       "Final Model")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercept)",
                                      "log Competitiveness",
                                      "log ENC",
                                      "Incumbent Vote Share",
                                      "Competing President",
                                      
                                      "Regulation on Debates",
                                      "Debates in Past Elections",
                                      "log GDP per Capita",
                                      "Electoral Democracy Index (VDEM)",
                                      "Media Corrupt (VDEM)",
                                      
                                      "Party Alignment",
                                      "TV Sets per Household"		,
                                      "Internet Access",
                                      "Political Advertising Prohibited"	 ,
                                      "Free Airtime Granted",
                                      "Proportion of Regional Elections with Debates",
                                      "Proportion of U.S. Elections with Debates"
                                      
                ),
                reorder.coef =  c(1,
                                  2,
                                  3,
                                  4,
                                  5,
                                  
                                  11,
                                  12,
                                  13,
                                  14,
                                  15,
                                  16,
                                  17,
                                  
                                  6,
                                  7,
                                  9,
                                  10,
                                  8
                ),
                include.aic = FALSE,
                include.bic = FALSE,
                include.loglik = FALSE,
                include.nobs = FALSE,
                custom.gof.rows = list("AIC" = stats_to_export$aic,
                                       "BIC" = stats_to_export$bic,
                                       "Log-Likelihood" = stats_to_export$loglik,
                                       "n observations" = stats_to_export$n_obs),
                caption = "<div style='text-align:left;'>
               <div style='font-weight:bold; font-size:110%; margin-bottom:4px;'>
                 Table 3. Regression results
               </div>
               Models were estimated using logistic regression with random intercepts for each country. The dependent variable is a binary outcome indicating whether a debate occurred (1) or not (0) in the election.  
             </div>",
                caption.above = T,
                center = T,
                bold = 0.1,
                file="anexos/ENG_tabla_random_intercepts.html")

## INTERPRETACION #####
#### PENDIENTE ### marginal effects graph #######

#### INTERPRETACION - importante elegir ####

# modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
# data_modelo_a_interpretar <- data_s_outliers 

# REESTIMO MODELO NO ESTANDARIZADO
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

modelo_a_interpretar <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                          "(1 | cat_pais)", sep = "+"), 
                                    family=binomial("logit"), 
                                    data = democracias_reservanoescalada,
                                    control = control)

# BUSCO DATA NO ESTANDARIZADA
data_modelo_a_interpretar <- democracias_reservanoescalada %>% 
  select(c("dico_hubo_debates",   
           "lnmarginvic",
           "nec",
           "lnnec",
           "voteshareincumbent",
           "dico_reeleccion" ,
           "propindivinternet" ,
           "accesogratuito" ,
           "avgpropdebatesregionxciclo" ,
           "regulaciondico" ,
           "cumsum_pastciclos" ,
           "lngdp",
           "democraciavdemelectoralcomp" ,
           "mediaqualitycorruptvdem",
           "cat_pais")) %>% 
  na.omit()
# vcov_modelo_a_interpretar <- vcovCL(final_random_intercepts, 
#                #cluster = democracias$elecid))
#                #cluster = data$elecid))
#                cluster = democracias$cat_pais)
  
  
# t the coefficient for X is the difference in the log odds.  https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404.


#### INTERPRETACION - ESCENARIOS CON VALORES PREDICHOS #####
### calculo de probas predichas # tengo que reducir la data para poder calcular asi nomas
#data_modelo_a_interpretar$probabilidades_predichas <- predict(modelo_a_interpretar, type = "response")
#data_modelo_a_interpretar$predicciones_binarias <- ifelse(data_modelo_a_interpretar$probabilidades_predichas>0.5,1,0)

# Crear un dataset base sobre el cual predecir 

levels <- data_modelo_a_interpretar$cat_pais %>% unique()
nlevels <- length(levels)
# version valores relevantes
valores_relevantes <- c(log(min(data_modelo_a_interpretar$nec)),
                        log(max(data_modelo_a_interpretar$nec)),
                        log(mean(data_modelo_a_interpretar$nec)),
                       log(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec)),
                       log(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec)),
                       log(1),
                       log(2),
                       log(3))

data_to_predict1 <- data.frame(
  lnnec = rep(valores_relevantes, 2*nlevels), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = 0, # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = mean(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=length(valores_relevantes)*nlevels) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
  cat_pais = levels
)

# Predecir probabilidades # ALTERNATIVAS PARA IGNORAR RANDOM EFFECTS
data_to_predict1$predicted_probs <- predict(modelo_a_interpretar, 
                                           newdata = data_to_predict1, 
                                           type = "response")

data_to_predict1 <- data_to_predict1 %>% 
  group_by(lnnec, regulaciondico) %>% 
  mutate(avg_predicted_probs = mean(predicted_probs)) %>% 
  ungroup()

preds <- marginaleffects::predictions(
  modelo_a_interpretar,
  newdata = data_to_predict1,
 re.form = NA  # Esto ignora los efectos aleatorios
)

data_to_predict1$predicted_probs2 <- preds$estimate %>% round(2)
data_to_predict1$conf.low <- preds$conf.low
data_to_predict1$conf.high <- preds$conf.high

data_to_predict1 <- data_to_predict1 %>% 
  mutate(confint = paste("(", round(conf.low,2),"-", round(conf.high,2), ")", sep = " "))

# yay! son lo msimo!

# tabla

referencias <- tibble( 
  nec = c( round(min(data_modelo_a_interpretar$nec), 2),
           round(max(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) + sd(data_modelo_a_interpretar$nec), 2),
          #  round(mean(data_modelo_a_interpretar$nec) + 2*sd(data_modelo_a_interpretar$nec), 2),
            round(mean(data_modelo_a_interpretar$nec) - sd(data_modelo_a_interpretar$nec), 2),
          1,
          2,
          3), 
  referencia = c("Min. observed value",
                 "Max. observed value",
                 "Observed mean",
                 "Observed mean + one std. dev.",
                 "Observed mean - one std. dev.",
                 "Dominant party",
                 "Bipartidism",
                 "Multipartidism")) 

 
tabla_data_to_predict <- data_to_predict1 %>% 
  mutate(nec = round(exp(lnnec),2)) %>% 
  select(nec, regulaciondico, predicted_probs2, confint) %>% 
  unique() %>% 
  pivot_wider(names_from = regulaciondico,
              values_from = c(predicted_probs2, confint)) %>% 
#  mutate(across(everything(),  ~  round(., 2))) %>%  # ya redondeado
  #mutate(nec = round(nec, 2)) %>% 
  left_join(referencias) %>% 
  arrange(nec) %>% 
  dplyr::rename("Predicted prob. with no regulations" = "predicted_probs2_0",
                "Predicted prob. with regulations" = "predicted_probs2_1",
                "Conf. Int. with no regulations" = "confint_0",
                "Conf. Int. with regulations" = "confint_1") %>% 
  select(referencia, nec, 
         "Predicted prob. with no regulations", "Conf. Int. with no regulations", 
         "Predicted prob. with regulations", "Conf. Int. with regulations")

tabla_data_to_predict %>% write.csv("anexos/ENG_tabla_data_to_predict.csv")

# yay! son casi identicas a lo estimado con modelo logit convencional !

# grafico
# comento porque preferimos el próximo gráfico
# ggplot(data_to_predict) +
#   geom_line(aes(x = exp(lnnec), y = predicted_probs, colour = as.factor(regulaciondico)))

# version con mas valores para graficar 
data_to_predict2 <- data.frame(
  lnnec = rep(seq(min(data_modelo_a_interpretar$lnnec, na.rm = TRUE), 
                    max(data_modelo_a_interpretar$lnnec, na.rm = TRUE),
                    #mean(data_modelo_sficativas$lnnec, na.rm = TRUE),
                    length.out = 20), 2*nlevels), # Cambiar por los valores que quieras probar
  lnmarginvic = mean(data_modelo_a_interpretar$lnmarginvic, na.rm = TRUE),
  voteshareincumbent = mean(data_modelo_a_interpretar$voteshareincumbent, na.rm = TRUE),
  dico_reeleccion = median(data_modelo_a_interpretar$dico_reeleccion, na.rm = TRUE), # Si es una variable dicotómica, fija en 0 o 1
  propindivinternet = mean(data_modelo_a_interpretar$propindivinternet, na.rm = TRUE),
  accesogratuito = median(data_modelo_a_interpretar$accesogratuito, na.rm = TRUE),
  avgpropdebatesregionxciclo = mean(data_modelo_a_interpretar$avgpropdebatesregionxciclo, na.rm = TRUE),
  regulaciondico = rep(c(0,1),each=20*nlevels) ,
  cumsum_pastciclos = mean(data_modelo_a_interpretar$cumsum_pastciclos, na.rm = TRUE),
  lngdp = mean(data_modelo_a_interpretar$lngdp, na.rm = TRUE),
  democraciavdemelectoralcomp = mean(data_modelo_a_interpretar$democraciavdemelectoralcomp, na.rm = TRUE),
  mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
  cat_pais = levels
  )

                
# Predecir probabilidades
# versiones viejas de calculo en test_modelos_dic2024.R 

# predicted_probs <- margins::prediction(model = modelo_a_interpretar,
#                               data = data_to_predict2,
#                               type = "response",
#                               vcov = vcov_modelo_a_interpretar,
#                               calculate_se = TRUE)

predicted_probs <- marginaleffects::predictions(
  modelo_a_interpretar,
  newdata = data_to_predict2,
  conf_level = 0.9,
  re.form = NA  # Esto ignora los efectos aleatorios
)

data_to_predict2$predicted_probs <- predicted_probs$estimate

# grafico

plot_interpretacion <- ggplot(predicted_probs) +
  geom_line(aes(x = exp(lnnec), 
                y = estimate, 
                colour = as.factor(regulaciondico))) +
  geom_ribbon(aes(x = exp(lnnec), 
                  ymin =  conf.low, 
                  ymax =  conf.high, 
                  fill = as.factor(regulaciondico)), alpha = 0.3) +
  theme_classic() +
  labs(
    title = "Probabilidad predicha de ocurrencia de un debate presidencial",
    subtitle = "Para distintos valores dentro del rango observado de NEC, con y sin regulación",
    caption = "Elaboración propia. 
    Intervalos de confianza ploteados al 90%.
    Escenarios predichos cuando el resto de las variables se encuentra:
    -en su media (para el caso de los indicadores continuos),
    -en su moda (para el caso de los indicadores dicotómicos).",
    fill = "Regulación sobre debates",
    colour = "Regulación sobre debates"
  ) +
  xlab("Número Efectivo de Candidatos") +
  ylab("Probabilidad predicha de que ocurra un debate") +
  scale_x_continuous(breaks= seq(1, 10, 0.5)) +
  scale_fill_manual(labels = c("No hay", "Hay"), breaks = c(0,1), values = c("grey40", "lawngreen")) +
  scale_colour_manual(labels = c("No hay", "Hay"), breaks = c(0,1), values = c("grey30", "limegreen")) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
  geom_vline(xintercept = c(2, 3), alpha = 0.5, linetype = 2) +
  theme(legend.position = "bottom")
 
plot_interpretacion %>% ggsave(filename = "images/ENG_plot_interpretacion_nec_regulacion.jpg",
                               width = 12,
                               height = 9)

## PENDIENTE DIAGNOSTICOS #######
###  The authors should show all diagnostic, multicollinearity and robustness test in the Appendix
