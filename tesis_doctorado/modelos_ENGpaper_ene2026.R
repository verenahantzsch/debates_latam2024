
# https://arcruz0.github.io/libroadp/logit.html#representaci%C3%B3n-visual-de-los-resultados
# primera version bastante avanzada de este codigo en test_modelos_dic2024.R
# librerias ########### 

library(tidyverse)
library(sandwich) # para clusterizar errores estandares
library(lmtest) # para clusterizar errores estandares
library(lme4)
library(patchwork)

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

variable_names <- tribble(
  ~name,                                          ~variable,
  "ENC",                                          "nec",
  "Free Airtime Granted",                         "accesogratuito",
  "Party Alignment",                              "alineamiento",
  "Proportion of Regional Elections w/Debates",   "avgpropdebatesregionxciclo",
  "Debates in Past Elections",                    "cumsum_pastciclos",
  "Electoral Democracy Index (VDEM)",             "democraciavdemelectoralcomp",
  "log GDP per Capita",                           "lngdp",
  "Closeness",                                    "marginvic",
  "log Closeness",                                "lnmarginvic",
  "Media Corrupt (VDEM)",                         "mediaqualitycorruptvdem",
  "Election Year",                                "ncat_eleccion",
  "Political Advertising Prohibited",             "prohibicionpropaganda",
  "Proportion of U.S. Elections w/ Debates",      "prop_elec_usa_ciclo",
  "Internet Access",                              "propindivinternet",
  "TV Sets per Household",                        "proptv",
  "Regulation on Debates",                        "regulaciondico",
  "Incumbent Vote Share",                         "voteshareincumbent",
  "Competing President",                          "dico_reeleccion",
  "log ENC",                                      "lnnec",
  "GDP per Capita",                               "gdpxcapita"
)

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

### mini exploracion VIS ####

skimr::skim(democracias)
# problemas de missing en propindiv internet (seguro se puede completar)
# problemas de missing en acceso gratuito (tratar de completar)
# problemas de missing en gdp (una picardia total)

#### para apendice 

vis_incluidas <- democracias_reservanoescalada %>% 
  select(prop_elec_usa_ciclo,
         avgpropdebatesregionxciclo,
         proptv,
         propindivinternet,
         alineamiento,
         prohibicionpropaganda,
         accesogratuito,
         nec,
         marginvic,
         voteshareincumbent,
         dico_reeleccion,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp#,
         #mediaqualitycorruptvdem
  )

skim_output <- skimr::skim(vis_incluidas) %>% 
  select(skim_variable, n_missing, numeric.mean, numeric.sd, 
         numeric.p0, numeric.p50, numeric.p100) %>%
  dplyr::rename(
    Variable = skim_variable,
    Missing = n_missing,
    Mean = numeric.mean,
    SD = numeric.sd,
    Min = numeric.p0,
    Median = numeric.p50,
    Max = numeric.p100
  )
skim_output %>% write_csv("./anexos/ENG2_tabla_vis.csv")

### defino funciones #########

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


stats_ml <- function(modelo, modelo_nulo, variable_dependiente){
  
  # LogLik
  ll1 <- as.numeric(logLik(modelo))
  ll0 <- as.numeric(logLik(modelo_nulo))
  n   <- nobs(modelo)
  
  # Pseudo R²
  R2_CS <- 1 - exp((2 / n) * (ll0 - ll1))
  R2_N  <- R2_CS / (1 - exp((2 / n) * ll0))
  R2_MF <- 1 - (ll1 / ll0)
  
  # Información
  AIC_v <- AIC(modelo)
  BIC_v <- BIC(modelo)
  
  # Count R2
  probabilidades_predichas <- predict(modelo, type = "response")
  predicciones_binarias <- ifelse(probabilidades_predichas>0.5,1,0)
  # Count R²: proporción de predicciones correctas
  count_r2 <- mean(predicciones_binarias == variable_dependiente)
  # Precisión base: proporción de la clase mayoritaria
  baseline_accuracy <- max(table(variable_dependiente)) / length(variable_dependiente)
  # Adjusted Count R²
  adjusted_count_r2 <- (count_r2 - baseline_accuracy) / (1 - baseline_accuracy)
  #  The adjusted count R2 is the proportion of correct guesses beyond the number that would be correctly guessed by choosing the largest marginal
  
  nombre <- deparse(substitute(modelo))
  # junto y paso como output
  tibble(
    stat = c("LogLik",
             "AIC",
             "BIC",
             "McFadden R²",
             "Cox & Snell R²",
             "Nagelkerke R²",
             "Count R²",
             "Adjusted Count R²"),
    value = c(ll1,
              AIC_v,
              BIC_v,
              R2_MF,
              R2_CS,
              R2_N,
              count_r2,
              adjusted_count_r2),
    modelo = nombre)
  
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

#### test tendencia
#Para la tendencia temporal: No basta con ver el gráfico. Corre una regresión simple (Logit o Poisson) donde la variable independiente sea el Año (como variable continua). Si el coeficiente del año es positivo y significativo ($p < 0.05$), puedes decir: "A logistic regression confirms a statistically significant positive time trend ($b = 0.XX, p < 0.01$)."


lm_timetrend <- glm(dico_hubo_debates ~ ncat_eleccion, 
                    family = binomial(link = "logit"), 
                    #data = data  # dos versiones de data, una completa, otra solo con democracias, filtrada arriba
                    data = democracias)
summary(lm_timetrend)

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
  labs(title = "Figure 1.A: Temporal Evolution of Debates",
       subtitle = "Number of Presidential Elections with and without Debates in Latin American Democracies, 1960–2023",
       y = "Count of Elections",
       x = "Decade")+#,
    #   caption = "Source: Author. 
    # The first and second rounds of presidential elections are counted separately when applicable. 
    # Only elections under democratic regimes (V-Dem polyarchy index > 0.45) are 
    # Percentages represent the number of elections with debates out of the total.
    #A logistic regression confirms a statistically significant positive time trend (b = 0.06, p < 0.0001)") +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))


plotname <- "ev_t"
filename <- paste("images/ENG2_plot_",  plotname, ".jpg", sep = "")
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

### test de significancia 

#Este test es ideal para tu variable de ocurrencia. La hipótesis nula ($H_0$) es que no hay asociación entre el país y la realización del debate.R
# Suponiendo que tu dataset se llama 'df'
# variables: 'pais' (factor) y 'debate_ocurre' (0/1)

# 1. Crear la tabla de contingencia
tabla_contingencia <- table(democracias$cat_pais, democracias$dico_hubo_debates)
# 2. Ejecutar el test de Chi-cuadrado
chi_test <- chisq.test(tabla_contingencia)
# 3. Ver resultados
print(chi_test)
# 4. Si el p-value < 0.05, hay diferencias significativas entre países.
# Para ver qué países se desvían más de lo esperado (residuos):
round(chi_test$residuals, 2)

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
  labs(title = "Figure 1.B: Geographical Distribution of Debates",
       subtitle = "Number of Presidential Elections with and without Debates in Latin American Democracies, by Country (1960–2023)",
       y = "Count of Elections",
       x = "Country",
       caption = "Source: Author’s elaboration.
       
       Percentages represent the proportion of elections in which at least one debate was held. 
       The sample is restricted to democratic regimes (V-Dem Polyarchy Index > 0.45). 
       First and second electoral rounds are treated as independent observations.
       See Appendix Table A1 for a full inventory of elections and debates per country.
       
       A logistic regression confirms a significant positive longitudinal trend (beta = 0.06, p < 0.001). 
       Cross-country variations are statistically significant at the 0.1% level (Pearson’s chi^2 = 42.28; p < 0.001).") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, size = 10),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))


plotname <- "ev_e"
filename <- paste("images/ENG2_plot_",  plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 7)



# cuentas
summary(base_elecciones_conysindebates_e$prop_elecciones_con_debates)
sd(base_elecciones_conysindebates_e$prop_elecciones_con_debates)

## panel vertical ####

panel_vertical <- plot_elecciones_conysindebates_t / plot_elecciones_conysindebates_e  +
  plot_annotation(
    title = "Figure 1. Spatial and Temporal Distribution of Presidential Debates in Latin America (1960–2023)"#,
   # tag_levels = "A"
  )
plotname <- "panelverticalte"
filename <- paste("images/ENG2_plot_",  plotname, ".jpg", sep = "")
ggsave(filename, width = 10, height = 14)

## Sample Tabla anexa para Appendix #####
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
  subset(dico_hubo_debates==0) %>% 
  mutate(ronda_year = paste(ncat_ronda, "° round ", ncat_eleccion, sep = "")) %>% 
  group_by(cat_pais) %>%
  summarise(
    elections_without_debates = paste(ronda_year, collapse = ", "),
    .groups = "drop"
  )

# join
 #
data_pais_debates_appendix <- data_pais_debates_appendix1 %>% 
  left_join(data_pais_debates_appendix2) %>% 
  left_join(countrynames %>% 
              mutate(cat_pais = ifelse(cat_pais == "Rep. Dominicana",
                                       "Republica Dominicana",
                                       cat_pais))) %>% 
  select(eng_cat_pais, elections_without_debates, elections_with_debates)

data_pais_debates_appendix %>% write_csv("anexos/ENG2_datapaisdebates.csv")

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
                              democraciavdemelectoralcomp" # +
                          # mediaqualitycorruptvdem "
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
                              democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "

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
                                    democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "

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
                                    democraciavdemelectoralcomp" # +
                          # mediaqualitycorruptvdem "

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
                                democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "
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
                                democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "
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
                         democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "

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
                         democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "

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
                           democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "


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
                           lngdp + 
                           democraciavdemelectoralcomp"# +
                           #mediaqualitycorruptvdem "


modelo_sficativas_variantes <- glm(formula_modelo_sficativas_variantes,
                                   family = binomial(link = "logit"), 
                                   #data = data 
                                   data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes)

robust_se_cluster_modelo_sficativas_variantes <- coeftest(modelo_sficativas_variantes, 
                                                          vcov = vcovCL(modelo_sficativas_variantes, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes)

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
                           prohibicionpropaganda +
                           accesogratuito +
                           avgpropdebatesregionxciclo + 
                           #prop_elec_usa_ciclo + # ES UNA CONSTANTE DADO BAJO N
                           regulaciondico +
                           cumsum_pastciclos +
                           lngdp + 
                           democraciavdemelectoralcomp"# +
                          # mediaqualitycorruptvdem "


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
#### sin log transformed ######

control_slogs <- lme4::glmer(paste(formula_modelo_sficativas, 
                                   "(1 | cat_pais)", sep = "+"), 
                             family=binomial("logit"), 
                             data = democracias,
                             control = control)
options(scipen=999)
summary(control_slogs)

#### Con medios corruptos #######
control_sin_medios_corruptos <- lme4::glmer(paste(formula_modelo_sficativas_variantes,
                                                             "+ mediaqualitycorruptvdem + (1 | cat_pais)", sep = ""), 
                                            family=binomial("logit"), 
                                            data = democracias,
                                            control = control)
summary(final_random_intercepts)
summary(control_sin_medios_corruptos)

#### Sin controles #######
control_sin_controles <- lme4::glmer(dico_hubo_debates ~ 
                                       lnmarginvic + 
                                       lnnec + 
                                       voteshareincumbent +
                                       dico_reeleccion +    
                                       propindivinternet +
                                       accesogratuito +
                                       avgpropdebatesregionxciclo +
                                       regulaciondico +
                                       cumsum_pastciclos +
                                       (1 | cat_pais), 
                                     family=binomial("logit"), 
                                     data = democracias,
                                     control = control)
summary(final_random_intercepts)
summary(control_sin_controles)
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
                                    data = democracias,
                                    control = control)

summary(final_random_intercepts)
summary(control_interactivo)

## Exporto resultados regresion multinivel #####
lista_random <-  list(difusion_random_intercepts,
                      sistemico_random_intercepts,
                      regulatorio_random_intercepts,
                      contingencia_random_intercepts,
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
                                      "H2 Proportion of Regional Elections with Debates",
                                      "H1 Proportion of U.S. Elections with Debates",
                                      
                                      "H12 Regulation on Debates",
                                      "H13 Debates in Past Elections",
                                      "log GDP per Capita",
                                      "Electoral Democracy Index (VDEM)",
                                     # "Media Corrupt (VDEM)",
                                      
                                      "H5 Party Alignment",
                                      "H3 TV Sets per Household"		,
                                      "H4 Internet Access",
                                      "H6 Political Advertising Prohibited",
                                      "H7 Free Airtime Granted",
                                      "H8 log Closeness",
                                      "H11 log ENC",
                                      "H9 Incumbent Vote Share",
                                      "H10 Competing President"
                                      
                                      
                ),
                reorder.coef =  c(1,
                                  3,
                                  2,

                                  9,
                                  10,
                                  8,

                                  11,
                                  12,

                                  13,
                                  15,
                                  16,
                                  14,

                                  4,
                                  5,
                                  6,
                                  7#,
                                  #8
                ),
                include.aic = FALSE,
                include.bic = FALSE,
                include.loglik = FALSE,
                include.nobs = FALSE,
                custom.gof.rows = list("N obs (elections)" = stats_to_export$n_obs,
                                       "AIC" = stats_to_export$aic,
                                       "BIC" = stats_to_export$bic,
                                       "Log-Likelihood" = stats_to_export$loglik),
                include.groups = TRUE, # Deja este en TRUE si quieres ver el conteo de países
                custom.gof.names = c("Number of Countries", "Variance: Country (Intercept)"),
                caption = "<div style='text-align:left;'>
               <div style='font-weight:bold; font-size:110%; margin-bottom:4px;'>
                 Table 1. Multilevel Logistic Regression Results for Presidential Debate Occurrence.
               </div>
               Models were estimated using mixed-effects logistic regression with random intercepts at the country level. <br>
               The dependent variable is a binary indicator of whether at least one presidential debate occurred during the election (1 = Yes, 0 = No). <br>
               Independent variables are standardized (z-scores) to facilitate comparison.<br>
               Observations (N) refer to individual elections.<br>
             </div>",
                caption.above = T,
                center = T,
                bold = 0.1,
                custom.note = "Standard errors are reported in parentheses. <br>
               Asterisks indicate statistical significance levels: %stars",
                
                file="anexos/ENG2_tabla_random_intercepts.html")

## INTERPRETACION #####
#### importante elegir ####

# modelo_a_interpretar <- modelo_sficativas_variantes_s_outliers
# data_modelo_a_interpretar <- data_s_outliers 

# REESTIMO MODELO NO ESTANDARIZADO
control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

modelo_a_interpretar <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                          "(1 | cat_pais)", sep = "+"), 
                                    family = binomial("logit"), 
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
           #"mediaqualitycorruptvdem",
           "cat_pais")) %>% 
  na.omit()
# vcov_modelo_a_interpretar <- vcovCL(final_random_intercepts, 
#                #cluster = democracias$elecid))
#                #cluster = data$elecid))
#                cluster = democracias$cat_pais)

levels <- data_modelo_a_interpretar$cat_pais %>% unique()
nlevels <- length(levels)
# t the coefficient for X is the difference in the log odds.  https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404.

#### Grafico de marginal effects PENDIENTE NUMERAR ########

predicted_probs <- marginaleffects::plot_predictions(
  modelo_a_interpretar,
  condition = c("lnnec", "regulaciondico"),
  draw = FALSE
)   


plot_interpretation <- ggplot(predicted_probs) +
  geom_line(aes(
    x = exp(lnnec),
    y = estimate,
    colour = as.factor(regulaciondico)
  )) +
  geom_ribbon(aes(
    x = exp(lnnec),
    ymin = conf.low,
    ymax = conf.high,
    fill = as.factor(regulaciondico)
  ), alpha = 0.3) +
  theme_classic() +
  labs(
    title = "Figure 2. Predicted Probability of a Presidential Debate Occurrence",
    subtitle = "Effects of Electoral Fragmentation (ENC) and the Presence of Debate Regulations",
    caption = paste(
      "Predicted probabilities are derived from the final multilevel logistic regression model. ",
      "Shaded areas represent 95% confidence intervals. ",
      "While the model estimates the effect of the log-transformed ENC, ", 
      "the x-axis has been back-transformed to its natural scale (units of candidates) to facilitate substantive interpretation across its observed range. ",
      "Horizontal and vertical dashed lines indicate substantive thresholds (50% probability and key ENC values) discussed in the text.",
      "All other covariates are held constant at their sample means (continuous variables) or modal values (categorical variables). ",
      sep = "\n"
    ),
    fill = "Debate regulation",
    colour = "Debate regulation"
  ) +
  xlab("Effective Number of Candidates (ENC)") +
  ylab("Predicted Probability of a Presidential Debate") +
  scale_x_continuous(breaks = seq(1, 10, 0.5)) +
  scale_fill_manual(
    labels = c("No regulation", "Regulation"),
    breaks = c(0, 1),
    values = c("grey40", "lawngreen")
  ) +
  scale_colour_manual(
    labels = c("No regulation", "Regulation"),
    breaks = c(0, 1),
    values = c("grey30", "limegreen")
  ) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
 geom_vline(xintercept = c(2, 3), alpha = 0.5, linetype = 2) +
  theme(legend.position = "bottom")


plot_interpretation %>% ggsave(filename = "images/ENG2_plot_interpretacion_nec_regulacion.jpg",
                               width = 12,
                               height = 9)

#### Escenarios (tabla) #####
### calculo de probas predichas # tengo que reducir la data para poder calcular asi nomas
#data_modelo_a_interpretar$probabilidades_predichas <- predict(modelo_a_interpretar, type = "response")
#data_modelo_a_interpretar$predicciones_binarias <- ifelse(data_modelo_a_interpretar$probabilidades_predichas>0.5,1,0)

# Crear un dataset base sobre el cual predecir 
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
 # mediaqualitycorruptvdem = mean(data_modelo_a_interpretar$mediaqualitycorruptvdem, na.rm = TRUE),
  cat_pais = levels
)

# Predecir probabilidades # ALTERNATIVAS PARA IGNORAR RANDOM EFFECTS

preds <- marginaleffects::predictions(
  modelo_a_interpretar,
  newdata = data_to_predict1,
  conf_level = 0.95,
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

tabla_data_to_predict %>% write_csv("./anexos/ENG2_tabla_data_to_predict.csv")


## CONTROLES Y DIAGNOSTICOS COPIADO DE GPT VER; VER LO ANTERIOR #######
###  The authors should show all diagnostic, multicollinearity and robustness test in the Appendix
##### Modelo estimado con robust standard errors ######
modelo_sficativas_variantes <- glm(formula_modelo_sficativas_variantes,
                                   family = binomial(link = "logit"), 
                                   #data = data 
                                   data = democracias)
options(scipen=999)
summary(modelo_sficativas_variantes)
robust_se_cluster_modelo_sficativas_variantes <- coeftest(modelo_sficativas_variantes, 
                                                          vcov = vcovCL(modelo_sficativas_variantes, 
                                                                        #cluster = democracias$elecid))
                                                                        #cluster = data$elecid))
                                                                        cluster = democracias$cat_pais))
print(robust_se_cluster_modelo_sficativas_variantes)

##### Fit statistics ######

###### defino data reducida #######
data_reducida <- democracias_reservanoescalada %>% 
  select(dico_hubo_debates,
         cat_pais,
         ncat_eleccion,
         ncat_ronda,
         elecid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
         alineamiento,
         proptv,
         propindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp,
         #mediaqualitycorruptvdem,
         lnpropindivinternet,
         lngdp,
         lnmarginvic,
         lnnec,
         lnvoteshareincumbent) %>% 
  na.omit()


data_modelo_sficativas <- democracias_reservanoescalada %>% 
  select(dico_hubo_debates,
         cat_pais,
         ncat_eleccion,
         ncat_ronda,
         elecid,
         obsid,
         marginvic,
         nec,
         voteshareincumbent,
         dico_reeleccion,
         # alineamiento,
         # proptv,
         #propindivinternet,
         # prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         gdpxcapita,
         democraciavdemelectoralcomp#,
         #mediaqualitycorruptvdem
         ) %>% 
  na.omit() %>% 
  left_join(democracias_reservanoescalada)


###### reestimo modelos con data reducida #####

modelo_0_semireducido <- lme4::glmer(dico_hubo_debates ~ 
                                       1 + (1 | cat_pais), 
                                     data = data_modelo_sficativas)

modelo_0_reducido <- lme4::glmer(dico_hubo_debates ~ 
                                   1 + (1 | cat_pais), 
                                 data = data_reducida)

contingencia_reducido <- lme4::glmer(paste(formula_modelo_contingencia_bis, 
                                           "(1 | cat_pais)", sep = "+"), 
                                     family=binomial("logit"), 
                                     data = data_reducida,
                                     # weights = NULL,
                                     control = control)

sistemico_reducido <- lme4::glmer(paste(formula_modelo_sistemico_bis, 
                                        "(1 | cat_pais)", sep = "+"), 
                                  family=binomial("logit"), 
                                  data = data_reducida,
                                  control = control)

regulatorio_reducido <- lme4::glmer(paste(formula_modelo_regulatorio_bis, 
                                          "(1 | cat_pais)", sep = "+"), 
                                    family=binomial("logit"), 
                                    data = data_reducida,
                                    control = control)

difusion_reducido <- lme4::glmer(paste(formula_modelo_difusion_bis, 
                                       "(1 | cat_pais)", sep = "+"), 
                                 family=binomial("logit"), 
                                 data = data_reducida,
                                 control = control)

final_reducido <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                    "(1 | cat_pais)", sep = "+"), 
                              family=binomial("logit"), 
                              data = data_reducida,
                              control = control)

final_noescalado <- lme4::glmer(paste(formula_modelo_sficativas_variantes, 
                                      "(1 | cat_pais)", sep = "+"), 
                                family=binomial("logit"), 
                                data = data_modelo_sficativas,
                                control = control)

all_reducido <- lme4::glmer(paste(formula_modelo_all, " (1 | cat_pais)", sep = "+"), 
                            family=binomial("logit"), 
                            data = data_reducida,
                            control = control)

###### creo y comparo medidas de ajuste ########
#Repasando: 
#LogLik: menos negativo, mejor (mas cerca del 0, mejor) 
#AIC y BIC: mas chicos, mejor 
#Distintos psuedoR2: mas grandes mejor, pero tampoco esperar muy grande (0.2 a 0.4 ya estaria bien)

#stats0 <- stats( modelo_0_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats4 <- stats_ml(contingencia_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats2 <- stats_ml(sistemico_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats3 <- stats_ml(regulatorio_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats1 <- stats_ml(difusion_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
#stats5 <- stats_ml(sficativas_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats6 <- stats_ml(final_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)
stats7 <- stats_ml(all_reducido, modelo_0_reducido, data_reducida$dico_hubo_debates)


all_stats <- rbind(#stats0,
  stats1,
  stats2,
  stats3,
  stats4,
  #stats5,
  stats6,
  stats7)

all_stats <- all_stats %>% 
  pivot_wider(names_from = modelo, values_from = value) %>% write_csv("anexos/ENG2_fitstats.csv")


##### test de Wald ####
# Esto testea cada efecto fijo usando estadísticos Wald χ²
wald_test <- car::Anova(final_random_intercepts, type = "II")  
# wald global
wald_global <- car::linearHypothesis(
  final_random_intercepts,
  names(lme4::fixef(final_random_intercepts))[-1]  # excluye el intercept
)
#LRT (modelo nulo vs final)
lrt_nulo <- anova(modelo_0_semireducido, final_noescalado, test = "Chisq")
nobs(modelo_0_semireducido)
nobs(final_noescalado)



##### Residuos - inspeccion de residuos ########

####### preparo calculos
# Residuos
res_dev <- residuals(final_random_intercepts, type = "deviance")
res_pearson <- residuals(final_random_intercepts, type = "pearson")

# Leverage (aprox; funciona razonablemente bien en glmer)
hat_vals <- hatvalues(final_random_intercepts)

# Pearson estandarizados
res_pearson_std <- res_pearson / sqrt(1 - hat_vals)

# Valores ajustados (en escala del link: log-odds)
fitted_vals <- fitted(final_random_intercepts)

# Índice de observación
obs_index <- seq_along(res_dev)

# IDs
obs_id <- data_modelo_sficativas$obsid

# Umbral clásico p def outliers
thr <- 2

# id outliers
outliers_dev <- which(abs(res_dev) > thr)
outliers_pearson <- which(abs(res_pearson_std) > thr)

outliers_all <- sort(unique(c(outliers_dev, outliers_pearson)))

######## primeros graficos: resiudos vs indice

jpeg("images/ENG2_plotresiduos1.jpg", width = 800, height = 600, quality = 90)

par(mfrow = c(1, 2),
    mar = c(4, 4, 3, 1),   # márgenes internos
    oma = c(6, 0, 0, 0) )   # márgenes externos (bottom, left, top, right))

# Deviance vs índice
plot(obs_index, res_dev,
     ylab = "Deviance residuals",
     xlab = "Observation index",
     # main = "(a) Deviance residuals vs. observation index",
     pch = 19, cex = 0.6)
mtext("(a) Deviance residuals vs. observation index", 
      side = 3, line = 0.5, adj = 0, cex = 0.9, font = 1)
abline(h = c(-2, 0, 2), col = c("red3", "gray40", "red3"), lty = c(2, 1, 2))

text(obs_index[outliers_dev],
     res_dev[outliers_dev],
     labels = obs_id[outliers_dev],
     pos = 4, cex = 0.7, col = "blue4")

# Pearson estandarizado vs índice
plot(obs_index, res_pearson_std,
     ylab = "Standardized Pearson residuals",
     xlab = "Observation index",
     #main = "(b) Standardized Pearson residuals vs. observation index",
     pch = 19, cex = 0.6)
mtext("(b) Standardized Pearson residuals vs. observation index", 
      side = 3, line = 0.5, adj = 0, cex = 0.9, font = 1)
abline(h = c(-2, 0, 2), col = c("red3", "gray40", "red3"), lty = c(2, 1, 2))

text(obs_index[outliers_pearson],
     res_pearson_std[outliers_pearson],
     labels = obs_id[outliers_pearson],
     pos = 4, cex = 0.7, col = "blue4")

#side: on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
#TITULO
mtext("Figure X. Residual diagnostics for the final multilevel logistic regression model", 
      font = 2, side = 3, cex = 1.2, line = -1.2, outer = TRUE )
#CAPTION
mtext("
(a) Deviance residuals plotted against observation index. 
(b) Standardized Pearson residuals plotted against observation index. 
 Dashed lines indicate conventional thresholds for large residuals. 
 A small number of isolated observations exhibit large residuals, but no systematic patterns indicative of model misspecification are observed. ", 
      side = 1, line = 4, cex = 0.75, outer = TRUE, adj = 1, font = 3 )

dev.off()

######## segundos graficos: resiudos vs fitted values

jpeg("images/ENG2_plotresiduos2.jpg", width = 800, height = 600, quality = 90)

par(mfrow = c(1, 2),
    mar = c(4, 4, 3, 1),   # márgenes internos
    oma = c(6, 0, 0, 0) )   # márgenes externos (bottom, left, top, right))


# Deviance vs fitted
plot(fitted_vals, res_dev,
     ylab = "Deviance residuals",
     xlab = "Fitted values (log-odds)",
     #main = "Deviance residuals vs. fitted values",
     pch = 19, cex = 0.6)
mtext("Figure A1.A Deviance residuals vs. fitted values", 
      side = 3, line = 0.5, adj = 0, cex = 0.9, font = 1)
abline(h = c(-2, 0, 2), col = c("red3", "gray40", "red3"), lty = c(2, 1, 2))

text(fitted_vals[outliers_dev],
     res_dev[outliers_dev],
     labels = obs_id[outliers_dev],
     pos = 4, cex = 0.7, col = "blue4")

# Pearson estandarizado vs fitted
plot(fitted_vals, res_pearson_std,
     ylab = "Standardized Pearson residuals",
     xlab = "Fitted values (log-odds)",
     #main = "Standardized Pearson residuals vs. fitted values",
     pch = 19, cex = 0.6)
mtext("Figure A1.B Standardized Pearson residuals vs. fitted values", 
      side = 3, line = 0.5, adj = 0, cex = 0.9, font = 1)
abline(h = c(-2, 0, 2), col = c("red3", "gray40", "red3"), lty = c(2, 1, 2))

text(fitted_vals[outliers_pearson],
     res_pearson_std[outliers_pearson],
     labels = obs_id[outliers_pearson],
     pos = 4, cex = 0.7, col = "blue4")

#side: on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
#TITULO
mtext("Figure A1. Residual diagnostics for the final multilevel logistic regression model", 
      font = 2, side = 3, cex = 1.2, line = -1.2, outer = TRUE )
#CAPTION
mtext("
Figure A1.A Deviance residuals plotted against fitted values.
Figure A1.B Standardized Pearson residuals plotted against fitted values.
Dashed horizontal lines indicate ±2 standardized residuals (the conventional threshold for large residuals).
Individual observations exceeding this threshold are labeled for transparency.
A small number of isolated observations exhibit large residuals; however, no systematic patterns indicative of model misspecification are observed.", 
      side = 1, line = 4, cex = 0.75, outer = TRUE, adj = 1, font = 3 )

dev.off()
# vuelvo al default
par(mfrow = c(1, 1),
    mar = c(4, 4, 3, 3),   # márgenes internos
    oma = c(0, 0, 0, 0) )

#### Influential observations ######

infl <- influence.ME::influence(final_random_intercepts, group = "cat_pais")

###### Cook's distance #########
cd <- cooks.distance(infl)
J <- nrow(cd) 
umbralcd <- 4 / J

jpeg("images/ENG2_plotcook.jpg", width = 800, height = 600, quality = 90)

par(mfrow = c(1, 1),
    mar = c(4, 4, 3, 3),   # márgenes internos
    oma = c(7, 0, 0, 0) ) # (bottom, left, top, right))

plot(cd,
     ylab = "Cook's distance",
     xlab = "Country (cluster)",
     main = "Cluster-level Cook's distance")
abline(h = umbralcd, lty = 2, col = "red4")

# Etiquetar SOLO los influyentes
idx <- which(cd > umbralcd)

text(idx,
     cd[idx],
     labels = rownames(cd)[idx],
     pos = 2,
     cex = 0.8,
     col = "blue4")

#CAPTION
mtext("
The plot shows the global influence of each country-level cluster on the model parameters. 
      While Chile and Uruguay slightly exceed the heuristic threshold (4/J), their influence remains moderate (D < 0.5). 
      Sensitivity analyses confirm that excluding these clusters does not substantively alter the direction or significance 
      of the main predictors, ensuring the robustness of the results (see Table XX).", 
      side = 1, line = 5, cex = 0.75, outer = TRUE, adj = 1, font = 3 )
#Figure X. Cluster-level DFBETAs for the final multilevel logistic regression model.

dev.off()

######  DFBETAs por cluster #####

dfb <- dfbetas(infl)
umbral <- 1
# Resumen rápido
summary(dfb)

# Grafico
jpeg("images/ENG2_plotdfbetas.jpg", width = 800, height = 600, quality = 90)

par(mfrow = c(1, 1),
    mar = c(5, 4, 3, 3),   # márgenes internos
    oma = c(9, 0, 0, 0) ) # (bottom, left, top, right))

colnames(dfb) <- c("(Intercept)"  ,
                   "log Closeness",     
                   "log ENC",     
                   "Incumbent Vote Share",
                   "Competing President",
                   "Internet Access",
                   "Free Airtime Granted",
                   "Regional Elections w/Debates", 
                   "Regulation on Debates",
                   "Debates in Past Elections",
                   "log GDP per Capita",
                   "Electoral Democracy Index"#,
                   #"Media Corrupt"
                   )

boxplot(dfb,
        las = 2,
        main = "Cluster-level DFBETAs",
        ylab = "DFBETA",
        cex.axis = 0.7  ) # ↓ achica labels eje x)
abline(h = c(-1, 1), lty = 2, col = "red3")


# Etiquetar SOLO por debajo/arribaumbral
idx <- which(abs(dfb) > umbral, arr.ind = TRUE)

text(
  x = idx[, "col"],           # coeficiente
  y = dfb[idx] + rnorm(nrow(idx), 0, 0.05),                 # valor DFBETA
  labels = rownames(dfb)[idx[, "row"]],  # país
  pos = 3,
  cex = 0.8,
  col = "blue4"
)

#CAPTION
mtext("
Distributions of DFBETAs across country clusters for each fixed-effect coefficient. 
Dashed lines indicate the |1| threshold for influential cases. 
The high concentration of values near zero and the fact that only 1.5% of country-parameter pairs 
(Chile, Peru, and Ecuador in specific controls) 
exceed the threshold confirm the robustness of the estimates. 
The direction and significance of the primary substantive predictors remain stable across all clusters. 
 ", 
      side = 1, line = 8.5, cex = 0.75, outer = TRUE, adj = 1, font = 3 )
#Figure X. Cluster-level DFBETAs for the final multilevel logistic regression model.
dev.off()

######  Panel Vertical Cook/DFBETAS #####
jpeg("images/ENG2_influence_panel.jpg", width = 900, height = 1200, quality = 95)

par(
  mfrow = c(2, 1),        # PANEL VERTICAL
  mar = c(5, 4, 3, 3),   # márgenes internos de cada plot
  oma = c(12, 0, 4, 0)   # márgenes externos (caption + título general)
)

###### (a) Cook's distance 

cd <- cooks.distance(infl)
J <- nrow(cd)
umbralcd <- 4 / J

plot(
  cd,
  ylab = "Cook's distance",
  xlab = "Country (cluster)",
  main = "Figure A2.A Cluster-level Cook's distance"
)
abline(h = umbralcd, lty = 2, col = "red4")

idx_cd <- which(cd > umbralcd)
text(
  idx_cd,
  cd[idx_cd],
  labels = rownames(cd)[idx_cd],
  pos = 2,
  cex = 0.8,
  col = "blue4"
)

###### (b) DFBETAs by cluster 

dfb <- dfbetas(infl)
umbral <- 1

colnames(dfb) <- c(
  "(Intercept)",
  "log Closeness",
  "log ENC",
  "Incumbent Vote Share",
  "Competing President",
  "Internet Access",
  "Free Airtime Granted",
  "Regional Elections w/Debates",
  "Regulation on Debates",
  "Debates in Past Elections",
  "log GDP per Capita",
  "Electoral Democracy Index"
)

boxplot(
  dfb,
  las = 2,
  main = "Figure A2.B Cluster-level DFBETAs",
  ylab = "DFBETA",
  cex.axis = 0.7
)
abline(h = c(-1, 1), lty = 2, col = "red3")

idx_dfb <- which(abs(dfb) > umbral, arr.ind = TRUE)

text(
  x = idx_dfb[, "col"],
  y = dfb[idx_dfb] + rnorm(nrow(idx_dfb), 0, 0.05),
  labels = rownames(dfb)[idx_dfb[, "row"]],
  pos = 3,
  cex = 0.75,
  col = "blue4"
)

###### CAPTION GENERAL (para toda la figura)
mtext(
  "  Figure A2. Cluster-level influence diagnostics for the final multilevel logistic regression model.", 
  side = 3, line = 1, cex = 1.6, outer = TRUE, adj = 0, font = 2 )
mtext(
  "Figure A2.A reports Cook’s distance by country cluster (J = 17). 
  While Chile slightly exceeds the heuristic threshold (4/J), its absolute influence remains moderate (D < 0.5). 
  Figure A2.B displays the distribution of DFBETAs across clusters for each fixed-effect coefficient; dashed horizontal lines indicate the conventional |1| threshold. 
  Only a small fraction of country–parameter pairs exceed this limit. 
  A sensitivity analysis excluding Chile confirms that the main findings remain robust, with no changes in the direction or statistical significance of the primary predictors.", 
  side = 1, line = 8.5, cex = 1, outer = TRUE, adj = 1, font = 3 )
 

dev.off()
#### Multicolinealidad ######### 
##### Correlation matrix ########## 

corr_modelo_a_probar <- data_modelo_sficativas %>%
  select(-cat_pais,
         -elecid,
         -obsid) %>% 
  select(ncat_eleccion,
         #  marginvic,
         lnmarginvic,
         # nec,
         lnnec,
         voteshareincumbent,
         # lnvoteshareincumbent,
         dico_reeleccion,
         alineamiento,
         proptv,
         propindivinternet,
         # lnpropindivinternet,
         prohibicionpropaganda,
         accesogratuito,
         avgpropdebatesregionxciclo,
         prop_elec_usa_ciclo,
         regulaciondico,
         cumsum_pastciclos,
         lngdp,
         # gdpxcapita,
         democraciavdemelectoralcomp,
         #mediaqualitycorruptvdem
         ) %>% 
  dplyr::rename( "Free Airtime Granted" = accesogratuito,
                 "Party Alignment" = alineamiento,
                 "Proportion of Regional Elections w/Debates" = avgpropdebatesregionxciclo ,
                 "Debates in Past Elections" = cumsum_pastciclos ,
                 "Electoral Democracy Index (VDEM)" = democraciavdemelectoralcomp ,
                 "log GDP per Capita" = lngdp ,
                 #"Competitiveness" = marginvic, 
                 "log Closeness" = lnmarginvic, 
                # "Media Corrupt (VDEM)" = mediaqualitycorruptvdem ,
                 "Election Year" = ncat_eleccion,
                 "Political Advertising Prohibited" = prohibicionpropaganda ,
                 "Proportion of U.S. Elections w/ Debates" =  prop_elec_usa_ciclo ,
                 "Internet Access" = propindivinternet ,
                 "TV Sets per Household" = proptv ,
                 "Regulation on Debates" = regulaciondico,
                 "Incumbent Vote Share" = voteshareincumbent,
                 "Competing President" =  dico_reeleccion ,
                 "log ENC" = lnnec)


summary(corr_modelo_a_probar)
correlation_matrix <- cor(corr_modelo_a_probar , use = "pairwise.complete.obs" )

# Generar el gráfico con resaltado


# Generar el gráfico con coordenadas capturadas
corr_plot <- corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("red", "white", "green"))(8), 
  diag = FALSE, 
  plot = NULL # Capturar coordenadas
)
# Crear el gráfico de correlación (parte inferior)
n <- ncol(correlation_matrix) # Número de columnas/filas

# Graficar la matriz de correlación
jpeg("images/ENG2_plotcorrs.jpg", width = 800, height = 600, quality = 90)

par(mfrow = c(1, 1), 
    mar = c(1, 1, 1, 1),   # márgenes internos
    oma = c(0, 0, 0, 0) )

corrplot::corrplot(
  correlation_matrix, 
  method = "circle",
  col = colorRampPalette(c("red", "white", "green"))(8), 
  type = "lower",   # Muestra solo la parte inferior
  addgrid.col = "gray90",
  diag = FALSE  ,    # Ocultar diagonal  ,
  tl.col = "gray10",
  tl.cex = 0.8
  # na.label.col = "gray80"
)

# Resaltar valores absolutos mayores a 0.25 con * y mayores a 0.5 con **
for (j in 1:n) {
  for (i in 1:n) {
    print(paste(i,j))
    if (i > j) { # Solo trabaja en el triángulo inferior (evita asteriscos en la parte oculta)
      if(is.na(correlation_matrix[i, j])){
        text(j, n - i + 1, "", col = "black", cex = 1.5)
      }
      else{
        # Resaltar valores entre 0.25 y 0.5
        if (abs(correlation_matrix[i, j]) > 0.25 && abs(correlation_matrix[i, j]) <= 0.5) {
          text(j, n - i + 1, round(correlation_matrix[i, j],1), col = "black", cex = 0.5)
        }
        #Resaltar valores mayores a 0.5
        if (abs(correlation_matrix[i, j]) > 0.5) {
          text(j, n - i + 1, round(correlation_matrix[i, j],1), col = "black", cex = 0.7)
        }
      }
    }
  }
}

#side: on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
mtext("Figure X. Correlation matrix among predictors", font = 2, side = 3, cex = 1.2, line = -2, outer = F )
dev.off()
##### VIFs Variance Inflation Factors ######

# vif de modelo principal
vifs <- performance::check_collinearity(final_random_intercepts) %>% 
  tibble() %>% 
  select(Term, VIF) %>% 
  mutate(Modelo = "Final")

# solo para cheq interno: vifs de otros modelos
vifs_completo <-  performance::check_collinearity(control_all) %>% 
  tibble() %>% 
  select(Term, VIF)  %>% 
  mutate(Modelo = "All")

vifs_scorrupt <-  performance::check_collinearity(control_sin_medios_corruptos) %>% 
  tibble() %>% 
  select(Term, VIF)

vifs_conyear <-  performance::check_collinearity(control_year) %>% 
  tibble() %>% 
  select(Term, VIF)  %>% 
  mutate(Modelo = "w/year")

vifs_conronda <-  performance::check_collinearity(control_ncatronda) %>% 
  tibble() %>% 
  select(Term, VIF) %>% 
  mutate(Modelo = "w/ronda")

vis_sinlog <- performance::check_collinearity(control_slogs) %>% 
  tibble() %>% 
  select(Term, VIF)  %>% 
  mutate(Modelo = "Non-log")

vis_sincontroles <- performance::check_collinearity(control_sin_controles) %>% 
  tibble() %>% 
  select(Term, VIF)  %>% 
  mutate(Modelo = "Sin controles")

all_vifs <- rbind(
  vis_sinlog,
  #vifs_conronda,
  #vifs_completo,
  #vifs_conyear,
  vifs) %>% 
  pivot_wider(names_from = Modelo, values_from = VIF) %>% 
  left_join(variable_names, by = c("Term" = "variable"))

all_vifs %>% write_csv("./anexos/ENG2_vifs.csv")

#### Excluding influential observations ######

clusters_influyentes <- rownames(cd)[cd > umbralcd]

data_sin_infl <-  data_modelo_sficativas %>% 
  subset(!cat_pais %in% clusters_influyentes)


control_sin_infl <- glmer(
  formula(modelo_a_interpretar),
  data = data_sin_infl,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(control_sin_infl)


## CONTROL PARA MI ##### 
lista_controles <-  list(control_all,
                         control_interactivo,
                         control_year,
                         control_ncatronda,
                         control_pasadolncumsum,
                         control_pasadodico,
                         control_sin_medios_corruptos,
                         control_sin_controles,
                         control_sin_infl,
                         final_random_intercepts)

stats_to_export = exportedstats(lista_controles)

texreg::htmlreg(lista_controles,
                custom.model.names = c("control_all",
                                       "control_interactivo",
                                       "control_year",
                                       "control_ncatronda",
                                       "control_pasadolncumsum",
                                       "control_pasadodico",
                                       "control_con_medios_corruptos",
                                       "control_sin_controles",
                                       "control_sin_infl",
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
                custom.gof.rows = list("n obs (elections)" = stats_to_export$n_obs,
                                       "AIC" = stats_to_export$aic,
                                       "BIC" = stats_to_export$bic,
                                       "Log-Likelihood" = stats_to_export$loglik),
                file="anexos/ENG2_tabla_modelos_controles.html",
                caption = "<div style='text-align:left;'>
               <div style='font-weight:bold; font-size:110%; margin-bottom:4px;'>
                 Tabla XXXX Modelos de control multinivel
               </div>
               Se presentan los resultados de la serie de modelos estimados con regresión logística con efectos aleatorios por país. Los coeficientes corresponden a variables estandarizadas.  
             </div>",
                caption.above = T,
                center = T,
                bold = 0.1)

## Exporto Modelos CONTROL PARA PAPER ##### 
lista_controles <-  list(control_all,
                         robust_se_cluster_modelo_sficativas_variantes,
                         control_year,
                         control_ncatronda,
                         control_interactivo,
                         control_sin_infl,
                         control_sin_controles)

stats_to_export = exportedstats(lista_controles)

texreg::htmlreg(lista_controles,
                custom.model.names = c("Full Model", #control_all",
                                       "Fixed Effects",##robust_se_cluster_modelo_sficativas_variantes",
                                       
                                       "W/time trends",
                                       "W/round",

                                      "W/interaction",
                                       "Sensitivity analysis",
                                      
                                      "Without controls")  ,
                stars = c(0.001, 0.01, 0.05, 0.1),
                custom.coef.names = c("(Intercept)",
                                      "H8 log Closeness",
                                      "H11 log ENC",
                                      "H9 Incumbent Vote Share",
                                      "H10 Competing President",

                                      "H5 Party Alignment",
                                      "H3 TV Sets per Household"		,
                                      "H4 Internet Access",

                                      "H6 Political Advertising Prohibited",
                                      "H7 Free Airtime Granted",

                                      "H2 Proportion of Regional Elections with Debates",
                                      #"H1 Proportion of U.S. Elections with Debates", # CONSTANTE EN EL FULL QUE ES EL UNICO QUE LO INCLUYE

                                      "H12 Regulation on Debates",
                                      "H13 Debates in Past Elections",
                                      "log GDP per Capita",
                                      "Electoral Democracy Index (VDEM)",
                                      # "Media Corrupt (VDEM)",
                                      
                                      "Election Round",
                                      "Year",
                                      "log ENC x Regulation on Debates"

                ),
                reorder.coef =  c(1,
                                  
                                  16,
                                  17,
                                  
                                  11,
                                  
                                  
                                  7,
                                  8,
                                  6,
                                  
                                  9,
                                  10,
                                  
                                  2,
                                  4,
                                  5,
                                  3,
                                  
                                  12,
                                  
                                  18,
                                  
                                  13,
                                  14,
                                  15
                ),
                include.aic = FALSE,
                include.bic = FALSE,
                include.loglik = FALSE,
                include.nobs = FALSE,
                custom.gof.rows = list("N obs (elections)" = stats_to_export$n_obs,
                                       "AIC" = stats_to_export$aic,
                                       "BIC" = stats_to_export$bic,
                                       "Log-Likelihood" = stats_to_export$loglik),
                include.groups = TRUE, # Deja este en TRUE si quieres ver el conteo de países
                custom.gof.names = c("Number of Countries", "Variance: Country (Intercept)"),
                file="anexos/ENG2_tabla_modelos_controles2.html",
                caption = "<div style='text-align:left;'>
               <div style='font-weight:bold; font-size:110%; margin-bottom:4px;'>
                Table A6. Alternative Model Specifications and Sensitivity Analyses.
  </div>
  The table presents results for alternative model specifications to assess the robustness of the main findings: <br>
  <br>
  <b>Full Model:</b> Includes all considered predictors, with the exception of the <i>Proportion of U.S. Elections with Debates</i>, which was dropped due to lack of variation following listwise deletion. This model exhibited multicollinearity issues and failed to converge; results are provided for transparency.<br>
  <b>Fixed Effects:</b> The final specification estimated via logistic regression with clustered standard errors at the country level.<br>
  <b>W/Time Trends:</b> Includes a control for linear time trends. This model faced multicollinearity issues as several predictors share similar positive temporal trends, complicating the isolation of individual effects.<br>
  <b>W/Round:</b> Includes a control for the election round. The main specification without this control is theoretically preferred, as the effect of the round is expected to be mediated by Electoral Closeness and ENC.<br>
  <b>W/Interaction:</b> Includes an interaction term between <i>log ENC</i> and <i>Regulation on Debates</i>.<br>
  <b>Sensitivity Analysis:</b> Excludes Chile, identified as a moderately influential case in diagnostic plots.<br>
  <b>Without Controls:</b> Excludes development (GDP) and democracy (V-Dem) indicators to retain Venezuela in the sample (J = 18).<br>
  <br>
  With the exception of the Fixed Effects specification, all models were estimated using multilevel logistic regression. The dependent variable is a binary indicator of whether at least one televised presidential debate occurred (1 = Yes, 0 = No).
</div>

             </div>",
                caption.above = T,
                center = T,
                bold = 0.1,
                custom.note = " Independent variables are standardized (z-scores) to facilitate comparison.<br>
               Observations (N) refer to individual elections.<br>
               Standard errors are reported in parentheses. <br>
               Asterisks indicate statistical significance levels: %stars")

##### otros #########
cor(democracias$ncat_ronda, democracias$nec)