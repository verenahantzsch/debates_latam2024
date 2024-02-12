# LIBRERIAS #####
library(tidyverse)


# DATOS PROPIOS #####
# mi base
data_base_años <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/base_debates_limpia.xlsx", sheet = "base_años")

## CARGA DE DATOS Q OF G ####
data_qog <- read.csv("C:/Users/carof/Documents/INVESTIGACION y BECAS/Datos e info/q of g/qog_std_ts_jan21.csv") 


### FILTRO 1 LATAM ######

data_qog_latam <- data_qog %>% 
  subset(ht_region=="2") # filtramos latam 

data_qog_latam %>% 
  write.csv("data_qog_latam.csv")

### FILTRO 2 POTENCIALES VARIABLES INTERES  ###### 
data_qog_latam_selection <- data_qog_latam %>% 
  select(cname, ccode, cname_year, year, version,
          gcb_pmedia,  aii_aio,   bti_aar,
          bti_ci, bti_csp, bti_ig , bti_pp, bti_psi, bti_sc, bti_seb ,
          eu_demd2jant, eu_demd3dens, eu_isiucpp, eu_isiunet, eu_isiusell, eu_isiux,
          fe_etfra, gpi_gpi, une_cinexp, une_screen,
          wdi_popurb,   wel_citrig, wel_sma, wvs_pmi12, 
          gd_ptsa, gd_ptsh, gd_ptss, nelda_rpae, nelda_vcdbe, svs_ind,
          ucdp_type3, bl_asymf, iiag_edu, iiag_hd, undp_hdi, 
          wdi_litrad,   wdi_internet, wdi_mobile, 
          bmr_demdur, lp_legor, 
          aii_q19, aii_q48 , aii_q50, aii_q51,
          ffp_hr, iaep_const, wbgi_rle, wbgi_rln, wbgi_rls, 
           wel_rli, wel_rol, 
          aii_q23, aii_q52 , aii_q53, aii_q54, aii_q55, aii_q58, aii_q59, 
          bti_foe, egov_egov, egov_epar, egov_hci, nelda_mbbe, 
          rsf_pfi , sgi_qdai, vdem_mecorrpt, wvs_confpr, wvs_conftv,
          oecd_migforpop_t1b,
          aii_q48, aii_q49, aii_q50, aii_q51,  bti_ps,
          cam_contest, cam_inclusive, 
          cpds_chg, cpds_enps, cpds_enpv, cpds_frel, cpds_tg, cpds_vt, 
          cses_pc, dev_altv1, dev_othv1, dev_regv1 , dev_tv1, 
          ess_trparl, ess_trpart, ess_trpolit,
          gol_dist, gol_enep, gol_enep1, 
          gol_enepo , gol_enpres, gol_est, gol_est_spec, gol_inst, gol_pest, gol_pr,
         gol_preel, gol_upseat, gol_uptier, gtm_pr,
          h_alignl1, h_alignl1l2, h_alignl2, 
          iaep_ee, iaep_ese, iaep_ise, iaep_nee, iaep_pnec, iaep_pveec, 
          iaep_snec, ideaesd_esf, ideaesd_esp, 
          ideavt_prescv, ideavt_presvt, iiag_par, jw_multiround, jw_oneparty,
          nelda_fme,
          nelda_mbbe,
          nelda_mtop,
         nelda_noe, 
         nelda_noea,
         nelda_noee,
         nelda_rpae,
         nelda_vcdbe,
         no_ef,
         pt_maj,
         sgi_qdep,
         van_comp,
         van_index,
         van_part,
         wvs_confpp,
         aii_q41,
         aii_q43,
         aii_q56, aii_q57,
         bmr_dem,
         bmr_dembr,
         bmr_demdur,
         bmr_demmis,
         bmr_demtran, bnr_dem, bti_aod, 
         bti_ic, chga_demo, chga_hinst, cses_sd,
         diat_ati, diat_iti, gwf_fail, gwf_failsub,
          gwf_failtype, gwf_failviolent, gwf_regimetype, ht_regtype, ht_regtype1, kun_wiqrpol_full, nelda_fme, no_ce, no_ufs, p_durable,
        p_polity2, sgi_qd, sgi_qdai, sgi_qdcr, sgi_qdep, 
         sgi_qdrl, vdem_polyarchy, wel_edi, 
         wel_regtype, wel_sys, oecd_fdindex_t1g,oecd_fdindex_t1h,
                         wdi_gdpcapcon2010, wdi_gdpcapcur, wdi_gini
         
         )

#### CODEBOOK SHORTCUT #####
# # algunas variables para ir filtrando 
# Corruption Perception: Media (gcb_pmedia)
# access to Information and Openness sub-index (aii_aio)
# Ethnic Fractionalization in the year 2000 (al_ethnic2000) 98
# Language Fractionalization in the year 2000 (al_language2000)
# Associational/Assembly Rights (bti_aar) 125
# Conict Intensity (bti_ci) 127
# Civil Society Participation (bti_csp)
# Interest Groups (bti_ig) 132
# Political Participation (bti_pp) 135
# Political and Social Integration (bti_psi) 136
# Social Capital (bti_sc) 137
# Socio-Economic Barriers (bti_seb)
# Population at 1st January, total (eu_demd2jant) 198
# Population density, average population per square km (eu_demd3dens)
# Internet use: civic or political participation (eu_isiucpp) 228
# Internet use: participating in social networks (eu_isiunet) 229
# Internet use: selling goods or services (eu_isiusell) 229
# Internet use: never (eu_isiux) 229
# Ethnic Fractionalization (fe_etfra) 268
# Global Peace Index (1-5 Less peaceful) (gpi_gpi) 313
# Cinema expenditure per capita (une_cinexp) 517
# Screen per capita (per 100,000 inhabitants) (une_screen) 522
# Distribution of the national population into urban regions (oecd_popgeo_g3a) 443
# Distribution of the national population into intermediate regions (oecd_popgeo_g3b) 443
# Distribution of the national population into rural regions (oecd_popgeo_g3c) 443
# Urban population % of total population (wdi_popurb) 609
# E-Participation Index . 0-1 best (wef_epi) 629
# Citizen Rights (wel_citrig) 639
# Social Movement Activity (wel_sma) 645
# Post-Materialist index 12-item (wvs_pmi12) 669
# Political Terror Scale - Amnesty International (gd_ptsa) 293
# Political Terror Scale - Human Rights Watch (gd_ptsh) 294
# Political Terror Scale - US State Department (gd_ptss) 294
# Riots and Protests after Election (nelda_rpae) 403
# Violence and Civilian Deaths before Election (nelda_vcdbe) 403
# Societal Violence Scale Index 1-5 (svs_ind) 502
# Internal armed conict (ucdp_type3) 513
# Average Schooling Years, Female and Male (bl_asymf)
# Education (iiag_edu) 366
# Human Development (iiag_hd) 367
# Human Development Index (undp_hdi) 516
# Literacy rate, adult total  % of people ages 15 and above  (wdi_litrad) 598
# Digital skills among active population. 1-7  best  (wef_dsap) 627
# Individuals using the Internet  % of population  (wdi_internet) 589
# Mobile cellular subscriptions  per 100 people  (wdi_mobile) 600
# Mobile-cellular telephone subscriptions/100 pop. (wef_mobile) 633
# Consecutive years of current regime type (bmr_demdur) 123
# Legal Origin (lp_legor) 398
# Law: independence of agencies that organize and monitor elections is guaranteed (aii_q19) 77
# Law: political parties are required to disclose public donations (Gov. funds) (aii_q48) 91
# Law: political parties are required to disclose private donations (aii_q50) 91
# Practice: pol. parties disclose private donations and this is available to publi (aii_q51) 92
# Human Rights and Rule of Law (p_hr) 272
# The Age of the Constitution  years  (iaep_const) 336
# Rule of Law, Estimate (wbgi_rle) 543
# Rule of Law, Number of Sources (wbgi_rln) 543
# Rule of Law, Standard Error (wbgi_rls) 543
# Organized crime. 1-7 best (wef_oc) 633
# Rule of Law Index (wel_rli) 644
# Rule of Law (wel_rol) 644
# Practice: candidates/pol. parties have fair access to state-owned media outlets (aii_q23) 78
# Practice: media organizations disclose their owner's identities to the public (aii_q52) 92
# Practice: journalists and editors adhere to professional practices in reporting (aii_q53) 93
# Law: it's legal to report accurate news even if it damages pub. gures' reput. (aii_q54) 93
# Practice: the government does not promote the media's self-censorship (aii_q55) 94
# Practice: ministries and autonomous agencies have websites (aii_q58) 95
# Practice: the public services regulatory agencies have websites (aii_q59) 96
# Freedom of Expression (bti_foe) 131
# E-Government Index (egov_egov) 178
# E-Participation Index (egov_epar) 179
# Human Capital Index (egov_hci) 179
# Media Bias before Election (nelda_mbbe) 401
# Press Freedom Index (rsf_p) 486
# Quality of Democracy: Access to Information (sgi_qdai) 494
# Media corrupt (vdem_mecorrpt) 538
# Condence: The Press (wvs_confpr) 664
# Condence: Television (wvs_conftv) 665
# Foreign population (oecd_migforpop_t1b) 434
# Law: political parties are required to disclose public donations Gov. funds (aii_q48) 91
# Practice: pol. parties disclose public donations and these are available to publ (aii_q49) 91
# Law: political parties are required to disclose private donations (aii_q50) 91
# Practice: pol. parties disclose private donations and this is available to publi (aii_q51) 92
# Free and Fair Elections (bti_e) 131
# Party System (bti_ps) 136
# Contestation standardized version (cam_contest) 144
# Inclusiveness standardized version  (cam_inclusive) 144
# Number of changes in government per year (cpds_chg) 155
# Eective number of parties on the seats level (cpds_enps) 155
# Eective number of parties on the votes level (cpds_enpv) 156
# Electoral fractionalization of the party system (Rae index) (cpds_frel) 156
# Type of Government (cpds_tg) 162
# Voter turnout in election (cpds_vt) 167
# 43
# Close to Political Party (cses_pc) 170
# Electoral Volatility - Parties above 1% (dev_altv1) 171
# Electoral Volatility - Parties below 1% (dev_othv1) 172
# Electoral Volatility - Parties entering/exiting party system (dev_regv1) 172
# Electoral Volatility - Total (dev_tv1) 172
# Trust in Parliament (ess_trparl) 196
# Trust in Political Parties (ess_trpart) 196
# Trust in Politicians (ess_trpolit) 197
# Districts (gol_dist) 306
# Eective Number of Electoral Parties (gol_enep) 306
# Eective Number of Electoral Parties 1 (gol_enep1) 306
# Eective Number of Electoral Parties (Others) (gol_enepo) 307
# Eective Number of Presidential Candidates (gol_enpres) 307
# Electoral System Type-3 classes (gol_est) 308
# Electoral System Type-11 classes (gol_est_spec) 308
# Institution (gol_inst) 308
# Presidential Electoral System Type (gol_pest) 310
# PR Type (gol_pr) 310
# Presidential Election (gol_preel) 311
# Upper Seats (gol_upseat) 311
# Upper Tier (gol_uptier) 311
# Proportional Representation (gtm_pr) 314
# Alignment Executive/Legislative Chamber (lower) (h_alignl1) 320
# Alignment Lower/Upper Legislative Chamber (h_alignl1l2) 321
# Alignment Executive/Legislative Chamber (upper) (h_alignl2) 321
# Election of the Executive (iaep_ee) 338
# Electoral System for the Executive (iaep_ese) 340
# Independence of Selection of Executive (iaep_ise) 341
# National Elections for an Executive (iaep_nee) 343
# Party Nomination of Executive Candidates (iaep_pnec) 345
# Party Vote Establish Executive Candidates (iaep_pveec) 346
# Self-Nomination of Executive Candidates (iaep_snec) 348
# Electoral System Family (ideaesd_esf) 358
# Electoral System for the President (ideaesd_esp) 360
# Presidential Election: Compulsory Voting (ideavt_prescv) 363
# Presidential Election: Voter Turnout (ideavt_presvt) 363
# Participation (iiag_par) 368
# Runo Elections (jw_multiround) 382
# Single Party System (jw_oneparty) 383
# First Multiparty Election (nelda_fme) 401
# Media Bias before Election (nelda_mbbe) 401
# Was More Than One Party Legal (nelda_mtop) 402
# Number of Elections, Total (nelda_noe) 402
# Number of Elections, Constituent Assembly (nelda_noea) 402
# Number of Elections, Executive (nelda_noee) 402
# Riots and Protests after Election (nelda_rpae) 403
# Violence and Civilian Deaths before Election (nelda_vcdbe) 403
# Electoral Family (no_ef) 404
# Electoral Integrity Rating (pei_eir) 466
# Perception of Electoral Integrity Index (pei_peii) 467
# Majoritarian Electoral Systems (pt_maj) 472
# Quality of Democracy: Electoral Process (sgi_qdep) 494
# Competition (van_comp) 529
# Index of Democratization (van_index) 530
# Participation (van_part) 530
# Condence: The Political Parties (wvs_confpp) 664
# Year of election (yri_yoe) 685
# Law: citizens have a right to request public information from state bodies (aii_q41) 87
# Practice: citizens can access legislative processes and documents (aii_q43) 88
# Practice: government doesn't promote the self-censorship of citizens online (aii_q56) 94
# Practice: government doesn't block (or ask ICT rms to block) online content (aii_q57) 95
# Dichotomous democracy measure (bmr_dem) 123
# Number of previous democratic breakdowns (bmr_dembr) 123
# Consecutive years of current regime type (bmr_demdur) 123
# Dichotomous democracy measure (incl. missing for some countries) (bmr_demmis) 124
# Democratic transition (bmr_demtran) 124
# Democratic Breakdown (bnr_dem) 124
# Approval of Democracy (bti_aod) 126
# Free and Fair Elections (bti_e) 131
# International Cooperation (bti_ic) 132
# Democracy (chga_demo) 154
# Regime Institutions (chga_hinst) 154
# Satisfaction with Democracy (cses_sd) 170
# Accountability Transparency (diat_ati) 173
# Information Transparency (diat_iti) 173
# Transparency Index (diat_ti) 173
# Duration of Autocratic Regime (gwf_duration) 318
# Regime Failure (gwf_fail) 318
# Regime Failure - Subsequent Regime Type (gwf_failsub) 319
# Regime Failure - Ending Type (gwf_failtype) 319
# Regime Failure - Level of Violence (gwf_failviolent) 319
# Regime Type (gwf_regimetype) 320
# Regime Type (ht_regtype) 331
# Regime Type (simplied) (ht_regtype1) 332
# Political World Institutional Quality Ranking (full obs.) (kun_wiqrpol_full) 391
# First Multiparty Election (nelda_fme) 401
# Classication of Executives (no_ce) 404
# Unitary or Federal State (no_ufs) 404
# Regime Durability (p_durable) 465
# Revised Combined Polity Score (p_polity2) 465
# Quality of Democracy (sgi_qd) 493
# Quality of Democracy: Access to Information (sgi_qdai) 494
# Quality of Democracy: Civil Rights and Political Liberties (sgi_qdcr) 494
# Quality of Democracy: Electoral Process (sgi_qdep) 494
# Quality of Democracy: Rule of Law (sgi_qdrl) 494
# Electoral democracy index (vdem_polyarchy) 539
# Eective Democracy Index (wel_edi) 641
# Regime Type (wel_regtype) 643
# Political System Type (wel_sys) 647
# Media (oecd_fdindex_t1g) 417
# Communications (oecd_fdindex_t1h) 417
# GDP per capita (constant 2010 US dollar) (wdi_gdpcapcon2010) 573
# GDP per capita (current US dollar) (wdi_gdpcapcur) 573
# Gini index (World Bank estimate) (wdi_gini) 579


# MAS FILTROS 3 NAS #######
data_qog_latam_selection <- data_qog_latam_selection %>% 
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )

data_qog_latam_selection %>% 
  write.csv("data_qog_latam_selection.csv")

data_qog_latam_selection <- read.csv("data_qog_latam_selection.csv")

# RENOMBRO VARIABLES CLAVE ####### 
data_qog_latam_selection_renamed <- data_qog_latam_selection %>% 
  dplyr::mutate(cname = str_replace(cname, "Brazil", "Brasil")) %>% 
  dplyr::mutate(cname = str_replace(cname, "Dominican Republic", "Republica Dominicana")) %>% 
  dplyr::rename("pais" = cname) %>% 
  dplyr::rename("año_electoral" = year) 

# UNO BASES Y LO GUARDO; PERO ACTUALIZAR SI CAMBIA BASE-AÑOS ####

joined_base_qog_selection <- data_base_años %>% 
  left_join(data_qog_latam_selection_renamed)

joined_base_qog_selection %>% 
  write.csv("joined_base_qog_selection.csv")



