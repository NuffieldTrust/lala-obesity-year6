########################################################################################################################

############# Blocks multiple linear regression #############

# Use literature themes to put variables in blocks
# Use stepwise regression, multicollinearity and knowledge of variables to refine block list
# Create a socioeconomic/demographic block which can be used for general adjustment
# Create a full multiple model with each theme and the socioeconomic/demographic block

# !!! Filtered out variables relating to the u5/u6 population
# !!! Included socioeconomic/demogrpahic variables under exsiting outcome themes if more relevant there

########################################################################################################################

############# Create long and wide file of variables with chosen outcome #############

# 2018/19 overweight and obese

# Wide

obesity_outcome_vars <- obesity_vars_unsupp %>%
  left_join(obesity_outcome_long %>%
              filter(variable_new == "yr6_ovw_ob_" & year_num == 2018) %>%
              select(utla17cd, value) %>%
              dplyr::rename(yr6_ovw_ob_1819 = value)
            , by = "utla17cd")

# Long

obesity_outcome_vars_long <- obesity_vars_long %>%
  left_join(obesity_outcome_long %>%
              filter(variable_new == "yr6_ovw_ob_" & year_num == 2018) %>%
              select(utla17cd, value) %>%
              dplyr::rename(yr6_ovw_ob_1819 = value)
            , by = "utla17cd")

########################################################################################################################

############# Import variable themes lookup #############

variable_themes <- read_csv("data/Variable_themes.csv")

########################################################################################################################

############# Obesogenic community environment #############

# Identify list of variables based on association to include in initial block
# Select from multiple time points

environment <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "obesogenic community environment" 
                                            | (theme == "socioeconomic" 
                                               & suggested == "obesogenic community environment")))$variable)
         & !str_detect(variable_new, "u5")) %>%
  select(variable, variable_new) %>%
  distinct(variable, variable_new)

environment_simple_models <- environment %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(yr6_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

#################################################################

# Create first multiple model and then use stepwise to refine
# Include one of each variables from the most associated list created in previous step
# Only keep one of any variables which you know to overlap e.g. prop_imd_15 and av_imd_15
# If the most associated list doesn't include an appropriate time point, include the most appropriate time point instead and drop the inappropriate

environment_model <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                   , active_new_se_1516
                                                                                   , av_air_imd_15
                                                                                   , prop_u11_rd_school_imd_15
                                                                                   , prop_u11_rd_store_imd_15
                                                                                   #, ffood_u11_phe_14
                                                                                   , ffood_u11_ahah2_17
                                                                                   , prop_cyp_inactive_se_mean_1718
                                                                                   , prop_rural_defra_11
                                                                                   , sp_chpa_mhclg_pl_1516_1819
                                                                                   , sp_chob_mhclg_pl_1617_1819
                                                                                   , sp_osp_mhclg_pl_1415_1819
                                                                                   , sp_sprec_mhclg_pl_1415_1819
                                                                                   , sp_sprecfac_mhclg_pl_1617_1819
                                                                                   , walk_leis_new_se_1718
                                                                                   #, walk_se_1213
                                                                                   , woodland_phe_15
                                                                                   , blue_u11_ahah2_17
                                                                                   , greenpas_u11_ahah2_17
                                                                                   , greenact_u11_ahah2_17
                                                                                   , leis_u11_ahah2_17))

stepwise_environment_model <- MASS::stepAIC(environment_model, trace = FALSE, direction = "both")

# Check for multicollinearity with variance inflation factor and pearson correlation

vif_stepwise_environment_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_environment_model), "term.labels"))
                                                 , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                 , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_environment_model <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_environment_model), "term.labels")))
                                     , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_environment_model[pearson_stepwise_environment_model > -0.8 & pearson_stepwise_environment_model < 0.8] <- NA

summary(stepwise_environment_model)

# Resolve multicollinearity

stepwise_environment_modela <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                                        , attr(terms(stepwise_environment_model), "term.labels")
                                                                                                        , -prop_rural_defra_11))

stepwise_environment_modelb <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                                        , attr(terms(stepwise_environment_model), "term.labels")
                                                                                                        , -prop_u11_rd_store_imd_15))

glance(stepwise_environment_modela)
glance(stepwise_environment_modelb)

summary(stepwise_environment_modela)
summary(stepwise_environment_modelb)

# Save block with standardised coefficients

environment_block <- tidy(stepwise_environment_modela) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_environment_modela)$standardized.coefficients))

########################################################################################################################

########################################################################################################################

############# Socioeconomic disadvantage #############

disadvantage <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "socioeconomic disadvantage"))$variable)
         & !str_detect(variable_new, "u5")
         & !str_detect(variable_new, "u6")) %>%
  select(variable, variable_new) %>%
  distinct(variable, variable_new)

disadvantage_simple_models <- disadvantage %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(yr6_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

disadvantage_model <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                   , unique(disadvantage_simple_models$variable)))

stepwise_disadvantage_model <- MASS::stepAIC(disadvantage_model, trace = FALSE, direction = "both")

vif_stepwise_disadvantage_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_disadvantage_model), "term.labels"))
                                                            , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_disadvantage_model <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_disadvantage_model), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_disadvantage_model[pearson_stepwise_disadvantage_model > -0.8 & pearson_stepwise_disadvantage_model < 0.8] <- NA

summary(stepwise_disadvantage_model)

disadvantage_block <- tidy(stepwise_disadvantage_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_disadvantage_model)$standardized.coefficients))

########################################################################################################################

############# Childhood stress #############

stress <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "childhood stress"))$variable)
         & !str_detect(variable_new, "u5")) %>%
  select(variable, variable_new) %>%
  distinct(variable, variable_new)

stress_simple_models <- stress %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(yr6_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

stress_model <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                   , unique(stress_simple_models$variable)
                                                                                   , -ace_paper_17
                                                                                   , -ace_mal_paper_17
                                                                                   , -ace_loc_paper_17
                                                                                   , -ace_fam_paper_17))

stepwise_stress_model <- MASS::stepAIC(stress_model, trace = FALSE, direction = "both")

vif_stepwise_stress_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_stress_model), "term.labels"))
                                                 , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                 , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_stress_model <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_stress_model), "term.labels")))
                                     , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_stress_model[pearson_stepwise_stress_model > -0.8 & pearson_stepwise_stress_model < 0.8] <- NA

summary(stepwise_stress_model)

stress_block <- tidy(stepwise_stress_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_stress_model)$standardized.coefficients))

########################################################################################################################

############# Breastfeeding #############

# No relevant data available for year 6 age group

########################################################################################################################

############# Neighbourhood safety #############

safety <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "neighbourhood safety"
                                            | (theme == "socioeconomic" & suggested == "neighbourhood safety")))$variable)
         & !str_detect(variable_new, "u5")) %>%
  select(variable, variable_new) %>%
  distinct(variable, variable_new)

safety_simple_models <- safety %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(yr6_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

safety_model <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                   , prop_u11_cri_imd_15
                                                                                   , av_accidents_imd_15))

stepwise_safety_model <- MASS::stepAIC(safety_model, trace = FALSE, direction = "both")

vif_stepwise_safety_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_safety_model), "term.labels"))
                                                 , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                 , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_safety_model <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_safety_model), "term.labels")))
                                     , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_safety_model[pearson_stepwise_safety_model > -0.8 & pearson_stepwise_safety_model < 0.8] <- NA

summary(stepwise_safety_model)

safety_block <- tidy(stepwise_safety_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_safety_model)$standardized.coefficients))

########################################################################################################################

############# Socioeconomic/demographic #############

# Exclude any of the socioeconomic/demographic variables that have now been absorbed into the existing outcome variable themes
# Create a block to adjust full models with
# Create a refined block using stepwise regression and checking for multicollinearity

socioeconomic <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "socioeconomic" 
                                            & (suggested != "obesogenic community environment" 
                                               & suggested != "neighbourhood safety")))$variable)
         & !str_detect(variable_new, "u5")) %>%
  select(variable, variable_new) %>%
  distinct(variable, variable_new)

socioeconomic_simple_models <- socioeconomic %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(yr6_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

#################################################################

# Create a multiple model
# Use the suggested variables attached to the themes to ensure only keeping one type of each type of socioeconomic/demographic variable
# If the year in the simple models output doesn't look appropriate then switch to appropriate year
# This block can be used to adjust other models although multicollinearity will need checking

socioeconomic_model <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                                , prop_cyp_inc_hmrc_11
                                                                                                , prop_u11_hse_afford_imd_15
                                                                                                , av_hea_imd_15
                                                                                                #, sp_ph_mhclg_pl_1516_1819
                                                                                                , prop_adult_skills_imd_15
                                                                                                , prop_nonwhite_ons_11
                                                                                                , sp_ey_esfa_1011
                                                                                                #, csc_mhclg_pl_1415_1819
                                                                                                , prop_u11_hse_crowd_imd_15
                                                                                                , sp_chnpres_mhclg_pool_1516_1819
                                                                                                , sp_chpres_mhclg_pl_1516_1819
                                                                                                , dwelling_pp_voa_18
                                                                                                , prop_hse_cond_imd_15
                                                                                                , av_edu_cyp_imd_15
                                                                                                , prop_u11_hse_heat_imd_15))

# Use stepwise regression to refine the list of variables

stepwise_socioeconomic <- MASS::stepAIC(socioeconomic_model, trace = FALSE, direction = "both")

# Check for multicollinearity (both VIF > 5 and pearson > 0.8)

vif_stepwise_socioeconomic <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_socioeconomic), "term.labels"))
                                                        , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                        , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_socioeconomic <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_socioeconomic), "term.labels")))
                                            , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_socioeconomic[pearson_stepwise_socioeconomic > -0.8 & pearson_stepwise_socioeconomic < 0.8] <- NA

glance(stepwise_socioeconomic)
summary(stepwise_socioeconomic)

# if multicollinearity exists then need to test model fit excluding each of the variables 

stepwise_socioeconomic1 <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                        , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                        , -prop_u11_hse_afford_imd_15))

stepwise_socioeconomic2 <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                                    , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                                    , -prop_nonwhite_ons_11))

glance(stepwise_socioeconomic1)
glance(stepwise_socioeconomic2)

summary(stepwise_socioeconomic1)

socioeconomic_block <- tidy(stepwise_socioeconomic1) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_socioeconomic1)$standardized.coefficients))

##################################################################################################################################

############# Block adjusted r squareds #############

blocks_adj_rsq <- tibble(block = c("environment", "disadvantage", "stress", "safety", "socioeconomic")
                         , adj_rsq = c(glance(stepwise_environment_modela)$adj.r.squared
                                       , glance(stepwise_disadvantage_model)$adj.r.squared
                                       , glance(stepwise_stress_model)$adj.r.squared
                                       , glance(stepwise_safety_model)$adj.r.squared
                                       , glance(stepwise_socioeconomic1)$adj.r.squared))

########################################################################################################################

############# Models with all themes included #############

# Adjust final blocks for socioeconomic/demographics using the refined socioeconomic block 

model_all3 <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                               , attr(terms(stepwise_environment_modela), "term.labels")
                                                                               , attr(terms(stepwise_disadvantage_model), "term.labels")
                                                                               , attr(terms(stepwise_stress_model), "term.labels")
                                                                               , attr(terms(stepwise_safety_model), "term.labels")
                                                                               , attr(terms(stepwise_socioeconomic1), "term.labels")
                                                                               ))

stepwise_model_all3 <- MASS::stepAIC(model_all3, trace = FALSE, direction = "both")

vif_stepwise_model_all3 <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(stepwise_model_all3), "term.labels"))
                                                       , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_model_all3 <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(stepwise_model_all3), "term.labels")))
                                         , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_model_all3[pearson_stepwise_model_all3 > -0.8 & pearson_stepwise_model_all3 < 0.8] <- NA

summary(stepwise_model_all3)
anova(stepwise_model_all3)

final_model_all3 <- tidy(stepwise_model_all3) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_model_all3)$standardized.coefficients))
final_fit_all3 <- glance(stepwise_model_all3)

############################

# Scaled/standardised coefficients

stdcoeff_model3 <- tidy(lm(scale(yr6_ovw_ob_1819) ~ scale(active_new_se_1516) + scale(sp_chpa_mhclg_pl_1516_1819) + scale(sp_sprecfac_mhclg_pl_1617_1819)
   + scale(walk_leis_new_se_1718) + scale(av_accidents_imd_15) + scale(prop_cyp_inc_hmrc_11) + scale(av_hea_imd_15)
   + scale(prop_adult_skills_imd_15) + scale(prop_nonwhite_ons_11), obesity_outcome_vars))

############################

# Relative importance

relaimpo::calc.relimp(stepwise_model_all3, rela = TRUE)

# Manual method

glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ walk_leis_new_se_1718, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ av_hea_imd_15, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ prop_adult_skills_imd_15, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ active_new_se_1516, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ prop_nonwhite_ons_11, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ sp_sprecfac_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared
glance(lm(yr6_ovw_ob_1819 ~ av_accidents_imd_15, obesity_outcome_vars))$r.squared

final_relimp_all3 <- tibble(prop_cyp_inc_hmrc_11 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11, obesity_outcome_vars))$r.squared
                 , walk_leis_new_se_1718 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11, obesity_outcome_vars))$r.squared
                 , av_hea_imd_15 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718, obesity_outcome_vars))$r.squared
                 , prop_adult_skills_imd_15 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15, obesity_outcome_vars))$r.squared
                 , active_new_se_1516 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15 + active_new_se_1516, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                    + prop_adult_skills_imd_15, obesity_outcome_vars))$r.squared
                 , prop_nonwhite_ons_11 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                    + prop_adult_skills_imd_15 + active_new_se_1516, obesity_outcome_vars))$r.squared
                 , sp_chpa_mhclg_pl_1516_1819 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11 
                                             + sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                    + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11, obesity_outcome_vars))$r.squared
                 , sp_sprecfac_mhclg_pl_1617_1819 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11 
                                             + sp_chpa_mhclg_pl_1516_1819 + sp_sprecfac_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                    + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11 
                                    + sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared
                 , av_accidents_imd_15 = glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                             + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11 
                                             + sp_chpa_mhclg_pl_1516_1819 + sp_sprecfac_mhclg_pl_1617_1819 
                                             + av_accidents_imd_15, obesity_outcome_vars))$r.squared 
                 - glance(lm(yr6_ovw_ob_1819 ~ prop_cyp_inc_hmrc_11 + walk_leis_new_se_1718 + av_hea_imd_15 
                                    + prop_adult_skills_imd_15 + active_new_se_1516 + prop_nonwhite_ons_11 
                                    + sp_chpa_mhclg_pl_1516_1819 + sp_sprecfac_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared) %>%
  pivot_longer(everything(), names_to = "model", values_to = "added_r_squared") %>%
  mutate(r_squared = sum(added_r_squared)
         , prop_r_squared = added_r_squared / r_squared)

############################

# Test model assumptions

final_all3_tests <- tibble(linear_sp_stat           = shapiro.test(stepwise_model_all3$residuals)$statistic
                           , linear_sp_pvalue       = shapiro.test(stepwise_model_all3$residuals)$p.value
                           , linear_mean_resid      = mean(stepwise_model_all3$residuals)
                           , autocorr_dw_stat       = lmtest::dwtest(stepwise_model_all3)$statistic
                           , autocorr_dw_pvalue     = lmtest::dwtest(stepwise_model_all3)$p.value
                           , homosced_bp_stat       = lmtest::bptest(stepwise_model_all3)$statistic
                           , homosced_bp_pvalue     = lmtest::bptest(stepwise_model_all3)$p.value
                           , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
) 

############################

# Top 5 areas where actual value is better than predicted

final_all3_best <- augment(stepwise_model_all3) %>%
  top_n(-5, .resid)

#################################################################

# Check for influential points

model_all3_influen <- lm(yr6_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% 
                            mutate(observation = row_number()) %>%
                            filter(observation %in% unique(augment(stepwise_model_all3) %>%
                                                             mutate(mean_cook_threshold = 4*mean(.cooksd)
                                                                    , observation = row_number()) %>%
                                                             filter(.cooksd < mean_cook_threshold))$observation) %>%
                            select(yr6_ovw_ob_1819, utla17cd, attr(terms(stepwise_model_all3), "term.labels")) 
)

vif_model_all3_influen <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(model_all3_influen), "term.labels"))
                                                     , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                     , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_model_all3_influen <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(model_all3_influen), "term.labels")))
                                         , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_model_all3_influen[pearson_model_all3_influen > -0.8 & pearson_model_all3_influen < 0.8] <- NA

summary(model_all3_influen)

final_model_all3_influen <- tidy(model_all3_influen) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
final_fit_all3_influen <- glance(model_all3_influen)

#############################

# Test model assumptions in influential model

final_all3_influen_tests <- tibble(linear_sp_stat   = shapiro.test(model_all3_influen$residuals)$statistic
                           , linear_sp_pvalue       = shapiro.test(model_all3_influen$residuals)$p.value
                           , linear_mean_resid      = mean(model_all3_influen$residuals)
                           , autocorr_dw_stat       = lmtest::dwtest(model_all3_influen)$statistic
                           , autocorr_dw_pvalue     = lmtest::dwtest(model_all3_influen)$p.value
                           , homosced_bp_stat       = lmtest::bptest(model_all3_influen)$statistic
                           , homosced_bp_pvalue     = lmtest::bptest(model_all3_influen)$p.value
                           , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
) 

############################

# Top 5 areas where actual value is better than predicted in influential model

final_all3_influen_best <- augment(model_all3_influen) %>%
  top_n(-5, .resid)

####################################################################################

# Test refining model to just significant variables

summary(stepwise_model_all3)

# sp_chpa, sp_sprecfac, rate_cin, prop_adult_skills

model_all3_1 <-  lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                         , attr(terms(stepwise_model_all3), "term.labels")
                                                                                         , -sp_chpa_mhclg_pl_1516_1819
))

model_all3_2 <-  lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                         , attr(terms(stepwise_model_all3), "term.labels")
                                                                                         , -sp_sprecfac_mhclg_pl_1617_1819
))

model_all3_3 <-  lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                         , attr(terms(stepwise_model_all3), "term.labels")
                                                                                         , -rate_cin_dfe_1617
))

model_all3_4 <-  lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                         , attr(terms(stepwise_model_all3), "term.labels")
                                                                                         , -prop_adult_skills_imd_15
))

model_all3_5 <-  lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% select(yr6_ovw_ob_1819, utla17cd
                                                                                         , attr(terms(stepwise_model_all3), "term.labels")
                                                                                         , -sp_chpa_mhclg_pl_1516_1819
                                                                                         , -sp_sprecfac_mhclg_pl_1617_1819
                                                                                         , -rate_cin_dfe_1617
                                                                                         , -prop_adult_skills_imd_15
))

glance(model_all3_1)
glance(model_all3_2)
glance(model_all3_3)
glance(model_all3_4)
glance(model_all3_5)

summary(model_all3_1)
summary(model_all3_2)
summary(model_all3_3)
summary(model_all3_4)
summary(model_all3_5)

refin_model_all3 <- tidy(model_all3_5) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
refin_fit_all3 <- glance(model_all3_5)

############################

# Test model assumptions on refined model

refin_all3_tests <- tibble(linear_sp_stat           = shapiro.test(model_all3_5$residuals)$statistic
                           , linear_sp_pvalue       = shapiro.test(model_all3_5$residuals)$p.value
                           , linear_mean_resid      = mean(model_all3_5$residuals)
                           , autocorr_dw_stat       = lmtest::dwtest(model_all3_5)$statistic
                           , autocorr_dw_pvalue     = lmtest::dwtest(model_all3_5)$p.value
                           , homosced_bp_stat       = lmtest::bptest(model_all3_5)$statistic
                           , homosced_bp_pvalue     = lmtest::bptest(model_all3_5)$p.value
                           , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
                           , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                                                , TRUE ~ 0)
) 

############################

# Top 5 areas where actual value is better than predicted in refined model

refin_all3_best <- augment(model_all3_5) %>%
  top_n(-5, .resid)

#################################################################

# Check for influential points in refined model

refin_model_all3_5_influen <- lm(yr6_ovw_ob_1819~ . -utla17cd, data = obesity_outcome_vars %>% 
                                   mutate(observation = row_number()) %>%
                                   filter(observation %in% unique(augment(model_all3_5) %>%
                                                                    mutate(mean_cook_threshold = 4*mean(.cooksd)
                                                                           , observation = row_number()) %>%
                                                                    filter(.cooksd < mean_cook_threshold))$observation) %>%
                                   select(yr6_ovw_ob_1819, utla17cd, attr(terms(model_all3_5), "term.labels")) 
)

vif_refin_model_all3_5_influen <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% select(attr(terms(refin_model_all3_5_influen), "term.labels"))
                                                            , obesity_outcome_vars$yr6_ovw_ob_1819)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_refin_model_all3_5_influen <- as_tibble(cor(obesity_outcome_vars %>% select(attr(terms(refin_model_all3_5_influen), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_refin_model_all3_5_influen[pearson_refin_model_all3_5_influen > -0.8 & pearson_refin_model_all3_5_influen < 0.8] <- NA

summary(refin_model_all3_5_influen)

refin_model_all3_influen <- tidy(refin_model_all3_5_influen) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
refin_fit_all3_influen <- glance(refin_model_all3_5_influen)

#############################

# Test model assumptions in refined influential model

refin_all3_influen_tests <- tibble(linear_sp_stat   = shapiro.test(refin_model_all3_5_influen$residuals)$statistic
                                   , linear_sp_pvalue       = shapiro.test(refin_model_all3_5_influen$residuals)$p.value
                                   , linear_mean_resid      = mean(refin_model_all3_5_influen$residuals)
                                   , autocorr_dw_stat       = lmtest::dwtest(refin_model_all3_5_influen)$statistic
                                   , autocorr_dw_pvalue     = lmtest::dwtest(refin_model_all3_5_influen)$p.value
                                   , homosced_bp_stat       = lmtest::bptest(refin_model_all3_5_influen)$statistic
                                   , homosced_bp_pvalue     = lmtest::bptest(refin_model_all3_5_influen)$p.value
                                   , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                                                        , TRUE ~ 0)
                                   , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                                                        , TRUE ~ 0)
                                   , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                                                        , TRUE ~ 0)
                                   , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                                                        , TRUE ~ 0)
                                   , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                                                        , TRUE ~ 0)
) 

############################

# Top 5 areas where actual value is better than predicted in refined influential model

refin_all3_influen_best <- augment(refin_model_all3_5_influen) %>%
  top_n(-5, .resid)

##################################################################################################################################

############# Save full model results #############

writexl::write_xlsx(list(environment_block = environment_block, disadvantage_block = disadvantage_block, stress_block = stress_block
                         #, bf_block = bf_block
                         , safety_block = safety_block, socioeconomic_block = socioeconomic_block
                         , blocks_adj_rsq = blocks_adj_rsq
                         , model3 = final_model_all3, stdcoeff_model3 = stdcoeff_model3
                         , model3_fit = final_fit_all3, model3_tests = final_all3_tests, model3_best = final_all3_best, model3_relimp = final_relimp_all3
                         , model3_influen = final_model_all3_influen, model3_fit_influen = final_fit_all3_influen, model3_tests_influen = final_all3_influen_tests, model3_best_influen = final_all3_influen_best
                         , refin_model3 = refin_model_all3, refin_model3_fit = refin_fit_all3, refin_model3_tests = refin_all3_tests, refin_model3_best = refin_all3_best, refin_model3_influen = refin_model_all3_influen, refin_model3_fit_influen = refin_fit_all3_influen, refin_model3_tests_influen = refin_all3_influen_tests, refin_model3_best_influen = refin_all3_influen_best)
                    ,"output/multiple_linear_regression/Blocks_full_models_obesity_yr6_UTLA.xlsx", col_names=TRUE)

##################################################################################################################################

############# Summary information for final model variables #############

final_vars <- obesity_vars_long %>%
  filter(variable %in% attr(terms(stepwise_model_all3), "term.labels")) %>%
  select(utla17cd, variable, value) %>%
  bind_rows(obesity_outcome_vars_long %>%
              filter(variable == "active_new_se_1516") %>%
              select(utla17cd, yr6_ovw_ob_1819) %>%
              rename(value = yr6_ovw_ob_1819) %>%
              mutate(variable = "yr6_ovw_ob_1819")) %>%
  group_by(variable) %>%
  mutate(q20 = quantile(value, c(.2))
         , q80 = quantile(value, c(.8))
         , quintile1 = case_when(value < q20 ~ value
                                 , TRUE ~ NA_real_)
         , quintile5 = case_when(value >= q80 ~ value
                                 , TRUE ~ NA_real_))

obesity_vars_desc <- final_vars %>%
  group_by(variable) %>%
  summarise(min = min(value)
            , q25 = quantile(value, c(.25))
            , q40 = quantile(value, c(.40))
            , median = quantile(value, c(.50))
            , q60 = quantile(value, c(.60))
            , q75 = quantile(value, c(.75))
            , max = max(value)
            , mean = mean(value)
            , sd = sd(value)
            , mean_q1 = mean(quintile1, na.rm = TRUE)
            , mean_q5 = mean(quintile5, na.rm = TRUE))

write_csv(obesity_vars_desc, "output/multiple_linear_regression/Blocks_final_vars_desc.csv")

##################################################################################################################################
