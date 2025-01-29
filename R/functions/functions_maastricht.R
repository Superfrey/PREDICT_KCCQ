#' Function to chnages factors
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
as_factor_function_maastricht <- function(data) {
  data %>%
    dplyr::mutate(
      diabetes_status_who = as.factor(diabetes_status_who),
      SEX = as.factor(SEX),
      N_Education_3cat = as.factor(N_Education_3cat),
      N_ALCOHOL_CAT = as.factor(N_alcohol_cat),
      smoking_4cat = as.factor(smoking_4cat),
      med_HT = as.factor(med_HT),
      med_LP = as.factor(med_LP),
      Ethnicity_cat = as.factor(Ethnicity_cat)
    )
}

#' Complete cases data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
complete_cases_data <- function(data) {
  data %>%
    filter(N_CVD == 0) %>%
    filter(!is.na(ECGvar_std_nn), !is.na(ECGvar_TP), #&
                  # ECGvar_std_nn < quantile(ECGvar_std_nn, 0.99, na.rm = T) &
                  # ECGvar_SDANN < quantile(ECGvar_SDANN, 0.99, na.rm = T)&
                  # ECGvar_SDNNi < quantile(ECGvar_SDNNi, 0.99, na.rm = T)&
                  # ECGvar_rMSSD < quantile(ECGvar_rMSSD, 0.995, na.rm = T)&
                  # ECGvar_pNN50 < quantile(ECGvar_pNN50, 0.998, na.rm = T) &
           ECGvar_std_nn < 350,
           ECGvar_SDANN < 250,
           ECGvar_SDNNi < 200,
           ECGvar_rMSSD < 120,
           ECGvar_pNN50 < 100,
           ECGvar_TP < 40000,
           ECGvar_LF < 4000,
           ECGvar_HF < 2500,
           ECGvar_VLF < 4000,
           ECGvar_VLF < 40000,
                  ECGholter_rhythm <= 2) %>%
    filter(!is.na(diabetes_status_who) & !is.na(Age) & !is.na(SEX) &
                   !is.na(Ethnicity_cat) & !is.na(N_Education_3cat)& !is.na(bmi) & !is.na(Tot_chol) & !is.na(Triglyc) &
                   !is.na(TOTALPA) & !is.na(N_ALCOHOL_CAT) & !is.na(smoking_4cat) & !is.na(med_HT) & !is.na(med_LP) & !is.na(MAP_mean)
               & !is.na(CF_PWV80))

}


#' Flow diagram output
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
flowchart_data <- function(data) {

  pop <- nrow(data)
  # 1. Exclusion Prior CVD
  data_cvd <- data %>%
    dplyr::filter(N_CVD == 0)



  cvd_exclusion <- nrow(data_cvd)

  # 2. Complete HRV
  data_hrv <- data_cvd %>%
    filter(!is.na(ECGvar_std_nn) & !is.na(ECGvar_TP) &
               ECGvar_std_nn < quantile(ECGvar_std_nn, 0.99, na.rm = T) &
               ECGvar_SDANN < quantile(ECGvar_SDANN, 0.99, na.rm = T)&
               ECGvar_SDNNi < quantile(ECGvar_SDNNi, 0.99, na.rm = T)&
               ECGvar_rMSSD < quantile(ECGvar_rMSSD, 0.995, na.rm = T)&
               ECGvar_pNN50 < quantile(ECGvar_pNN50, 0.998, na.rm = T)

           & ECGholter_rhythm <= 2)

  hrv_exclusion <- nrow(data_hrv)
  ### PWV tree
  # 3. Complete PWV
  data_pwv <- data_hrv %>%
    filter(!is.na(CF_PWV80))

  pwv_exclusion <- nrow(data_pwv)

  # 4. Complete case information on covaraite
  data_complete <- data_pwv %>%
    filter(
      !is.na(diabetes_status_who), !is.na(Age), !is.na(SEX),
      !is.na(Ethnicity_cat), !is.na(N_Education_3cat), !is.na(bmi), !is.na(Tot_chol), !is.na(Triglyc),
      !is.na(TOTALPA), !is.na(N_ALCOHOL_CAT), !is.na(smoking_4cat), !is.na(med_HT), !is.na(med_LP), !is.na(MAP_mean)
    )

  complete_exclusion <- nrow(data_complete)

  ### Car DC tree
  # 3A. Complete PWV
  data_car <- data_pwv %>%
    filter(!is.na(Car_DC))

  Car_DC_exclusion <- nrow(data_car)

  # 4B. Complete case information on covaraite
  data_complete_car <- data_car %>%
    filter(
      !is.na(diabetes_status_who), !is.na(Age), !is.na(SEX),
      !is.na(Ethnicity_cat), !is.na(N_Education_3cat), !is.na(bmi), !is.na(Tot_chol), !is.na(Triglyc),
      !is.na(TOTALPA), !is.na(N_ALCOHOL_CAT), !is.na(smoking_4cat), !is.na(med_HT), !is.na(med_LP), !is.na(MAP_mean)
    )

  complete_car_DC_exclusion <- nrow(data_complete_car)

  steps <- c("population", "step 1", "step 2", "step 3", "step 4", "step 3a", "step 4a")
  step_names <- c("total_population", "prior_cvd_exclusion", "HRV_recorded", "PWV_recorded", "complete_information_covariates_PWV", "Car_DC_recorded", "complete_information_covariates_Car_DC")
  population_flow <- c(pop, cvd_exclusion, hrv_exclusion, pwv_exclusion, complete_exclusion, Car_DC_exclusion, complete_car_DC_exclusion)
  excluded <- c(pop - pop, pop - cvd_exclusion, cvd_exclusion - hrv_exclusion, hrv_exclusion - pwv_exclusion, pwv_exclusion - complete_exclusion, hrv_exclusion - Car_DC_exclusion, Car_DC_exclusion - complete_car_DC_exclusion)

  flowchart <- data.frame(steps = steps, step_names = step_names, population_flow = population_flow, excluded = excluded)

  return(flowchart)
}

#' Prpare and clear dataset
#'
#'
#' @param filepath
#'
#' @return
#' @export complete case data
#'
#' @examples
read_data_maastricht <- function(filepath) {
  data <- haven::read_sav(filepath)

  data <- data %>%
    dplyr::mutate(
      diabetes_status = ifelse(HbA1c_perc < 5.7 & Glucose_t0 < 6.1, 1, NA),
      diabetes_status = ifelse((HbA1c_perc > 5.7 & HbA1c_perc < 6.5) | (Glucose_t0 >= 6.1 & Glucose_t0 <= 6.9), 2, diabetes_status),
      diabetes_status = ifelse(HbA1c_perc > 6.5 | Glucose_t0 > 7, 3, diabetes_status)
    ) %>%
    dplyr::mutate(
      diabetes_status_who = structure(ifelse(N_GTS_WHO == 0, 1, NA), label = "normoglycemia"),
      diabetes_status_who = structure(ifelse(N_GTS_WHO == 1 | N_GTS_WHO == 2, 2, diabetes_status_who), label = "prediabetes"),
      diabetes_status_who = structure(ifelse(N_GTS_WHO == 3, 3, diabetes_status_who), label = "type-2 diabetes"),
      diabetes_status_who = as.factor(diabetes_status_who)
    )

  data[] <- lapply(data, function(x) {
    attributes(x) <- NULL
    x
  })
  data <- as_factor_function_maastricht(data)
  return(data)
}

#' Stadadization of z-score
#'
#' @param data
#'
#' @return Standardized zscore hrv
#' @export
#'
#' @examples
standardisation_hrv <- function(data) {
  scale_data <- data %>%
    dplyr::select(
      "ECGvar_avg_nn", "ECGvar_std_nn", "ECGvar_rMSSD", "ECGvar_SDANN",
      "ECGvar_SDNNi", "ECGvar_SDSD", "ECGvar_NN50", "ECGvar_pNN50", "ECGvar_TP", "ECGvar_ULF", "ECGvar_VLF", "ECGvar_LF", "ECGvar_HF",
      "ECGvar_LFHF"
    )

  scl <- apply(scale_data, 2, sd)
  mn <- apply(scale_data, 2, mean)

  data_z <- as.data.frame(scale(scale_data, scale = scl, center = mn))

  data_z_hrv <- data %>%
      mutate(mean_IBI = ECGvar_avg_nn) %>%
    dplyr::select(
      RandomID, diabetes_status_who, Age, SEX,
      Ethnicity_cat, N_Education_3cat, bmi, Tot_chol, Triglyc,
      TOTALPA, N_ALCOHOL_CAT, smoking_4cat, N_Chol_ratio, med_HT, med_LP, MAP_mean, CF_PWV80, Car_DC, med_HT_beta, HbA1c_perc,
      LDL, HDL, HbA1c_mol, Glucose_t0_FP, Glucose_t0, mean_IBI, HR_mean, med_DM
    ) %>%
    bind_cols(data_z) %>%
    mutate(
      ECGvar_z_hrv_td = (ECGvar_std_nn + ECGvar_rMSSD + ECGvar_SDANN + ECGvar_SDNNi + ECGvar_pNN50) / 5,
      ECGvar_z_hrv_fq = (ECGvar_TP + ECGvar_ULF + ECGvar_VLF + ECGvar_LF + ECGvar_HF) / 5
    )
}

#' 24 hour HRV data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
hour_24_hrv_data <- function(data) {
  data_hrv <- data %>%
    dplyr::select(!tidyselect::starts_with("ECGvar_"))

  data_hrv_24 <- data %>%
    dplyr::select(tidyselect::starts_with("ECGvar_"), RandomID) %>%
    dplyr::select(!(tidyselect::ends_with("day") | tidyselect::ends_with("night") | tidyselect::ends_with("arousal") |
      tidyselect::ends_with("LTV") | tidyselect::ends_with("PVCs") | tidyselect::ends_with("_STV")), RandomID) %>%
    dplyr::right_join(data_hrv, by = "RandomID")

  return(data_hrv_24)
}

#' Night and day HRV
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
hrv_data_night_day <- function(data) {
  data_day_night <- data %>%
    dplyr::select(!tidyselect::starts_with("ECGvar_"))

  data_hrv_circ <- data %>%
    dplyr::select(tidyselect::starts_with("ECGvar_"), RandomID) %>%
    dplyr::select(tidyselect::ends_with("day") | tidyselect::ends_with("night"), RandomID) %>%
    dplyr::right_join(data_day_night, by = "RandomID")

  return(data_hrv_circ)
}

#' descriptive_continuous_statisitcs
#'
#' @param data
#'
#' @return
#'  summary statistics for continous variables
#' @export
#'  a table with variables mean and sd
#' @examples
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(hrv) %>% # can you do something about the group and define this in the function?
    dplyr::summarise(dplyr::across(hrv_value, list(mean = mean, sd = sd, min = min, max = max, prec99 = ~ quantile(., probs = 0.99)))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}


#' Plot distrubution of continous scale
#'
#' @param dataset
#'
#' @return histograms
#' @export facetplot of histograms
#'
#' @examples
plot_distribution <- function(data) {
  ggplot2::ggplot(data, aes(x = hrv_value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(hrv), scales = "free")
}

#' #' Change column names to snakecase
#' #'
#' #' @param data
#' #' @param cols
#' #'
#' #' @return lowercase letters
#' #' @export
#' #'
#' #' @examples to_two
#' #'
#' column_values_to_snake_case <- function(data) {
#'     data %>%
#'         dplyr::mutate_all(snakecase::to_snake_case)
#' }


#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#' Age, diabetes_status_who, SEX, N_Education_3cat, MAP_mean, N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP

create_recipe_spec_m1 <- function(data, hrv_variable, echo) { # , arterial_marker
  recipes::recipe(data) %>%
    recipes::update_role({{ hrv_variable }},
      SEX,
      diabetes_status_who,
      Age, N_Education_3cat, MAP_mean,
      #mean_IBI,
      new_role = "predictor"
    ) %>% ## add for model 1 and 2
    recipes::update_role(CF_PWV80, new_role = "outcome") %>% # {{arterial_marker}}
    # brecipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>% # standardization done before with collected z-score
    recipes::step_log(CF_PWV80) # check again add PWV to log-transformation %>%
}

#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#' Age, diabetes_status_who, SEX, N_Education_3cat, MAP_mean, N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP

create_recipe_spec_m2 <- function(data, hrv_variable) { # , arterial_marker
  recipes::recipe(data) %>%
    recipes::update_role({{ hrv_variable }},
      SEX,
      diabetes_status_who,
      med_HT,
      Age, N_Education_3cat, MAP_mean,
      N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_LP,
      #mean_IBI,
      new_role = "predictor"
    ) %>% ## add for model 1 and 2
    recipes::update_role(CF_PWV80, new_role = "outcome") %>% # {{arterial_marker}} %>%
    # recipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>%
    # recipes::step_log(tidyselect::starts_with("ECGvar_"))

    recipes::step_log(CF_PWV80) # check again add PWV to log-transformation %>%
}


#' #' Change column names to snakecase
#' #'
#' #' @param data
#' #' @param cols
#' #'
#' #' @return lowercase letters
#' #' @export
#' #'
#' #' @examples to_two
#' #'
#' column_values_to_snake_case <- function(data) {
#'     data %>%
#'         dplyr::mutate_all(snakecase::to_snake_case)
#' }


#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#' Age, diabetes_status_who, SEX, N_Education_3cat, MAP_mean, N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP

create_recipe_spec_m1_CAD <- function(data, hrv_variable) { # , arterial_marker
  recipes::recipe(data) %>%
    recipes::update_role({{ hrv_variable }},
      SEX,
      diabetes_status_who,
      Age, N_Education_3cat, MAP_mean,
      # mean_IBI,
      new_role = "predictor"
    ) %>% ## add for model 1 and 2
    recipes::update_role(Car_DC, new_role = "outcome") %>% # {{arterial_marker}}
    # recipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>% # standardization done before with collected z-score
    recipes::step_log(Car_DC) # check again add PWV to log-transformation %>%
}

#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#' Age, diabetes_status_who, SEX, N_Education_3cat, MAP_mean, N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP

create_recipe_spec_m2_CAD <- function(data, hrv_variable) { # , arterial_marker
  recipes::recipe(data) %>%
    recipes::update_role({{ hrv_variable }},
      SEX,
      diabetes_status_who,
      Age, N_Education_3cat, MAP_mean,
      N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP,
      #mean_IBI,
      new_role = "predictor"
    ) %>% ## add for model 1 and 2
    recipes::update_role(Car_DC, new_role = "outcome") %>% # {{arterial_marker}} %>%
    # recipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>%
    # recipes::step_log(tidyselect::starts_with("ECGvar_"))

    recipes::step_log(Car_DC) # check again add PWV to log-transformation %>%
}
#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#' Age, diabetes_status_who, SEX, N_Education_3cat, MAP_mean, N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP

create_recipe_spec_inct_m2 <- function(data, hrv_variable, interaction_factor) { # , arterial_marker
  recipes::recipe(data) %>%
    recipes::update_role({{ hrv_variable }},
      SEX,
      diabetes_status_who,
      Age, N_Education_3cat, MAP_mean, HR_mean,
      N_ALCOHOL_CAT, smoking_4cat, TOTALPA, bmi, N_Chol_ratio, Triglyc, med_HT, med_LP,
      #mean_IBI,
      new_role = "predictor"
    ) %>% ## add for model 1 and 2
    recipes::update_role(CF_PWV80, new_role = "outcome") %>% # {{arterial_marker}} %>%
    # recipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>%
    # recipes::step_log(tidyselect::starts_with("ECGvar_"))
    recipes::step_interact(terms = ~ {{ hrv_variable }}:{{ interaction_factor }}) %>%
    recipes::step_log(CF_PWV80) # check again add PWV to log-transformation %>%
}

#' Create a workflow object of the model and transformations.
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
#'
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted.
#'
#' @return A data frame.
#'
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(conf.int = TRUE, conf.level = 0.95) # conf.int = TRUE, conf.level = 0.95
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results_m1 <- function(data) { # , arterial_marker
  create_model_workflow(
    parsnip::linear_reg() %>%
      parsnip::set_engine("lm"),
    data %>%
      create_recipe_spec_m1(tidyselect::starts_with("ECGvar_")) # dobbelt check, {{arterial_marker}}
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results_m2 <- function(data) { # , arterial_marker
  create_model_workflow(
    parsnip::linear_reg() %>%
      parsnip::set_engine("lm"),
    data %>%
      create_recipe_spec_m2(tidyselect::starts_with("ECGvar_")) # dobbelt check, {{arterial_marker}}
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results_m1_cad <- function(data) { # , arterial_marker
  create_model_workflow(
    parsnip::linear_reg() %>%
      parsnip::set_engine("lm"),
    data %>%
      create_recipe_spec_m1_CAD(tidyselect::starts_with("ECGvar_")) # dobbelt check, {{arterial_marker}}
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results_m2_cad <- function(data) { # , arterial_marker
  create_model_workflow(
    parsnip::linear_reg() %>%
      parsnip::set_engine("lm"),
    data %>%
      create_recipe_spec_m2_CAD(tidyselect::starts_with("ECGvar_")) # dobbelt check, {{arterial_marker}}
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}
#' Make HRV dataframe into longformat
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
df_hrv_long_format <- function(data) {

  data %>%
    pivot_longer(cols = tidyselect::starts_with("ECGvar"), names_to = "hrv", values_to = "hrv_value")

}

#' Pivot long format to wide format
#' and summary mean by metabolite
#'
#' @param data
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
hrv_to_wide <- function(data) {
  data %>%
    tidyr::pivot_wider(
      names_from = hrv,
      values_from = hrv_value,
    )
}
#' Convert the long form dataset into a list of wide form data frames.
#'
#' @param data The lipidomics dataset.
#'
#' @return A list of data frames.
#'
split_by_hrv <- function(data) {
  data %>%
    dplyr::group_split(hrv) %>%
    purrr::map(hrv_to_wide)
} # go through


#' Loop analysis with multiple analysis in one go
#'
#' @param data
#'
#' @return estimates and p-values
#' @export a table
#'
#' @examples
loop_analysis_hrv_m1 <- function(data) { # , arterial_marker
  data %>%
    split_by_hrv() %>%
    purrr::map(generate_model_results_m1) %>% # {{arterial_marker}}
    list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "ECGvar_"))
}

loop_analysis_hrv_m2 <- function(data) { # , arterial_marker
  data %>%
    split_by_hrv() %>%
    purrr::map(generate_model_results_m2) %>% # {{arterial_marker}}
    list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "ECGvar_"))
}

loop_analysis_hrv_m1_cad <- function(data) { # , arterial_marker
  data %>%
    split_by_hrv() %>%
    purrr::map(generate_model_results_m1_cad) %>% # {{arterial_marker}}
    list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "ECGvar_"))
}

loop_analysis_hrv_m2_cad <- function(data) { # , arterial_marker
  data %>%
    split_by_hrv() %>%
    purrr::map(generate_model_results_m2_cad) %>% # {{arterial_marker}}
    list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "ECGvar_"))
}

#' Create names for table
#'
#' @param model_results
#' @param data for the analusis
#'
#' @return metabolites names
#' @export
#'
#' @examples
add_original_hrv_names <- function(model_results, data) {
  data %>%
    mutate(term = hrv) %>%
    mutate(term = str_c("ECGvar_", term)) %>%
    distinct(term, hrv) %>%
    mutate(term = stringr::str_remove("ECG_var", term)) %>%
    right_join(model_results, by = "term")
}

#' calculate estimates and get names combined
#'
#' @param data and models
#'
#' @return table with estimates
#' @export
#'
#' @examples
calculate_estimates_m1 <- function(data) {
  data %>%
    loop_analysis_hrv_m1() %>%
    add_original_hrv_names(data)
}

calculate_estimates_m2 <- function(data) {
  data %>%
    loop_analysis_hrv_m2() %>%
    add_original_hrv_names(data)
}

calculate_estimates_m1_cad <- function(data) {
  data %>%
    loop_analysis_hrv_m1_cad() %>%
    add_original_hrv_names(data)
}

calculate_estimates_m2_cad <- function(data) {
  data %>%
    loop_analysis_hrv_m2_cad() %>%
    add_original_hrv_names(data)
}

#' arrange_and_exponetiate results
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
arrange_and_exponetiate <- function(results) {
  results %>%
    dplyr::mutate(
      estimate = (exp(estimate) - 1) * 100,
      conf.low = (exp(conf.low) - 1) * 100,
      conf.high = (exp(conf.high) - 1) * 100,
      p.value = round(p.value, 4)
    ) %>%
    dplyr::mutate(hrv = stringr::str_replace(term, "ECGvar_", "")) %>%
    dplyr::arrange(match(hrv, c(
      "avg_nn", "std_nn", "SDANN", "SDNNi", "rMSSD", "SDSD", "NN50", "pNN50", "z_hrv_td",
      "TP", "HF", "LF", "VLF", "ULF", "LFHF", "z_hrv_fq"
    ))) %>%
    dplyr::mutate(hrv = factor(hrv, levels = hrv))
}

#' Forrest function for results
#'
#' @param results
#'
#' @return
#' @export forrest plot
#'
#' @examples
plot_estimates_by_group <- function(results) {
  results %>%
    ggplot(aes(
      x = estimate,
      y = hrv,
      xmin = conf.low,
      xmax = conf.high,
      group = desc(group),
      fill = group,
      col = group
    )) +
    geom_point(position = position_dodge(width = c(0.8))) +
    geom_pointrange(position = position_dodge(width = c(0.8))) +
    # geom_text(aes(label = sprintf("%.2f%% (%.2f, %.2f)", estimate, conf.low, conf.high)),
    #               vjust = 0, hjust = -0.5, size = 3, position = position_dodge(width = c(1.1))) +
    coord_fixed(xlim = c(-2.5, 8)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
      axis.title.y = element_blank()
    ) +
    xlab("Percentage higher PWV")
}


plot_estimates_by_group_em <- function(results) {

    results %>%
        ggplot(aes(
            x = estimate,
            y = hrv,
            xmin = conf.low,
            xmax = conf.high,
            group = desc(group),
            fill = group,
            col = group
        )) +
        geom_point(position = position_dodge(width = c(2.5))) +
        geom_pointrange(position = position_dodge(width = c(2.5))) +
        # geom_text(aes(label = sprintf("%.2f%% (%.2f, %.2f)", estimate, conf.low, conf.high)),
        #               vjust = 0, hjust = -0.5, size = 3, position = position_dodge(width = c(1.1))) +
        coord_fixed(xlim = c(-2.5, 8)) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        theme_bw() +
        theme(
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y = element_blank()
        ) +
        xlab("Percentage higher PWV")
}


#' Forrest function for results by group
#'
#' @param results
#'
#' @return
#' @export forrest plot
#'
#' @examples
plot_estimates <- function(results) {
  results %>%
    ggplot(aes(
      x = estimate,
      y = hrv,
      xmin = conf.low,
      xmax = conf.high,
    ), ) +
    geom_point() +
    geom_pointrange() +
    coord_fixed(xlim = c(-5, 1)) +
    scale_y_discrete(labels = rev(c(
      "Mean IBI", "SDNN*", "SDANN*", "SDNN index*", "RMSSD*", "SDSD", "NN50", "pNN50*", "Z-score: Time domain HRV", "Total power*", "High frequency*", "Low frequency*", "Very-low frquency*", "Ultra-low frequency*",
      "Low-to-High frequency ratio", "Z-score: Frequency domain HRV"
    )), limits = rev) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
      axis.title.y = element_blank()
    ) +
    xlab("Percentage higher PWV")
}


## Effect modification

effect_modificator_coefficient <- function(model, exposure_included, effect_modificator_text) {
  # exposure_included <- deparse(substitute(exposure_included))
  effect_modificator_estimates <- multcomp::glht(model, linfct = c(paste0(exposure_included, "+", effect_modificator_text)))
  estimate_for_effect_modificator <- confint(effect_modificator_estimates)

  estimate_for_effect_modificator <- as.data.frame(estimate_for_effect_modificator$confint)

  return(estimate_for_effect_modificator)
}


#' effect_modification_loop_diabetes status
#'
#' @param data in long format
#'
#' @return model 1 and 2
#' @export results
#'
#' @examples
effect_modification_loop_diabetes_m1 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 1
  m1 <- lm(paste("log(CF_PWV80) ~", exposure, "*diabetes_status_who + SEX + MAP_mean + Age + N_Education_3cat"), data = data)

  model_1 <- m1
  m1_est <- t(as.data.frame(ci.exp(m1)[2, c(1:3)]))

  ## normoglycemia
  estimate_list_no_m1 <- data.frame(HRV = exposure, group = "normoglycemia", model = "model_1", round(((m1_est - 1) * 100), 4))
  names(estimate_list_no_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_no_m1) <- NULL

  ## prediabetes
  text_em_pre <- paste0(exposure, ":diabetes_status_who2 == 2")
  effect_modification_pre_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_pre)) - 1) * 100
  estimate_list_pre_m1 <- data.frame(HRV = exposure, group = "prediabetes", model = "model_1", round(effect_modification_pre_m1, 4))
  names(estimate_list_pre_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_pre_m1 <- cbind(estimate_list_pre_m1, p_value_ict = round(ci.lin(m1)[10, 4], 5)) # model 1
  rownames(estimate_list_pre_m1) <- NULL

  ## type-2 diabetes
  text_em_t2d <- paste0(exposure, ":diabetes_status_who3 == 3")
  effect_modification_t2d_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_t2d)) - 1) * 100
  estimate_list_t2d_m1 <- data.frame(HRV = exposure, group = "type-2_diabetes", model = "model_1", round(effect_modification_t2d_m1, 4))
  names(estimate_list_t2d_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_t2d_m1 <- cbind(estimate_list_t2d_m1, p_value_ict = round(ci.lin(m1)[11, 4], 5)) # model 1
  rownames(estimate_list_t2d_m1) <- NULL
  results <- dplyr::bind_rows(estimate_list_no_m1, estimate_list_pre_m1, estimate_list_t2d_m1)

  return(results)
}




effect_modification_loop_diabetes_m2 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 2
  m2 <- lm(paste("log(CF_PWV80) ~", exposure, "*diabetes_status_who + SEX + Age  +
                  as.factor(Ethnicity_cat) + as.factor(N_Education_3cat) + bmi + N_Chol_ratio + Triglyc  +
                  TOTALPA + as.factor(N_ALCOHOL_CAT) + as.factor(smoking_4cat) +
                  med_HT +
                   med_LP + MAP_mean"), data = data)

  model_2 <- m2
  m2_est <- t(as.data.frame(ci.exp(m2)[2, c(1:3)]))

  ## normoglycemia
  estimate_list_no_m2 <- data.frame(HRV = exposure, group = "normoglycemia", model = "model_2", round(((m2_est - 1) * 100), 4))
  names(estimate_list_no_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_no_m2) <- NULL

  ## prediabetes
  text_em_pre <- paste0(exposure, ":diabetes_status_who2 == 2")
  effect_modification_pre_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_pre)) - 1) * 100
  estimate_list_pre_m2 <- data.frame(HRV = exposure, group = "prediabetes", model = "model_2", round(effect_modification_pre_m2, 4))
  names(estimate_list_pre_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_pre_m2 <- cbind(estimate_list_pre_m2, p_value_ict = round(ci.lin(m2)[22, 4], 5)) # model 2
  rownames(estimate_list_pre_m2) <- NULL

  ## type-2 diabetes
  text_em_t2d <- paste0(exposure, ":diabetes_status_who3 == 3")
  effect_modification_t2d_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_t2d)) - 1) * 100
  estimate_list_t2d_m2 <- data.frame(HRV = exposure, group = "type-2_diabetes", model = "model_2", round(effect_modification_t2d_m2, 4))
  names(estimate_list_t2d_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_t2d_m2 <- cbind(estimate_list_t2d_m2, p_value_ict = round(ci.lin(m2)[23, 4], 5)) # model 2
  rownames(estimate_list_t2d_m2) <- NULL
  results <- dplyr::bind_rows(estimate_list_no_m2, estimate_list_pre_m2, estimate_list_t2d_m2)

  return(results)
}

effect_modification_loop_diabetes_cad_m1 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 1
  m1 <- lm(paste("log(Car_DC) ~", exposure, "*diabetes_status_who + SEX + MAP_mean + Age + N_Education_3cat"), data = data)

  model_1 <- m1
  m1_est <- t(as.data.frame(ci.exp(m1)[2, c(1:3)]))

  ## normoglycemia
  estimate_list_no_m1 <- data.frame(HRV = exposure, group = "normoglycemia", model = "model_1", round(((m1_est - 1) * 100), 4))
  names(estimate_list_no_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_no_m1) <- NULL

  ## prediabetes
  text_em_pre <- paste0(exposure, ":diabetes_status_who2 == 2")
  effect_modification_pre_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_pre)) - 1) * 100
  estimate_list_pre_m1 <- data.frame(HRV = exposure, group = "prediabetes", model = "model_1", round(effect_modification_pre_m1, 4))
  names(estimate_list_pre_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_pre_m1 <- cbind(estimate_list_pre_m1, p_value_ict = round(ci.lin(m1)[10, 4], 5)) # model 1
  rownames(estimate_list_pre_m1) <- NULL

  ## type-2 diabetes
  text_em_t2d <- paste0(exposure, ":diabetes_status_who3 == 3")
  effect_modification_t2d_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_t2d)) - 1) * 100
  estimate_list_t2d_m1 <- data.frame(HRV = exposure, group = "type-2_diabetes", model = "model_1", round(effect_modification_t2d_m1, 4))
  names(estimate_list_t2d_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_t2d_m1 <- cbind(estimate_list_t2d_m1, p_value_ict = round(ci.lin(m1)[11, 4], 5)) # model 1
  rownames(estimate_list_t2d_m1) <- NULL
  results <- dplyr::bind_rows(estimate_list_no_m1, estimate_list_pre_m1, estimate_list_t2d_m1)

  return(results)
}




effect_modification_loop_diabetes_cad_m2 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 2
  m2 <- lm(paste("log(Car_DC) ~", exposure, "*diabetes_status_who + SEX + Age  +
                  as.factor(Ethnicity_cat) + as.factor(N_Education_3cat) + bmi + N_Chol_ratio + Triglyc  +
                  TOTALPA + as.factor(N_ALCOHOL_CAT) + as.factor(smoking_4cat) +
                  med_HT +
                   med_LP + MAP_mean"), data = data)

  model_2 <- m2
  m2_est <- t(as.data.frame(ci.exp(m2)[2, c(1:3)]))

  ## normoglycemia
  estimate_list_no_m2 <- data.frame(HRV = exposure, group = "normoglycemia", model = "model_2", round(((m2_est - 1) * 100), 4))
  names(estimate_list_no_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_no_m2) <- NULL

  ## prediabetes
  text_em_pre <- paste0(exposure, ":diabetes_status_who2 == 2")
  effect_modification_pre_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_pre)) - 1) * 100
  estimate_list_pre_m2 <- data.frame(HRV = exposure, group = "prediabetes", model = "model_2", round(effect_modification_pre_m2, 4))
  names(estimate_list_pre_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_pre_m2 <- cbind(estimate_list_pre_m2, p_value_ict = round(ci.lin(m2)[22, 4], 5)) # model 2
  rownames(estimate_list_pre_m2) <- NULL

  ## type-2 diabetes
  text_em_t2d <- paste0(exposure, ":diabetes_status_who3 == 3")
  effect_modification_t2d_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_t2d)) - 1) * 100
  estimate_list_t2d_m2 <- data.frame(HRV = exposure, group = "type-2_diabetes", model = "model_2", round(effect_modification_t2d_m2, 4))
  names(estimate_list_t2d_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_t2d_m2 <- cbind(estimate_list_t2d_m2, p_value_ict = round(ci.lin(m2)[23, 4], 5)) # model 2
  rownames(estimate_list_t2d_m2) <- NULL
  results <- dplyr::bind_rows(estimate_list_no_m2, estimate_list_pre_m2, estimate_list_t2d_m2)

  return(results)
}

#' effect_modification_loop_sex
#'
#' @param data in long format
#'
#' @return model 1 and 2
#' @export results
#'
#' @examples
effect_modification_loop_sex_m1 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 1
  m1 <- lm(paste("log(CF_PWV80) ~", exposure, "* SEX + diabetes_status_who  + MAP_mean + Age + N_Education_3cat"), data = data)

  model_1 <- m1
  m1_est <- t(as.data.frame(ci.exp(m1)[2, c(1:3)]))

  ## men
  estimate_list_men_m1 <- data.frame(HRV = exposure, group = "men", model = "model_1", round(((m1_est - 1) * 100), 4))
  names(estimate_list_men_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_men_m1) <- NULL

  ## women
  text_em_women <- paste0(exposure, ":SEX2 == 2")
  effect_modification_women_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_women)) - 1) * 100
  estimate_list_women_m1 <- data.frame(HRV = exposure, group = "women", model = "model_1", round(effect_modification_women_m1, 4))
  names(estimate_list_women_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_women_m1 <- cbind(estimate_list_women_m1, p_value_ict = round(ci.lin(m1)[10, 4], 5)) # model 1
  rownames(estimate_list_women_m1) <- NULL

  results <- dplyr::bind_rows(estimate_list_men_m1, estimate_list_women_m1)


  return(results)
}




effect_modification_loop_sex_m2 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 2
  m2 <- lm(paste("log(CF_PWV80) ~", exposure, "* SEX + diabetes_status_who  + Age  +
                  as.factor(Ethnicity_cat) + as.factor(N_Education_3cat) + bmi + N_Chol_ratio + Triglyc  +
                  TOTALPA + as.factor(N_ALCOHOL_CAT) + as.factor(smoking_4cat) +
                  med_HT +
                   med_LP + MAP_mean"), data = data)

  model_2 <- m2
  m2_est <- t(as.data.frame(ci.exp(m2)[2, c(1:3)]))

  ## men
  estimate_list_men_m2 <- data.frame(HRV = exposure, group = "men", model = "model_2", round(((m2_est - 1) * 100), 4))
  names(estimate_list_men_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_men_m2) <- NULL

  ## women
  text_em_women <- paste0(exposure, ":SEX2 == 2")
  effect_modification_women_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_women)) - 1) * 100
  estimate_list_women_m2 <- data.frame(HRV = exposure, group = "women", model = "model_2", round(effect_modification_women_m2, 4))
  names(estimate_list_women_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_women_m2 <- cbind(estimate_list_women_m2, p_value_ict = round(ci.lin(m2)[22, 4], 5)) # model 2
  rownames(estimate_list_women_m2) <- NULL


  results <- dplyr::bind_rows(estimate_list_men_m2, estimate_list_women_m2)

  return(results)
}

effect_modification_loop_sex_cad_m1 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 1
  m1 <- lm(paste("log(Car_DC) ~", exposure, "* SEX + diabetes_status_who  + MAP_mean + Age + N_Education_3cat"), data = data)

  model_1 <- m1
  m1_est <- t(as.data.frame(ci.exp(m1)[2, c(1:3)]))

  ## men
  estimate_list_men_m1 <- data.frame(HRV = exposure, group = "men", model = "model_1", round(((m1_est - 1) * 100), 4))
  names(estimate_list_men_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_men_m1) <- NULL

  ## women
  text_em_women <- paste0(exposure, ":SEX2 == 2")
  effect_modification_women_m1 <- (exp(effect_modificator_coefficient(m1, exposure, text_em_women)) - 1) * 100
  estimate_list_women_m1 <- data.frame(HRV = exposure, group = "women", model = "model_1", round(effect_modification_women_m1, 4))
  names(estimate_list_women_m1)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_women_m1 <- cbind(estimate_list_women_m1, p_value_ict = round(ci.lin(m1)[10, 4], 5)) # model 1
  rownames(estimate_list_women_m1) <- NULL


  results <- dplyr::bind_rows(estimate_list_men_m1, estimate_list_women_m1)

  return(results)
}




effect_modification_loop_sex_cad_m2 <- function(data) {
  exposure <- data %>%
    dplyr::select(starts_with("ECGvar_"))

  exposure <- names(exposure)

  ## Model 2
  m2 <- lm(paste("log(Car_DC) ~", exposure, "* SEX + diabetes_status_who  + Age  +
                  as.factor(Ethnicity_cat) + as.factor(N_Education_3cat) + bmi + N_Chol_ratio + Triglyc  +
                  TOTALPA + as.factor(N_ALCOHOL_CAT) + as.factor(smoking_4cat) +
                  med_HT +
                   med_LP + MAP_mean"), data = data)

  model_2 <- m2
  m2_est <- t(as.data.frame(ci.exp(m2)[2, c(1:3)]))

  ## men
  estimate_list_men_m2 <- data.frame(HRV = exposure, group = "men", model = "model_2", round(((m2_est - 1) * 100), 4))
  names(estimate_list_men_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  rownames(estimate_list_men_m2) <- NULL

  ## women
  text_em_women <- paste0(exposure, ":SEX2 == 2")
  effect_modification_women_m2 <- (exp(effect_modificator_coefficient(m2, exposure, text_em_women)) - 1) * 100
  estimate_list_women_m2 <- data.frame(HRV = exposure, group = "women", model = "model_2", round(effect_modification_women_m2, 4))
  names(estimate_list_women_m2)[4:6] <- c("estimate", "conf.low", "conf.high")
  estimate_list_women_m2 <- cbind(estimate_list_women_m2, p_value_ict = round(ci.lin(m2)[22, 4], 5)) # model 2
  rownames(estimate_list_women_m2) <- NULL


  results <- dplyr::bind_rows(estimate_list_men_m2, estimate_list_women_m2)

  return(results)
}

# Supplemental functions

#' Heart rate correted HRV
#'
#' @param data
#'
#' @return Standardized zscore hrv
#' @export
#'
#' @examples
correction_hrv <- function(data) {
    corrected_data <- data %>%
        dplyr::select(
            "ECGvar_avg_nn", "ECGvar_std_nn", "ECGvar_rMSSD", "ECGvar_SDANN",
            "ECGvar_SDNNi", "ECGvar_SDSD", "ECGvar_NN50", "ECGvar_pNN50", "ECGvar_TP", "ECGvar_ULF", "ECGvar_VLF", "ECGvar_LF", "ECGvar_HF",
            "ECGvar_LFHF"
        ) %>%
        dplyr::mutate(
            ECGvar_cv_sdnn = 100* (ECGvar_std_nn/ECGvar_avg_nn),
            ECGvar_cv_sdann = 100* (ECGvar_SDANN/ECGvar_avg_nn),
            ECGvar_cv_rmssd = 100* (ECGvar_std_nn/ECGvar_avg_nn),
            ECGvar_cv_sdnni = 100* (ECGvar_rMSSD/ECGvar_avg_nn),
            ECGvar_cv_pNN50 = 100* (ECGvar_pNN50/ECGvar_avg_nn),
            ECGvar_cv_TP = 100* (ECGvar_TP/ECGvar_avg_nn^2),
            ECGvar_cv_HF = 100* (ECGvar_HF/ECGvar_avg_nn^2),
            ECGvar_cv_LF = 100* (ECGvar_LF/ECGvar_avg_nn^2),
            ECGvar_cv_VLF = 100* (ECGvar_VLF/ECGvar_avg_nn^2),
            ECGvar_cv_ULF = 100* (ECGvar_ULF/ECGvar_avg_nn^2),


        ) %>%
        dplyr::select(!c(
            "ECGvar_avg_nn", "ECGvar_std_nn", "ECGvar_rMSSD", "ECGvar_SDANN",
            "ECGvar_SDNNi", "ECGvar_SDSD", "ECGvar_NN50", "ECGvar_pNN50", "ECGvar_TP", "ECGvar_ULF", "ECGvar_VLF", "ECGvar_LF", "ECGvar_HF",
            "ECGvar_LFHF")
        )

    scl <- apply(corrected_data, 2, sd)
    mn <- apply(corrected_data, 2, mean)

    data_z <- as.data.frame(scale(corrected_data, scale = scl, center = mn))





    data_z_hrv <- data %>%
        mutate(mean_IBI = ECGvar_avg_nn) %>%
        dplyr::select(
            RandomID, diabetes_status_who, Age, SEX,
            Ethnicity_cat, N_Education_3cat, bmi, Tot_chol, Triglyc,
            TOTALPA, N_ALCOHOL_CAT, smoking_4cat, N_Chol_ratio, med_HT, med_LP, MAP_mean, CF_PWV80, Car_DC, med_HT_beta, HbA1c_perc,
            LDL, HDL, HbA1c_mol, Glucose_t0_FP, Glucose_t0, mean_IBI
        ) %>%
        bind_cols(data_z) %>%
        mutate(
            ECGvar_z_cv_hrv_td = (ECGvar_cv_sdnn + ECGvar_cv_rmssd + ECGvar_cv_sdann + ECGvar_cv_sdnni + ECGvar_cv_pNN50) / 5,
            ECGvar_z_cv_hrv_fq = (ECGvar_cv_TP + ECGvar_cv_ULF + ECGvar_cv_VLF + ECGvar_cv_LF + ECGvar_cv_HF) / 5
        )
}



#' Add labels on Maastricht dataset
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
labels_on_data <- function(data) {

    labelled::var_label(data$SEX) <- "Sex"
    labelled::var_label(data$Age) <- "Age (years)"
    labelled::var_label(data$Ethnicity_cat) <- "Ethnicity "
    labelled::var_label(data$N_Education_3cat) <- "Education"
    labelled::var_label(data$N_ALCOHOL_CAT) <- "Alcohol consumption"
    labelled::var_label(data$NIT_alcoholtot) <- "Alcohol total (g/day)"
    labelled::var_label(data$smoking_4cat) <-  "Smoking status"
    labelled::var_label(data$smoking_3cat) <- "Smoking status"
    labelled::var_label(data$TOTALPA) <- "Total physical activity (hours/week)"
    labelled::var_label(data$MVPA) <- "Moderate to vigorous physical activity (hours/week)"
    labelled::var_label(data$bmi) <- "BMI (kg/m\u00B2)"
    labelled::var_label(data$waist) <- "Waist (cm)"
    labelled::var_label(data$HbA1c_mol) <- "HbA1c (mmol/mol)"
    labelled::var_label(data$HbA1c_perc) <- "HbA1c (%)"
    labelled::var_label(data$Glucose_t0_FP) <- "Fasting plasma glucose (mmol/L)"
    labelled::var_label(data$LDL) <- "LDL (mmol/L)"
    labelled::var_label(data$HDL) <- "HDL (mmol/L)"
    labelled::var_label(data$Tot_chol) <- "Total cholesterol (mmol/L)"
    labelled::var_label(data$Triglyc) <- "Triglycerides (mmol/L)"
    labelled::var_label(data$N_Chol_ratio) <- "Total cholesterol-to-HDL cholesterol ratio"
    labelled::var_label(data$diabetes_status_who) <- "Glucose metabolism status"
    labelled::var_label(data$N_DM_DUR_2) <- "Duration of type-2 diabetes (only for diagnosed participants)"
    labelled::var_label(data$SBP_mean) <- "Systolic blood pressure (mmHg)"
    labelled::var_label(data$DBP_mean) <- "Diastolic blood pressure (mmHg)"
    labelled::var_label(data$MAP_mean) <- "Mean arterial pressure (mmHg)"
    labelled::var_label(data$Car_DC) <- "Carotid artery distensibility (10-3/kPa)"
    labelled::var_label(data$CF_PWV80) <- "Carotid-femoral pulse wave velocity (m/s)"
    labelled::var_label(data$N_HT) <- "Participants with hypertension (count)"
    labelled::var_label(data$med_DM) <- "Glucose lowering medication"
    labelled::var_label(data$med_HT) <- "Antihypertensive medication"
    labelled::var_label(data$med_HT_beta) <- "Use beta-blockers"
    labelled::var_label(data$med_HT_diuretic_aldos) <- "Use diuretics (aldosterone antagonists)"
    labelled::var_label(data$med_HT_diuretic) <- "Use diuretics"
    labelled::var_label(data$med_LP) <- "Lipid-lowering medication"
    labelled::var_label(data$ECGvar_avg_nn) <- "Mean IBI (ms)"
    labelled::var_label(data$ECGvar_std_nn) <- "SDNN (ms)"
    labelled::var_label(data$ECGvar_rMSSD) <- "RMSSD (ms)"
    labelled::var_label(data$ECGvar_SDANN) <- "SDANN (ms)"
    labelled::var_label(data$ECGvar_SDNNi) <- "SDNNi (ms)"
    # labelled::var_label(data$ECGvar_SDSD) <-
    # labelled::var_label(data$ECGvar_NN50) <-
    labelled::var_label(data$ECGvar_pNN50) <- "pNN50 (%)"
    labelled::var_label(data$ECGvar_TP) <-  "TP (ms\u00B2)"
    labelled::var_label(data$ECGvar_LF) <-  "LF (ms\u00B2)"
    labelled::var_label(data$ECGvar_VLF) <-  "VLF (ms\u00B2)"
    labelled::var_label(data$ECGvar_ULF) <- "ULF (ms\u00B2)"
    labelled::var_label(data$ECGvar_HF) <- "HF (ms\u00B2)"

    return(data)

}

# hrv_labels_on_data <- function(data) {
#     labelled::var_label(data$avg_nn) <- "Mean IBI (ms)"
#     labelled::var_label(data$std_nn) <- "SDNN (ms)"
#     labelled::var_label(data$rMSSD) <- "RMSSD (ms)"
#     labelled::var_label(data$SDANN) <- "SDANN (ms)"
#     labelled::var_label(data$SDNNi) <- "SDNNi (ms)"
#     # labelled::var_label(data$SDSD) <-
#     # labelled::var_label(data$NN50) <-
#     labelled::var_label(data$pNN50) <- "pNN50 (%)"
#     labelled::var_label(data$TP) <-  "TP (ms\u00B2)"
#     labelled::var_label(data$LF) <-  "LF (ms\u00B2)"
#     labelled::var_label(data$VLF) <-  "VLF (ms\u00B2)"
#     labelled::var_label(data$ULF) <- "ULF (ms\u00B2)"
#     labelled::var_label(data$HF) <- "HF (ms\u00B2)"
#
#     return(data)
#
# }

#' Recode data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
recode_data <- function(data) {
    data %>% mutate(SEX = recode(SEX, "1" = "Men", "2" = "Women"),
                    Ethnicity_cat = recode(Ethnicity_cat, "1" = "White", "2" = "Non-white"),
                    N_Education_3cat = recode(N_Education_3cat, "1"= "Low (No education, (un)completed primary education, or lower vocational education)",
                                              "2" = "Middle (intermediate vocational education or higher secondary education)",
                                              "3" = "High (Higher vocational education or university education)"),
                    N_ALCOHOL_CAT = recode(N_ALCOHOL_CAT, "0" = "None",
                                           "1" = "Low (Women: ≤ 7, Men: ≤ 14)",
                                           "2" = "High (Women: > 7, Men: > 14)"),
                    smoking_4cat = recode(smoking_4cat, "0" = "Never",
                                          "1" = "Former (quit > 6 months ago)",
                                          "2" = "Former (quit < 6 months ago)",
                                          "3" = "Current"),
                    diabetes_status_who = recode(diabetes_status_who, "1" = "Normal glucose metabolism",
                                                 "2" = "Prediabetes",
                                                 "3" = "Type 2 Diabetes"),
                    N_HT = recode(N_HT, "0" = "No", "1" = "Yes"),
                    med_DM = recode(med_DM, "0" = "No", "1" = "Yes"),
                    med_HT = recode(med_HT, "0" = "No", "1" = "Yes"),
                    med_HT_beta = recode(med_HT_beta, "0" = "No", "1" = "Yes"),
                    med_HT_diuretic_aldos = recode(med_HT_diuretic_aldos, "0" = "No", "1" = "Yes"),
                    med_HT_diuretic = recode(med_HT_diuretic, "0" = "No", "1" = "Yes"),
                    med_LP = recode(med_LP, "0" = "No", "1" = "Yes")
    )

}
