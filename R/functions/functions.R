kccq_score_function <- function(data) {

    data_work <- data %>%
        dplyr::mutate(hf_symptoms = as.factor(hf_symptoms))


    data_kccq_symp <- data_work %>%
        dplyr::filter(hf_symptoms== 1) %>%

        dplyr::rowwise() %>%
        dplyr::mutate(
            Q1a_rescaled = 100 * (sum(kccq_shower[c(kccq_shower) > 0], na.rm = T)-1) / 4,
            Q1a_rescaled = ifelse(is.na(kccq_shower) | Q1a_rescaled < 0, NA, Q1a_rescaled),

            Q1b_rescaled = 100 * (sum(kccq_walk[c(kccq_walk) > 0], na.rm = T)-1) / 4,
            Q1b_rescaled = ifelse(is.na(kccq_walk) | Q1b_rescaled < 0, NA, Q1b_rescaled),

            Q1c_rescaled = 100 * (sum(kccq_jog[c(kccq_jog) > 0], na.rm = T)-1) / 4,
            Q1c_rescaled = ifelse(is.na(kccq_jog) | Q1c_rescaled < 0, NA, Q1c_rescaled),

            # KCCQ12-SF = average of rescaled responses
            Physical_Limitation_Score = mean(c(Q1a_rescaled, Q1b_rescaled, Q1c_rescaled), na.rm = TRUE),
            Physical_Limitation_Score = ifelse(sum(is.na(Q1a_rescaled),is.na(Q1b_rescaled), is.na(Q1c_rescaled))>= 2, NA, Physical_Limitation_Score),

            Physical_Limitation_Score = mean(c(Q1a_rescaled, Q1b_rescaled, Q1c_rescaled), na.rm = TRUE),
            Physical_Limitation_Score = ifelse(sum(is.na(Q1a_rescaled),is.na(Q1b_rescaled), is.na(Q1c_rescaled))>= 2, NA, Physical_Limitation_Score),

            Q2_rescaled = 100 * ((sum(kccq_swelling_freq, na.rm = T)+1)-1) / 4, # add plus one to correct categorization
            Q2_rescaled = ifelse(is.na(kccq_swelling_freq), NA, Q2_rescaled),

            Q3_rescaled = 100 * (sum(kccq_fatigue_freq, na.rm = T) - 1) / 6,
            Q3_rescaled = ifelse(is.na(kccq_fatigue_freq), NA, Q3_rescaled),

            Q4_rescaled = 100 * (sum(kccq_sob_freq, na.rm = T) - 1) / 6,
            Q4_rescaled = ifelse(is.na(kccq_sob_freq), NA, Q4_rescaled),

            Q5_rescaled = 100 * (sum(kccq_sleep_sit_freq, na.rm = T) - 1) / 4,
            Q5_rescaled = ifelse(is.na(kccq_sleep_sit_freq), NA, Q5_rescaled),

            Symptom_Frequency_Score = mean(c(Q2_rescaled, Q3_rescaled, Q4_rescaled, Q5_rescaled), na.rm = TRUE),
            Symptom_Frequency_Score = ifelse(sum(is.na(Q2_rescaled),is.na(Q3_rescaled),
                                                 is.na(Q4_rescaled),is.na(Q5_rescaled))>= 2, NA, Symptom_Frequency_Score),

            Q6_rescaled = 100 * (sum(kccq_qol1, na.rm = T)-1) / 4,
            Q6_rescaled = ifelse(is.na(kccq_qol1), NA, Q6_rescaled),

            Q7_rescaled = 100 * (sum(kccq_qol2, na.rm = T) - 1) / 4,
            Q7_rescaled = ifelse(is.na(kccq_qol2), NA, Q7_rescaled),

            Quality_Life_Score = mean(c(Q6_rescaled, Q7_rescaled), na.rm = TRUE),
            Quality_Life_Score = ifelse(sum(is.na(Q6_rescaled),is.na(Q7_rescaled))== 2, NA, Quality_Life_Score),


            Q8a_rescaled = 100 * (sum(kccq_qol3_hobbies[c(kccq_qol3_hobbies) > 0], na.rm = T)-1) / 4,
            Q8a_rescaled = ifelse(is.na(kccq_qol3_hobbies) | Q8a_rescaled < 0, NA, Q8a_rescaled),

            Q8b_rescaled = 100 * (sum(kccq_qol3_chores[c(kccq_qol3_chores) > 0], na.rm = T)-1) / 4,
            Q8b_rescaled = ifelse(is.na(kccq_qol3_chores) | Q8b_rescaled < 0, NA, Q8b_rescaled),

            Q8c_rescaled = 100 * (sum(kccq_qol3_visiting[c(kccq_qol3_visiting) > 0], na.rm = T)-1) / 4,
            Q8c_rescaled = ifelse(is.na(kccq_qol3_visiting) | Q8c_rescaled < 0, NA, Q8c_rescaled),

            Social_Limitation_Score = mean(c(Q8a_rescaled, Q8b_rescaled, Q8c_rescaled), na.rm = TRUE),
            Social_Limitation_Score = ifelse(sum(is.na(Q8a_rescaled), is.na(Q8b_rescaled), is.na(Q8c_rescaled))>= 2, NA, Social_Limitation_Score),


            Overall_Summary_Score = mean(c(Physical_Limitation_Score, Symptom_Frequency_Score, Quality_Life_Score, Social_Limitation_Score), na.rm = TRUE),
            Overall_Summary_Score = ifelse(sum(is.na(Physical_Limitation_Score), is.na(Symptom_Frequency_Score),
                                               is.na(Quality_Life_Score), is.na(Social_Limitation_Score))>= 2, NA, Overall_Summary_Score),

            Clinical_Summary_Score = mean(c(Physical_Limitation_Score, Symptom_Frequency_Score), na.rm = TRUE),
            Clinical_Summary_Score = ifelse(sum(is.na(Physical_Limitation_Score), is.na(Symptom_Frequency_Score))>= 2, NA, Clinical_Summary_Score)
        ) %>%
        dplyr::ungroup() %>%
        rename(Q1a_shower_rescaled = Q1a_rescaled,
               Q1b_walking_rescaled = Q1b_rescaled,
               Q1c_Jogging_rescaled = Q1c_rescaled,
               Q2_swelling_rescaled = Q2_rescaled,
               Q3_fatigue_rescaled = Q3_rescaled,
               Q4_shortness_breath_rescaled = Q4_rescaled,
               Q5_sleep_shortness_breath_rescaled = Q5_rescaled,
               Q6_enjoyment_rescaled = Q6_rescaled,
               Q7_living_condition_rescaled = Q7_rescaled,
               Q8a_affect_hobbies_rescaled = Q8a_rescaled,
               Q8b_affect_working_rescaled = Q8b_rescaled,
               Q8c_affect_visiting_rescaled = Q8c_rescaled) %>%
        dplyr::mutate(rec_id = record_id)


    data_kccq_no_symp <- data_work %>%
    dplyr::filter(hf_symptoms == 0) %>%
   dplyr::mutate(Q1a_shower_rescaled = 100,
    Q1b_walking_rescaled = 100,
    Q1c_Jogging_rescaled = 100,
    Q2_swelling_rescaled = 100,
    Q3_fatigue_rescaled = 100,
    Q4_shortness_breath_rescaled = 100,
    Q5_sleep_shortness_breath_rescaled = 100,
    Q6_enjoyment_rescaled = 100,
    Q7_living_condition_rescaled = 100,
    Q8a_affect_hobbies_rescaled = 100,
    Q8b_affect_working_rescaled = 100,
    Q8c_affect_visiting_rescaled = 100,
    Physical_Limitation_Score = 100,
    Symptom_Frequency_Score = 100,
    Quality_Life_Score = 100,
    Social_Limitation_Score = 100,
    Overall_Summary_Score  = 100,
    Clinical_Summary_Score  = 100)

    data_no_quest <- data_work %>%
        dplyr::filter(is.na(hf_symptoms))

    final_data <- dplyr::bind_rows(data_kccq_symp, data_kccq_no_symp, data_no_quest)

    return(final_data)
}

#' Echo wrangling
#'
#' @param echo_data
#'
#' @return
#' @export
#'
#' @examples
echo_data_transformation <- function(echo_data) {

    echo_data %>%
        dplyr::mutate(id_num = floor(record_id),
                      time_point = ifelse(record_id %% 1 == 0, "baseline", "follow_up")) %>%
        dplyr::mutate(
            LAE_lf_art_enlarg = case_when(laesvialmlm >= 34 ~ "LAE",
                                          laesvialmlm < 34 ~ "Normal"),

            LAE_lf_art_enlarg = fct_relevel(LAE_lf_art_enlarg, "LAE", "Normal"),

            LVH_lf_vent_hyptro = case_when(gender == 1 & lvdmassindexgm >= 115 ~ "LVH",
                                              gender == 2 & lvdmassindexgm >= 95 ~ "LVH",
                                               gender == 1 & lvdmassindexgm < 115 ~ "Normal",
                                               gender == 2 & lvdmassindexgm < 95 ~ "Normal"),
            LVH_lf_vent_hyptro = fct_relevel(LVH_lf_vent_hyptro, "LVH", "Normal"),

            DD_dia_dys = case_when(eepsept >= 13 ~ "DD",
                                   eepsept < 13 ~ "Normal"),
            DD_dia_dys = fct_relevel(DD_dia_dys, "DD", "Normal"),

           glpsavgafi = ifelse(glpsavgafi>0, glpsavgafi*(-1), glpsavgafi), # make positive values to negative if they are above 0
           GLS_imparied = case_when(glpsavgafi >= -16 ~ "imparied",
                                    glpsavgafi < -16 ~ "Normal"),
           GLS_imparied = fct_relevel(GLS_imparied, "imparied", "Normal"),

           GLS_abnormal = case_when(glpsavgafi >= -18 ~ "abnormal",
                                    glpsavgafi < -18 ~ "Normal"),
           GLS_abnormal = fct_relevel(GLS_abnormal, "abnormal","Normal"),

            HFrEF = case_when(lvef4dautolvq <= 40 ~ "HFrEF",
                            lvef4dautolvq > 40 ~ "no HFrEF"),
            HFrEF = fct_relevel(HFrEF,"HFrEF", "no HFrEF"),

            HFmrEF = case_when(lvef4dautolvq < 50 & lvef4dautolvq > 40 ~ "HFmrEF",
                  lvef4dautolvq >= 50 ~ "no HFmrEF"),
            HFmrEF = fct_relevel(HFrEF,"HFmrEF", "no HFmrEF"),

            # HFpEF need work
            HFpEF  = case_when(lvef4dautolvq >= 50 &
                LVH_lf_vent_hyptro== "LVH" ~ "HFpEF",
                lvef4dautolvq >= 50 & mveeavg > 9 ~ "HFpEF",
                LVH_lf_vent_hyptro== "normal" & lvef4dautolvq >= 50 ~ "no HFpEF",
                 mveeavg <= 9 &  lvef4dautolvq >= 50 ~ "no HFpEF"),

            HFpEF = fct_relevel(HFpEF,"HFpEF", "no HFpEF"), # Something is wrong prevalence of HFrEF are very high

            HFmrEF_rEF = case_when(lvef4dautolvq < 50 ~ "HFmrEF or HFrEF",
                   lvef4dautolvq >= 50 ~ "no HFmrEF or HFrEF"),
            HFmrEF_rEF = fct_relevel(HFmrEF_rEF,"HFmrEF or HFrEF", "no HFmrEF or HFrEF"),

            any_HF = case_when(HFmrEF_rEF == "HFmrEF or HFrEF" | HFpEF == "HFpEF" ~ "Heart failure",
                               HFmrEF_rEF == "no HFmrEF or HFrEF" & HFpEF == "no HFpEF" ~ "No heart failure"),
            any_HF = fct_relevel(any_HF,"Heart failure", "No heart failure")

        )

}

#' Echo wrangling
#'
#' @param echo_data
#'
#' @return
#' @export
#'
#' @examples
echo_data_transformation_table <- function(echo_data) {

    echo_data %>%
        dplyr::mutate(id_num = floor(record_id),
                      time_point = ifelse(record_id %% 1 == 0, "baseline", "follow_up")) %>%
        dplyr::mutate(
            LAE_lf_art_enlarg = case_when(laesvialmlm >= 34 ~ "LAE",
                                          laesvialmlm < 34 ~ "Normal_LA"),

            LAE_lf_art_enlarg = fct_relevel(LAE_lf_art_enlarg, "LAE", "Normal_LA"),

            LVH_lf_vent_hyptro = case_when(gender == 1 & lvdmassindexgm >= 115 ~ "LVH",
                                           gender == 2 & lvdmassindexgm >= 95 ~ "LVH",
                                           gender == 1 & lvdmassindexgm < 115 ~ "Normal_LV",
                                           gender == 2 & lvdmassindexgm < 95 ~ "Normal_LV"),
            LVH_lf_vent_hyptro = fct_relevel(LVH_lf_vent_hyptro, "LVH", "Normal_LV"),

            DD_dia_dys = case_when(eepsept >= 13 ~ "DD",
                                   eepsept < 13 ~ "Normal_DF"),
            DD_dia_dys = fct_relevel(DD_dia_dys, "DD", "Normal_DF"),

            glpsavgafi = ifelse(glpsavgafi>0, glpsavgafi*(-1), glpsavgafi), # make positive values to negative if they are above 0
            GLS_imparied = case_when(glpsavgafi >= -16 ~ "imparied",
                                     glpsavgafi < -16 ~ "Normal_GLS"),
            GLS_imparied = fct_relevel(GLS_imparied, "imparied_GLS", "Normal_GLS"),

            GLS_abnormal = case_when(glpsavgafi >= -18 ~ "abnormal",
                                     glpsavgafi < -18 ~ "Normal_GLS"),
            GLS_abnormal = fct_relevel(GLS_abnormal, "abnormal","Normal_GLS"),

            HFrEF = case_when(lvef4dautolvq <= 40 ~ "HFrEF",
                              lvef4dautolvq > 40 ~ "no HFrEF"),
            HFrEF = fct_relevel(HFrEF,"HFrEF", "no HFrEF"),

            HFmrEF = case_when(lvef4dautolvq < 50 & lvef4dautolvq > 40 ~ "HFmrEF",
                               lvef4dautolvq >= 50 ~ "no HFmrEF"),
            HFmrEF = fct_relevel(HFrEF,"HFmrEF", "no HFmrEF"),

            # HFpEF need work
            HFpEF  = case_when(lvef4dautolvq >= 50 &
                                   LVH_lf_vent_hyptro== "LVH" ~ "HFpEF",
                               lvef4dautolvq >= 50 & mveeavg > 9 ~ "HFpEF",
                               LVH_lf_vent_hyptro== "Normal_LV" & lvef4dautolvq >= 50 ~ "no HFpEF",
                               mveeavg <= 9 &  lvef4dautolvq >= 50 ~ "no HFpEF"),

            HFpEF = fct_relevel(HFpEF,"HFpEF", "no HFpEF"), # Something is wrong prevalence of HFrEF are very high

            HFmrEF_rEF = case_when(lvef4dautolvq < 50 ~ "HFmrEF or HFrEF",
                                   lvef4dautolvq >= 50 ~ "no HFmrEF or HFrEF"),
            HFmrEF_rEF = fct_relevel(HFmrEF_rEF,"HFmrEF or HFrEF", "no HFmrEF or HFrEF"),

            any_HF = case_when(HFmrEF_rEF == "HFmrEF or HFrEF" | HFpEF == "HFpEF" ~ "Heart failure",
                               HFmrEF_rEF == "no HFmrEF or HFrEF" & HFpEF == "no HFpEF" ~ "No heart failure"),
            any_HF = fct_relevel(any_HF,"Heart failure", "No heart failure")

        )

}

create_recipe_spec <- function(data, kccq_variable, echo_marker) {

    recipes::recipe(data) %>%

        recipes::update_role({{ kccq_variable }}, gender, age,
                             new_role = "predictor"
        ) %>%
        recipes::step_normalize({{ echo_marker }}) %>%
        recipes::update_role({{ echo_marker }}, new_role = "outcome") %>%
        recipes::update_role(record_id, new_role = "id variable")

    #%>% # {{arterial_marker}}
    #   # brecipes::step_normalize(tidyselect::starts_with("ECGvar_")) %>% # standardization done before with collected z-score
    #  recipes::step_log(CF_PWV80)
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
    estimate_ci <- workflow_fitted_model %>%
        workflows::extract_fit_parsnip() %>%
        broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
        bind_cols(workflow_fitted_model %>%
                      workflows::extract_fit_parsnip() %>%
                      broom::glance() %>%
                      dplyr::select(-c(statistic, p.value)))
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results <- function(data) { # , arterial_marker
    create_model_workflow(
        parsnip::linear_reg() %>%
            parsnip::set_engine("lm"),
        data %>%
            create_recipe_spec(kccq_variable = tidyselect::starts_with("t_kccq_"), echo_marker = tidyselect::starts_with("echo_")) # dobbelt check, {{arterial_marker}}
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
df_kccq_long_format <- function(data) {

    data %>%
        pivot_longer(cols = tidyselect::starts_with("t_kccq_"), names_to = "kccq", values_to = "kccq_domain")

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
kccq_to_wide <- function(data) {
    data %>%
        tidyr::pivot_wider(
            names_from = kccq,
            values_from = kccq_domain,
        )
}
#' Convert the long form dataset into a list of wide form data frames.
#'
#' @param data The lipidomics dataset.
#'
#' @return A list of data frames.
#'
split_by_kccq <- function(data) {
    data %>%
        dplyr::group_split(kccq) %>%
        purrr::map(kccq_to_wide)
} # go through


#' Loop analysis with multiple analysis in one go
#'
#' @param data
#'
#' @return estimates and p-values
#' @export a table
#'
#' @examples
loop_analysis_kccq <- function(data) { # , arterial_marker
    data %>%
        split_by_kccq() %>%
        purrr::map(generate_model_results) %>% # {{arterial_marker}}
        list_rbind() %>%
        dplyr::filter(stringr::str_detect(term, "t_kccq_"))
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
        mutate(term = kccq) %>%
        mutate(term = str_c("t_kccq_", term)) %>%
        # distinct(term, kccq) %>%
        mutate(term = stringr::str_remove("t_kccq_", term)) %>%
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
calculate_estimates <- function(data) {
    data %>%
        loop_analysis_kccq() %>%
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
            estimate = estimate,
            conf.low = conf.low,
            conf.high = conf.high,
            p.value = round(p.value, 4)
        ) %>%
    dplyr::mutate(kccq = stringr::str_replace(term, "t_kccq_", "")) %>%
    dplyr::arrange(match(kccq, c(
        "Q1a_shower_rescaled", "Q1b_walking_rescaled", "Q1c_Jogging_rescaled", "Physical_Limitation_Score", "Q2_swelling_rescaled",
        "Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled", "Symptom_Frequency_Score", "Q6_enjoyment_rescaled",
        "Q7_living_condition_rescaled", "Quality_Life_Score", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled","Social_Limitation_Score",
        "Overall_Summary_Score", "Clinical_Summary_Score"))) %>%
    dplyr::mutate(kccq = factor(kccq, levels = kccq))
}

arrange_and_exponetiate_overall <- function(results) {
    results %>%
        dplyr::filter(term == "t_kccq_Physical_Limitation_Score"|term == "t_kccq_Symptom_Frequency_Score"| term =="t_kccq_Quality_Life_Score"|
                      term =="t_kccq_Social_Limitation_Score"| term =="t_kccq_Overall_Summary_Score"| term =="t_kccq_Clinical_Summary_Score") %>%
        dplyr::mutate(
            estimate = estimate,
            conf.low = conf.low,
            conf.high = conf.high,
            p.value = round(p.value, 4)
        ) %>%
        dplyr::mutate(kccq = stringr::str_replace(term, "t_kccq_", "")) %>%
        dplyr::arrange(match(kccq, c("Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                                     "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score"))) %>%
        dplyr::mutate(kccq = factor(kccq, levels = kccq))
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
            y = kccq,
            xmin = conf.low,
            xmax = conf.high,
        ), ) +
        geom_point() +
        geom_pointrange() +
        geom_text(aes( label = paste0("R2 = ", round(r.squared, 2),"; Obs = ", nobs), y = kccq), hjust = -0.2, nudge_y = 0.30, size = 3) +
        coord_cartesian(clip = 'off') +
        scale_y_discrete(labels = rev(c(
            "Q1a_shower_rescaled", "Q1b_walking_rescaled", "Q1c_Jogging_rescaled", "Physical_Limitation_Score", "Q2_swelling_rescaled",
            "Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled", "Symptom_Frequency_Score", "Q6_enjoyment_rescaled",
            "Q7_living_condition_rescaled", "Quality_Life_Score", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled","Social_Limitation_Score",
            "Overall_Summary_Score", "Clinical_Summary_Score")), limits = rev) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        theme_bw() +
        theme(
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y = element_blank()
        )
}

plot_estimates_overall <- function(results) {
    results %>%
        ggplot(aes(
            x = estimate,
            y = kccq,
            xmin = conf.low,
            xmax = conf.high,
        ), ) +
        geom_point() +
        geom_pointrange() +
        geom_text(aes( label = paste0("R2 = ", round(r.squared, 2),"; Obs = ", nobs), y = kccq), hjust = -0.2, nudge_y = 0.30, size = 3) +
        coord_cartesian(clip = 'off') +
        scale_y_discrete(labels = rev(c("Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                                        "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")), limits = rev) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        theme_bw() +
        theme(
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y = element_blank()
        )
}
#' lm_function with multiple outcomes
#'
#' @param data
#' @param echo_outcome
#' @param hf_symp
#'
#' @return
#' @export
#'
#' @examples
lm_function <- function(data, echo_outcome, hf_symp) {
    lm_model <- lm(as.formula(paste("scale(",echo_outcome,")", "~", hf_symp)), data = data)

    results <- broom::tidy(lm_model, conf.int = TRUE, conf.level = 0.95) %>%
        dplyr::filter(!term == "(Intercept)") %>%
        dplyr::mutate(outcome = paste0(echo_outcome)) %>%
        bind_cols(broom::glance(lm_model) %>%
         dplyr::select(-c(statistic, p.value))) %>%
        dplyr::select(outcome,estimate,conf.low, conf.high, p.value, nobs)
    return(results)
}


#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted.
#'
#' @return A data frame.
#'
tidy_model_output_or <- function(workflow_fitted_model) {
    estimate_ci <- workflow_fitted_model %>%
        workflows::extract_fit_parsnip() %>%
        broom::tidy(conf.int = TRUE, conf.level = 0.95, exponentiate  = TRUE)# %>%
                                 # dplyr::select(-c(statistic, p.value)))# %>%
        # bind_cols(workflow_fitted_model %>%
        #               workflows::extract_fit_parsnip() %>%
        #               broom::glance())
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results_or <- function(data) { # , arterial_marker
    create_model_workflow(
        parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm"),
        data %>%
            create_recipe_spec(kccq_variable = tidyselect::starts_with("t_kccq_"), echo_marker = tidyselect::starts_with("echo_")) # dobbelt check, {{arterial_marker}}
    ) %>%
        parsnip::fit(data) %>%
        tidy_model_output_or() %>%
        bind_cols(nrow(data))
}






#' Loop analysis with multiple analysis in one go
#'
#' @param data
#'
#' @return estimates and p-values
#' @export a table
#'
#' @examples
loop_analysis_kccq_or <- function(data) { # , arterial_marker
    data %>%
        split_by_kccq() %>%
        purrr::map(generate_model_results_or) %>% # {{arterial_marker}}
        list_rbind() %>%
        dplyr::filter(stringr::str_detect(term, "t_kccq_"))
}

#' calculate estimates and get names combined
#'
#' @param data and models
#'
#' @return table with estimates
#' @export
#'
#' @examples
calculate_estimates_or <- function(data) {
    data %>%
        loop_analysis_kccq_or() %>%
        add_original_hrv_names(data)
}



#' Forrest function for results by group
#'
#' @param results
#'
#' @return
#' @export forrest plot
#'
#' @examples
plot_estimates_or <- function(results) {
    results %>%
        ggplot(aes(
            x = estimate,
            y = kccq,
            xmin = conf.low,
            xmax = conf.high,
        ), ) +
        geom_point() +
        geom_pointrange() +
        geom_text(aes(label = paste0("Obs = ", nobs), y = kccq), hjust = -0.2, nudge_y = 0.30, size = 3) +
        coord_trans(x = "log10" ,clip = 'off') +
        scale_y_discrete(labels = rev(c(
            "Q1a_shower_rescaled", "Q1b_walking_rescaled", "Q1c_Jogging_rescaled", "Physical_Limitation_Score", "Q2_swelling_rescaled",
            "Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled", "Symptom_Frequency_Score", "Q6_enjoyment_rescaled",
            "Q7_living_condition_rescaled", "Quality_Life_Score", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled","Social_Limitation_Score",
            "Overall_Summary_Score", "Clinical_Summary_Score")), limits = rev) +
        geom_vline(xintercept = 1, linetype = "dotted") +
        theme_bw() +
        theme(
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y = element_blank()
        )
}


#' confusion matrix
#'
#' @param data
#' @param test
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
con_matrix<- function(data, test, outcome)
{
    test <- as.factor(data[[test]])
    reference <- factor(data[[outcome]], labels = c(0,1))
    test <- fct_rev(test)
    reference <- fct_rev(reference)

    output <- confusionMatrix(test, reference)

    return(output)
}

#' Prediction performance AU Logistic regression
#'
#' @param data
#' @param echo_metric
#'
#' @return
#' @export
#'
#' @examples
prediction_performance_logistic <- function(data, echo_metric)
{
    results <- generate_model_or(data, echo_metric)

    fit_model <- results %>%
        parsnip::fit(data)

    estimates <- fit_model %>%
        tidy_model_output_or() %>%
        slice(-1)

    resamples_bootstrap <- rsample::bootstraps(data, times = 100)

    performance <- fit_resamples(fit_model, resamples_bootstrap, metrics = metric_set(roc_auc, pr_auc, accuracy, sensitivity, specificity) )%>%
        collect_metrics()

    output <- list(estimates = estimates,
                   performance = performance)

    return(performance)
}

create_recipe_spec_log <- function(data, echo_marker) {

    predictors <- setdiff(names(data), {{ echo_marker }})

    recipes::recipe(data) %>%
        #recipes::step_normalize({{ kccq_variable }}) %>%
        recipes::update_role({{ echo_marker }}, new_role = "outcome") %>%
        recipes::update_role(all_of(predictors), new_role = "predictor") %>%
        recipes::update_role(record_id, new_role = "id variable")

}

#' Generate the results of a model
#'
#' @param data
#'
#' @return A data frame.
#'
generate_model_or <- function(data, echo_marker) { # , arterial_marker
    create_model_workflow(
        parsnip::logistic_reg(mode = "classification") %>%
            parsnip::set_engine("glm"),
        data %>%
            create_recipe_spec_log(echo_marker = {{echo_marker}})
    )
}

#' Prediction from KCCQ questionnaire
#'
#' @param data
#' @param covariates
#' @param kccq_domain
#' @param echo_score
#'
#' @return
#' @export
#'
#' @examples
prediction_model_LR <- function(data, covariates, kccq_domain, echo_score){

    # covariates <- c("age","gender")
    # kccq_domain <- c("Symptom_Frequency_Score")
    # echo_score <- c("GLS_imparied")


    data_pred <- data %>%
        select(all_of(c(covariates, kccq_domain, echo_score))) %>%
        drop_na() %>%
        mutate(across(
            .cols = all_of(kccq_domain) %>% keep(~ str_detect(.x, "score|rescaled")),
            .fns = log
        ))

    model_specs <- parsnip::logistic_reg(mode = "classification") %>%
        parsnip::set_engine("glm")

    # Create the formula dynamically
    formula <- as.formula(paste(echo_score, "~ ."))

    # Define the recipe
    recipe_specs <- recipes::recipe(formula, data = data_pred)#%>%
       # recipes::step_log(all_of(kccq_domain), offset = 1)

    workflow_status <- workflows::workflow() %>%
        workflows::add_model(model_specs) %>%
        workflows::add_recipe(recipe_specs) %>%
        fit(data_pred)

    resamples_bootstrap <- rsample::bootstraps(data_pred, times = 100)

    performance <- fit_resamples(workflow_status, resamples_bootstrap, metrics = metric_set(roc_auc, pr_auc, accuracy, sensitivity, specificity) )%>%
        collect_metrics()

    performance <- performance %>%
        dplyr::mutate(KCCQ = paste(kccq_domain),
                      Echo = paste(echo_score),
                      numbers = paste(nrow(data_pred)),
                      est = mean,
                      low_ci = mean - std_err*1.96,
                      high_ci = mean + std_err*1.96)

    return(performance)
}


#' Prediction across
#'
#' @param data
#' @param echo_marker
#'
#' @return
#' @export
#'
#' @examples
prediction_LR_across_echo <- function(data, echo_marker) {
    kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                              "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")
    covariates <- c("age","gender")



    results_age_sex <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_kccq_domain")



    ## include sex and age. prior heart attack, anti-hypertensives
    covariates_cvd <- c("age","gender","prior_cvd")


    results_age_sex_cvd <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates_cvd, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_cvd_kccq_domain")

    covariates_cvd_bmi <- c("age","gender","prior_cvd","bmi")


    results_age_sex_cvd_bmi <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates_cvd_bmi, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_cvd_bmi_kccq_domain")

    covariates_cvd_bmi_diabetes_dur <- c("age","gender","prior_cvd","bmi", "diabetes_duration")

    results_age_sex_cvd_bmi_diabetes_dur <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates_cvd_bmi_diabetes_dur, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_cvd_bmi_diabetes_dur_kccq_domain")

    ##################################### ALL KCCQ questions

    kccq_summary <- c("dummy_score", "hf_symptoms","Q1a_shower_rescaled",
                      "Q1b_walking_rescaled", "Q1c_Jogging_rescaled","Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled","Q6_enjoyment_rescaled",
                      "Q7_living_condition_rescaled", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled")

    covariates <- c("age","gender")

    # Use purrr::map to apply the function for each column to exclude
    results_age_sex_questionnaire <- map(kccq_summary, ~ {
        prediction_model_LR(data, covariates, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind()%>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_kccq_quest")

    ## Prior CVD

    results_age_sex_cvd_quest <- map(kccq_summary, ~ {
        prediction_model_LR(data, covariates_cvd, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity")) %>%
        mutate(prediction_model = "age_sex_cvd_kccq_quest")

    KCCQ_performance <- rbind(results_age_sex, results_age_sex_cvd, results_age_sex_cvd_bmi, results_age_sex_cvd_bmi_diabetes_dur, results_age_sex_questionnaire, results_age_sex_cvd_quest)

    return(KCCQ_performance)

}



prediction_LR_across_echo_kccq_symp <- function(data, echo_marker) {
    kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_symp", "Symptom_Frequency_symp", "Quality_Life_symp",
                              "Social_Limitation_symp", "Overall_Summary_symp", "Clinical_Summary_symp")
    covariates <- c("age","gender")



    results_age_sex <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric %in% c("roc_auc", "sensitivity", "specificity"))%>%
        mutate(prediction_model = "age_sex_kccq_domain")



    ## include sex and age. prior heart attack, anti-hypertensives
    covariates_cvd <- c("age","gender","prior_cvd")


    results_age_sex_cvd <- map(kccq_summary_sex_age, ~ {
        prediction_model_LR(data, covariates_cvd, kccq_domain = .x,
                            echo_marker
        )
    }) %>%
        list_rbind() %>%
        filter(.metric== "roc_auc")%>%
        mutate(prediction_model = "age_sex_cvd_kccq_domain")
    #
    # ##################################### ALL KCCQ questions
    #
    # kccq_summary <- c("dummy_score", "hf_symptoms","Q1a_shower_rescaled",
    #                   "Q1b_walking_rescaled", "Q1c_Jogging_rescaled","Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled","Q6_enjoyment_rescaled",
    #                   "Q7_living_condition_rescaled", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled")
    #
    # covariates <- c("age","gender")
    #
    # # Use purrr::map to apply the function for each column to exclude
    # results_age_sex_questionnaire <- map(kccq_summary, ~ {
    #     prediction_model_LR(study_data_complete, covariates, kccq_domain = .x,
    #                         echo_marker
    #     )
    # }) %>%
    #     list_rbind()%>%
    #     filter(.metric== "roc_auc")%>%
    #     mutate(prediction_model = "age_sex_kccq_quest")
    #
    # ## Prior CVD
    #
    # results_age_sex_cvd_quest <- map(kccq_summary, ~ {
    #     prediction_model_LR(study_data_complete, covariates_cvd, kccq_domain = .x,
    #                         echo_marker
    #     )
    # }) %>%
    #     list_rbind() %>%
    #     filter(.metric== "roc_auc") %>%
    #     mutate(prediction_model = "age_sex_cvd_kccq_quest")

    KCCQ_performance <- rbind(results_age_sex, results_age_sex_cvd
                              #,results_age_sex_questionnaire, results_age_sex_cvd_quest
                              )

    return(KCCQ_performance)

}



calculate_watch_dm_risk_score <- function(df) {
    df %>%
        mutate(FPG = glucosef*18.015,
               Serum_Cr = creat/88.42,
               HDL_C = hdl*38.684
        ) %>%
        mutate(
            Age_Score = case_when(
                age < 50 ~ 0,
                age >= 50 & age <= 54 ~ 1,
                age >= 55 & age <= 59 ~ 2,
                age >= 60 & age <= 64 ~ 3,
                age >= 65 & age <= 69 ~ 4,
                age >= 70 & age <= 74 ~ 5,
                age >= 75 ~ 6
            ),
            BMI_Score = case_when(
                bmi < 25 ~ 0,
                bmi >= 25 & bmi <= 34 ~ 1,
                bmi >= 35 & bmi <= 39 ~ 2,
                bmi >= 40 ~ 5
            ),
            SBP_Score = case_when(
                bp_syst_mean < 100 ~ 0,
                bp_syst_mean >= 100 & bp_syst_mean <= 139 ~ 1,
                bp_syst_mean >= 140 & bp_syst_mean <= 159 ~ 2,
                bp_syst_mean >= 160 ~ 5
            ),
            FPG_Score = case_when(
                FPG < 125 ~ 0,
                FPG >= 125 & FPG <= 199 ~ 1,
                FPG >= 200 & FPG <= 299 ~ 2,
                FPG >= 300 ~ 5
            ),
            Serum_Cr_Score = case_when(
                Serum_Cr < 1.0 ~ 0,
                Serum_Cr >= 1.0 & Serum_Cr <= 1.49 ~ 2,
                Serum_Cr >= 1.50 ~ 5
            ),
            DBP_Score = case_when(
                bp_diast_mean < 60 ~ 2,
                bp_diast_mean >= 60 & bp_diast_mean <= 80 ~ 0,
                bp_diast_mean > 80 ~ 2
            ),
            HDL_C_Score = case_when(
                HDL_C < 30 ~ 2,
                HDL_C >= 30 & HDL_C <= 59 ~ 1,
                HDL_C >= 60 ~ 0
            ),
            # QRS_Score = case_when(
            #     QRS >= 120 ~ 3,
            #     QRS < 120 ~ 0
            # ),
            Prior_MI_Score = if_else(heart_attack == 1, 3, 0),
            Prior_CABG_Score = if_else(heart_bypass == 1, 2, 0),
            watch_dm_risk_score = Age_Score + BMI_Score + SBP_Score + FPG_Score +
                Serum_Cr_Score + DBP_Score + HDL_C_Score +
                #QRS_Score +
                Prior_MI_Score + Prior_CABG_Score
        )
}


# Function to calculate the risk score
calculate_ARIC_HF_risk_score <- function(df, coeffs) {
    df %>%
        mutate(
            Risk_Score = Age * coeffs$Age +
                Black * coeffs$Black +
                Male * coeffs$Male +
                HeartRate * coeffs$HeartRate +
                SBP * coeffs$SBP +
                BP_Medication * coeffs$BP_Medication +
                Diabetes * coeffs$Diabetes +
                CHD * coeffs$CHD +
                CurrentSmoker * coeffs$CurrentSmoker +
                FormerSmoker * coeffs$FormerSmoker +
                BMI * coeffs$BMI
        )
}
