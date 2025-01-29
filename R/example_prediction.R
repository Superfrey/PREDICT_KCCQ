
study_data_complete <- study_data_complete %>%
    mutate(dummy_score = 1) %>%
    mutate(Physical_Limitation_symp = case_when(Physical_Limitation_Score< 96 ~ "1",
                                                Physical_Limitation_Score >= 96 ~ "0"),

           Symptom_Frequency_symp=case_when(Symptom_Frequency_Score < 97 ~ "1",
                                            Symptom_Frequency_Score >= 97 ~ "0"),

           Quality_Life_symp=case_when(Quality_Life_Score < 94 ~ "1",
                                       Quality_Life_Score >= 94 ~ "0"),

        Social_Limitation_symp=case_when(Social_Limitation_Score< 96 ~ "1",
                                         Social_Limitation_Score >= 94 ~ "0"),

        Overall_Summary_symp=case_when(Overall_Summary_Score < 94 ~ "1",
                                       Overall_Summary_Score >= 94 ~ "0"),

        Clinical_Summary_symp=case_when(Clinical_Summary_Score < 98 ~ "1",
                                        Clinical_Summary_Score >= 98 ~ "0")
    )

kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                          "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")
covariates <- c("age","gender")
GLS <- c("GLS_imparied")


# # results_age_sex <- map(kccq_summary_sex_age, ~ {
# #     prediction_model_LR(study_data_complete, covariates, kccq_domain = .x,
# #         "GLS_imparied"
# #     )
# # }) %>%
# #     list_rbind()
# #
# # results_age_sex  %>%
# #     filter(.metric== "roc_auc")
# #
# #
# # ## include sex and age. prior heart attack, anti-hypertensives
# # covariates_cvd <- c("age","gender","heart_attack")
# #
# #
# # results_age_sex_cvd <- map(kccq_summary_sex_age, ~ {
# #     prediction_model_LR(study_data_complete, covariates_cvd, kccq_domain = .x,
# #                         "GLS_imparied"
# #     )
# # }) %>%
# #     list_rbind()
# #
# # results_age_sex_cvd  %>%
# #     filter(.metric== "roc_auc")
#
# ##################################### ALL KCCQ questions
#
# kccq_summary <- c("dummy_score", "hf_symptoms","Q1a_shower_rescaled",
#                   "Q1b_walking_rescaled", "Q1c_Jogging_rescaled","Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled","Q6_enjoyment_rescaled",
#                   "Q7_living_condition_rescaled", "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled")
#
# covariates <- c("age","gender")
# GLS <- c("GLS_imparied")
#
# # Use purrr::map to apply the function for each column to exclude
# results_age_sex_questionnaire <- map(kccq_summary, ~ {
#     prediction_model_LR(study_data_complete, covariates, kccq_domain = .x,
#                         "GLS_imparied"
#     )
# }) %>%
#     list_rbind()
#
# results_age_sex_questionnaire  %>%
#     filter(.metric== "roc_auc")
#
# ## Prior CVD
#
# results_age_sex_cvd_quest <- map(kccq_summary, ~ {
#     prediction_model_LR(study_data_complete, covariates_cvd, kccq_domain = .x,
#                         "GLS_imparied"
#     )
# }) %>%
#     list_rbind()
#
# results_age_sex_cvd_quest  %>%
#     filter(.metric== "roc_auc")

## All defined echo measures

#
# hf_diagnosis <- c("LAE_lf_art_enlarg", "GLS_imparied", "DD_dia_dys", "LVH_lf_vent_hyptro", "HFpEF", "HFmrEF_rEF")
#
#
# KCCQ_performance <- map(hf_diagnosis, ~ {
#     prediction_LR_across_echo(study_data_complete,.x
#     )
# }) %>%
#     list_rbind()
#
# write.csv(KCCQ_performance, file = here("results_data/KCCQ_domain_performance_across_echo_LR_model.csv") )
#
# KCCQ_performance %>%
#     filter(prediction_model == "age_sex_kccq_domain") %>%
#     ggplot(aes(x = Echo, y = KCCQ, fill = est)) +
#     geom_tile() +
#     scale_fill_gradient(low = "white", high = "red") +
#     labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "AUC across KCCQ domains and ")+
#     geom_text(aes(label = round(est,2)), color = "black", size = 3)



hf_diagnosis <- c("abn_ee","abn_gls","abn_lavi","lvh")

study_data_fct <- study_data_complete %>%
    filter(if_all(c("abn_ee","abn_gls","abn_lavi","lvh"), ~ !is.na(.)))

study_data_fct <- study_data_fct %>%
    mutate(across(c("abn_ee", "abn_gls", "abn_lavi", "lvh"),
                  ~ factor(as.numeric(as.character(.)) - 1, levels = c(0, 1), labels = c("0", "1"))))

study_data_fct <- study_data_fct %>%
    filter(if_all(c("abn_ee", "abn_gls", "abn_lavi", "lvh"), ~ !is.na(.))) %>%
    mutate(across(c("abn_ee", "abn_gls", "abn_lavi", "lvh"),
                  ~ factor(ifelse(is.na(.), 0, as.numeric(as.character(.)) - 1), levels = c(0, 1))))

str(study_data_fct$abn_ee)


KCCQ_performance <- map(hf_diagnosis, ~ {
    prediction_LR_across_echo(study_data_fct,.x
    )
}) %>%
    list_rbind()

write.csv(KCCQ_performance, file = here("results_data/KCCQ_domain_performance_across_echo_LR_model_upd.csv") )

KCCQ_performance %>%
    filter(prediction_model == "age_sex_kccq_domain") %>%
    ggplot(aes(x = Echo, y = KCCQ, fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "AUC across KCCQ domains and ")+
    geom_text(aes(label = round(est,2)), color = "black", size = 3)













#####################################################Sypmtoms KCCQ ###################################################################################################

kccq_summary_sex_age <- c("dummy_symp", "hf_symptoms", "Physical_Limitation_symp", "Symptom_Frequency_symp", "Quality_Life_symp",
                          "Social_Limitation_symp", "Overall_Summary_symp", "Clinical_Summary_symp")
covariates <- c("age","gender")
GLS <- c("GLS_imparied")


results_age_sex <- map(kccq_summary_sex_age, ~ {
    prediction_model_LR(study_data_complete, covariates, kccq_domain = .x,
                        "GLS_imparied"
    )
}) %>%
    list_rbind()

results_age_sex  %>%
    filter(.metric== "roc_auc")


## include sex and age. prior heart attack, anti-hypertensives
covariates_cvd <- c("age","gender","heart_attack")


results_age_sex_cvd <- map(kccq_summary_sex_age, ~ {
    prediction_model_LR(study_data_complete, covariates_cvd, kccq_domain = .x,
                        "GLS_imparied"
    )
}) %>%
    list_rbind()

results_age_sex_cvd  %>%
    filter(.metric== "roc_auc")

##################################### ALL KCCQ questions






KCCQ_symp <- map(hf_diagnosis, ~ {
    prediction_LR_across_echo_kccq_symp(study_data_complete,.x
    )
}) %>%
    list_rbind()

write.csv(KCCQ_symp, file = here("results_data/KCCQ_symp_performance_across_echo_LR_model.csv") )
