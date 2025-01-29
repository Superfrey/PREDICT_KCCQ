library("bit64")
library("vroom")
hf_diagnosis <- c("any_abnormality")

KCCQ_performance <- map(hf_diagnosis, ~ {
    prediction_LR_across_echo(study_data_fct,.x
    )
}) %>%
    list_rbind()

write.csv(KCCQ_performance, file = here("results_data/KCCQ_domain_performance_across_echo_LR_model_any_echo_abn.csv") )

KCCQ_performance <- vroom(here("results_data/KCCQ_domain_performance_across_echo_LR_model_any_echo_abn.csv") )

KCCQ_performance %>%
    filter(prediction_model == "age_sex_kccq_domain") %>%
    ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
    geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
    scale_y_discrete(labels = c("dummy_score"= "Age and sex", "hf_symptoms" = "+ Any symptoms last 14 days",
                                "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))


KCCQ_performance %>%
    filter(prediction_model == "age_sex_cvd_kccq_domain") %>%
    ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Indication of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
    geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
    scale_y_discrete(labels = c("dummy_score"= "Age, sex and CVD history", "hf_symptoms" = "+ Any symptoms last 14 days",
                                "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))

KCCQ_performance %>%
    filter(prediction_model == "age_sex_cvd_bmi_kccq_domain") %>%
    ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Indication of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
    geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
    scale_y_discrete(labels = c("dummy_score"= "Age, sex, BMI and CVD history", "hf_symptoms" = "+ Any symptoms last 14 days",
                                "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))


KCCQ_performance %>%
    filter(prediction_model == "age_sex_cvd_bmi_diabetes_dur_kccq_domain") %>%
    ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Indication of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
    geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
    scale_y_discrete(labels = c("dummy_score"= "Age, sex, BMI, diabetes duration and CVD history", "hf_symptoms" = "+ Any symptoms last 14 days",
                                "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))
#
# #################################################################################
#
# study_data_fct <- study_data_fct %>%
#     mutate(dummy_score = 1) %>%
#     mutate(Physical_Limitation_symp = case_when(Physical_Limitation_Score< 96 ~ "1",
#                                                 Physical_Limitation_Score >= 96 ~ "0"),
#
#            Symptom_Frequency_symp=case_when(Symptom_Frequency_Score < 97 ~ "1",
#                                             Symptom_Frequency_Score >= 97 ~ "0"),
#
#            Quality_Life_symp=case_when(Quality_Life_Score < 94 ~ "1",
#                                        Quality_Life_Score >= 94 ~ "0"),
#
#            Social_Limitation_symp=case_when(Social_Limitation_Score< 96 ~ "1",
#                                             Social_Limitation_Score >= 94 ~ "0"),
#
#            Overall_Summary_symp=case_when(Overall_Summary_Score < 94 ~ "1",
#                                           Overall_Summary_Score >= 94 ~ "0"),
#
#            Clinical_Summary_symp=case_when(Clinical_Summary_Score < 98 ~ "1",
#                                            Clinical_Summary_Score >= 98 ~ "0")
#     )
#
#
# KCCQ_symp <- map(hf_diagnosis, ~ {
#     prediction_LR_across_echo_kccq_symp(study_data_fct,.x
#     )
# }) %>%
#     list_rbind()
#
# KCCQ_symp %>%
#     filter(prediction_model == "age_sex_kccq_domain") %>%
#     filter(Echo == "any_abnormality") %>%
#     ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
#     geom_tile() +
#     scale_fill_gradient(low = "white", high = "red") +
#     labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
#     geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
#     scale_y_discrete(labels = c("dummy_score"= "Age and sex", "hf_symptoms" = "+ SOB last 14 days",
#                                 "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))
#
# KCCQ_performance %>%
#     filter(prediction_model == "age_sex_cvd_kccq_domain") %>%
#     filter(Echo == "any_abnormality") %>%
#     ggplot(aes(x = .metric, y = fct_inorder(KCCQ), fill = est)) +
#     geom_tile() +
#     scale_fill_gradient(low = "white", high = "red") +
#     labs(x = "Indication of heart failure", y = "KCCQ domains", title = "Incremental dianogstic performance across KCCQ domains")+
#     geom_text(aes(label = paste0(round(est,2)," (",round(low_ci,2),"; ", round(high_ci,2),")")), color = "black", size = 3) +
#     scale_y_discrete(labels = c("dummy_score"= "Age, sex and CVD history", "hf_symptoms" = "+ SOB last 14 days",
#                                 "Physical_Limitation_Score" = "+ Physical limitation score", "+ Symptom_Frequency_Score"= "+ Symptom frequency score", "Quality_Life_Score" = "+ Quality life score", "Social_Limitation_Score"= "+ Social limitation score", "Clinical_Summary_Score" = "+ Clinical summary score", "Overall_Summary_Score" = "+ Overall Summary Score"))
#
