KCCQ_performance <- read.csv(here("results_data/KCCQ_domain_performance_across_echo_LR_model.csv"))

KCCQ_performance %>%
    filter(prediction_model == "age_sex_kccq_domain") %>%
    ggplot(aes(x = Echo, y = KCCQ, fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "AUC across KCCQ domains and ")+
    geom_text(aes(label = round(est,2)), color = "black", size = 3)


KCCQ_symp <- read.csv(here("results_data/KCCQ_symp_performance_across_echo_LR_model.csv"))

KCCQ_performance %>%
    filter(prediction_model == "age_sex_kccq_domain") %>%
    filter(!Echo %in% c("HFmrEF_rEF","HFpEF"))  %>%
    ggplot(aes(x = Echo, y = fct_inorder(KCCQ), fill = est)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Subtypes of heart failure", y = "KCCQ domains", title = "AUC across KCCQ domains and ")+
    geom_text(aes(label = round(est,2)), color = "black", size = 3) +
    scale_y_discrete(labels = c("dummy_score"= "Age and sex", "hf_symptoms" = "+ SOB last 14 days",
                                "Physical_Limitation_Score" = "+ Physical limitation score",
                                "+ Symptom_Frequency_Score"= "+ Symptom frequency score",
                                "Quality_Life_Score" = "+ Quality life score",
                                "Social_Limitation_Score"= "+ Social limitation score",
                                "Clinical_Summary_Score" = "+ Clinical summary score",
                                "Overall_Summary_Score" = "+ Overall Summary Score"))
