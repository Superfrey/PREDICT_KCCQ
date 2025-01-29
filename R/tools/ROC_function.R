roc_function_multiple <- function(data, outcome, predictors, direction = ">") {
    # Build the formula dynamically

    data <- data  %>%
        mutate(across(
            .cols = all_of(predictors) %>% keep(~ str_detect(.x, "score|rescaled")),
            .fns = log
        ))

    formula_string <- paste(outcome, "~", paste(predictors, collapse = " + "))
    formula <- as.formula(formula_string)

    # Apply the roc function with the formula
    roc_estimates <- roc(formula, data = data, direction = direction) %>%
        ggroc()+
        geom_abline(slope = 1, intercept = 1, linetype = "dotted" )+
        theme_minimal() +
        theme(legend.key.size = unit(0.1, 'cm'),
              legend.text = element_text(size = 5),
              legend.title = element_text(size = 5)) +
        scale_fill_brewer(palette = "Dark2")

    # Return the ROC object
    return(roc_estimates)
}



roc_kccq <- function(data_roc, hf_outcome) {
    # Overall

    overall <- c("Social_Limitation_Score" , "Clinical_Summary_Score" ,"Physical_Limitation_Score",
                 "Overall_Summary_Score" , "Quality_Life_Score" , "Symptom_Frequency_Score")

    roc.list <- roc_function_multiple(data_roc, hf_outcome, overall)



    #Physical_Limitation_Score
    physical <- c("Q1a_shower_rescaled", "Q1b_walking_rescaled", "Q1c_Jogging_rescaled",  "Physical_Limitation_Score")

    roc.list_physical <- roc_function_multiple(data_roc, hf_outcome, physical)


    #Symptom_Frequency_Score

    symptom <- c("Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled","Symptom_Frequency_Score")

    roc.list_symp <- roc_function_multiple(data_roc, hf_outcome, symptom)


    #Quality_Life_Score

    qls <- c( "Q6_enjoyment_rescaled",
              "Q7_living_condition_rescaled",  "Quality_Life_Score")


    roc.list_qls <- roc_function_multiple(data_roc, hf_outcome, qls)


    #Social_Limitation_Score

    social <- c( "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled", "Social_Limitation_Score")

    roc.list_social <- roc_function_multiple(data_roc, hf_outcome, social)


    final_plot <- ggarrange(roc.list, roc.list_physical, roc.list_symp, roc.list_qls, roc.list_social
                            , ncol = 2, nrow = 3, widths = c(2, 2))
    final_plot_output <- annotate_figure(final_plot, top = text_grob(paste0(hf_outcome), face = "bold", size = 14))

    return(final_plot_output)

}

roc_kccq_overall <- function(data_roc, hf_outcome) {
    # Overall

    overall <- c("Social_Limitation_Score" , "Clinical_Summary_Score" ,"Physical_Limitation_Score",
                 "Overall_Summary_Score" , "Quality_Life_Score" , "Symptom_Frequency_Score")

    roc.list <- roc_function_multiple(data_roc, hf_outcome, overall)+
        ggtitle(paste0(hf_outcome))


    return(roc.list)

}



# data_roc_kccq <- study_data %>%
#     filter(hf_symptoms == 1)
#
# roc_kccq(data_roc_kccq, "DD_dia_dys")

best_threshold_roc <- function(results_list) {
    best_roc <- coords(results_list, "best", ret = "threshold", best.method = "closest.topleft", transpose = FALSE)
    best_se_sp <- coords(results_list, as.numeric(best_roc), transpose = FALSE)
    predictor_name <- results_list$predictor.name
    outcome_name <- results_list$response.name
    best_performance <- cbind(outcome_name, predictor_name, best_se_sp)
    return(best_performance)
}


roc_function_num_best <- function(data, outcome, predictors, direction = ">") {
    # Build the formula dynamically
    formula_string <- paste(outcome, "~", paste(predictors, collapse = " + "))
    formula <- as.formula(formula_string)

    # Apply the roc function with the formula
    roc_estimates <- roc(formula, data = data, direction = direction)

    best_roc <- map(roc_estimates, ~ best_threshold_roc(.x)) %>%
        list_rbind() %>%
        as.data.frame()
    # Return the ROC object
    return(best_roc)
}

roc_kccq_num <- function(data_roc, hf_outcome) {
    # Overall

    overall <- c("Social_Limitation_Score" , "Clinical_Summary_Score" ,"Physical_Limitation_Score",
                 "Overall_Summary_Score" , "Quality_Life_Score" , "Symptom_Frequency_Score")

    roc.list <- roc_function_num_best(data_roc, hf_outcome, overall)



    #Physical_Limitation_Score
    physical <- c("Q1a_shower_rescaled", "Q1b_walking_rescaled", "Q1c_Jogging_rescaled")

    roc.list_physical <- roc_function_num_best(data_roc, hf_outcome, physical)


    #Symptom_Frequency_Score

    symptom <- c("Q3_fatigue_rescaled", "Q4_shortness_breath_rescaled", "Q5_sleep_shortness_breath_rescaled")

    roc.list_symp <- roc_function_num_best(data_roc, hf_outcome, symptom)


    #Quality_Life_Score

    qls <- c( "Q6_enjoyment_rescaled",
              "Q7_living_condition_rescaled")


    roc.list_qls <- roc_function_num_best(data_roc, hf_outcome, qls)


    #Social_Limitation_Score

   # social <- c( "Q8a_affect_hobbies_rescaled", "Q8b_affect_working_rescaled", "Q8c_affect_visiting_rescaled")

    #roc.list_social <- roc_function_num_best(data_roc, hf_outcome, social)


    final_plot <- rbind(roc.list, roc.list_physical, roc.list_symp, roc.list_qls)#, roc.list_social)


    return(final_plot)

}
