
study_data_complete <- study_data_complete %>%
    mutate(dummy_score = 1)

library(tidyverse)
library(parsnip)
library(workflows)
library(rsample)
library(recipes)
library(yardstick)
table(study_data_complete$LAE_lf_art_enlarg, study_data_pred$LAE_lf_art_enlarg)
study_data_pred <- study_data_complete %>%
    mutate(

        LAE_lf_art_enlarg = fct_relevel(LAE_lf_art_enlarg, "Normal","LAE"),
        LAE_lf_art_enlarg = as.factor(as.numeric(as.factor(LAE_lf_art_enlarg))-1),


        LVH_lf_vent_hyptro = fct_relevel(LVH_lf_vent_hyptro, "Normal","LVH"),
        LVH_lf_vent_hyptro = as.factor(as.numeric(as.factor(LVH_lf_vent_hyptro))-1),

        DD_dia_dys = fct_relevel(DD_dia_dys,"Normal", "DD"),
        DD_dia_dys = as.factor(as.numeric(as.factor(DD_dia_dys))-1),

        GLS_imparied = fct_relevel(GLS_imparied, "Normal","imparied"),
        GLS_imparied = as.factor(as.numeric(as.factor(GLS_imparied))-1),

        GLS_abnormal = fct_relevel(GLS_abnormal, "Normal","abnormal"),
        GLS_abnormal = as.factor(as.numeric(as.factor(GLS_abnormal))-1),

        HFrEF = fct_relevel(HFrEF,"no HFrEF","HFrEF"),
        HFrEF = as.factor(as.numeric(as.factor(HFrEF))-1),

        # HFmrEF = fct_relevel(HFrEF,"no HFmrEF", "HFmrEF"),
        # HFmrEF = as.numeric(as.factor(HFmrEF))-1,


        HFpEF = fct_relevel(HFpEF,"no HFpEF", "HFpEF"), # Something is wrong prevalence of HFrEF are very high
        HFpEF = as.factor(as.numeric(as.factor(HFpEF))-1),

        #HFmrEF_rEF = fct_relevel(HFmrEF_rEF,  "no HFmrEF or HFrEF", "HFmrEF or HFrEF"),
        #HFmrEF_rEF = as.numeric(HFmrEF_rEF)-1,

        any_HF = fct_relevel(any_HF,"No heart failure","Heart failure"),
            any_HF = as.factor(as.numeric(as.factor(any_HF))-1)
        )


kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                          "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")
covariates_cvd <- c("age","gender","heart_attack")
test <- prediction_model_LR_plot(study_data_complete,covariates_cvd, "hf_symptoms", "LAE_lf_art_enlarg")


    # Prepare data
    data_pred <- study_data_pred %>%
        select(all_of(c(covariates_cvd, "Symptom_Frequency_Score", "LAE_lf_art_enlarg"))) %>%
        drop_na() %>%
        mutate(across(
            .cols = all_of("Symptom_Frequency_Score") %>% keep(~ str_detect(.x, "score|rescaled")),
            .fns = log
        )) %>%
        mutate(LAE_lf_art_enlarg = factor(LAE_lf_art_enlarg, levels = c(0, 1)))

    # Model specification
    model_specs <- logistic_reg(mode = "classification") %>%
        set_engine("glm")

    # Create formula dynamically
    formula <- as.formula(paste("LAE_lf_art_enlarg", "~ ."))

    # Define the recipe
    recipe_specs <- recipe(formula, data = data_pred)

    # Define workflow
    workflow_status <- workflow() %>%
        add_model(model_specs) %>%
        add_recipe(recipe_specs) %>%
        fit(data_pred)

    # Bootstrap resampling
    resamples_bootstrap <- bootstraps(data_pred, times = 100)

    # Fit resamples and collect predictions
    resample_results <- fit_resamples(
        workflow_status,
        resamples = resamples_bootstrap,
        control = control_resamples(save_pred = TRUE),
        metrics = metric_set(roc_auc, pr_auc, accuracy, sensitivity, specificity)
    )

    # Collect ROC curve data
    roc_curve_data <- resample_results %>%
        collect_predictions()

#
#     metrics <- roc_curve_data %>%
#         mutate(id = as.factor(id)) %>%
#         group_by(id) %>% # Group by resample ID if needed
#         mutate(
#             roc_auc = roc_auc(truth = "LAE_lf_art_enlarg", estimate = .pred_class),
#             pr_auc = pr_auc(truth = "LAE_lf_art_enlarg", estimate = .pred_class),
#             accuracy = accuracy(truth = "LAE_lf_art_enlarg", estimate = .pred_class),
#             sensitivity = sensitivity(truth = "LAE_lf_art_enlarg", estimate = .pred_class),
#             specificity = specificity(truth = "LAE_lf_art_enlarg", estimate = .pred_class)
#         )


    # Plot the ROC curve
    roc_plot <- roc_curve_data %>%
        mutate(.pred_class = as.numeric(.pred_class)) %>%
        roc_curve(truth = !!sym("LAE_lf_art_enlarg"), .pred_1) %>%
        autoplot()

    # Compute ROC curves for each bootstrap sample
    roc_curve_by_bootstrap <- roc_curve_data %>%
        group_by(id) %>%
        roc_curve(truth = LAE_lf_art_enlarg,.pred_1)

    mean_roc_curve <- roc_curve_by_bootstrap %>%
        group_by(.fpr) %>%
        summarise(
            mean_tpr = mean(.tpr, na.rm = TRUE),
            lower_tpr = quantile(.tpr, 0.025, na.rm = TRUE),
            upper_tpr = quantile(.tpr, 0.975, na.rm = TRUE)
        )

    # Plot the mean ROC curve with confidence intervals
    ggplot(aggregated_roc, aes(x = .fpr)) +
        geom_line(aes(y = mean_tpr), color = "blue") +
        geom_ribbon(aes(ymin = lower_tpr, ymax = upper_tpr), alpha = 0.2, fill = "blue") +
        labs(
            x = "False Positive Rate (FPR)",
            y = "True Positive Rate (TPR)",
            title = "Mean ROC Curve with 95% CI"
        )



    return(list(
        performance = collect_metrics(resample_results),
        roc_curve = roc_plot
    ))
}

