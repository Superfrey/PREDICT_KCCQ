
study_data_complete <- study_data_complete %>%
    mutate(dummy_score = 1)

library(tidyverse)
library(parsnip)
library(workflows)
library(rsample)
library(recipes)
library(yardstick)
kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
                          "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")



kccq_summary_sex_age <- c("dummy_score", "hf_symptoms", "Physical_Limitation_symp", "Symptom_Frequency_symp", "Quality_Life_symp",
                          "Social_Limitation_symp", "Overall_Summary_symp", "Clinical_Summary_symp")

covariates_cvd <- c("age","gender","heart_attack")
study_data_complete$GLS_imparied
test <- prediction_model_LR_plot(study_data_complete,covariates_cvd, "Symptom_Frequency_Score", "GLS_imparied")

test$roc_curve

prediction_model_LR_plot <- function(data, covariates, kccq_domain, echo_score) {


    # Prepare data
    data_pred <- data %>%
        select(all_of(c(covariates, kccq_domain, echo_score))) %>%
        drop_na() %>%
        mutate(across(
            .cols = all_of(kccq_domain) %>% keep(~ str_detect(.x, "score|rescaled")),
            .fns = log
        ))

    # Model specification
    model_specs <- logistic_reg(mode = "classification") %>%
        set_engine("glm")

    # Create formula dynamically
    formula <- as.formula(paste(echo_score, "~ ."))

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
        collect_predictions() %>%
        roc_curve(truth = !!sym(echo_score), .pred_imparied)

    # Plot the ROC curve
    roc_plot <- roc_curve_data %>%
        autoplot()

    return(list(
        performance = collect_metrics(resample_results),
        roc_curve = roc_plot
    ))
}

