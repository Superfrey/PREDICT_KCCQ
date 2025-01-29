
covariates_cvd, "hf_symptoms", "GLS_imparied"
# Prepare data
data_pred <- study_data_complete %>%
    select(all_of(c(covariates_cvd, "Symptom_Frequency_Score", "GLS_imparied"))) %>%
    drop_na() %>%
    mutate(across(
        .cols = all_of("Symptom_Frequency_Score") %>% keep(~ str_detect(.x, "score|rescaled")),
        .fns = log
    ))

# Model specification
model_specs <- logistic_reg(mode = "classification") %>%
    set_engine("glm")

# Create formula dynamically
formula <- as.formula(paste("GLS_imparied", "~ ."))

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
    roc_curve(truth = "GLS_imparied", .pred_imparied)

# Plot the ROC curve
roc_plot <- roc_curve_data %>%
    autoplot()

performance = collect_metrics(resample_results)
roc_curve = roc_plot
