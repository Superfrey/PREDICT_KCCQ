training_data <- study_data_fct %>%
select(gender, age,
       #"bmi",
       #diabetes_duration,
       #watch_dm_risk_score,

       #hdl,
       #chol, ldl, trig,
       #a1c_pcent,
       prior_ischemic_event,
       prior_PCI,

       # "mveesept",
       # "lvmi",
       # "lavi",
       # "gls_av",
       # "abn_ee","abn_lavi","lvh",
       #"abn_gls",
       any_abnormality,
        "Physical_Limitation_Score", "Symptom_Frequency_Score",
        "Quality_Life_Score", "Social_Limitation_Score",# "Overall_Summary_Score",
       "Clinical_Summary_Score"
) %>%
    mutate(gender= as.factor(gender),
           age= as.numeric(age),
          # bmi= as.numeric(bmi),
           #diabetes_duration= as.numeric(diabetes_duration),
           #watch_dm_risk_score,

           #hdl,
           #chol, ldl, trig,
           #a1c_pcent= as.numeric(a1c_pcent),
           #prior_ischemic_event = as.factor(prior_ischemic_event),
           #prior_PCI = as.factor(prior_PCI)
           )

set.seed(234)
library("rpart")
library("partykit")
library("rpart.plot")

kccq_score_folds <- vfold_cv(training_data)

set.seed(345)
rf_recipe <- recipe(any_abnormality ~ ., data = training_data)

tune_spec <-
    decision_tree(
        cost_complexity = 0.00001,
        tree_depth = 4
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")

tree_wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(rf_recipe)

fit_dt <-
    tree_wf %>%
    parsnip::fit(training_data)

fit_dt %>%
    extract_fit_engine() %>%
    rpart.plot(roundint = FALSE)

tune_spec <-
    decision_tree(
        cost_complexity = tune(),
        tree_depth = tune()
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")

tree_grid <- grid_regular(
    cost_complexity(),
    tree_depth(range = c(1, 5)), # Define range from 1 to 5
    levels = c(cost_complexity = 5, tree_depth = 5) # Specify levels
)

tree_grid %>%
    count(tree_depth)

git branch -M main
git push -u origin main
##################################### BMI focused #######################################

training_data <- study_data_fct %>%
    select(gender,# age,
           "bmi",
           #diabetes_duration,
           #watch_dm_risk_score,

           #hdl,
           #chol, ldl, trig,
           #a1c_pcent,
           #prior_ischemic_event,
           #prior_PCI,

           # "mveesept",
           # "lvmi",
           # "lavi",
           # "gls_av",
           # "abn_ee","abn_lavi","lvh",
           #"abn_gls",
           any_abnormality,
           "Physical_Limitation_Score", "Symptom_Frequency_Score",
           "Quality_Life_Score", "Social_Limitation_Score",# "Overall_Summary_Score",
           "Clinical_Summary_Score"
    ) %>%
    mutate(gender= as.factor(gender),
           #age= as.numeric(age),
           # bmi= as.numeric(bmi),
           #diabetes_duration= as.numeric(diabetes_duration),
           #watch_dm_risk_score,

           #hdl,
           #chol, ldl, trig,
           #a1c_pcent= as.numeric(a1c_pcent),
           #prior_ischemic_event = as.factor(prior_ischemic_event),
           #prior_PCI = as.factor(prior_PCI)
    )

set.seed(234)

kccq_score_folds <- vfold_cv(training_data)

rf_recipe <- recipe(any_abnormality ~ ., data = training_data)

tune_spec <-
    decision_tree(
        cost_complexity = 0.00001,
        tree_depth = 4
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")

tree_wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(rf_recipe)

fit_dt <-
    tree_wf %>%
    parsnip::fit(training_data)

fit_dt %>%
    extract_fit_engine() %>%
    rpart.plot(roundint = FALSE)

tune_spec <-
    decision_tree(
        cost_complexity = tune(),
        tree_depth = tune()
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")

tree_grid <- grid_regular(
    cost_complexity(),
    tree_depth(range = c(1, 5)), # Define range from 1 to 5
    levels = c(cost_complexity = 5, tree_depth = 5) # Specify levels
)

tree_grid %>%
    count(tree_depth)




######################################## Tune specific ########################################

tree_wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(rf_recipe)

mix_mtrc  <-  metric_set(roc_auc, pr_auc, accuracy)
#mix_mtrc  <- metric_set(brier_survival_integrated, concordance_survival, roc_auc_survival)
tree_res <-
    tree_wf %>%
    tune_grid(
        resamples = kccq_score_folds,
        grid = tree_grid,
        metrics = mix_mtrc
    )

tree_res %>%
    show_best("roc_auc")

tree_res %>%
    show_best("pr_auc")

tree_res %>%
    show_best("accuracy")

tree_res %>%
    collect_metrics()

tree_res %>%
    collect_metrics() %>%
    mutate(tree_depth = factor(tree_depth)) %>%
    ggplot(aes(cost_complexity, mean, color = tree_depth)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric, scales = "free", nrow = 2) +
    scale_x_log10(labels = scales::label_number()) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

best_tree <- tree_res %>%
    select_best("roc_auc")

final_wf <-
    tree_wf %>%
    finalize_workflow(best_tree)


final_fit <-
    final_wf %>%
    last_fit(kccq_score_folds)
?last_fit
# ROC curve



final_fit %>%
    collect_predictions() %>%
    roc_curve(surv_outcome, .pred_PS) %>%
    autoplot()




####
dt <- training_data %>%
decision_tree_model_cvd() %>%
    parsnip::fit(training_data)

rpart_plot_data <- dt %>%
    parsnip::extract_fit_engine()


rpart.plot::rpart.plot(rpart_plot_data$rpart)
rpart_plot_data$survfit
plot(rpart_plot_data$survfit)
