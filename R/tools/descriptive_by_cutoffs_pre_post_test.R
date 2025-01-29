
final_table_SOB_pre_test <- all_outputs_diag_sob %>%
    mutate(pre_test_odds  = round(pre_test_odds,2),
           post_test_odds = round(post_test_odds,2),
           post_test_probability = round(post_test_probability,2)) %>%
    rename('2' = "pre_test_odds")%>%
    group_by(kccq) %>%
    mutate(`2` = as.character(`2`),
           '1' = NA) %>%
    mutate(performance_metric = "Pre-test odds") %>%
    arrange(kccq, echo, performance_metric) %>%
    pivot_wider(
        names_from = echo,
        values_from = c(`1`, `2`)
    )%>%
    select(performance_metric,'1_abn_ee',"2_abn_ee", "1_abn_gls", "2_abn_gls",  "1_abn_lavi", "2_abn_lavi",
           "1_lvh", "2_lvh",  "1_any_abnormality", , "2_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    group_by(kccq, performance_metric) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))

final_table_SOB_post <- all_outputs_diag_sob %>%
    mutate(pre_test_odds  = round(pre_test_odds,2),
           post_test_odds = round(post_test_odds,2),
           post_test_probability = round(post_test_probability,2)) %>%
    rename(
        '1' = "post_test_probability",
        `2` = "post_test_odds") %>%
    mutate(
        `1` = as.character(`1`),
        `2` = as.character(`2`)) %>%
    mutate(performance_metric = "Post-test probability/ Post-test odds") %>%
    arrange(kccq, echo,performance_metric) %>%
    group_by(kccq, performance_metric) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c( `1`, `2`)
    ) %>%
    select(performance_metric, '1_abn_ee',"2_abn_ee", "1_abn_gls", "2_abn_gls",  "1_abn_lavi", "2_abn_lavi",
           "1_lvh", "2_lvh",  "1_any_abnormality", , "2_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    group_by(kccq, performance_metric) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))

final_table_post <- all_outputs_diag %>%
    mutate(pre_test_odds  = round(pre_test_odds,2),
           post_test_odds = round(post_test_odds,2),
           post_test_probability = round(post_test_probability,2)) %>%
    rename(
        '1' = "post_test_probability",
        `2` = "post_test_odds") %>%
    mutate(
        `1` = as.character(`1`),
        `2` = as.character(`2`)) %>%
    mutate(performance_metric = "Post-test probability/ Post-test odds") %>%
    arrange(kccq, echo,performance_metric) %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c( `1`, `2`)
    ) %>%
    select(performance_metric, '1_abn_ee',"2_abn_ee", "1_abn_gls", "2_abn_gls",  "1_abn_lavi", "2_abn_lavi",
           "1_lvh", "2_lvh",  "1_any_abnormality", , "2_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    group_by(kccq, performance_metric) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))

######################## Likelyhood ratio ##################################################################################

final_table_SOB_lr <- all_outputs_diag_sob %>%
    rename(
        '1' = "lr_n",
        `2` = "lr_p") %>%
    mutate(
        `1` = as.character(`1`),
        `2` = as.character(`2`)) %>%
    mutate(performance_metric = "likeliyhood ratio- / likeliyhood ratio+") %>%
    arrange(kccq, echo,performance_metric) %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c( `1`, `2`)
    ) %>%
    select(performance_metric, '1_abn_ee',"2_abn_ee", "1_abn_gls", "2_abn_gls",  "1_abn_lavi", "2_abn_lavi",
           "1_lvh", "2_lvh",  "1_any_abnormality", , "2_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    group_by(kccq, performance_metric) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))

final_table_lr <- all_outputs_diag %>%
    rename(
        '1' = "lr_n",
        `2` = "lr_p") %>%
    mutate(
        `1` = as.character(`1`),
        `2` = as.character(`2`)) %>%
    mutate(performance_metric = "likeliyhood ratio- / likeliyhood ratio+") %>%
    arrange(kccq, echo,performance_metric) %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c( `1`, `2`)
    ) %>%
    select(performance_metric, '1_abn_ee',"2_abn_ee", "1_abn_gls", "2_abn_gls",  "1_abn_lavi", "2_abn_lavi",
           "1_lvh", "2_lvh",  "1_any_abnormality", , "2_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    group_by(kccq, performance_metric) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))

######################## combined ################################################################################################
combined_kccq_performance <- rbind(final_table_post, final_table_lr) %>%
    arrange(kccq, performance_metric)

pre_post_test_table <- rbind(final_table_SOB_pre_test, final_table_SOB_post,final_table_SOB_lr,combined_kccq_performance)


######################### Table ################################################################################################

# colnames(pre_post_test_table) <- c(
#     "KCCQ",
#     "category",
#     "Normal",
#     "Abnormal",
#     "Normal",
#     "Abnormal",
#     "Normal",
#     "Abnormal",
#     "Normal",
#     "Abnormal",
#     "Normal",
#     "Abnormal"
# )

kbl(pre_post_test_table[, -1])  %>%
    kable_classic() %>%
    add_header_above(c("Symptoms" = 1, "Diastolic dysfunction (E/e’≥ 14)" = 2,
                       "GLS impaired (GLS ≥ −16%)" = 2,
                       "Left atrium enlargement (Left atrial volume index >34 ml/m2)" = 2,
                       "Left ventricular hypertrophy (LVMI ≥95 g/m2 (F), ≥115 g/m2 (M))" = 2, "Any subtype of HF" = 2)) %>%
    group_rows("Pre-test probability", 1,1) %>%
    group_rows("Any symptoms of shotness of breath or swealling", 2,3) %>%
    group_rows("Clinical Score 75 cutoff", 4, 5) %>%
    group_rows("Clinical Score 80 cutoff", 6, 7)  %>%
    group_rows("Clinical Score 90 cutoff", 8, 9)  %>%
    group_rows("Clinical Score 100 cutoff", 10, 11)  %>%

    group_rows("Overall Score 75 cutoff", 12, 13)  %>%
    group_rows("Overall Score 80 cutoff", 14, 15)  %>%
    group_rows("Overall Score 90 cutoff", 16, 17)  %>%
    group_rows("Overall Score 100 cutoff", 18, 19)  %>%

    group_rows("Physical Score 75 cutoff", 20, 21)  %>%
    group_rows("Physical Score 80 cutoff", 22, 23) %>%
    group_rows("Physical Score 90 cutoff", 24, 25)  %>%
    group_rows("Physical Score 100 cutoff", 26, 27)  %>%

    group_rows("Symptom Score 75 cutoff", 28, 29)  %>%
    group_rows("Symptom Score 80 cutoff", 30, 31) %>%
    group_rows("Symptom Score 90 cutoff", 32, 33) %>%
    group_rows("Symptom Score 100 cutoff", 34, 35) %>%
    group_rows("WATCH-DM high risk (score > 11)", 36, 37)
