library(dplyr)
library(tidyr)
library("kableExtra")


# Prepare and merge

sensitivity_specificity <- all_outputs_diag %>%
    group_by(kccq) %>%
    summarise(across(c(sensitivity, specificity), max, na.rm = TRUE)) %>%
    mutate(Var1 = "Sensitivity/Specificity") %>%
    rename(`0` = "specificity",
           `1` = "sensitivity") %>%
    mutate(`0` = as.character(`0`),
           `1` = as.character(`1`))
str(all_outputs)

# Combine with main data
final_table <- bind_rows(all_outputs, all_outputs_diag %>%
                             rename(`1` = "specificity",
                                    `0` = "sensitivity") %>%
                             mutate(`0` = as.character(`0`),
                                    `1` = as.character(`1`))) %>%
    arrange(kccq, echo, Var1)

results_table <- final_table %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c(`0`, `1`)
    ) %>%
    select("Var1",'0_DD_dia_dys', '1_DD_dia_dys', "0_GLS_imparied", "1_GLS_imparied", "0_LAE_lf_art_enlarg", "1_LAE_lf_art_enlarg", "0_LVH_lf_vent_hyptro",
           "1_LVH_lf_vent_hyptro") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    # Recode the Var1 column
    mutate(
        Var1 = case_when(
            Var1 == "0" ~ "above",
            Var1 == "1" ~ "below",
            is.na(Var1) ~ "Sensitivity/Specificity",
            TRUE ~ Var1
        )
    )
results_table_knitr <- results_table
colnames(results_table_knitr) <- c(
    "KCCQ",
    "category",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal"
)


# Print the result
print(ordered_data)
kbl(results_table_knitr)  %>%
    kable_classic() %>%
    add_header_above(c("KCCQ" = 1, "Category" = 1, "Diastolic dysfunction (E/e’≥ 14)" = 2,"GLS impaired (GLS ≥ −16%)" = 2, "Left atrium enlargement (Left atrial volume index >34 ml/m2)" = 2,"Left ventricular hypertrophy (LVMI ≥95 g/m2 (F), ≥115 g/m2 (M))" = 2))
