kCCQ_TABS <- c("watch_dm_11_high_risk_cu","Overall_Score_100cu",
               "Overall_Score_90cu", "Overall_Score_80cu", "Overall_Score_75cu",
               "Clinical_Score_100cu","Clinical_Score_90cu", "Clinical_Score_80cu",
               "Symptom_Score_100cu","Symptom_Score_90cu", "Symptom_Score_80cu",
               "Physical_Score_100cu", "Physical_Score_90cu", "Physical_Score_80cu"
)


study_data_fct_DM_watch <- study_data_fct %>%
    filter(Clinical_Score_75cu == "0")

hf_diagnosis <- c("abn_ee","abn_gls","abn_lavi","lvh", "any_abnormality")

result <- expand_grid(kccq = kCCQ_TABS, echo = hf_diagnosis) %>%
    rowwise() %>%
    mutate(distribution = list(
        # Generate the table and create a data frame with counts and percentages
        as.data.frame(table(study_data_fct_DM_watch[[kccq]], study_data_fct_DM_watch[[echo]])) %>%
            mutate(percentage = Freq / sum(Freq) * 100)
    )) %>%
    unnest(distribution) %>%
    mutate(distribution = paste0(Freq ," (", round(percentage,1),")"))


output_freq <- function(data, kccq_in, echo_in){

    data %>%
        filter(kccq == kccq_in , echo == echo_in) %>%
        select(-c(Freq, percentage)) %>%
        pivot_wider(
            names_from = Var2,  # The levels in Var2 become column names
            values_from = distribution,  # Fill with the `distribution` column
            values_fill = list(distribution = "0 (0)")  # Fill missing combinations
        )}

all_outputs <- result %>%
    # Group by kccq and echo
    group_by(kccq, echo) %>%
    # Group_split creates a list of data frames for each combination
    group_split() %>%
    # Map over each group and apply the output function
    map(~ output_freq(.x, unique(.x$kccq), unique(.x$echo))) %>%
    list_rbind()

final_output <- all_outputs %>%
    group_by(kccq, Var1) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | ")), .groups = "drop")


#########################SENSITIVITY SPECIFICITY########################################



result_diag <- expand_grid(kccq = kCCQ_TABS, echo = hf_diagnosis) %>%
    rowwise() %>%
    mutate(distribution = list(
        # Generate the table and create a data frame with counts and percentages
        as.data.frame(table(study_data_fct_DM_watch[[kccq]], study_data_fct_DM_watch[[echo]])) %>%
            mutate(percentage = Freq / sum(Freq) * 100)
    )) %>%
    unnest(distribution) %>%
    mutate(distribution = paste0(Freq ," (", round(percentage,1),")"))

##############################################################################


output_diag <- function(data, kccq_in, echo_in) {
    filtered_data <- data %>%
        filter(kccq == kccq_in, echo == echo_in)

    two_by_two_table <- filtered_data %>%
        select(Var1, Var2, Freq) %>%
        pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
        arrange(desc(Var1))


    two_by_two_table <- two_by_two_table%>%
        select(-Var1) %>% # Remove the Var1 column
        as.matrix() %>% # Convert the remaining columns to a matrix
        `rownames<-`(two_by_two_table$Var1) %>% # Set row names from Var1
        as.table()

    # Add dimension names
    dimnames(two_by_two_table) <- list(
        Prediction = c("1", "0"), # Rows correspond to Prediction
        Reference = c("1", "0")  # Columns correspond to Reference
    )

    sensitivity_specificity <- epiR::epi.tests(two_by_two_table[c(1, 2), c(2, 1)])
    performance <- as.data.frame(sensitivity_specificity$detail) %>%
        dplyr::filter(statistic %in% c("se", "sp", "pv.pos", "pv.neg", "lr.pos", "lr.neg", "nndx")) %>%
        mutate(
            est = ifelse(statistic %in% c("se", "sp", "pv.pos", "pv.neg"), est * 100, est),
            lower = ifelse(statistic %in% c("se", "sp", "pv.pos", "pv.neg"), lower * 100, lower),
            upper = ifelse(statistic %in% c("se", "sp", "pv.pos", "pv.neg"), upper * 100, upper),
            performance = paste0(round(est, 1), " (", round(lower, 1), "; ", round(upper, 1), ")")
        )

    # Return summarised results
    resutls <- tibble(
        kccq = kccq_in,
        echo = echo_in,
        sensitivity = performance$performance[performance$statistic=="se"],
        specificity = performance$performance[performance$statistic=="sp"],
        tpp = performance$performance[performance$statistic=="pv.pos"],
        npp = performance$performance[performance$statistic=="pv.neg"],
        lr_p = performance$performance[performance$statistic=="lr.pos"],
        lr_n = performance$performance[performance$statistic=="lr.neg"],
        numbers_need_to_diagnose = performance$performance[performance$statistic=="nndx"]
    )

    return(resutls)
}

all_outputs_diag <- result_diag %>%
    # Group by kccq and echo
    group_by(kccq, echo) %>%
    # Group_split creates a list of data frames for each combination
    group_split() %>%
    # Map over each group and apply the output function
    map(~ output_diag(.x, unique(.x$kccq), unique(.x$echo))) %>%
    list_rbind() %>%
    select(kccq, echo, specificity, sensitivity, tpp, npp)

all_outputs_diag

############################Bind it together #######################
library(dplyr)
library(tidyr)
library("kableExtra")

# Prepare and merge

sensitivity_specificity <- all_outputs_diag %>%
    group_by(kccq) %>%
    summarise(across(c(sensitivity, specificity), max, na.rm = TRUE)) %>%
    mutate(Var1 = "Specificity/Sensitivity") %>%
    rename(`1` = "specificity",
           `0` = "sensitivity") %>%
    mutate(`0` = as.character(`0`),
           `1` = as.character(`1`))

tpp_npp <- all_outputs_diag %>%
    group_by(kccq) %>%
    summarise(across(c(npp, tpp), max, na.rm = TRUE)) %>%
    rename(`1` = "npp",
           `0` = "tpp") %>%
    mutate(`0` = as.character(`0`),
           `1` = as.character(`1`))

sensitivity_specificity <- bind_rows(sensitivity_specificity, tpp_npp)
# Combine with main data
final_table <- bind_rows(all_outputs, all_outputs_diag %>%
                             rename(`0` = "specificity",
                                    `1` = "sensitivity") %>%
                             mutate(`0` = as.character(`0`),
                                    `1` = as.character(`1`),
                                    Var1 = "Specificity/Sensitivity"), all_outputs_diag %>%
                             rename(`1` = "tpp",
                                    `0` = "npp") %>%
                             mutate(`0` = as.character(`0`),
                                    `1` = as.character(`1`),
                                    Var1 = "NPV/PPV")) %>%
    arrange(kccq, echo, Var1)

final_table
desired_order <- c("above", "below", "Specificity/Sensitivity", "NPV/PPV")
results_table <- final_table %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c(`0`, `1`)
    ) %>%
    select("Var1",'0_abn_ee', '1_abn_ee', "0_abn_gls", "1_abn_gls", "0_abn_lavi", "1_abn_lavi", "0_lvh",
           "1_lvh", "0_any_abnormality", "1_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    # Recode the Var1 column
    mutate(
        Var1 = case_when(
            Var1 == "0" ~ "above",
            Var1 == "1" ~ "below",
            Var1 == "Specificity/Sensitivity" ~ "Specificity/Sensitivity",
            Var1 == "NPV/PPV" ~ "NPV/PPV",
            TRUE ~ Var1
        )
    ) %>%  group_by(kccq, Var1) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))%>%
    mutate(Var1 = factor(Var1, levels = desired_order)) %>%
    arrange(kccq, Var1)

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
    "Normal",
    "Abnormal",
    "Normal"
)

############################### Adding SOB ###############################



output_freq_sob <- function(data, echo_in){

    data %>%
        filter(echo == echo_in) %>%
        select(-c(Freq, percentage)) %>%
        pivot_wider(
            names_from = Var2,  # The levels in Var2 become column names
            values_from = distribution,  # Fill with the `distribution` column
            values_fill = list(distribution = "0 (0)")  # Fill missing combinations
        )
}


output_diag_sob <- function(data, echo_in) {
    filtered_data <- data %>%
        filter(echo == echo_in)

    two_by_two_table <- filtered_data %>%
        select(Var1, Var2, Freq) %>%
        pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
        arrange(desc(Var1))


    two_by_two_table <- two_by_two_table%>%
        select(-Var1) %>% # Remove the Var1 column
        as.matrix() %>% # Convert the remaining columns to a matrix
        `rownames<-`(two_by_two_table$Var1) %>% # Set row names from Var1
        as.table()

    # Add dimension names
    dimnames(two_by_two_table) <- list(
        Prediction = c("1", "0"), # Rows correspond to Prediction
        Reference = c("1", "0")  # Columns correspond to Reference
    )

    sensitivity_specificity <- epiR::epi.tests(two_by_two_table[c(1, 2), c(2, 1)])
    performance <- as.data.frame(sensitivity_specificity$detail) %>%
        dplyr::filter(statistic == "se" | statistic == "sp" | statistic == "pv.pos" | statistic == "pv.neg") %>%
        mutate(performance = paste0(round(est*100,1), " (", round(lower*100,1), "; ", round(upper*100,1), ")"))

    # Return summarised results
    resutls <- tibble(
        kccq = "Any SOB",
        echo = echo_in,
        sensitivity = performance$performance[performance$statistic=="se"],
        specificity = performance$performance[performance$statistic=="sp"],
        tpp = performance$performance[performance$statistic=="pv.pos"],
        npp = performance$performance[performance$statistic=="pv.neg"])

    return(resutls)
}



result_sob <- expand_grid(echo = hf_diagnosis) %>%
    rowwise() %>%
    mutate(distribution = list(
        # Generate the table and create a data frame with counts and percentages
        as.data.frame(table(study_data_fct_DM_watch$hf_symptoms, study_data_fct_DM_watch[[echo]])) %>%
            mutate(percentage = Freq / sum(Freq) * 100)
    )) %>%
    unnest(distribution) %>%
    mutate(distribution = paste0(Freq ," (", round(percentage,1),")"))

all_outputs_sob <- result_sob %>%
    # Group by kccq and echo
    group_by(echo) %>%
    # Group_split creates a list of data frames for each combination
    group_split() %>%
    # Map over each group and apply the output function
    map(~ output_freq_sob(.x, unique(.x$echo))) %>%
    list_rbind() %>%
    mutate(kccq = "Any SOB")

all_outputs_diag_sob <- result_sob %>%
    # Group by kccq and echo
    group_by(echo) %>%
    # Group_split creates a list of data frames for each combination
    group_split() %>%
    # Map over each group and apply the output function
    map(~ output_diag_sob(.x, unique(.x$echo))) %>%
    list_rbind()



final_table_SOB <- bind_rows(all_outputs_sob, all_outputs_diag_sob %>%
                                 rename(`0` = "specificity",
                                        `1` = "sensitivity") %>%
                                 mutate(`0` = as.character(`0`),
                                        `1` = as.character(`1`))) %>%
    arrange(kccq, echo, Var1)

final_table_SOB <- bind_rows(all_outputs_sob, all_outputs_diag_sob %>%
                                 rename(`0` = "specificity",
                                        `1` = "sensitivity") %>%
                                 mutate(`0` = as.character(`0`),
                                        `1` = as.character(`1`),
                                        Var1 = "Specificity/Sensitivity"), all_outputs_diag_sob %>%
                                 rename(`1` = "tpp",
                                        `0` = "npp") %>%
                                 mutate(`0` = as.character(`0`),
                                        `1` = as.character(`1`),
                                        Var1 = "NPV/PPV")) %>%
    arrange(kccq, echo, Var1)
desired_order <- c("Absent", "Present", "Specificity/Sensitivity", "NPV/PPV")

results_table_sob <- final_table_SOB %>%
    group_by(kccq) %>%
    # Pivot the data: create separate columns for each echo and Var1 value
    pivot_wider(
        names_from = echo,
        values_from = c(`0`, `1`)
    ) %>%
    select("Var1",'0_abn_ee', '1_abn_ee', "0_abn_gls", "1_abn_gls", "0_abn_lavi", "1_abn_lavi", "0_lvh",
           "1_lvh", "0_any_abnormality", "1_any_abnormality") %>%
    # Ungroup the data frame to enable recoding
    ungroup() %>%
    # Recode the Var1 column
    mutate(
        Var1 = case_when(
            Var1 == "0" ~ "Absent",
            Var1 == "1" ~ "Present",
            Var1 == "Specificity/Sensitivity" ~ "Specificity/Sensitivity",
            Var1 == "NPV/PPV" ~ "NPV/PPV",
            TRUE ~ Var1
        )
    ) %>%  group_by(kccq, Var1) %>%
    summarise(across(everything(), ~ paste(na.omit(.), collapse = " | "), .names = "{col}"))%>%
    mutate(Var1 = factor(Var1, levels = desired_order)) %>%
    arrange(kccq, Var1)

results_table_final_assymptomatical_risk <- bind_rows(results_table_sob, results_table)
results_table_knitr <- results_table_final
colnames(results_table_knitr) <- c(
    "KCCQ",
    "category",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal",
    "Normal",
    "Abnormal"
)

###############################################################################
# Print the result
results_table_knitr$KCCQ
kbl(results_table_knitr[, -1])  %>%
    kable_classic() %>%
    add_header_above(c("Symptoms" = 1, "Diastolic dysfunction (E/e’≥ 14)" = 2,
                       "GLS impaired (GLS ≥ −16%)" = 2,
                       "Left atrium enlargement (Left atrial volume index >34 ml/m2)" = 2,
                       "Left ventricular hypertrophy (LVMI ≥95 g/m2 (F), ≥115 g/m2 (M))" = 2, "Any subtype of HF" = 2)) %>%
    group_rows("Any symptoms of shotness of breath or swealling", 1,4) %>%  # Group rows 1-2 under "Category A"
    group_rows("Clinical Score 75 cutoff", 5, 8) %>%
    group_rows("Clinical Score 80 cutoff", 9, 12)  %>%
    group_rows("Clinical Score 90 cutoff", 13, 16)  %>%
    group_rows("Overall Score 75 cutoff", 17, 20)  %>%
    group_rows("Overall Score 80 cutoff", 21, 24)  %>%
    group_rows("Overall Score 90 cutoff", 25, 28)  %>%
    group_rows("Physical Score 75 cutoff", 29, 32)  %>%
    group_rows("Physical Score 80 cutoff", 33, 36) %>%
    group_rows("Physical Score 90 cutoff", 37, 40)  %>%
    group_rows("Symptom Score 75 cutoff", 41, 44)  %>%
    group_rows("Symptom Score 80 cutoff", 45, 48) %>%
    group_rows("Symptom Score 90 cutoff", 49, 52)
