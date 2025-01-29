kCCQ_TABS <- c("Overall_Score_90cu", "Overall_Score_80cu",
               "Clinical_Score_90cu", "Clinical_Score_80cu",
               "Symptom_Score_90cu", "Symptom_Score_80cu",
               "Physical_Score_80cu", "Physical_Score_90cu"
               )

hf_diagnosis <- c("LAE_lf_art_enlarg", "GLS_imparied", "LVH_lf_vent_hyptro", "DD_dia_dys") ## "HFpEF", "HFmrEF_rEF"
study_data_complete_table<- study_data_complete %>%
    echo_data_transformation_table()


study_data_complete_diag <- study_data_complete_table %>%
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

result <- expand_grid(kccq = kCCQ_TABS, echo = hf_diagnosis) %>%
    rowwise() %>%
    mutate(distribution = list(
        # Generate the table and create a data frame with counts and percentages
        as.data.frame(table(study_data_complete_diag[[kccq]], study_data_complete_diag[[echo]])) %>%
            mutate(percentage = Freq / sum(Freq) * 100)
    )) %>%
    unnest(distribution) %>%
    mutate(distribution = paste0(Freq ," (", round(percentage,1),")"))


# result%>%
#     filter(kccq == "Overall_Score_90cu" , echo== "LAE_lf_art_enlarg") %>%
#     select(-c(Freq, percentage)) %>%
#     pivot_wider(
#         names_from = Var2,  # The levels in Var2 become column names
#         values_from = distribution,  # Fill with the `distribution` column
#         values_fill = list(distribution = "0 (0)")  # Fill missing combinations
#     )


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
        as.data.frame(table(study_data_complete_diag[[kccq]], study_data_complete_diag[[echo]])) %>%
            mutate(percentage = Freq / sum(Freq) * 100)
    )) %>%
    unnest(distribution) %>%
    mutate(distribution = paste0(Freq ," (", round(percentage,1),")"))
# head(result_diag)
# result_diag %>%
#     filter(kccq == "Overall_Score_90cu" , echo== "LAE_lf_art_enlarg") %>%
#     select(-c(distribution, percentage)) %>%
#     mutate(sensitivity = ifelse(Var2=="1", result_diag$Freq[result_diag$Var2 == "1"][2]/sum(result_diag$Freq[result_diag$Var2 == "1"][1:2])*100, NA),
#            specificity = ifelse(Var2=="0", result_diag$Freq[result_diag$Var2 == "0"][1]/sum(result_diag$Freq[result_diag$Var2 == "0"][1:2])*100, NA)) %>%
#     select(-c(Var1, Var2, Freq)) %>%
#     slice(1,4)%>%
#     summarise(
#         sensitivity = max(sensitivity, na.rm = TRUE),
#         specificity = max(specificity, na.rm = TRUE),
#         #.groups = "drop"  # Ungroup after summarising
#     )
#
#
# output_diag <- function(data, kccq_in, echo_in) {
#     filtered_data <- data %>%
#         filter(kccq == kccq_in, echo == echo_in)
#
#     # Calculate sensitivity and specificity for the filtered data
#     sensitivity <- filtered_data %>%
#         filter(Var2 == "0") %>%
#         mutate(sensitivity = Freq[Var1 == "0"] / sum(Freq) * 100) %>%
#         mutate(sensitivity = round(sensitivity,0)) %>%
#         pull(sensitivity)
#
#     specificity <- filtered_data %>%
#         filter(Var2 == "1") %>%
#         mutate(specificity = Freq[Var1 == "1"] / sum(Freq) * 100) %>%
#         mutate(specificity = round(specificity,0)) %>%
#         pull(specificity)
#
#     # Return summarised results
#     tibble(
#         kccq = kccq_in,
#         echo = echo_in,
#         sensitivity = ifelse(length(sensitivity) > 0, sensitivity, NA),
#         specificity = ifelse(length(specificity) > 0, specificity, NA)
#     )
# }
#
# all_outputs_diag <- result_diag %>%
#     # Group by kccq and echo
#     group_by(kccq, echo) %>%
#     # Group_split creates a list of data frames for each combination
#     group_split() %>%
#     # Map over each group and apply the output function
#     map(~ output_diag(.x, unique(.x$kccq), unique(.x$echo))) %>%
#     list_rbind()


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

    sensitivity_specificity <- epiR::epi.tests(two_by_two_table)
    performance <- as.data.frame(sensitivity_specificity$detail) %>%
        dplyr::filter(statistic == "se" | statistic == "sp") %>%
        mutate(performance = paste0(round(est*100,1), " (", round(lower*100,1), "; ", round(upper*100,1), ")"))

    # Return summarised results
    resutls <- tibble(
        kccq = kccq_in,
        echo = echo_in,
        sensitivity = performance$performance[performance$statistic=="se"],
        specificity = performance$performance[performance$statistic=="sp"])

    return(resutls)
}

all_outputs_diag <- result_diag %>%
    # Group by kccq and echo
    group_by(kccq, echo) %>%
    # Group_split creates a list of data frames for each combination
    group_split() %>%
    # Map over each group and apply the output function
    map(~ output_diag(.x, unique(.x$kccq), unique(.x$echo))) %>%
    list_rbind()


############################Bind it together #######################
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

    sensitivity_specificity <- epiR::epi.tests(two_by_two_table)
    performance <- as.data.frame(sensitivity_specificity$detail) %>%
        dplyr::filter(statistic == "se" | statistic == "sp") %>%
        mutate(performance = paste0(round(est*100,1), " (", round(lower*100,1), "; ", round(upper*100,1), ")"))

    # Return summarised results
    resutls <- tibble(
        kccq = "Any SOB",
        echo = echo_in,
        sensitivity = performance$performance[performance$statistic=="se"],
        specificity = performance$performance[performance$statistic=="sp"])

    return(resutls)
}



result_sob <- expand_grid(echo = hf_diagnosis) %>%
    rowwise() %>%
    mutate(distribution = list(
        # Generate the table and create a data frame with counts and percentages
        as.data.frame(table(study_data_complete_diag$hf_symptoms, study_data_complete_diag[[echo]])) %>%
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
                                 rename(`1` = "specificity",
                                        `0` = "sensitivity") %>%
                                 mutate(`0` = as.character(`0`),
                                        `1` = as.character(`1`))) %>%
    arrange(kccq, echo, Var1)

results_table_sob <- final_table_SOB %>%
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
            Var1 == "0" ~ "No symptoms",
            Var1 == "1" ~ "Symptoms",
            is.na(Var1) ~ "Sensitivity/Specificity",
            TRUE ~ Var1
        )
    )

results_table_knitr <- bind_rows(results_table_sob, results_table)
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

###############################################################################
# Print the result
kbl(results_table_knitr[, -1])  %>%
    kable_classic() %>%
    add_header_above(c("Symptoms" = 1, "Diastolic dysfunction (E/e’≥ 14)" = 2,
                       "GLS impaired (GLS ≥ −16%)" = 2,
                       "Left atrium enlargement (Left atrial volume index >34 ml/m2)" = 2,
                       "Left ventricular hypertrophy (LVMI ≥95 g/m2 (F), ≥115 g/m2 (M))" = 2)) %>%
    group_rows("Any shortness of breath", 1,3) %>%  # Group rows 1-2 under "Category A"
    group_rows("Clinical Score 80 cutoff", 4, 6) %>%
    group_rows("Clinical Score 90 cutoff", 7, 9)  %>%
    group_rows("Overall Score 80 cutoff", 10, 12)  %>%
    group_rows("Overall Score 90 cutoff", 13, 15)  %>%
    group_rows("Physical Score 80 cutoff", 16, 18)  %>%
    group_rows("Physical Score 90 cutoff", 19, 21)  %>%
    group_rows("Symptom Score 80 cutoff", 22, 24)  %>%
    group_rows("Symptom Score 90 cutoff", 25, 27)

