study_data_complete %>%
    mutate(type_of_hf_symp = case_when(hf_symptoms == 0 ~ 'Asymptomatic',
                                       # Overall_Summary_Score > 75 ~ 'Asyptomatic defined by KCCQ > 75',
                                       # Overall_Summary_Score <= 75 ~ 'Syptomatic defined by KCCQ < 75'),
           Clinical_Summary_Score > 75 ~ 'Asyptomatic defined by KCCQ',
           Clinical_Summary_Score <= 75 ~ 'Syptomatic defined by KCCQ'),
           Quality_Life_Score = as.numeric(Quality_Life_Score),

           gender = haven::as_factor(gender)) %>%
    select(type_of_hf_symp, gender, age,"bmi",
           diabetes_duration,
           watch_dm_risk_score,

           hdl,
           chol, ldl, trig,
           a1c_pcent, a1c_mmol,egfr ,
           bp_syst_mean, bp_diast_mean, waist_1, hip_1,
           #heart_rate, heart_rate2 ,
           heart_rate3,
           prior_ischemic_event,
           prior_PCI,

           "mveesept",
                   "lvmi",
                   "lavi",
                   "gls_av",
           "abn_ee","abn_gls","abn_lavi","lvh", any_abnormality,
           "Physical_Limitation_Score", "Symptom_Frequency_Score",
           "Quality_Life_Score", "Social_Limitation_Score", "Clinical_Summary_Score", "Overall_Summary_Score",
           ) %>%
    gtsummary::tbl_summary(by = type_of_hf_symp,
                           missing = "no",
                           type = list(Quality_Life_Score ~ "continuous"))



tot_pop <- study_data_complete

pop_without_known_heart_failure <- study_data_complete %>%
            filter(hf_hx == "0" | is.na(hf_hx))

population_with_know_hf <- study_data_complete %>%
                                    filter(hf_hx == "1")

population_with_no_symp <- study_data_complete %>%
                                    filter(hf_hx == "0" | is.na(hf_hx),
                                           hf_symptoms == "0")

population_with_symp <- study_data_complete %>%
    filter(hf_hx == "0" | is.na(hf_hx),
               hf_symptoms == "1")

population_with_symp_kccq_over <- study_data_complete %>%
    filter(hf_hx == "0" | is.na(hf_hx),
           hf_symptoms == "1",
           Clinical_Summary_Score > 75)

population_with_symp_kccq_under <- study_data_complete %>%
    filter(hf_hx == "0" | is.na(hf_hx),
           hf_symptoms == "1",
           Clinical_Summary_Score <= 75)
tot_pop %>% filter(any_abnormality==1)

data.frame(
    population_steps = c("tot_pop", "pop_without_known_heart_failure", "population_with_know_hf",
                         "population_with_no_symp", "population_with_symp", "population_with_symp_kccq_over", "population_with_symp_kccq_under"),
    numbers = c(nrow(tot_pop), nrow(pop_without_known_heart_failure), nrow(population_with_know_hf),
                nrow(population_with_no_symp), nrow(population_with_symp), nrow(population_with_symp_kccq_over) ,
                nrow(population_with_symp_kccq_under)),

                abn_numbers = c(nrow(tot_pop %>% filter(any_abnormality==1)), nrow(pop_without_known_heart_failure %>% filter(any_abnormality==1)),
                            nrow(population_with_know_hf %>% filter(any_abnormality==1)),
                            nrow(population_with_no_symp %>% filter(any_abnormality==1)), nrow(population_with_symp %>% filter(any_abnormality==1)),
                            nrow(population_with_symp_kccq_over %>% filter(any_abnormality==1)),
                            nrow(population_with_symp_kccq_under %>% filter(any_abnormality==1)))
                )
#
# install.packages("DiagrammeR")
#
# myGraph = grViz("digraph ParticipantFlow {
#   rankdir=TD;
#   node [shape=box, style=rounded, fontname="Arial", width=4.2];
#
#   // Nodes
#   source_background [label="PREDICT study population:\nN = 514\nAny heart failure abnormality (256)"];
#   source_eligiable [label="Without known heart failure\nN = 512"];
#   source_hf_symp [label="With heart failure symptoms\nN = 123\nAny heart failure abnormality (70)"];
#   source_hf_no_symp [label="Without heart failure symptoms\nN = 389\nAny heart failure abnormality (192)"];
#   source_kccq_symp [
#         label="KCCQ symptoms\n(clinical KCCQ summary < 75) \nN = 53\nAny heart failure abnormality (31)"
#     xlabel="PPV = 62.7%"
#     labelloc="b" // Places the main label at the bottom of the node
#     labeldistance=0.5 // Adjusts the distance between the node and xlabel
#     xlabelfontsize=1
#     ];
#   source_kccq_no_symp [label="Without KCCQ symptoms\n (clinical KCCQ summary > 75) \nN = 70\nAny heart failure abnormality (39)"
#    xlabel="NPV = 45.8%"];
#
#   // Exclusion annotations
#   exclusions_1 [label="With known heart failure\nN = 2", style=dashed, fontcolor=red];
#
#   // Edges
#   source_background -> source_eligiable
#   source_eligiable -> source_hf_symp;
#   source_eligiable -> source_hf_no_symp;
#   source_hf_symp -> source_kccq_symp[label="Sensitivty = 12.1 % \n Specificity = 91.2 %" labeltooltip="this is a tooltip"];
#   source_hf_symp -> source_kccq_no_symp;
#
#
#   source_eligiable -> exclusions_1 [label=" " style=dashed];
#
#
#
#
#   // Adjust positioning
#   { rank=same; source_eligiable; exclusions_1 }
#
#
# }
# "
# )

table(test$type_of_hf_symp)
study_data_fct %>%
    filter(hf_symptoms== "1") %>%
    mutate(type_of_hf_symp = case_when(
        Clinical_Summary_Score > 75 & any_abnormality == "1"  ~ 'Asyptomatic defined by KCCQ and HF',
        Clinical_Summary_Score > 75 & any_abnormality == "0"  ~ 'Asyptomatic defined by KCCQ without HF',
        Clinical_Summary_Score <= 75 & any_abnormality == "1" ~ 'Syptomatic defined by KCCQ and HF',
        Clinical_Summary_Score <= 75 & any_abnormality == "0" ~ 'Syptomatic defined by KCCQ without HF'
    ),
           Quality_Life_Score = as.numeric(Quality_Life_Score),

           gender = haven::as_factor(gender)) %>%
    select(type_of_hf_symp, gender, age,"bmi",
           diabetes_duration,
           hdl,
           chol, ldl, trig,
           a1c_pcent, a1c_mmol,
           bp_syst_mean, bp_diast_mean, waist_1, hip_1,
           #heart_rate, heart_rate2 ,
           heart_rate3,
           prior_ischemic_event,
           prior_PCI,
           egfr ,
           "mveesept",
           "lvmi",
           "lavi",
           "gls_av",
           #"abn_ee","abn_gls","abn_lavi","lvh",
           #any_abnormality,
         #  "Physical_Limitation_Score", "Symptom_Frequency_Score",
         #  "Quality_Life_Score", "Social_Limitation_Score", "Clinical_Summary_Score", "Overall_Summary_Score",
           watch_dm_risk_score) %>%
    gtsummary::tbl_summary(by = type_of_hf_symp,
                           missing = "no"#,
                           #type = list(Quality_Life_Score ~ "continuous")
                           ) %>%
    add_p()


hf_symptoms_plot_group<- study_data_fct %>%
    filter(hf_symptoms== "1") %>%
    mutate(type_of_hf_symp = case_when(
        Clinical_Summary_Score > 75 & any_abnormality == "1"  ~ 'Asyptomatic defined by KCCQ and HF',
        Clinical_Summary_Score > 75 & any_abnormality == "0"  ~ 'Asyptomatic defined by KCCQ without HF',
        Clinical_Summary_Score <= 75 & any_abnormality == "1" ~ 'Syptomatic defined by KCCQ and HF',
        Clinical_Summary_Score <= 75 & any_abnormality == "0" ~ 'Syptomatic defined by KCCQ without HF'
    ),
    Quality_Life_Score = as.numeric(Quality_Life_Score),

    gender = haven::as_factor(gender))
hf_symptoms_plot_group %>%
    select(type_of_hf_symp, gender, age,"bmi",
           diabetes_duration,
           hdl,
           chol, ldl, trig,
           a1c_pcent, a1c_mmol,
           bp_syst_mean, bp_diast_mean, waist_1, hip_1,
           #heart_rate, heart_rate2 ,
           heart_rate3,
           af_hx,
           three_point_mace,
           egfr ,
           "mveesept",
           "lvmi",
           "lavi",
           "gls_av",
           "abn_ee","abn_gls","abn_lavi","lvh", any_abnormality,
           "Physical_Limitation_Score", "Symptom_Frequency_Score",
           "Quality_Life_Score", "Social_Limitation_Score", "Clinical_Summary_Score", "Overall_Summary_Score",
           watch_dm_risk_score) %>%
    gtsummary::tbl_summary(by = type_of_hf_symp,
                           missing = "no",
                           type = list(Quality_Life_Score ~ "continuous")) %>%
    add_p()
hist(study_data_complete$watch_dm_risk_score)




######################### ggplot groups ##########################

#### Age #################
hf_symptoms_plot_group %>%
    ggplot(aes(x= age, y= mveesept, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= age, y= gls_av, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= age, y= lvmi, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= age, y= lavi, colour = type_of_hf_symp))+
    geom_point()

#### BMI #############
hf_symptoms_plot_group %>%
    ggplot(aes(x= bmi, y= mveesept, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= bmi, y= gls_av, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= bmi, y= lvmi, colour = type_of_hf_symp))+
    geom_point()

hf_symptoms_plot_group %>%
    ggplot(aes(x= bmi, y= lavi, colour = type_of_hf_symp))+
    geom_point()


study_data_fct %>%
    mutate(type_of_hf_symp = case_when(hf_symptoms == 0 ~ 'No symptoms',
                                       # Overall_Summary_Score > 75 ~ 'Asyptomatic defined by KCCQ > 75',
                                       # Overall_Summary_Score <= 75 ~ 'Syptomatic defined by KCCQ < 75'),
                                       Clinical_Summary_Score > 75 ~ 'No symptoms defined by KCCQ',
                                       Clinical_Summary_Score <= 75 ~ 'Symptoms defined by KCCQ'),
           Quality_Life_Score = as.numeric(Quality_Life_Score),

           gender = haven::as_factor(gender)) %>%
    select(type_of_hf_symp, gender, age,"bmi",
           diabetes_duration,
           watch_dm_risk_score,

          # hdl,
          # chol, ldl, trig,
          # a1c_pcent, a1c_mmol,egfr ,
           bp_syst_mean, bp_diast_mean,# waist_1,# hip_1,
           #heart_rate, heart_rate2 ,
           # heart_rate3,
           prior_ischemic_event,
           prior_PCI,

           "mveesept",
           "lvmi",
           "lavi",
           "gls_av"#,
           #"abn_ee","abn_gls","abn_lavi","lvh", any_abnormality
           # "Physical_Limitation_Score", "Symptom_Frequency_Score",
           # "Quality_Life_Score", "Social_Limitation_Score", "Clinical_Summary_Score", "Overall_Summary_Score",
    ) %>%
    gtsummary::tbl_summary(by = type_of_hf_symp,
                           missing = "no") %>%
    add_overall()
