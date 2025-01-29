library(here)
library(bit64)
source(here::here("R/1_data_management/prep_library.R"))
source(here::here("R/functions/functions.R"))

# Import KCCQ data
#kccq_data_old <- haven::read_dta("S:/Progression of DM Complications/Questionnaires. Physical Function Tests. REDCap pdf copy for printing/KCCQ/Data 2024-07-16/kccq_bl_2024-07-16.dta")
kccq_data <- read.csv("S:/Jonas Schaarup/ProgressionOfDiabeti_DATA_2024-12-12_1538.csv")

kccq_data <- kccq_data %>%
    group_by(record_id) %>%
    mutate(
        # Capture the stroke value from baseline
        baseline_heart_attack = stroke[redcap_event_name == "baseline_arm_1"],
        # Use baseline_stroke for fu1_arm_1 rows
        heart_attack = if_else(
            redcap_event_name == "fu1_arm_1",
            baseline_heart_attack,
            heart_attack
        ),

        baseline_stroke = stroke[redcap_event_name == "baseline_arm_1"],
        # Use baseline_stroke for fu1_arm_1 rows
        stroke = if_else(
            redcap_event_name == "fu1_arm_1",
            baseline_stroke,
            stroke
        ),

        baseline_heart_bypass = heart_bypass[redcap_event_name == "baseline_arm_1"],
        # Use baseline_stroke for fu1_arm_1 rows
        heart_bypass = if_else(
            redcap_event_name == "fu1_arm_1",
            baseline_heart_bypass,
            heart_bypass
        )#,

        # baseline_pad_revasc = pad_revasc[redcap_event_name == "baseline_arm_1"],
        # pad_revasc = if_else(
        #     redcap_event_name == "fu1_arm_1",
        #     baseline_pad_revasc,
        #     pad_revasc
        # )
    ) %>%
    ungroup()

kccq_data_score <- kccq_data %>%
    filter(!is.na(hf_symptoms))

kccq_data_score <- kccq_data_score %>%
     kccq_score_function()

kccq_data <- kccq_data_score %>%
    as_factor() %>%
    mutate(kccq_date = as.Date(kccq_date, format = "%d/%m/%Y"),
           screening_date  = as.Date(screening_date, format = "%d/%m/%Y"),
           type_of_hf_symp = case_when(hf_symptoms == 0 ~ 'Asymptomatic',
                                       Overall_Summary_Score > 75 ~ 'Assyptomatic defined by KCCQ',
                                       Overall_Summary_Score < 75 ~ 'Syptomatic defined by KCCQ'),
           diabetes_duration = age - age_dm_diag)


kccq_data_gplimpse <- kccq_data %>%
    select(record_id,"redcap_event_name", Physical_Limitation_Score)
table(!is.na(kccq_data$Physical_Limitation_Score)) # 1090 KCCQ scores


### Import Echo data
echo_data <- haven::read_dta("S:/Jonas Schaarup/echo_2025-01-08.dta")
common_col <- setdiff(intersect(names(kccq_data_score), names(echo_data)), c("record_id","redcap_event_name"))

filtered_echo_df <- echo_data

### Merge Echo
PREDICT_data_old <- haven::read_dta("S:/Jonas Schaarup/Redcapandecho190824.dta")

extra_covariates <- PREDICT_data_old %>%
    mutate(record_id = ifelse(grepl("\\.5", record_id), floor(record_id), record_id)) %>%
    select("record_id","redcap_event_name", a1c_mmol, a1c_pcent, glucosef,chol,
           hdl,trig, ldl, albumin, ur_albumin_1, ur_albumin_2, ur_albumin_3, ur_albumin_4,
           egfr ,creat, ur_creat_1, ur_creat_2, ur_creat_3, ur_creat_4)

study_data <- left_join(kccq_data, filtered_echo_df, by = c("record_id", "redcap_event_name")) %>%
    mutate(gender = gender.y) %>%
    left_join(extra_covariates, by = c("record_id", "redcap_event_name"))

table(!is.na(study_data$Physical_Limitation_Score), !is.na(study_data$gls_av))
table(study_data$hf_hx)


summary(kccq_data$kccq_date)
summary(kccq_data$screening_date)
start_kccq_date <-  min(kccq_data$kccq_date, na.rm = TRUE)

study_data <- study_data %>%
    filter(screening_date > start_kccq_date)

length(unique(study_data$record_id))
table(!is.na(study_data$Physical_Limitation_Score), !is.na(study_data$gls_av))
table(!is.na(study_data$Physical_Limitation_Score), !is.na(study_data$gls_av))

study_data_gplimpse <-study_data %>%
    select(record_id, redcap_event_name,"abn_ee","abn_gls","abn_lavi","lvh","Physical_Limitation_Score", "Symptom_Frequency_Score", "Quality_Life_Score",
           "Social_Limitation_Score", "Overall_Summary_Score", "Clinical_Summary_Score")

# Exclude population before on start of KCCQ
# study_data <- study_data %>%
#     dplyr::filter(!is.na(Physical_Limitation_Score) | !is.na(Symptom_Frequency_Score) #|
#                   #!is.na(Quality_Life_Score) | !is.na(Social_Limitation_Score)
#                   #| !is.na(Overall_Summary_Score) | !is.na(Clinical_Summary_Score)) %>%
#     dplyr::filter(hf_hx == 0) %>%
#     dplyr::mutate(hf_symptoms = as_factor(hf_symptoms),
#                   hf_symptoms = as.factor(hf_symptoms))# %>%

table(!is.na(study_data$Physical_Limitation_Score), !is.na(study_data$gls_av))

study_data_complete <- study_data %>%
    dplyr::filter(!is.na(Physical_Limitation_Score) | !is.na(Symptom_Frequency_Score) | !is.na(Quality_Life_Score) | !is.na(Social_Limitation_Score) | !is.na(Overall_Summary_Score) | !is.na(Clinical_Summary_Score))%>%
    mutate(dummy_score = 1)%>%
    filter(if_any(c("abn_ee","abn_gls","abn_lavi","lvh"), ~ !is.na(.))) %>%
    mutate(any_abnormality = if_else(abn_ee == 1 | abn_gls == 1 | abn_lavi == 1 | lvh == 1, 1, 0))


study_data_long <- study_data_complete %>%
    filter(hf_hx == "0" | is.na(hf_hx)) %>%
    rename_with(~ paste0("t_kccq_",.), c(Physical_Limitation_Score, Symptom_Frequency_Score, Quality_Life_Score, Social_Limitation_Score, Overall_Summary_Score, Clinical_Summary_Score
                                         )) %>%
    rename_with(~ paste0("t_kccq_",.), ends_with("rescaled")) %>%
    df_kccq_long_format()

study_data_long_dicho <- study_data_complete %>%
    filter(hf_hx == "0" | is.na(hf_hx)) %>%
    rename_with(~ paste0("t_kccq_", .),
                c(Physical_Limitation_Score, Symptom_Frequency_Score, Quality_Life_Score,
                  Social_Limitation_Score, Overall_Summary_Score, Clinical_Summary_Score)) %>%
    rename_with(~ paste0("t_kccq_", .), ends_with("rescaled")) %>%
    mutate(across(starts_with("t_kccq_"),
                  ~ factor(ifelse(. < 100, "low", "high"), levels = c("low", "high")))) %>%
    df_kccq_long_format()


study_data_complete <- study_data_complete %>%
    calculate_watch_dm_risk_score() %>%
    mutate(
           Clinical_Score_9100cu = case_when(Clinical_Summary_Score == 100 ~ "0",
                                           Clinical_Summary_Score < 100 ~ "1"),
           Clinical_Score_90cu = case_when(Clinical_Summary_Score > 90 ~ "0",
                                           Clinical_Summary_Score <=90 ~ "1"),
           Clinical_Score_80cu = case_when(Clinical_Summary_Score > 80 ~ "0",
                                           Clinical_Summary_Score <=80 ~ "1"),
           Clinical_Score_75cu = case_when(Clinical_Summary_Score > 75 ~ "0",
                                           Clinical_Summary_Score <=75 ~ "1"),

           Overall_Score_9100cu = case_when(Overall_Summary_Score == 100 ~ "0",
                                           Overall_Summary_Score < 100 ~ "1"),

           Overall_Score_90cu= case_when(Overall_Summary_Score > 90 ~ "0",
                                         Overall_Summary_Score <=90 ~ "1"),
           Overall_Score_80cu= case_when(Overall_Summary_Score > 80 ~ "0",
                                         Overall_Summary_Score <=80 ~ "1"),
           Overall_Score_75cu= case_when(Overall_Summary_Score > 75 ~ "0",
                                         Overall_Summary_Score <=75 ~ "1"),

           Symptom_Score_9100cu = case_when(Symptom_Frequency_Score == 100 ~ "0",
                                           Symptom_Frequency_Score < 100 ~ "1"),

           Symptom_Score_90cu = case_when(Symptom_Frequency_Score > 90 ~ "0",
                                          Symptom_Frequency_Score <=90 ~ "1"),
           Symptom_Score_80cu = case_when(Symptom_Frequency_Score > 80 ~ "0",
                                          Symptom_Frequency_Score <=80 ~ "1"),
           Symptom_Score_75cu = case_when(Symptom_Frequency_Score > 75 ~ "0",
                                          Symptom_Frequency_Score <=75 ~ "1"),

           Physical_Score_9100cu = case_when(Physical_Limitation_Score == 100 ~ "0",
                                            Physical_Limitation_Score < 100 ~ "1"),

           Physical_Score_90cu = case_when(Physical_Limitation_Score > 90 ~ "0",
                                          Physical_Limitation_Score <=90 ~ "1"),
           Physical_Score_80cu = case_when(Physical_Limitation_Score > 80 ~ "0",
                                          Physical_Limitation_Score <=80 ~ "1"),
           Physical_Score_75cu = case_when(Physical_Limitation_Score > 75 ~ "0",
           Physical_Limitation_Score <=75 ~ "1"),

           watch_dm_11_high_risk_cu= case_when(watch_dm_risk_score >= 11 ~ "1",
                                     watch_dm_risk_score < 11 ~ "0"),

           watch_dm_11_high_risk_sob_cu= case_when(watch_dm_risk_score >= 11 & hf_symptoms== "1"~ "1",
                                                                             hf_symptoms== "0" ~ "0"),

           watch_dm_11_high_risk_kccq_cu= case_when(watch_dm_risk_score >= 11 & Clinical_Score_80cu== "1"~ "1",
                                                    Clinical_Score_80cu== "0" ~ "0"),
    prior_cvd = if_else(heart_attack == 1 |mi_fu1 == 1|stroke_fu_fu1==1 | stroke == 1 | heart_bypass == 1 | cardiac_proc_fu1 == 1#| pad_revasc == 1
                        , 1, 0),
    prior_ischemic_event = if_else(heart_attack == 1 | stroke == 1 | mi_fu1 == 1| stroke_fu_fu1==1  #| pad_revasc == 1
                                   , 1, 0),
    prior_PCI = if_else(heart_bypass == 1| cardiac_proc_fu1 == 1 #| pad_revasc == 1
                        , 1, 0),
    three_point_mace = prior_cvd)

hf_diagnosis <- c("abn_ee","abn_gls","abn_lavi","lvh")## "HFpEF", "HFmrEF_rEF"

study_data_fct <- study_data_complete %>%
    mutate(across(c("abn_ee","abn_gls","abn_lavi","lvh","any_abnormality"), as.factor),
       )

study_data_fct <- study_data_fct %>%
    filter(hf_hx == "0" | is.na(hf_hx)) %>%
    filter(if_any(c("abn_ee","abn_gls","abn_lavi","lvh"), ~ !is.na(.))) %>%
    mutate(any_abnormality = as.factor(as.numeric(any_abnormality)-1),
           any_abnormality = as_factor(any_abnormality))

table(study_data_fct$three_point_mace)

population_below_75kccq <- study_data_fct %>%
    filter(Clinical_Score_75cu == "1") %>%
    filter(any_abnormality == "0")

write.csv(population_below_75kccq, file = here("data-raw/population_below_75kccq_normal_echo.csv") )


