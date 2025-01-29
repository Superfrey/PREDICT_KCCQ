

library(dplyr)
library(stringr)

hist(df$walk_time_tot)
kccq_data$vigourous_number_other

study_data_fct <- study_data_fct %>%
    # Incidental walking
    mutate(
        walk_time_inc = ifelse(!is.na(walk_number_to_places), walking_minutes + (walking_hours * 60), NA),
        walk_time_inc = ifelse(walk_number_to_places == 0, 0, walk_time_inc)
    ) %>%

    # Fitness-oriented walking
    mutate(
        walk_time_fit = ifelse(!is.na(walk_number_fitness), walking_fitness_minutes + (walking_fitness_hours * 60), NA),
        walk_time_fit = ifelse(walk_number_fitness == 0, 0, walk_time_fit)
    ) %>%

    # Vigorous exercise
    mutate(
        vig_other_hour = ifelse(!is.na(vigourous_number_other), str_sub(vigorous_time_other, 1, 2), NA),
        vig_other_min = ifelse(!is.na(vigourous_number_other), str_sub(vigorous_time_other, -2, -1), NA),
        vig_other_hour_num = as.numeric(vig_other_hour),
        vig_other_min_num = as.numeric(vig_other_min),
        vig_other_time = ifelse(!is.na(vigourous_number_other), (vig_other_hour_num * 60) + vig_other_min_num, NA)
    ) %>%
    select(-vig_other_hour, -vig_other_min, -vig_other_hour_num, -vig_other_min_num) %>%

    # Moderate exercise
    mutate(
        mod_hour = ifelse(!is.na(moderate_number), str_sub(moderate_time, 1, 2), NA),
        mod_min = ifelse(!is.na(moderate_number), str_sub(moderate_time, -2, -1), NA),
        mod_hour_num = as.numeric(mod_hour),
        mod_min_num = as.numeric(mod_min),
        mod_time = ifelse(!is.na(moderate_number), (mod_hour_num * 60) + mod_min_num, NA)
    ) %>%
    select(-mod_hour, -mod_min, -mod_hour_num, -mod_min_num) %>%

    # Strength training
    mutate(
        strength_hour = ifelse(!is.na(strength_number), str_sub(strength_time, 1, 2), NA),
        strength_min = ifelse(!is.na(strength_number), str_sub(strength_time, -2, -1), NA),
        strength_hour_num = as.numeric(strength_hour),
        strength_min_num = as.numeric(strength_min),
        strength_time_calc = ifelse(!is.na(strength_number), (strength_hour_num * 60) + strength_min_num, NA)
    ) %>%
    select(-strength_hour, -strength_min, -strength_hour_num, -strength_min_num) %>%

    # Average walk time per session
    mutate(
        walktime_inc_per_sess = ifelse(!is.na(walk_number_to_places) & !is.na(walk_time_inc), walk_time_inc / walk_number_to_places, NA),
        walktime_fit_per_sess = ifelse(!is.na(walk_number_fitness) & !is.na(walk_time_fit), walk_time_fit / walk_number_fitness, NA)
    ) %>%

    # Handle inconsistent data
    mutate(
        walk_number_to_places = ifelse(walktime_inc_per_sess < 10, NA, walk_number_to_places),
        walk_time_inc = ifelse(walktime_inc_per_sess < 10, NA, walk_time_inc),
        walk_number_fitness = ifelse(walktime_fit_per_sess < 10, NA, walk_number_fitness),
        walk_time_fit = ifelse(walktime_fit_per_sess < 10, NA, walk_time_fit)
    ) %>%

    # Ensure zeros recorded correctly
    mutate(
        walk_time_inc = ifelse(walk_number_to_places == 0 & !is.na(walk_number_to_places), 0, walk_time_inc),
        walk_time_fit = ifelse(walk_number_fitness == 0 & !is.na(walk_number_fitness), 0, walk_time_fit),
        walk_time_tot = ifelse(walk_number_to_places == 0 & walk_number_fitness == 0, 0, NA), ## changed
        mod_time = ifelse(moderate_number == 0 & !is.na(moderate_number), 0, mod_time),
        vig_other_time = ifelse(vigourous_number_other == 0 & !is.na(vigourous_number_other), 0, vig_other_time)
    ) %>%

    # Total walking
    mutate(
        walk_time_tot = ifelse(!is.na(walk_time_inc), walk_time_inc, 0) +
            ifelse(!is.na(walk_time_fit), walk_time_fit, 0),
        walk_time_tot = ifelse(is.na(walk_number_to_places) | is.na(walk_number_fitness), NA, walk_time_tot)
    ) %>%

    # Truncate outliers
    mutate(
        walk_time_inc = ifelse(walk_time_inc > 840 & !is.na(walk_time_inc), 840, walk_time_inc),
        walk_time_fit = ifelse(walk_time_fit > 840 & !is.na(walk_time_fit), 840, walk_time_fit),
        walk_time_tot = ifelse(walk_time_tot > 840 & !is.na(walk_time_tot), 840, walk_time_tot),
        mod_time = ifelse(mod_time > 840 & !is.na(mod_time), 840, mod_time),
        vig_other_time = ifelse(vig_other_time > 840 & !is.na(vig_other_time), 840, vig_other_time)
    ) %>%

    # Total exercise time
    mutate(
        exercise_time = ifelse(!is.na(walk_time_tot), walk_time_tot, 0) +
            ifelse(!is.na(mod_time), mod_time, 0) +
            ifelse(!is.na(vig_other_time), vig_other_time * 2, 0),
        exercise_time = ifelse(exercise_time > 1680 & !is.na(exercise_time), 1680, exercise_time),
        exercise_time = ifelse(is.na(walk_number_to_places) | is.na(walk_number_fitness) | is.na(moderate_number) | is.na(vigourous_number_other), NA, exercise_time)
    ) %>%

    mutate(walk_time_recommendation = case_when(walk_time_tot >= 180 ~ "meet_reccomendation",
                                     walk_time_tot < 180 ~ "Do_not_meet_reccomendation")) %>%
    # Logic check: strength training consistency
    filter((strength_time_calc - (mod_time + vig_other_time)) <= 0 |
               is.na(strength_time_calc) | is.na(mod_time) | is.na(vig_other_time))


########################################################################################
diabetes_duration_physical <- lm(log(gls_av) ~ walk_time_recommendation  + gender + age , data = study_data_fct)
hist(resid(diabetes_duration_physical))
ci.exp(diabetes_duration_physical)
hist(study_data_fct$gls_av)

diabetes_duration_physical <- lm(Clinical_Summary_Score ~ walk_time_recommendation + age + gender , data = study_data_fct)
summary(diabetes_duration_physical)
hist(resid(diabetes_duration_physical))
ci.lin(diabetes_duration_physical)
hist(study_data_fct$exercise_time)

plot(study_data_fct$walk_time_tot,study_data_fct$Physical_Limitation_Score)

plot(study_data_fct$walk_time_tot,study_data_fct$Clinical_Summary_Score)

plot(study_data_fct$walk_time_tot,study_data_fct$Symptom_Frequency_Score)



plot(study_data_fct$exercise_time,study_data_fct$Physical_Limitation_Score)

plot(study_data_fct$exercise_time,study_data_fct$Clinical_Summary_Score)

plot(study_data_fct$exercise_time,study_data_fct$Symptom_Frequency_Score)

ci.exp(lm(log(Clinical_Summary_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))

ci.exp(lm(log(Clinical_Summary_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(Physical_Limitation_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(Symptom_Frequency_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(Overall_Summary_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))


ci.exp(lm(log(gls_av) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(mveesept) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(lavi) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(lm(log(lvmi) ~ walk_time_recommendation + age + gender , data = study_data_fct))

# Association with KCCQ domain but not echo measures

############################### Hba1c ###########################################################

ci.exp(lm(log(Clinical_Summary_Score) ~ a1c_pcent + age + gender, data = study_data_fct))
ci.lin(lm(Physical_Limitation_Score ~ a1c_pcent + age + gender , data = study_data_fct))
ci.exp(lm(log(Symptom_Frequency_Score) ~ a1c_pcent + age + gender , data = study_data_fct))
ci.exp(lm(log(Overall_Summary_Score) ~ a1c_pcent + age + gender , data = study_data_fct))


ci.exp(lm(log(gls_av) ~ a1c_pcent + age + gender , data = study_data_fct))
ci.exp(lm(log(mveesept) ~ a1c_pcent + age + gender , data = study_data_fct))
ci.exp(lm(log(lavi) ~ a1c_pcent + age + gender , data = study_data_fct))
ci.exp(lm(log(lvmi) ~ a1c_pcent + age + gender , data = study_data_fct))

###############################

ci.exp(glm(as.factor(Clinical_Summary_Score ~ walk_time_recommendation + age + gender , data = study_data_fct))

round(ci.exp(glm(as.factor(Clinical_Score_75cu) ~ walk_time_recommendation + age + gender , data = study_data_fct, family = binomial(link = 'logit'))),3)
ci.exp(glm(Physical_Limitation_Score ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(glm(log(Symptom_Frequency_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))
ci.exp(glm(log(Overall_Summary_Score) ~ walk_time_recommendation + age + gender , data = study_data_fct))
str(study_data_fct$ Overall_Score_80cu)
