
study_data_fct_80 <- study_data_fct %>%
    filter(Clinical_Score_80cu=="1")

plot(study_data_fct$watch_dm_risk_score, study_data_fct$gls_av)

plot(study_data_fct_80$watch_dm_risk_score, study_data_fct_80$gls_av)

plot(study_data_fct_80$watch_dm_risk_score, study_data_fct_80$lvmi)

plot(study_data_fct_80$watch_dm_risk_score, study_data_fct_80$lavi)

plot(study_data_fct_80$watch_dm_risk_score, study_data_fct_80$mveesept)


plot(study_data_fct$Clinical_Summary_Score, study_data_fct$gls_av)

plot(study_data_fct$Clinical_Summary_Score, study_data_fct$lvmi)

plot(study_data_fct$Clinical_Summary_Score, study_data_fct$lavi)

plot(study_data_fct$Clinical_Summary_Score, study_data_fct$mveesept)
