

# Time based dataset
rt_toxicityData = read.csv("data-raw/Fake_data_time.csv", stringsAsFactors = FALSE)


rt_toxicityData$Registration_date = as.Date(rt_toxicityData$Registration_date,format = "%d/%m/%Y")
rt_toxicityData$ae_onset_date = as.Date(rt_toxicityData$ae_onset_date,format = "%d/%m/%Y")
rt_toxicityData$ae_resolve_date = as.Date(rt_toxicityData$ae_resolve_date,format = "%d/%m/%Y")
rt_toxicityData$end_of_assessment_date = as.Date(rt_toxicityData$end_of_assessment_date,format = "%d/%m/%Y")


for(col in c("Cycle_1_date", "Cycle_2_date", "Cycle_3_date", "Cycle_4_date", "Cycle_5_date", "Cycle_6_date","end_of_treatment_date")) {
  rt_toxicityData[,col] = as.Date(rt_toxicityData[,col],format = "%d/%m/%Y")
}

rt_patientData = rt_toxicityData
rt_patientData = rt_patientData[c(1,7,12,18,23,27,28),]

rt_patientData = rt_patientData[,c("patientNo", "Treatment", "Registration_date", "Cycle_1_date", "Cycle_2_date", "Cycle_3_date", "Cycle_4_date", "Cycle_5_date", "Cycle_6_date","end_of_treatment_date", "end_of_assessment_date")]

rt_toxicityData = rt_toxicityData[,c("patientNo","toxicity", "category", "grade","ae_onset_date", "ae_resolve_date")]
rt_toxicityData = rt_toxicityData[-27,]

save(rt_toxicityData, file = "data/rt_toxicityData.rda")
save(rt_patientData, file = "data/rt_patientData.rda")

