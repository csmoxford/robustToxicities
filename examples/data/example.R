options=defaultToxicityOptions(trialName="FAKEtrial", folderPath="", fileName = "Fake_data.csv")




data = read.csv("examples/data/Fake_data.csv", stringsAsFactors = FALSE)

data = nameDatabase(data,
                    patid = "patientNo",
                    treatment = "Treatment",
                    ae_term = "toxicity",
                    ae_system = "category",
                    ae_grade = "grade",
                    ae_start_date = "ae_onset_date",
                    ae_end_date = "ae_resolve_date",
                    ae_cont_end_study = "ae_cont_end",
                    date_stopped_treatment = "end_of_treatment_date",
                    dateColumnNames = c("Registration_date", "Randomisation_date", "Cycle_1_date", "Cycle_2_date", "Cycle_3_date", "Cycle_4_date", "Cycle_5_date", "Cycle_6_date"))


cycleLabels = c("Registration", "Randomisation","Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "Cycle 5", "Cycle 6")



treatmentLabels = c("Placebo","Fake Drug")

toxDB = robustToxicities(data, cycleLabels, options, treatmentLabels)

toxDB@options@tabulationPercent = TRUE

print_toxTable_summary(toxDB, printMethod = "print")

toxDB@options@tabulationPercent = FALSE
print_toxTable_summary(toxDB, printMethod = "table")

print_toxTable_cycle(toxDB, cycles = 2 )

toxDB@options@plotLeftSideOption = "patid"
toxDB@options@plotStartTreatment = "cycle_start_date_3"
toxDB@options@plotCycleLength = 7
toxDB@options@plotxMin = -21
toxDB@options@plotxMax = 70


toxDB@options@plotLeftSideOption = "both"
toxPlot_time(toxDB)


# baseline
toxDB@cycleLabels[1]
print_toxTable_cycle(toxDB, cycles = 1)

# all toxicities (worst grade by patient)
print_toxTable_cycle(toxDB, cycles = "all")

# not cumulative
toxDB@options@cumulativeGrades = TRUE
print_toxTable_cycle(toxDB, cycles = 2)

# multi treatment
toxDB@cleanData$treatment = floor(runif(length(toxDB@cleanData$treatment),1,4))
toxDB@treatmentLabels = c("Treat1", "Treat2","Treat3")

print_toxTable_summary(toxDB, printMethod = "latex")

print_toxTable_cycle(toxDB, cycles = 2)
print_toxTable_cycle(toxDB, cycles = 2, printMethod = "latex")


toxDB@options@tabulationMethod = "all"
toxDB@options@tabulationMethod = "worst"

toxDB@options@cycleColumnMerge = "1,2|3,4,5"
print_toxTable_cycle(toxDB, cycles = 2)
