# Patient Level Data
data("rt_patientData")
# Toxicity Level Data
data("rt_toxicityData")


# Run the setup command passing in all the column names.
rt = SetupRobustToxicities(
  toxData = rt_toxicityData,
  patientData = rt_patientData,
  patidCol = "patientNo", treatmentCol = "Treatment",
  toxCategoryCol = "category", toxNameCol = "toxicity",
  toxGradeCol = "grade", dateOfStartOfToxWindow = "Registration_date",
  dateOfStartTox = "ae_onset_date", dateOfEndTox = "ae_resolve_date",
  dateOfEndOfToxWindow = "end_of_assessment_date",
  periodDividerCols = c("Registration_date", "Cycle_1_date","Cycle_2_date",
                        "Cycle_3_date", "Cycle_4_date", "Cycle_5_date", "Cycle_6_date"),
  periodDividerLabels = c("Pre treatment", "Cycle 1","Cycle 2",
                          "Cycle 3","Cycle 4","Cycle 5", "Cycle 6"),
  treatmentCodes = NULL, treatmentLabels = NULL, options = NULL)

# Look for queries. Note: must be called before running any
# of the functions on this class.
rt = QueryRobustToxicities(rt)

##########################################################
par(mar = c(4,6,3,2))
ToxPlot_byCycle(rt)

##########################################################
# subset to a specific set of adverse events
rt@toxData$ass_TRUE = rt@toxData$toxicity == "Sore Throat"
par(mar = c(4,6,3,2))
ToxPlot_byCycle(rt)
