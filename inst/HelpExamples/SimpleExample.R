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

###########################################################
# Table Examples.
###########################################################
# Summary, worst grade by cycle
ToxTable_summary(rt)

# ReporteRs flextable version
ft = FT_ToxTable_summary(rt)
ft$GetTable()


# Worst grade by patient for each toxicity type
ToxTable_cycle(rt)

# ReporteRs flextable version
ft = FT_ToxTable_cycle(rt)
ft$GetTable()

# Worst grade by category
ToxTable_category(rt)

# ReporteRs flextable version
ft = FT_ToxTable_category(rt)
ft$GetTable()

# Alternative style for worst grade by category
ToxTable_categories(rt)

###########################################################
# Plot Examples
###########################################################
ToxPlot_byToxicity(rt)

ToxPlot_byPatient(rt)

ToxPlot_byCycle(rt)

###########################################################
# Alternative specification to cycles
###########################################################
# wrapper for toxPlot_byCycle adding alternative boundaries
timeBoundaries = c(0,21,42,63,84,105,126)
rt2 = CreateTimeDividers(rt, timeBoundaries)
ToxTable_summary(rt2)


ToxPlot_byTime(rt, timeBoundaries = timeBoundaries)

