

test.LoadPackageData = function() {

  # Patient Level Data
  data("rt_patientData")
  # Toxicity Level Data
  data("rt_toxicityData")

  checkEquals(dim(rt_patientData), c(7,11), "rt_patientData not loaded correctly from file")
  checkEquals(dim(rt_toxicityData), c(28,6), "rt_toxicityData not loaded correctly from file")
}


test.SetupAndQueriesWorks = function() {

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

  checkTrue(!rt@wasQueried, "SetupRobustToxicities not completed correctly")

  # Look for queries. Note: must be called before running any
  # of the functions on this class.
  rt = QueryRobustToxicities(rt)

  checkTrue(rt@wasQueried, "SetupRobustToxicities not completed correctly")

}

test.SetupRobustToxicitiesDataFrameInputOnly = function() {

  # Patient Level Data
  data("rt_patientData")
  # Toxicity Level Data
  data("rt_toxicityData")

  rt_patientData = list(rt_patientData)

  names(rt_toxicityData)[1] = "patient"

  ret = tryCatch(
    SetupRobustToxicities(
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
    , error = function(e) e)

  checkEquals(ret$message, "patientData must be of class data.frame", "Correct error message not thrown when patientData is not a data.frame")

  rt_patientData = as.data.frame(rt_patientData)
  rt_toxicityData = list(rt_toxicityData)

  ret = tryCatch(
    SetupRobustToxicities(
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
    , error = function(e) e)

  checkEquals(ret$message, "toxData must be of class data.frame", "Correct error message not thrown when toxData is not a data.frame")


}

test.SetupRobustToxicitiesErrorMessages = function() {

  # Patient Level Data
  data("rt_patientData")
  # Toxicity Level Data
  data("rt_toxicityData")


  names(rt_toxicityData)[1] = "patient"

  # Run the setup command passing in all the column names.
  ret = tryCatch(
    SetupRobustToxicities(
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
    , error = function(e) e)

  checkEquals(ret$message, "Column named patientNo not found in toxData data.frame", "Correct error message not thrown when toxData doesn't contain patidCol")

  names(rt_toxicityData)[1] = "patientNo"
  names(rt_patientData)[1] = "patient"

  # Run the setup command passing in all the column names.
  ret = tryCatch(
    SetupRobustToxicities(
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
    , error = function(e) e)

  checkEquals(ret$message, "Column named patientNo not found in patientData data.frame", "Correct error message not thrown when patientData doesn't contain patidCol")
}
