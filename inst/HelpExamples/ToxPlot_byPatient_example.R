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
ToxPlot_byPatient(rt)


##########################################################
# Subset to a range. Usefull for plotting over a number of figures if there
# are lots of adverse events
ToxPlot_byPatient(rt, rowID_range = c(1,3), plotLeftSideOption = "both")

##########################################################
# subset to a specific set of adverse events
rt@toxData$ass_TRUE = rt@toxData$toxicity == "Headache"
ToxPlot_byPatient(rt)


#########################################################
# Add event data
rt@toxData$ass_TRUE = TRUE

event_EOT = ToxPlot_eventInfo(
  columns = c("end_of_treatment_date"),
  label = c("End Of Treatment"),
  lwd = 4,
  col = c("blue")
)

event_EOA = ToxPlot_eventInfo(
  columns = c("end_of_assessment_date"),
  label = c("End Of Assessment"),
  lwd = 4,
  col = c("green")
)
ToxPlot_byPatient(rt)
ToxPlot_byPatient(rt,
                  xlim = c(-7, 100),
                  events = list(event_EOT, event_EOA))

#########################################################
# Change offset event
event_SOT = ToxPlot_eventInfo(
  columns = c("Registration_date"),
  label = c("Registration Date"),
  lwd = 4,
  col = c("orange")
)

ToxPlot_byPatient(rt,
                  xlim = c(-67, 40),
                  xlab = "Days from end of treatment",
                  events = list(event_SOT, event_EOT, event_EOA),
                  offsetEvent = "end_of_treatment_date")
