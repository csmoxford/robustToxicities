# toxData = read.csv()
data(rt_patientData)
data(rt_toxicityData)


colnames(rt_patientData)
colnames(rt_toxicityData)

rt = SetupRobustToxicities(
  toxData = rt_toxicityData,
  patientData = rt_patientData,
  patidCol = "patientNo",
  treatmentCol = "Treatment",
  toxCategoryCol = "category",
  toxNameCol = "toxicity",
  toxGradeCol = "grade",
  dateOfStartOfToxWindow = "Registration_date",
  dateOfStartTox = "ae_onset_date",
  dateOfEndTox = "ae_resolve_date",
  dateOfEndOfToxWindow = "end_of_assessment_date",
  periodDividerCols = c("Cycle_1_date", "Cycle_2_date", "Cycle_3_date", "Cycle_4_date", "Cycle_5_date", "Cycle_6_date"),
  periodDividerLabels = c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "Cycle 5", "Cycle 6")
)


rt = QueryRobustToxicities(rt)

# toxicity options.
rt@options@toxTable_tabulationPercent = TRUE
rt@options@toxTable_cycle_toxicityOrder = "n"


rt@options@toxTable_mergeGrades = "n|1|2|3|4,5"

# Summary table
ToxTable_summary(rt)

# flexTable version of summary table (prettyTables wrapper for the ReporteRs package)
ft = FT_ToxTable_summary(rt)
ft$GetTable()

# Cycle 1
tble = ToxTable_cycle(rt, "Cycle_1_date")
tble

# all cycles
ft = FT_ToxTable_category(rt, "all")
ft$GetTable()


# toxicity plot with one row per adverse event per patient
toxPlot_byToxicity(rt,
                   plotLeftSideOption = "both",
                   xlim = c(-7,60),
                   plotCycleLength = 21,
                   plotCycles = 6,
                   plotXLegendScale = "weeks")

# toxicity plot with one row per patietns
toxPlot_byPatient(rt,
                  plotLeftSideOption = "both",
                  xlim = c(-7,60),
                  plotCycleLength = 21,
                  plotCycles = 6,
                  plotXLegendScale = "days")

# toxicity sumamry plot by cycle
par(mar=c(3,6,2,2))
toxPlot_byCycle(rt,
                gradeRequired = 1,
                tableSpace = 0.05)

# toxicity sumamry plot by time boundaries from baseline
par(mar=c(5,6,2,3))
toxPlot_byTime(rt,
               gradeRequired = 1,
               tableSpace = 0.05,
               legendPosition = "bottom",
               timeBoundaries = c(0:10)*7)

# underlying data for this plot can be extracted using:
rt2 = CreateTimeDividers(rt,
                         timeBoundaries = c(0:10)*7)
# then tabulated using ToxTable_summary
ToxTable_summary(rt2)
# note that CreateTimeDividers overwrites the rt@periodDividerCols and periodDividerLabelsl;
