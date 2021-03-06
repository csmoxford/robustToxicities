% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SetupRobustToxicities.R
\name{SetupRobustToxicities}
\alias{SetupRobustToxicities}
\alias{robustToxicities}
\title{robustToxicitiesClass generator}
\usage{
SetupRobustToxicities(toxData, patientData, patidCol, treatmentCol = NULL,
  toxCategoryCol, toxNameCol, toxGradeCol, dateOfStartOfToxWindow,
  dateOfStartTox, dateOfEndTox, dateOfEndOfToxWindow,
  periodDividerCols = character(0), periodDividerLabels = character(0),
  treatmentCodes = NULL, treatmentLabels = NULL, options = NULL)
}
\arguments{
\item{toxData}{The toxicity level data set}

\item{patientData}{The patient level data}

\item{patidCol}{Column name for the participant identifier}

\item{treatmentCol}{Column name for the treatment. Will be created if not provided}

\item{toxCategoryCol}{Column name for aderse event category}

\item{toxNameCol}{Column name for adverse event name}

\item{toxGradeCol}{Column name for the adverse event grade}

\item{dateOfStartOfToxWindow}{Column name for date of study entry (eg registration)}

\item{dateOfStartTox}{Column name for date of adverse event start or change in grade}

\item{dateOfEndTox}{Column name for date of adverse event end or change in grade}

\item{dateOfEndOfToxWindow}{Column name for the end of the time window for the particitant to be observed for toxicities (optional)}

\item{periodDividerCols}{Column names for date dividing times into periods or cycles (optional)}

\item{periodDividerLabels}{Display names for data periodDividerCols
param treatmentLabels A vector of treatment labels}

\item{treatmentCodes}{Levels of treatment in the treatmentCol}

\item{treatmentLabels}{What to name each treatment in output tables}

\item{options}{Optional. An object of class toxicityOptions. The easiest place to start is with \code{DefaultToxicityOptions()}. See \code{\link{DefaultToxicityOptions}} for more details on options.}
}
\value{
An object of class \code{\link{robustToxicitiesClass}}
}
\description{
The robustToxicities package aims to make creating publication ready table and graphs from time based toxicity data easy. The package also performs some built in data cleaning actions.
}
\details{
Run this to create an object of class \code{\link{robustToxicitiesClass}}. Then run it through \code{\link{QueryRobustToxicities}} to check for errors before creating tables and graphs of the data.

This function takes two linked data.frames. A one row per patient, patient level data.frame (\code{patientData}) and a one row per toxicity data.frame of toxicities (\code{toxData}). The remaining parameters tell the package where the columns which are required to create the plots and graphs are. There is also an options class (\code{\link{toxicityOptions-class}}) which can be edited from the default later. Since the \code{\link{robustToxicitiesClass}} object is an s4 class slots are accessed using the @ symbol.
}
\examples{
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

# With causality
# Not provided so generate some
rt@toxData$causality1 = sample(1:5,28, replace = TRUE)
rt@toxData$causality2 = sample(1:5,28, replace = TRUE)

causality = ToxPlot_causalityInfo(
  columns = c("causality1","causality2"),
  names = c("A","BA"),
  width = 1.5,
  pch = c(NA,NA,4,8,16),
  cex = 1.2)


ToxPlot_byToxicity(rt,
                   causality = causality)




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

}
