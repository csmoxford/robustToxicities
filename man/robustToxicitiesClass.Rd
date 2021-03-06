% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_robustToxicitiesClass.R
\name{robustToxicitiesClass}
\alias{robustToxicitiesClass}
\alias{robustToxicitiesClass-class}
\title{The robustToxicitiesClass}
\description{
This is core object of this package. This object stores the original dataset as well as the automatically cleaned dataset and a list of notes and queries generated when cleaning the dataset. A list of options is also provided to store plot and tabulation options and provide additional metadata. Finally treatment and cycle labels are also required.
}
\section{Slots}{

\describe{
\item{\code{toxData}}{The toxicitydataset}

\item{\code{patientData}}{Patient level data}

\item{\code{patidCol}}{Column name for the participant identifier}

\item{\code{treatmentCol}}{Column name for the treatment}

\item{\code{toxCategoryCol}}{Column name for aderse event category}

\item{\code{toxNameCol}}{Column name for adverse event name}

\item{\code{toxGradeCol}}{Column name for the adverse event grade}

\item{\code{dateOfStartTox}}{Column name for date of adverse event start or change in grade}

\item{\code{dateOfEndTox}}{Column name for date of adverse event end or change in grade}

\item{\code{dateOfStartOfToxWindow}}{Column name for date of study entry (eg registration)}

\item{\code{dateOfEndOfToxWindow}}{Column name for the end of the time window for the particitant to be observed for toxicities}

\item{\code{periodDividerCols}}{Column names for date dividing times into periods or cycles (optional)}

\item{\code{periodDividerLabels}}{Display names for data periodDividerCols}

\item{\code{treatmentCodes}}{Codes which match the values in treatmentCol}

\item{\code{treatmentLabels}}{Labels to used instea of the treatment codes}

\item{\code{queries}}{A data.frame containing all the queries and note generated when loading the data}

\item{\code{options}}{An s4 object of class \code{\link{toxicityOptions-class}} containing options and metadata for the files.}

\item{\code{wasQueried}}{Logical detailing if queries were run on this object.}
}}

