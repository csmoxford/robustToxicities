#'
#' robustToxicitiesClass generator
#'
#' The robustToxicities package aims to make creating publication ready table and graphs from time based toxicity data easy. The package also performs some built in data cleaning actions.
#'
#' Run this to create an object of class \code{\link{robustToxicitiesClass}}. Then run it through \code{\link{QueryRobustToxicities}} to check for errors before creating tables and graphs of the data.
#'
#' This function takes two linked data.frames. A one row per patient, patient level data.frame (\code{patientData}) and a one row per toxicity data.frame of toxicities (\code{toxData}). The remaining parameters tell the package where the columns which are required to create the plots and graphs are. There is also an options class (\code{\link{toxicityOptions-class}}) which can be edited from the default later. Since the \code{\link{robustToxicitiesClass}} object is an s4 class slots are accessed using the @ symbol.
#'
#' @param toxData The toxicity level data set
#' @param patientData The patient level data
#' @param patidCol Column name for the participant identifier
#' @param treatmentCol Column name for the treatment. Will be created if not provided
#' @param toxCategoryCol Column name for aderse event category
#' @param toxNameCol Column name for adverse event name
#' @param toxGradeCol Column name for the adverse event grade
#' @param dateOfStartOfToxWindow Column name for date of study entry (eg registration)
#' @param dateOfStartTox Column name for date of adverse event start or change in grade
#' @param dateOfEndTox Column name for date of adverse event end or change in grade
#' @param dateOfEndOfToxWindow Column name for the end of the time window for the particitant to be observed for toxicities (optional)
#' @param periodDividerCols Column names for date dividing times into periods or cycles (optional)
#' @param periodDividerLabels Display names for data periodDividerCols
#' param treatmentLabels A vector of treatment labels
#' @param treatmentCodes Levels of treatment in the treatmentCol
#' @param treatmentLabels What to name each treatment in output tables
#' @param options Optional. An object of class toxicityOptions. The easiest place to start is with \code{DefaultToxicityOptions()}. See \code{\link{DefaultToxicityOptions}} for more details on options.
#'
#' @return An object of class \code{\link{robustToxicitiesClass}}
#'
#' @example inst/HelpExamples/SimpleExample.R
#' @aliases robustToxicities
#'
#' @export SetupRobustToxicities
SetupRobustToxicities = function(toxData, patientData, patidCol, treatmentCol = NULL, toxCategoryCol, toxNameCol, toxGradeCol, dateOfStartOfToxWindow, dateOfStartTox, dateOfEndTox, dateOfEndOfToxWindow, periodDividerCols = character(0), periodDividerLabels = character(0), treatmentCodes = NULL, treatmentLabels = NULL, options = NULL) {

  # databases are data.frames
  if(!"data.frame" %in% class(toxData)) {
    stop("toxData must be of class data.frame")
  }
  if(!"data.frame" %in% class(patientData)) {
    stop("patientData must be of class data.frame")
  }

  # databases contain the correct coulmns as named by the user.
  for(column in c(patidCol, toxCategoryCol, toxNameCol, toxGradeCol, dateOfStartTox, dateOfEndTox)) {
    if(!column %in% names(toxData)) {
      stop("Column named ", column," not found in toxData data.frame")
    }
  }

  for(column in c(patidCol, dateOfStartOfToxWindow, dateOfEndOfToxWindow, periodDividerCols)) {
    if(!column %in% names(patientData)) {
      stop("Column named ", column," not found in patientData data.frame")
    }
  }

  # Check patientData has 1 line per patient
  nPatients = length(unique(patientData[,patidCol]))
  if(nPatients != dim(patientData)[1]){
    stop("Patients in nPatients are not all unique. This data should be one row per patient.")
  }

  patids = unique(toxData[,patidCol])
  missingPatients = which(!patids %in% patientData[,patidCol])
  if(length(missingPatients) > 0) {
    message("The following patients have toxicities but are missing in patientData: ", paste0(patids[missingPatients],collapse = ", "))
    warning("The following patients have toxicities but are missing in patientData: ", paste0(patids[missingPatients],collapse = ", "), call. = FALSE)
  }

  if(is.null(treatmentCol)) {
    message("No treatment column was provided, creating Treatment column with value NA")
    treatmentCol = "Treatment"
    patientData$Treatment = "NA"
  }

  # define treatmentCodes and names if not provided
  if(is.null(treatmentCodes)) {
    treatmentCodes = unique(patientData[,treatmentCol])
  }
  if(is.null(treatmentLabels)) {
    treatmentLabels = as.character(treatmentCodes)
  }

  # check data is not a string
  if(class(toxData[,dateOfStartTox]) == "character") {
    stop("Column ",dateOfStartTox," in toxData must be numeric or date format")
  }
  if(class(toxData[,dateOfEndTox]) == "character") {
    stop("Column ", dateOfEndTox, " in toxData must be numeric or date format")
  }
  if(class(patientData[,dateOfStartOfToxWindow]) == "character") {
    stop("Column ",dateOfStartOfToxWindow, " in patientData must be numeric or date format")
  }
  if(class(patientData[,dateOfEndOfToxWindow]) == "character") {
    stop("Column ",dateOfEndOfToxWindow," in patientData must be numeric or date format")
  }

  # check label length match if provided
  if(length(periodDividerCols) > 0) {
    if(length(periodDividerCols) != length(periodDividerLabels)) {
      message("Lengths of periodDividerCols and periodDividerLabels do not match, setting periodDividerLabels to periodDividerCols")
      periodDividerLabels = periodDividerCols
    }
  }

  # if no options provided use the defaults
  if(is.null(options)) {
    options = DefaultToxicityOptions()
  }

  # Create a treatment column in toxData for convinience
  toxData[ ,treatmentCol] = sapply(toxData[,patidCol], function(x) patientData[patientData[,patidCol] == x,treatmentCol])

  dm = dim(toxData)
  # ass_TRUE is used for subsetting.
  toxData$ass_TRUE = TRUE

  ################################################################################
  # Create the empty query database
  queryNames = c("patid", "ae", "problem_type", "message")
  queries = data.frame(matrix("",nrow = 0,ncol = length(queryNames)),stringsAsFactors = FALSE)
  names(queries) = queryNames
  ################################################################################
  # create the robustToxicitiesClass object
  obj = .robustToxicitiesClass(
    toxData = toxData,
    patientData = patientData,
    patidCol = patidCol,
    treatmentCol = treatmentCol,
    toxCategoryCol = toxCategoryCol,
    toxNameCol = toxNameCol,
    toxGradeCol = toxGradeCol,
    dateOfStartOfToxWindow = dateOfStartOfToxWindow,
    dateOfStartTox = dateOfStartTox,
    dateOfEndTox = dateOfEndTox,
    dateOfEndOfToxWindow = dateOfEndOfToxWindow,
    periodDividerCols = periodDividerCols,
    periodDividerLabels = periodDividerLabels,
    treatmentCodes = treatmentCodes,
    treatmentLabels = treatmentLabels,
    queries = queries,
    wasQueried = FALSE,
    options = options)

  return(obj)
}
