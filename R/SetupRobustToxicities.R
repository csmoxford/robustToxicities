#' The robustToxicities generator
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
#' @param options Optional. An object of class toxicityOptions. The easiest place to start is with \code{DefaultToxicityOptions()}.
#'
#'
#' @example inst/HelpExamples/SimpleExample.R
#' @export SetupRobustToxicities
SetupRobustToxicities = function(toxData, patientData, patidCol, treatmentCol = NULL, toxCategoryCol, toxNameCol, toxGradeCol, dateOfStartOfToxWindow, dateOfStartTox, dateOfEndTox, dateOfEndOfToxWindow, periodDividerCols = character(0), periodDividerLabels = character(0), treatmentCodes = NULL, treatmentLabels = NULL, options = NULL) {

  if(!"data.frame" %in% class(toxData)) {
    stop("toxData must be of class data.frame")
  }
  if(!"data.frame" %in% class(patientData)) {
    stop("patientData must be of class data.frame")
  }


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

  if(is.null(treatmentCodes)) {
    treatmentCodes = unique(patientData[,treatmentCol])
  }
  if(is.null(treatmentLabels)) {
    treatmentLabels = as.character(treatmentCodes)
  }

  if(class(toxData[,dateOfStartTox]) == "character") {
    stop("Column for toxData$dateOfStartTox must be numeric or date format")
  }
  if(class(toxData[,dateOfEndTox]) == "character") {
    stop("Column for toxData$dateOfEndTox must be numeric or date format")
  }
  if(class(patientData[,dateOfStartOfToxWindow]) == "character") {
    stop("Column for patientData$dateOfStartOfToxWindow must be numeric or date format")
  }
  if(class(patientData[,dateOfEndOfToxWindow]) == "character") {
    stop("Column for patientData$dateOfEndOfToxWindow must be numeric or date format")
  }

  if(length(periodDividerCols) > 0) {
    if(length(periodDividerCols) != length(periodDividerLabels)) {
      message("Lengths of periodDividerCols and periodDividerLabels do not match, setting periodDividerLabels to periodDividerCols")
      periodDividerLabels = periodDividerCols
    }
  }

  if(is.null(options)) {
    options = DefaultToxicityOptions()
  }

  toxData[ ,treatmentCol] = sapply(toxData[,patidCol], function(x) patientData[patientData[,patidCol] == x,treatmentCol])

  dm = dim(toxData)
  ################################################################################
  # Check fields are provided (all need these fields)


  toxData$ass_TRUE = TRUE

  ################################################################################
  # Create the empty query database
  queryNames = c("patid", "ae", "problem_type", "message")
  queries = data.frame(matrix("",nrow = 0,ncol = length(queryNames)),stringsAsFactors = FALSE)
  names(queries) = queryNames
  ################################################################################

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
