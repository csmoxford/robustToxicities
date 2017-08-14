
#' The robustToxicities class
#'
#' This is core object of this package. This object stores the original dataset as well as the automatically cleaned dataset and a list of notes and queries generated when cleaning the dataset. A list of options is also provided to store plot and tabulation options and provide additional metadata. Finally treatment and cycle labels are also required.
#'
#' @slot toxData The toxicitydataset
#' @slot patientData Patient level data
#' @slot patidCol Column name for the participant identifier
#' @slot treatmentCol Column name for the treatment
#' @slot toxCategoryCol Column name for aderse event category
#' @slot toxNameCol Column name for adverse event name
#' @slot toxGradeCol Column name for the adverse event grade
#' @slot dateOfStartTox Column name for date of adverse event start or change in grade
#' @slot dateOfEndTox Column name for date of adverse event end or change in grade
#' @slot dateOfStartOfToxWindow Column name for date of study entry (eg registration)
#' @slot dateOfEndOfToxWindow Column name for the end of the time window for the particitant to be observed for toxicities
#' @slot periodDividerCols Column names for date dividing times into periods or cycles (optional)
#' @slot periodDividerLabels Display names for data periodDividerCols
#' @slot treatmentCodes Codes which match the values in treatmentCol
#' @slot treatmentLabels Labels to used instea of the treatment codes
#' @slot queries A data.frame containing all the queries and note generated when loading the data
#' @slot options An s4 object of class \code{\link{toxicityOptions-class}} containing options and metadata for the files.
#' @slot wasQueried Logical detailing if queries were run on this object.


#' @importFrom stringr word
#' @exportClass robustToxicitiesClass
.robustToxicitiesClass = setClass(
  "robustToxicitiesClass",
  slots = c(
    toxData = "data.frame",
    patientData = "data.frame",
    patidCol = "character",
    treatmentCol = "character",
    toxCategoryCol = "character",
    toxNameCol = "character",
    toxGradeCol = "character",
    dateOfStartOfToxWindow = "character",
    dateOfStartTox = "character",
    dateOfEndTox = "character",
    dateOfEndOfToxWindow = "character",
    periodDividerCols = "character",
    periodDividerLabels = "character",
    treatmentCodes = "ANY",
    treatmentLabels = "character",
    queries = "data.frame",
    wasQueried = "logical",
    options = "toxicityOptions"), validity = function(object){

      validObject(object@options)


      # Check columns are present
      name = names(object@toxData)
      for (colName in c(object@patidCol, object@toxNameCol, object@toxCategoryCol, object@toxGradeCol,  object@dateOfStartTox, object@dateOfEndTox)) {
        if (!colName %in% name) {
          return(paste0("Column with name ",colName, " was not found in the toxData and is required."))
        }
      }

      name = names(object@patientData)
      for (colName in c(object@patidCol, object@treatmentCol, object@dateOfStartOfToxWindow, object@dateOfEndOfToxWindow, object@periodDividerCols)) {
        if (!colName %in% name) {
          return(paste0("Column with name ",colName, " was not found in the patientData and is required."))
        }
      }

      return(TRUE)
    })





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
#' @export SetupRobustToxicities
SetupRobustToxicities = function(toxData, patientData, patidCol, treatmentCol = NULL, toxCategoryCol, toxNameCol, toxGradeCol, dateOfStartOfToxWindow, dateOfStartTox, dateOfEndTox, dateOfEndOfToxWindow, periodDividerCols = character(0), periodDividerLabels = character(0), treatmentCodes = NULL, treatmentLabels = NULL, options = NULL) {

  if(!"data.frame" %in% class(toxData)) {
    stop("toxData must be of class data.frame")
  }
  if(!"data.frame" %in% class(patientData)) {
    stop("toxData must be of class data.frame")
  }


  nPatients = length(unique(patientData[,patidCol]))
  if(nPatients != dim(patientData)[1]){
    stop("Patients in nPatients are not all unique. This data should be one row per patient.")
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

  toxData[[treatmentCol]] = sapply(toxData[,patidCol], function(x) patientData[patientData[,patidCol] == x,treatmentCol])

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
