
#' The robustToxicitiesClass
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

#' @name robustToxicitiesClass
#' @aliases robustToxicitiesClass-class
#' @importFrom stringr word
#' @exportClass robustToxicitiesClass
NULL

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





