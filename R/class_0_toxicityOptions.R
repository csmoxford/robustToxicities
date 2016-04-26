

#' Toxicity Options
#'
#' An object containing all the key options for creating the toxicity tables.
#' This is a key slot in the \code{\link{robustToxicities-class}}.
#'
#' @name toxicityOptions-class
#' @rdname toxicityOptions-class
#'
#' @slot trialName Trial Name (character string)
#' @slot folderPath Reference to the folder the data file is stored in (character string)
#' @slot fileName Reference to the stored data (character string)
#' @slot timeType One of "Time" or "Cycle" to denote whether the data is primarily time based or only cycle / time period based
#' @slot displayNotes A logical value used by \code{\link{robustToxicities}} to determine whether to print note or note
#' @slot tabulationMethod One of "worst" or "all" determining if all toxicity changes are counted or only the worst reported grade in a time period
#' @slot tabulationPercent A logical value used to determine if toxicity tables should report counts (FALSE) or percentages (TRUE)
#' @slot cumulativeGrades A logical value used to determine whether toxicity grades should be reported cumulatively or not
#' @slot discardBaseline A logical value used to determine if toxicities reported at baseline should be reported or not
#' @slot plotStartTreatment The column name of the treatment start date used in plotting (character string)
#' @slot plotLeftSideOption What should be displayed on the left hand side of the plot. One of "patid", "treatment" or "both"
#' @slot plotxMin minimum x axis limit
#' @slot plotxMax maximum x axis limit
#' @slot plotCycleLength optional cycle length value
#' @slot plotCycles optional number of cycles to plot
#' @slot plotPxHeight Number of pixels to use to generate plot vertically
#' @slot plotPxWidth Number of pixels to use to generate plot horizontally
#' @slot sumCycleMerge Cycles to merge in the \code{\link{print_toxTable_summary}}. Use numeric values with | to divide the merged cycles and , to divide cycles in a merge e.g. "1,2|3,4,5" is two merged time periods with the first 2 time periods and the last 3 time periods.
#' @slot sumColumnMerge Grades to merge in the \code{\link{print_toxTable_summary}}. Similar syntax to sumCycleMerge
#' @slot cycleCycleMerge Cycles to merge in the \code{\link{print_toxTable_cycle}}. Similar syntax to sumCycleMerge
#' @slot cycleColumnMerge Grades to merge in the \code{\link{print_toxTable_cycle}}. Similar syntax to sumCycleMerge
#' @slot cycleCategoryMerge A list of categories to collapse down to one row in the \code{\link{print_toxTable_cycle}}.
#'
#' @import methods
#' @exportClass toxicityOptions
.toxicityOptions = setClass("toxicityOptions", slots = c(
  trialName = "character",
  folderPath = "character",
  fileName = "character",
  timeType = "character",
  displayNotes = "logical",
  tabulationMethod = "character",
  tabulationPercent = "logical",
  cumulativeGrades = "logical",
  discardBaseline = "logical",
  plotStartTreatment = "character",
  plotLeftSideOption = "character",
  plotxMin = "numeric",
  plotxMax = "numeric",
  plotCycleLength = "numeric",
  plotCycles = "numeric",
  plotPxHeight = "numeric",
  plotPxWidth = "numeric",
  sumCycleMerge = "character",
  sumColumnMerge = "character",
  cycleCycleMerge = "character",
  cycleColumnMerge = "character",
  cycleCategoryMerge = "character" # what is this supposed to be?
),validity = function(object) {

  if(!object@timeType %in% c("time","cycle")) {
    return("Option timeType is not one of 'time' or 'cycle'")
  }

  if(!object@tabulationMethod %in% c("worst","all")) {
    return("Option tabulationMethod is not one of 'worst' or 'all'")
  }


  return(TRUE)
})

#' Default toxicity options generator, requiring metadata only.
#'
#' @name defaultToxicityOptions
#' @rdname toxicityOptions-class
#'
#' @param trialName Trial or study Name
#' @param folderPath Path to the folder containing the data
#' @param fileName Name of the file containing the data
#' @param timeType One of "Time" or "Cycle" to denote whether the data is primarily time based or only cycle / time period based
#'
#'
#' @export defaultToxicityOptions
defaultToxicityOptions = function(trialName, folderPath = NULL, fileName = "", timeType="time") {

  if(!timeType %in% c("time", "cycle")){
    stop("timeType must be one of time or cycle")
  }


  .toxicityOptions(
    trialName = trialName,
    folderPath = folderPath,
    fileName = fileName,
    timeType = timeType,
    displayNotes = TRUE,
    tabulationMethod = "worst",
    tabulationPercent = FALSE,
    cumulativeGrades = TRUE,
    discardBaseline = FALSE,
    plotStartTreatment = "cycle_start_date_1",
    plotLeftSideOption = "treatment",
    plotxMin = -7,
    plotxMax = 60,
    plotCycleLength = 21,
    plotCycles = 6,
    plotPxHeight = 0,
    plotPxWidth = 1100,
    sumCycleMerge = "",
    sumColumnMerge = "total|1|2|3|4,5",
    cycleColumnMerge = "1|2|3|4,5",
    cycleCycleMerge = "",
    cycleCategoryMerge = "" # collapse CTCAE categories in table
  )
}
