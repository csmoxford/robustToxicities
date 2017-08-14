

#' Toxicity Options
#'
#' An object containing all the key options for creating the toxicity tables.
#' This is a key slot in the \code{\link{robustToxicities-class}}.
#'
#' @name toxicityOptions-class
#' @rdname toxicityOptions-class
#'
#' @slot displayNotes A logical value used by \code{\link{robustToxicities}} to determine whether to print note or note
#' @slot toxTable_cycle_tabulationMethod One of "worst" or "all" determining if all toxicity changes are counted or only the worst reported grade in a time period
#' @slot toxTable_tabulationPercent A logical value used to determine if toxicity tables should report counts (FALSE) or percentages (TRUE)
#' @slot toxTable_tabulationZeros A logical value used to determine if zeros should be included, default TRUE
#' @slot toxTable_cumulativeGrades A logical value used to determine whether toxicity grades should be reported cumulatively or not
#' @slot toxTable_discardToxAtStudyEntry A logical value used to determine if toxicities reported at baseline should be reported or not
#' @slot toxTable_mergeGrades Grades to merge in the \code{\link{print_toxTable_cycle}}. Columns are seperated by "|" and merged values are seperated by ",". "n"
#' @slot toxTable_cycle_toxicityOrder What order should the data be returned in. "c" ordered by categories and then adverse events. "a" ordered by adverse events. "n" ordered by number of adverse events. The n option can be followed by a number to denote the minimum grade to use for sorting. e.g. "n3" will order by grades 3-5 and then 1-5 for ties within grades 3-5.
#'
#' @import methods
#' @exportClass toxicityOptions
.toxicityOptions = setClass("toxicityOptions", slots = c(
  displayNotes = "logical",
  toxTable_cycle_tabulationMethod = "character",
  toxTable_tabulationPercent = "logical",
  toxTable_tabulationZeros = "logical",
  toxTable_cumulativeGrades = "logical",
  toxTable_discardToxAtStudyEntry = "logical",
  toxTable_mergeGrades = "character",
  toxTable_cycle_toxicityOrder = "character"
),validity = function(object) {

  if(!object@toxTable_cycle_tabulationMethod %in% c("worst","all")) {
    return("Option toxTable_cycle_tabulationMethod is not one of 'worst' or 'all'")
  }


  return(TRUE)
})

#' Default toxicity options generator, requiring metadata only.
#'
#' @name DefaultToxicityOptions
#' @rdname toxicityOptions-class
#'

#'
#' @export DefaultToxicityOptions
DefaultToxicityOptions = function() {
  .toxicityOptions(
    displayNotes = TRUE,
    toxTable_cycle_tabulationMethod = "worst",
    toxTable_tabulationPercent = FALSE,
    toxTable_tabulationZeros = TRUE,
    toxTable_cumulativeGrades = TRUE,
    toxTable_discardToxAtStudyEntry = FALSE,
    toxTable_mergeGrades = "n|1|2|3|4,5",
    toxTable_cycle_toxicityOrder = "c"
  )
}
