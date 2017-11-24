#' Tabulation of toxicity categories in a cycle
#'
#' Returns a toxicity table with the requested data according to the ass_TRUE column, for the cycles requested. Note this is a wrapper function which essentially replaces the toxicity names with the categories, updates the toxID's and then calls \code{\link{ToxTable_cycle}} on the categories. This function could be used as a template for summarising other data.
#'
#' @param rt an object of class robustToxicities
#' @param cycles The cycle column names, or index in rt@rt@periodDividerCols of the cycles to tabulate. May also be "all" to use all cycles
#'
#' @details
#' This function acts as a wrapper for \code{\link{ToxTable_cycle}} to get category data instead of toxicity level data.
#'
#' @export ToxTable_category
ToxTable_category = function(rt, cycles = "all") {

  rt@toxData[,rt@toxNameCol] = rt@toxData[,rt@toxCategoryCol]

  rt@toxData = rt@toxData[order(rt@toxData[,rt@toxNameCol]),]

  rt@toxData$ass_toxID = as.numeric(factor(rt@toxData[,rt@toxNameCol]))

  tble = ToxTable_cycle(rt,cycles)
  nme = names(tble)
  tble = tble[,-2] # interesting that this affects the names...
  names(tble) = nme[-2]
  return(tble)
}
