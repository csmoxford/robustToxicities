




#' Toxicity table of categories by cycle/s
#'
#' Returns a toxicity table with the requested data according to the ass_TRUE column, for the cycles requested.
#'
#' @param rt an object of class robustToxicities
#' @param cycles The cycle column names, or index in rt@rt@periodDividerCols of the cycles to tabulate. May also be "all" to use all cycles
#'
#' @details
#' This function acts as a wrapper for ToxTable_cycle to get category data instead of toxicity level data.
#'
#' @export ToxTable_category
ToxTable_category = function(rt, cycles = "all") {

  rt@toxData[,rt@toxNameCol] = rt@toxData[,rt@toxCategoryCol]

  rt@toxData = rt@toxData[order(rt@toxData[,rt@toxNameCol]),]

  rt@toxData$ass_toxID = as.numeric(factor(rt@toxData[,rt@toxNameCol]))

  tble = ToxTable_cycle(rt,cycles)
  tble = tble[,-2]
  return(tble)
}
