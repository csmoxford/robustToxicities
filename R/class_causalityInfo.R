
.causalityInfo = setClass("causalityInfo", slots = c(
  columns = "character",
  names = "character",
  width = "numeric",
  pch = "numeric",
  cex = "numeric",
  col = "ANY",
  labels = "character"
))


#' causalityInfo class
#'
#' Stores causality data to pass to toxPlot_byToxicity in the causality parameter. The columns containing the data in toxData should hold numeric data.
#'
#' The default values expect a number between 1 and 5 and plot a symbol for values 3,4 and 5. This can be changed using pch.
#'
#' @param columns A vector of column names for toxData
#' @param names A short identifier to place at the top of each column
#' @param width The width to provide on the plot for each causality. This is on the scale of days on the plot
#' @param pch The pch symbol to use for each level of causality
#' @param cex The size of each symbol
#' @param col The colour to use for each level of causality
#' @param labels Labels for the non NA pch levels, used in the legend
#' @return An object of class causalityInfo containing the same slots as paramters taken by ToxPlot_causalityInfo
#'
#' @rdname ToxPlot_causalityInfo
#' @aliases causalityInfo, causalityInfo-class
#' @exportClass causalityInfo
#' @export ToxPlot_causalityInfo
ToxPlot_causalityInfo = function(columns, names = character(0), width = 1.5, pch = c(NA,NA,4,8,16), cex = 1, col = 1, labels = c("Possibly related", "Probably related", "Definitely related")) {
  if(length(col) == 1) {
    col = rep(col,length(pch))
  }
  .causalityInfo(
    columns = columns,
    names = names,
    width = width,
    pch = pch,
    cex =cex,
    col = col,
    labels = labels
    )
}
