#' Summary plot of toxicities over time
#'
#' This is a wrapper function for \code{\link{ToxPlot_byCycle}} which takes timeBoundaries.
#'
#' @param rt An object of class robustToxicities
#' @param gradeRequired Only include adverse events with at least this grade
#' @param timeBoundaries A vector of times from the dateOfStartOfToxWindow
#' @param xlab The xaxis label. "days","weeks" and "months" are converted to "Time from registration (days)" etc.
#' @param col A vector of colours to plot each arm with
#' @param tableSpace A parameter to assist in vertical row spacing the table appropriately
#' @param las numeric in {0,1,2,3}; the style of axis labels. {0: always parallel to the axis}, {1: always horizontal [default]}, {2: always perpendicular to the axis}, {3: always vertical}
#' @param legendPosition The location to place the legend see \code{\link{legend}} for details
#' @param add TRUE/FALSE whether to add to an existing plot or start a new one
#'
#'
#'
#' @seealso \code{\link{ToxPlot_byPatient}}, \code{\link{ToxPlot_byToxicity}}, \code{\link{ToxPlot_byCycle}}

#' @export ToxPlot_byTime
ToxPlot_byTime = function(rt, gradeRequired = 1, timeBoundaries, xlab = "days", col = c("blue","red"), tableSpace = 0.1, las = 1, legendPosition = "right", add = FALSE) {

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  validObject(rt)

  rt = CreateTimeDividers(rt, timeBoundaries)

  ToxPlot_byCycle(rt, gradeRequired = gradeRequired, col = col, tableSpace = tableSpace, las = las, legendPosition = legendPosition, add = add)

  if(xlab == "days") {
    text = "Time from registration (days)"
  } else if(xlab == "weeks") {
    text = "Time from registration (weeks)"
  } else if(xlab == "months") {
    text = "Time from registration (months)"
  } else {
    text = xlab
  }
  mtext(1,text = text , line = 2.5, cex = 1.2)

}
