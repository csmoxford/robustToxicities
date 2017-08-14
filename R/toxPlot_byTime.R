

#' @export toxPlot_byTime
toxPlot_byTime = function(rt, gradeRequired = 1, timeBoundaries, labelUnits = "days", colors = c("blue","red"), tableSpace = 0.1, las = 1, legendPosition = "right", add = FALSE) {

  rt = CreateTimeDividers(rt, timeBoundaries)

  toxPlot_byCycle(rt, gradeRequired = gradeRequired, colors = colors, tableSpace = tableSpace, las = las, legendPosition = legendPosition, add = add)

  if(labelUnits == "days") {
    text = "Time from registration (days)"
  } else if(labelUnits == "weeks") {
    text = "Time from registration (weeks)"
  } else {
    text = "Time from registration (months)"
  }
  mtext(1,text = text , line = 2.5, cex = 1.2)

}
