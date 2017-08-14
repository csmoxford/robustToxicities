
#' Create period divides as a function of time from start of toxicity window
#'
#' Takes a set of numeric time boundaries from baseline and creates the corresponding columns in patientData, periodDividerCols and periodDividerLabels. This returns the updated robustToxicitiesClass object.
#'
#' @param rt Object of class robustToxicitiesClass
#' @param timeBoundaries a numeric vector of times from the start of toxicity window
#' @param labelUnits days, weeks or months used to automatically generate labels in the form of from-to.
#'
#' @export CreateTimeDividers
CreateTimeDividers = function(rt, timeBoundaries, labelUnits = "days") {

  rt@periodDividerLabels = character(0)
  rt@periodDividerCols = character(0)
  for(i in 1:(length(timeBoundaries)-1)){
    if(labelUnits == "days") {
      rt@periodDividerLabels[i] = paste0(timeBoundaries[i], "-", timeBoundaries[i+1])
    } else if(labelUnits == "weeks") {
      rt@periodDividerLabels[i] = paste0(timeBoundaries[i]/7, "-", timeBoundaries[i+1]/7)
    } else { # months
      rt@periodDividerLabels[i] = paste0(floor(timeBoundaries[i]/30.4375), "-", timeBoundaries[i+1]/30.4375)
    }
    rt@periodDividerCols[i] = paste0("time",i)
    rt@patientData[[paste0("time",i)]] = rt@patientData[,rt@dateOfStartOfToxWindow] + timeBoundaries[i]
  }
  return(rt)
}
