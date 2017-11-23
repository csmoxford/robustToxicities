
.eventInfo = setClass("eventInfo", slots = c(
  columns = "character",
  label = "character",
  lwd = "numeric",
  col = "ANY"
))

setValidity("eventInfo", function(object) {

  if(length(object@columns) < 1) {
    return("columns must be a least length 1")
  }
  if(length(object@label) != 1) {
    return("label should be of length 1")
  }
  if(length(object@lwd) != 1) {
    return("lwd should be of length 1")
  }
  if(length(object@col) != 1) {
    return("col should be of length 1")
  }
  TRUE
})

#' eventInfo class
#'
#' Stores event data to pass to toxPlot_byToxicity in the causality parameter. The columns containing the data in patientData should hold date of class "Date".
#'
#'
#' @param columns A vector of columns contain the event
#' @param label A short name for the event
#' @param lwd Line width
#' @param col The colour to use for each event
#'
#' @return An object of class eventInfo containing the same slots as paramters taken by ToxPlot_causalityInfo
#'
#' @rdname ToxPlot_eventInfo
#' @aliases eventInfo, eventInfo-class
#' @exportClass eventInfo
#' @export ToxPlot_eventInfo
ToxPlot_eventInfo = function(columns, label = columns[1], lwd = 4, col = "grey") {
  .eventInfo(
    columns = columns,
    label = label,
    lwd = lwd,
    col = col
  )
}
