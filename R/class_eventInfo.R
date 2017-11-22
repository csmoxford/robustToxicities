
.eventInfo = setClass("eventInfo", slots = c(
  columns = "character",
  label = "character",
  lwd = "numeric",
  col = "ANY"
))


#' eventInfo class
#'
#' Stores event data to pass to toxPlot_byToxicity in the causality parameter. The columns containing the data in patientData should hold date of class "Date".
#'
#'
#' @param columns A vector of columns contain the event
#' @param labels A short name for the event
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
