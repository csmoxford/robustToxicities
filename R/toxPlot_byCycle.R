#' Summary plot of toxicities by cycle
#'
#' This plot summarises the proportion of patients having an adverse event in each time period as defined by periodDividerCols in the \code{\link{robustToxicitiesClass}}.
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
#' @example inst/HelpExamples/ToxPlot_byCycle_example.R
#'
#'
#' @seealso \code{\link{ToxPlot_byPatient}}, \code{\link{ToxPlot_byToxicity}}, \code{\link{ToxPlot_byTime}}

#' @export ToxPlot_byCycle
ToxPlot_byCycle = function(rt, gradeRequired = 1, colors = c("blue","red"), tableSpace = 0.1, las = 1, legendPosition = "right", add = FALSE) {

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  rt@options@toxTable_cumulativeGrades = TRUE
  rt@options@toxTable_mergeGrades = "n|1|2|3|4|5"
  rt@options@toxTable_tabulationPercent = FALSE

  toxTable = ToxTable_summary(rt)

  toxTable = toxTable[-dim(toxTable)[1],]

  dta = list()

  for(trt in 1:length(rt@treatmentCodes)){
    dta[[paste0("total",trt)]] = toxTable[,2 + 6*(trt-1)]
    dta[[paste0("tox",trt)]] = toxTable[,2 + gradeRequired + 6*(trt-1)]
  }

  xlim = c(1,dim(toxTable)[1])
  ylim = c(0,1 + tableSpace*length(rt@treatmentCodes))

  if(!add) {
    plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }

  for(i in 1:length(rt@treatmentCodes)){
    xdata = 1:dim(toxTable)[1]
    ydata = dta[[paste0("tox",i)]]/dta[[paste0("total",i)]]
    polygon(c(xdata[1],xdata,xdata[length(xdata)]),
            c(0, ydata, 0),
            col = adjustcolor(colors[i], alpha.f = 0.25),
            border = colors[i],
            lwd = 2)

    text(xdata,1 + tableSpace*(1+length(rt@treatmentLabels) - i),labels = dta[[paste0("total",i)]], xpd = TRUE)
    axis(2, 1 + tableSpace*(1+length(rt@treatmentLabels) - i), labels = rt@treatmentLabels[i], las = 2, tick = FALSE)
     }

  if(!add){
    # axes
    axis(1, labels = rt@periodDividerLabels, at = 1:length(rt@periodDividerLabels), las = las, lwd = 2)
    axis(2, labels = 0:5/5, at = 0:5/5, las = 2, lwd = 2)

    legend(legendPosition,border = colors, fill = adjustcolor(colors, alpha.f = 0.25), legend = rt@treatmentLabels, bty = "n")
  }

}
