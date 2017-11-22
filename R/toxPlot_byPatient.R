#' Plot patients worst grade over time
#'
#' This function plots the worst grade adverse event for each patient over time.
#'
#' @param rt an object of class robustToxicities
#' @param rowID_range optional, a length 2 vector detailing the minimum and maximum row to plot
#' @param plot whether to plot the graph or return the number of rows to plot
#' @param xlim Range to plot on xaxis. Default is c(-7,60)
#' @param xlab xaxis title / label
#' @param plotCycleLength Cycle length is used to add greater highlights to vertical lines. Default is 21
#' @param plotLeftSideOption What to display on right axis. Options are: "treatment", "patid" or "both". Default is "treatment"
#' @param plotXLegendScale What scale to use on xaxis. Options are "days","weeks","months". Default is "days"
#' @param permitMarSet Allow the function to set the mar for the plot
#' @param events a list of Objects of type eventInfo.
#' @param offsetEvent the name of a column in patientData to use as time 0. If not provided the start of assessment date is used
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.
#'
#' #' @seealso \code{\link{ToxPlot_byToxicity}}, \code{\link{ToxPlot_byTime}}, \code{\link{ToxPlot_byCycle}}
#'
#' @example inst/HelpExamples/ToxPlot_byPatient_example.R
#'
#' @export ToxPlot_byPatient
ToxPlot_byPatient = function(rt, rowID_range = NULL, plot = TRUE,
                             plotLeftSideOption = "treatment",
                             xlim = c(-7,60),
                             xlab = character(0),
                             plotCycleLength = 21,
                             plotCycles = 6,
                             plotXLegendScale = "days",
                             permitMarSet = TRUE,
                             events = list(),
                             offsetEvent = NULL) {

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  validObject(rt)

  if(length(events) > 0) {
    for(i in 1:length(events)) {
      if(class(events[[i]]) != "eventInfo") {
        stop("Items passed to ... must be events")
      }
    }
  }

  .toxPlot_time = function(rt, toxDataSub, rowID_range = NULL, cols, xlab, events, offsetEvent) {


    if(is.null(rt@patientData$rowID)) {
        rt@patientData = rt@patientData[order(rt@patientData[,rt@treatmentCol],rt@patientData[,rt@patidCol]),]
        rt@patientData$rowID = dim(rt@patientData)[1]:1
    }

    toxDataSub$gid = sapply(toxDataSub[,rt@patidCol], function(x){ rt@patientData$rowID[rt@patientData[,rt@patidCol] == x] })

    ####################################################################
    ## Change offset to event different to start of tox window
    if(!is.null(offsetEvent)) {
      rt@patientData$newOffsetAmount =  rt@patientData[,offsetEvent] - rt@patientData[,rt@dateOfStartOfToxWindow]
      toxDataSub$offSetDateAmount = sapply(toxDataSub[,rt@patidCol], function(patid) rt@patientData$newOffsetAmount[rt@patientData[,rt@patidCol] == patid] )

      toxDataSub$rel_ae_start = toxDataSub$rel_ae_start - toxDataSub$offSetDateAmount
      toxDataSub$rel_ae_end = toxDataSub$rel_ae_end - toxDataSub$offSetDateAmount
    } else {
      toxDataSub$offSetDateAmount = 0
      rt@patientData$newOffsetAmount = 0
    }


    ylim = c(min(rt@patientData$rowID)-0.5, max(rt@patientData$rowID) + 0.5)


    ##############################################################
    # get plot region size and split-screen
    size = dev.size("in")

    sizeBase = ifelse(size[1] < 9, 1, 0.6)
    ratioBase = sizeBase/size[2]
    if(permitMarSet) {
      par(mar=c(3.5,3,0.75,0.75))
    }
    split.screen(
      figs = matrix(c(
        0,1,ratioBase,1,
        0,1,0,ratioBase
      ),ncol=4, byrow =TRUE))



    ##############################################################
    # Main plot
    screen(1)
    plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")


    ud = 0.5 # up down size of polygons 0.5 would fill the rows

    for(j in 1:5){
      toxDataSub2 = toxDataSub[toxDataSub[,rt@toxGradeCol] == j,]
      if(dim(toxDataSub2)[1] > 0){
        rect(toxDataSub2$rel_ae_start,toxDataSub2$gid - ud,toxDataSub2$rel_ae_end+1,toxDataSub2$gid + ud,col = cols[j], border = NA)
      }
    }

    if(plotXLegendScale == "days") {
      mx = floor(xlim[2] / 7)
      axis(1, labels = -10:mx * 7, at = -10:mx*7, pos = ylim[1])
      if(length(xlab) == 0) {
        xlab = "Days from start of treatment"
      }
    } else if(plotXLegendScale == "weeks") {
      mx = floor(xlim[2] / 7)
      axis(1, labels = -10:mx, at = -10:mx*7, pos = ylim[1])
      mtext("Weeks from start of treatment", side = 1, line = 2.5, cex = par("cex"))
      if(length(xlab) == 0) {
        xlab = "Weeks from start of treatment"
      }
    } else if(plotXLegendScale == "months") {
      mx = floor(xlim[2] / 7)
      axis(1, labels = -10:mx, at = -10:mx*30.4, pos = ylim[1])
      if(length(xlab) == 0) {
        xlab = "Months from start of treatment"
      }
    }else {
      stop("plotXLegendScale must be one of 'days', 'weeks', 'months'")
    }

    mtext(xlab,side = 1, line = 2.5, cex = par("cex"))

    abline(v = -10:500*7, lty = 2, col = "lightgrey")
    abline(h = 1:1000-0.5, lty = 1, col = "lightgrey")
    if (plotCycleLength > 0) {
      abline(v = 0:plotCycles * plotCycleLength, col="grey")
    }


    #########################################################
    ## get label positions for treatment and patid
    patientData = rt@patientData
    treatment.lab = sapply(rt@treatmentCodes, function(x) mean(patientData$rowID[patientData[,rt@treatmentCol] == x]))
    treatmentLine = sapply(rt@treatmentCodes, function(x) max(patientData$rowID[patientData[,rt@treatmentCol] == x])) + 0.5

    abline(h = treatmentLine,lwd = 2)

    #########################################################
    # Add any provided events
    if(length(events) > 0) {
      for(event in events) {
        for(i in 1:length(event@columns)) {
          patientData[,event@columns[i]] = patientData[,event@columns[i]] - rt@patientData[,rt@dateOfStartOfToxWindow]  - patientData$newOffsetAmount
          segments(patientData[,event@columns[i]],patientData$rowID-0.5,patientData[,event@columns[i]],patientData$rowID+0.5, lwd=event@lwd, col= event@col, lend = 1)
        }
      }
    }


    #########################################################
    ## LHS patid and or treatment
    if(plotLeftSideOption == "patid") {
      axis(2,labels = patientData[,rt@patidCol], at = patientData$rowID, tick = FALSE)
    } else if(plotLeftSideOption %in% c("treatment", "both")) {
      axis(2,labels = rt@treatmentLabels, at = treatment.lab, tick = FALSE)

      if(plotLeftSideOption == "both") {
        text(x = xlim[1] + 0.25 , y = patientData$rowID, labels = patientData[,rt@patidCol], pos = 4)
      }
    }

    box(lwd=2)

  }
  #############################################################
  # End of internal function
  #############################################################


  if (class(rt) != "robustToxicitiesClass") {
    stop("rt must be of class robustToxicities")
  }

  # subset to specific stuff if required
  toxDataSub = rt@toxData[rt@toxData$ass_TRUE == TRUE, ]

  cols = c("#00CC00", "#FF9900", "red", "#551A8B", "black")
  cols = c("#98cee2", "#4c7bd3", "#ff8d00", "#ff0000", "#b719b4")


  val = .toxPlot_time(rt,toxDataSub, rowID_range, cols = cols, events = events, xlab = xlab, offsetEvent = offsetEvent)

  #############################################################
  #############################################################
  ## legend
  screen(2)
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="", xaxs = "i", yaxs = "i")

  numItems = 5 + length(events)
  numRowLegend = ceiling(numItems / 5)



  # row 1: grade 1-5
  pos = .legendGetPosition(1:5,5,numItems)
  pos$x = pos$x + 0.05
  xsize = 0.025
  xoffset = 0.03
  ysize = 0.2 / numRowLegend
  label = c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5")

  par(xpd=TRUE)
  text(pos$x - xoffset, pos$y,labels = label, pos = 2)
  rect(pos$x - xsize, pos$y - ysize, pos$x + xsize, pos$y + ysize, col = cols, border = cols)

  curItems = 5

  if(length(events) > 0) {
    pos = .legendGetPosition(curItems + 1:length(events) , 5, numItems)
    pos$x = pos$x + 0.05


    xsize = 0.03
    xoffset = 0.03
    ysize =  0.2 / numRowLegend

    text(pos$x - xoffset, pos$y, labels = sapply(events, function(e) e@label), pos = 2)
    segments(pos$x, pos$y - ysize, pos$x, pos$y + ysize, col = sapply(events, function(e) e@col), lwd = sapply(events, function(e) e@lwd))
  }

  par(xpd=FALSE)
  # Do we really want to close screens?
  close.screen(all.screens = TRUE)

}
