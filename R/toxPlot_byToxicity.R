#' Plot toxicities over time
#'
#' This function plots the worst grade of each toxicity over time. There should be no overlap between toxicities but in the case that there is the wors grade is given priority.
#'
#' @param rt an object of class robustToxicities
#' @param rowID_range optional, a length 2 vector detailing the minimum and maximum row to plot
#' @param plotNow whether to plot the graph or return the number of rows to plot
#' @param xlim Range to plot on xaxis. Default is c(-7,60)
#' @param xlab xaxis title / label
#' @param plotCycleLength Cycle length is used to add greater highlights to vertical lines. Default is 21
#' @param plotLeftSideOption What to display on right axis. Options are: "treatment", "patid" or "both". Default is "treatment"
#' @param plotXLegendScale What scale to use on xaxis. Options are "days","weeks","months". Default is "days"
#' @param permitMarSet Allow the function to set the mar for the plot
#' @param causality Adds causality columns to the plot on the righthand side. This must be an object of type \code{\link{causalityInfo-class}}
#' @param events a list of Objects of type eventInfo.
#' @param offsetEvent the name of a column in patientData to use as time 0.
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.
#'
#' @seealso \code{\link{ToxPlot_byPatient}}, \code{\link{ToxPlot_byTime}}, \code{\link{ToxPlot_byCycle}}
#'
#' @example inst/HelpExamples/ToxPlot_byToxicity_example.R
#'
#' @export ToxPlot_byToxicity
ToxPlot_byToxicity = function(rt, rowID_range = NULL, plotNow = TRUE,
                   plotLeftSideOption = "treatment",
                   xlim = c(-7,60),
                   xlab = character(0),
                   plotCycleLength = 21,
                   plotCycles = 6,
                   plotXLegendScale = "days",
                   permitMarSet = TRUE,
                   causality = NULL,
                   events = list(),
                   offsetEvent = NULL) {

  if(class(events) == "eventInfo") {
    events = list(events)
  }

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  if(!is.null(causality)) {
    if(class(causality) != "causalityInfo") {
      stop("causality must be of class causalityInfo. This can be generated using the function ToxPlot_causalityInfo")
    }
  }

  validObject(rt)


  if(length(events) > 0) {
    for(i in 1:length(events)) {
      if(class(events[[i]]) != "eventInfo") {
        stop("Items passed to ... must be events")
      }
    }
  }

  #######################################################
  .toxPlot_fun = function(rt, toxDataSub, rowID_range = NULL, plotNow = TRUE, cols, xlab, causality, events) {


    ####################################################################

    toxDataSub = toxDataSub[order(toxDataSub[,rt@treatmentCol],toxDataSub[,rt@patidCol],toxDataSub[,rt@toxCategoryCol],toxDataSub[,rt@toxNameCol],toxDataSub$rel_ae_start),]
    un = unique(toxDataSub[, c(rt@patidCol, rt@toxCategoryCol, rt@toxNameCol)])


    ####################################################################
    ## Change offset to event different to start of tox window
    if(!is.null(offsetEvent)) {
      rt@patientData$newOffsetAmount =  rt@patientData[,offsetEvent] - rt@patientData[,rt@dateOfStartOfToxWindow]
      toxDataSub$offSetDateAmount = sapply(toxDataSub[,rt@patidCol], function(patid) rt@patientData$newOffsetAmount[rt@patientData[,rt@patidCol] == patid] )

      toxDataSub$rel_ae_start = toxDataSub$rel_ae_start - toxDataSub$offSetDateAmount
      toxDataSub$rel_ae_end = toxDataSub$rel_ae_start - toxDataSub$offSetDateAmount
    } else {
      toxDataSub$offSetDateAmount = 0
      rt@patientData$newOffsetAmount = 0
    }



    # give each item a row id for all common toxicities
    toxDataSub$gid = 0
    for (i in 1:length(toxDataSub$gid)) {
      st = which(toxDataSub[i, rt@patidCol] == un[,rt@patidCol] & toxDataSub[i,rt@toxCategoryCol] == un[,rt@toxCategoryCol] & toxDataSub[i,rt@toxNameCol] == un[,rt@toxNameCol])
      toxDataSub$gid[i] = st
    }

    if(!plotNow) {
      return(max(toxDataSub$gid))
    }

    # reduce to only the rows needed this time
    if(!is.null(rowID_range)){
      toxDataSub = toxDataSub[toxDataSub$gid %in% rowID_range[1]:rowID_range[2],]
    }

    #####################################################################
    # colouring
    toxDataSub$col = ""
    for (i in 1:length(toxDataSub$col)) {
      if (toxDataSub[i, rt@toxGradeCol]>0) {
        toxDataSub$col[i] = cols[toxDataSub[i, rt@toxGradeCol]]
      }
    }

    ylim = c(min(toxDataSub$gid)-0.5, max(toxDataSub$gid) + 0.5)

    ##############################################################
    # get plot region size and split-screen
    size = dev.size("in")

    numItems = 5 + length(events) + ifelse(!is.null(causality), length(causality@labels), 0)
    numRowLegend = ceiling(numItems / 5)

    sizeBase = 0.1 + numRowLegend*0.4
    ratioBase = sizeBase/size[2]
    if(permitMarSet){
      par(mar=c(3.5,3,0.75,0.75))
      if(!is.null(causality)) {
        par(mar=c(3.5,3,2.5,0.75))
      }
    }
    split.screen(
      figs = matrix(c(
        0,1,ratioBase,1,
        0,1,0,ratioBase
      ),ncol=4, byrow =TRUE))



    ##############################################################
    # Main plot

    addX = 0
    if(!is.null(causality)) {
      addX = causality@width * length(causality@columns)
    }

    screen(1)
    plot(0, 0, xlim = c(xlim[1],xlim[2]+addX), ylim = ylim, type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")


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
    abline(h = 1:1000-0.5, lty = 2, col = "lightgrey", lwd = 0.5)
    if (plotCycleLength > 0) {
      abline(v = 0:plotCycles * plotCycleLength, col="grey")
    }




    #########################################################
    ## add toxicities
    ud = 0.3 # up down size of polygons 0.5 would fill the rows

    for(j in 1:5){
      toxDataSub2 = toxDataSub[toxDataSub[,rt@toxGradeCol] == j,]

      toxDataSub2 = toxDataSub2[toxDataSub2$rel_ae_start < xlim[2],]
      if(dim(toxDataSub2)[1] > 0){
        toxEndxlimRestricted = pmin(toxDataSub2$rel_ae_end + 1, xlim[2])
        rect(toxDataSub2$rel_ae_start,toxDataSub2$gid - ud, toxEndxlimRestricted, toxDataSub2$gid + ud, col = cols[j], border = NA)
      }
    }


    # Add any provided events
    if(length(events) > 0) {
      for(event in events) {
        for(i in 1:length(event@columns)) {

          rt@patientData[,event@columns[i]] = rt@patientData[,event@columns[i]] - rt@patientData$newOffsetAmount

        toxDataSub$eventDate = sapply(toxDataSub[,rt@patidCol] ,function(x) rt@patientData[rt@patientData[,rt@patidCol] == x, event@columns[i]])
        toxDataSub$relEventTime = toxDataSub$eventDate - toxDataSub[,rt@dateOfStartOfToxWindow]

        segments(toxDataSub$relEventTime,toxDataSub$gid-0.5,toxDataSub$relEventTime,toxDataSub$gid+0.5, lwd=event@lwd, col= event@col, lend = 1)
        }
      }
    }


    #########################################################
    ## get label positions for treatment and patid
    a = sort(c(min(toxDataSub$gid)-1, by(toxDataSub$gid, toxDataSub[,rt@patidCol], max)) + 0.5)
    b = sort(c(min(toxDataSub$gid)-1, by(toxDataSub$gid, toxDataSub[,rt@treatmentCol], max)) + 0.5)


    patid.lab = rep(0, length(a) - 1)
    treatment.lab = rep(0, length(b) - 1)
    for (i in 1:length(patid.lab)) {
      patid.lab[i] = (a[i] + a[i + 1]) / 2
    }
    for(i in 1:length(treatment.lab)) {
      treatment.lab[i] = (b[i] + b[i + 1]) / 2
    }

    abline(h = a,lwd = 2, col = "grey")
    abline(h = b,lwd = 2)
    #########################################################
    trtLabels = rt@treatmentLabels[sapply(unique(toxDataSub[,rt@treatmentCol]), function(x) which(x == rt@treatmentCodes))]
    ## LHS patid and or treatment
    if(plotLeftSideOption == "patid") {
      axis(2,labels = unique(toxDataSub[,rt@patidCol]), at = patid.lab, tick = FALSE)
    } else if(plotLeftSideOption %in% c("treatment", "both")) {
      axis(2,labels = trtLabels, at = treatment.lab, tick = FALSE)

      if(plotLeftSideOption == "both") {
        text(x = xlim[1] + 0.25 , y = patid.lab, labels = unique(toxDataSub[,rt@patidCol]), pos = 4)
      }
    }

    #########################################################
    ## RHS toxicity names
    gid = unique(toxDataSub$gid)
    toxicities = sapply(gid, function(x) toxDataSub[which(toxDataSub$gid == x)[1],rt@toxNameCol])

    text(xlim[2], gid, labels=toxicities, pos = 2, offset=0.25)
    box(lwd=2)

    #########################################################
    ## add Causality
    if(!is.null(causality)) {
      par(xpd = TRUE)
      for(columnIndex in 1:length(causality@columns)) {
        column = causality@columns[columnIndex]

        causalityValues = sapply(gid, function(x) max(1,toxDataSub[which(toxDataSub$gid == x), column], na.rm = TRUE))
        # rect(xlim[2],toxDataSub2$gid-ud,xlim[2] + 2, toxDataSub2$gid+ud,col = 1, border = NA)
        points(rep(xlim[2] + causality@width*(- 0.5 + columnIndex),length(gid)),gid, pch = causality@pch[causalityValues], col = causality@col[causalityValues], cex = causality@cex)

        if(length(causality@names) >0) {
          top = ylim[2] + 1
          text(xlim[2]  + causality@width*(- 0.5 + columnIndex), ylim[2]+0.5, labels = causality@names[columnIndex])
        } else {
          top = ylim[2]
        }
        rect(xlim[2],ylim[1], xlim[2] + causality@width*columnIndex, top, lwd=2)
      }
      par(xpd = FALSE)
    }
  }
  #######################################################

  if (class(rt) != "robustToxicitiesClass") {
    stop("rt must be of class robustToxicities")
  }

  # subset to specific stuff if required
  toxDataSub = rt@toxData[rt@toxData$ass_TRUE, ]

  cols = c("#00CC00","#FF9900","red","#551A8B","black")
  cols = c("#98cee2", "#4c7bd3","#ff8d00","#ff0000","#b719b4")

  val = .toxPlot_fun(rt = rt, toxDataSub = toxDataSub, rowID_range = rowID_range, plotNow = plotNow, cols = cols, xlab = xlab, causality = causality, events = events)

  if(!plotNow) {
    return(val)
  }

  #############################################################
  ## legend
  screen(2)
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="", xaxs = "i", yaxs = "i")

  numItems = 5 + length(events) + ifelse(!is.null(causality), length(causality@labels), 0)

  getPosition = function(index, itemsOnRow, totalItems) {

    pos = list(x = rep(NA,length(index)),y = rep(NA,length(index)))

    fullMod = function(i, mod) {
      ret = i %% mod
      if(ret == 0) {return(mod)}
      return(ret)
    }

    for(j in 1:length(index)) {
      i = index[j]
      numberOfRows = ceiling(totalItems / itemsOnRow)
      areOnRow = ceiling(i / itemsOnRow)

      if(areOnRow < numberOfRows || totalItems == itemsOnRow * numberOfRows) {
        pos$x[j] = fullMod(i,itemsOnRow) / (itemsOnRow) - 0.5/itemsOnRow
      } else {
        itemsOnThisRow = fullMod(totalItems,itemsOnRow)
        pos$x[j] = fullMod(i,itemsOnRow) / (itemsOnRow) - 0.5/itemsOnRow + (itemsOnRow - itemsOnThisRow)/(2*itemsOnRow)
      }
      pos$y[j] = (numberOfRows + 1 - areOnRow)  / (numberOfRows + 1)

    }
    return(pos)
  }


  # row 1: grade 1-5
  pos = getPosition(1:5,5,numItems)
  pos$x = pos$x + 0.05
  xsize = 0.025
  xoffset = 0.03
  ysize = 0.2 / numRowLegend
  label = c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5")

  par(xpd=TRUE)
  text(pos$x - xoffset, pos$y,labels = label, pos = 2)
  rect(pos$x - xsize, pos$y - ysize, pos$x + xsize, pos$y + ysize, col = cols, border = cols)

  curItems = 5

  if(!is.null(causality)) {
  pos = getPosition(curItems + 1:length(causality@labels) , 5, numItems)
  pos$x = pos$x + 0.05
  curItems = curItems + length(causality@labels)

    xsize = 0.03
    xoffset = 0.03

    text(pos$x - xoffset, pos$y, labels = causality@labels, pos = 2)
    points(pos$x, pos$y, pch = causality@pch[!is.na(causality@pch)], col = causality@col[!is.na(causality@pch)], cex = 1.5)
  }

    if(length(events) > 0) {
      pos = getPosition(curItems + 1:length(events) , 5, numItems)
      pos$x = pos$x + 0.05
      curItems = curItems + length(causality@labels)


      xsize = 0.03
      xoffset = 0.03
      ysize =  0.2 / numRowLegend

      text(pos$x - xoffset, pos$y, labels = sapply(events, function(e) e@label), pos = 2)
      segments(pos$x, pos$y - ysize, pos$x, pos$y + ysize, col = sapply(events, function(e) e@col), lwd = sapply(events, function(e) e@lwd))
    }

  par(xpd=FALSE)

  close.screen(all.screens = TRUE)
}




