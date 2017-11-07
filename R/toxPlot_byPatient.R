

#' @export toxPlot_byPatient
toxPlot_byPatient = function(rt, rowID_range = NULL, plot = TRUE,
                              plotLeftSideOption = "treatment",
                              xlim = c(-7,60),
                              plotCycleLength = 21,
                              plotCycles = 6,
                              plotXLegendScale = "days") {

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  .toxPlot_time = function(rt, toxDataSub, rowID_range = NULL, cols = cols) {


    if(is.null(rt@patientData$rowID)) {
        rt@patientData = rt@patientData[order(rt@patientData[,rt@treatmentCol],rt@patientData[,rt@patidCol]),]
        rt@patientData$rowID = dim(rt@patientData)[1]:1
    }

    toxDataSub$gid = sapply(toxDataSub[,rt@patidCol], function(x){ rt@patientData$rowID[rt@patientData[,rt@patidCol] == x] })


    ylim = c(min(rt@patientData$rowID)-0.5, max(rt@patientData$rowID) + 0.5)


    ##############################################################
    # get plot region size and split-screen
    size = dev.size("in")

    sizeBase = ifelse(size[1] < 9, 1, 0.6)
    ratioBase = sizeBase/size[2]
    par(mar=c(3.5,3,0.75,0.75))
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


    # end of assessment vertical line
    segments(toxDataSub[, "rel_ae_ent_assessment"],toxDataSub$gid-0.5,toxDataSub[, "rel_ae_ent_assessment"],toxDataSub$gid+0.5,lwd=4,col="grey")



    if(plotXLegendScale == "days"){
      axis(1, labels = -10:500 * 7, at = -10:500*7, pos = ylim[1])
      mtext("Days from start of treatment",side = 1, line = 2.5, cex = par("cex"))
    } else if(plotXLegendScale == "weeks"){
      axis(1, labels = -10:500, at = -10:500*7, pos = ylim[1])
      mtext("Weeks from start of treatment", side = 1, line = 2.5, cex = par("cex"))
    } else {
      axis(1, labels = -10:500, at = -10:500*as.numeric(plotXLegendScale), pos = ylim[1])
      mtext("Cycles from start of treatment", side = 1, line = 2.5, cex = par("cex"))
    }

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

  cols = c("#00CC00","#FF9900","red","#551A8B","black")
  cols = c("#98cee2", "#4c7bd3","#ff8d00","#ff0000","#b719b4")


  val = .toxPlot_time(rt,toxDataSub, rowID_range, cols = cols)

  #############################################################
  #############################################################
  ## legend
  screen(2)

  size = dev.size("in")
  sizeBase = ifelse(size[1] < 9, 1, 0.6)

  par(mar=c(0,3,0,0.75))
  plot(0,0,type="n",axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="", xaxs = "i", yaxs = "i")

  if(sizeBase == 0.6){
    xpos = c(0.2, 0.4, 0.6, 0.8, 1) - 0.05
    ypos = c(0.5, 0.5, 0.5, 0.5, 0.5)
    xsize = 0.03
    xoffset = 0.03
    ysize = 0.2
  } else {
    xpos = c(0.33, 0.66, 1, 0.33, 0.66) - 0.2
    ypos = c(0.7, 0.7, 0.7, 0.25, 0.25)
    xsize = 0.03
    xoffset = 0.03
    ysize = 0.13
  }
  label = c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5")

  par(xpd=TRUE)
  for(i in 1:5){
    text(xpos[i]-xoffset, ypos[i],labels=label[i],pos=2)
    polygon(xpos[i]+xsize*c(-1,-1,1,1),ypos[i]+ysize*c(-1,1,1,-1),col=cols[i],border=cols[i])
  }
  par(xpd=FALSE)
  #############################################################
  #############################################################
  #############################################################

  # Do we really want to close screens?
  close.screen(all.screens = TRUE)

}
