#' Plot toxicities
#'
#' This function plots all the toxicities provided over time
#'
#' @param rt an object of class robustToxicities
#' @param rowID_range optional, a length 2 vector detailing the minimum and maximum row to plot
#' @param plot whether to plot the graph or return the number of rows to plot
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.

#' @export toxPlot_byToxicity

toxPlot_byToxicity = function(rt, rowID_range = NULL, plot = TRUE,
                   plotLeftSideOption = "treatment",
                   xlim = c(-7,60),
                   plotCycleLength = 21,
                   plotCycles = 6,
                   plotXLegendScale = "days") {


  #######################################################
  .toxPlot_time = function(rt, toxDataSub, rowID_range = NULL, plot = TRUE, cols) {


    ####################################################################
    ## Convert dates to relative to start of treatment date



    toxDataSub = toxDataSub[order(toxDataSub[,rt@treatmentCol],toxDataSub[,rt@patidCol],toxDataSub[,rt@toxCategoryCol],toxDataSub[,rt@toxNameCol],toxDataSub$rel_ae_start),]
    un = unique(toxDataSub[, c(rt@patidCol, rt@toxCategoryCol, rt@toxNameCol)])

    # give each item a row id for all common toxicities
    toxDataSub$gid = 0
    for (i in 1:length(toxDataSub$gid)) {
      st = which(toxDataSub[i, rt@patidCol] == un[,rt@patidCol] & toxDataSub[i,rt@toxCategoryCol] == un[,rt@toxCategoryCol] & toxDataSub[i,rt@toxNameCol] == un[,rt@toxNameCol])
      toxDataSub$gid[i] = st
    }

    if(!plot) {
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
    abline(h = 1:1000-0.5, lty = 2, col = "lightgrey", lwd = 0.5)
    if (plotCycleLength > 0) {
      abline(v = 0:plotCycles * plotCycleLength, col="grey")
    }




    #########################################################
    ## add toxicities
    ud = 0.3 # up down size of polygons 0.5 would fill the rows

    for(j in 1:5){
      toxDataSub2 = toxDataSub[toxDataSub[,rt@toxGradeCol] == j,]
      if(dim(toxDataSub2)[1] > 0){
        rect(toxDataSub2$rel_ae_start,toxDataSub2$gid - ud,toxDataSub2$rel_ae_end+1,toxDataSub2$gid + ud,col = cols[j], border = NA)
      }
    }

    # end of assessment vertical line
    segments(toxDataSub[, "rel_ae_ent_assessment"],toxDataSub$gid-0.5,toxDataSub[, "rel_ae_ent_assessment"],toxDataSub$gid+0.5,lwd=4,col="grey")


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
    print(unique(toxDataSub[,rt@treatmentCol]))
    print(rt@treatmentCodes)
    print(which(unique(toxDataSub[,rt@treatmentCol]) %in% rt@treatmentCodes))
    trtLabels = rt@treatmentLabels[sapply(unique(toxDataSub[,rt@treatmentCol]), function(x) which(x == rt@treatmentCodes))]
    print(trtLabels)
    ## LHS patid and or treatment
    if(plotLeftSideOption == "patid") {
      axis(2,labels = unique(toxDataSub[,rt@patidCol]), at = patid.lab, tick = FALSE)
    } else if(plotLeftSideOption %in% c("treatment", "both")) {
      print(treatment.lab)
      axis(2,labels = trtLabels, at = treatment.lab, tick = FALSE)

      if(plotLeftSideOption == "both") {
        text(x = xlim[1] + 0.25 , y = patid.lab, labels = unique(toxDataSub[,rt@patidCol]), pos = 4)
      }
    }

    #########################################################
    ## RHS toxicity names

    text(xlim[2],toxDataSub$gid,labels=toxDataSub[,rt@toxNameCol],pos = 2,offset=0.25)
    box(lwd=2)

  }
  #######################################################

  if (class(rt) != "robustToxicitiesClass") {
    stop("rt must be of class robustToxicities")
  }

  # subset to specific stuff if required
  toxDataSub = rt@toxData[rt@toxData$ass_TRUE == TRUE, ]

  cols = c("#00CC00","#FF9900","red","#551A8B","black")
  cols = c("#98cee2", "#4c7bd3","#ff8d00","#ff0000","#b719b4")


  val = .toxPlot_time(rt,toxDataSub, rowID_range, plot, cols = cols)

  if(!plot) {
    return(val)
  }

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

  close.screen(all.screens = TRUE)
}




