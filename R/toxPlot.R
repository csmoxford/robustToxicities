#' Plot toxicities
#'
#' This function plots all the toxicities provided over time
#'
#' @param toxDB an object of class robustToxicities
#' @param rowID_range optional, a length 2 vector detailing the minimum and maximum row to plot
#' @param plot whether to plot the graph or return the number of rows to plot
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.

#' @export toxPlot

toxPlot = function(toxDB, rowID_range = NULL, plot = TRUE) {

  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class robustToxicities")
  }

  # subset to specific stuff if required
  cleanDataSub = toxDB@cleanData[toxDB@cleanData$ass_TRUE == TRUE, ]

  cols = c("#00CC00","#FF9900","red","black","black")

  # switch between time and cycle run
  if(toxDB@options@timeType == "time") {
    val = .toxPlot_time(toxDB,cleanDataSub, rowID_range, plot, cols = cols)
  } else if(toxDB@options@timeType == "cycle") {
    val = .toxPlot_cycle(toxDB, rowID_range, plot, cols = cols)
  } else {
    stop("time type must be one of 'time' or 'cycle'")
  }

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
    xpos = c(0.25, 0.5, 0.75, 1) - 0.05
    ypos = c(0.5, 0.5, 0.5, 0.5)
    xsize = 0.03
    xoffset = 0.03
    ysize = 0.2
  } else {
    xpos = c(0.5, 1, 0.5, 1) - 0.2
    ypos = c(0.3, 0.3, 0.7, 0.7)
    xsize = 0.03
    xoffset = 0.03
    ysize = 0.13
  }
  label = c("Grade 1", "Grade 2", "Grade 3", "Grade 4 or 5")

  for(i in 1:4){
    text(xpos[i]-xoffset, ypos[i],labels=label[i],pos=2)
    polygon(xpos[i]+xsize*c(-1,-1,1,1),ypos[i]+ysize*c(-1,1,1,-1),col=cols[i],border=cols[i])
  }

  close.screen(all.screens = TRUE)
}


.toxPlot_time = function(toxDB, cleanDataSub, rowID_range = NULL, plot = TRUE, cols) {


  ####################################################################
  ## Convert dates to relative to start of treatment date
  cleanDataSub$rel_start   = cleanDataSub$ae_start_date - cleanDataSub[,toxDB@options@plotStartTreatment]
  cleanDataSub$rel_end     = cleanDataSub$ae_end_date   - cleanDataSub[,toxDB@options@plotStartTreatment]
  cleanDataSub$rel_ent_trt = cleanDataSub$date_stopped_treatment  - cleanDataSub[,toxDB@options@plotStartTreatment]
  cleanDataSub$rel_end_assessment = cleanDataSub$date_end_assessment - cleanDataSub[,toxDB@options@plotStartTreatment]

  cleanDataSub = cleanDataSub[order(cleanDataSub$treatment,cleanDataSub$patid,cleanDataSub$ass_category,cleanDataSub$ass_toxicity_disp,cleanDataSub$ae_start_date),]
  un = unique(cleanDataSub[, c("patid", "ass_category", "ass_toxicity_disp")])

  # give each item a row id for all common toxicities
  cleanDataSub$gid = 0
  for (i in 1:length(cleanDataSub$gid)) {
    st = which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
    cleanDataSub$gid[i]=which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
  }

  if(!plot) {
    return(max(cleanDataSub$gid))
  }

  # reduce to only the rows needed this time
  if(!is.null(rowID_range)){
    cleanDataSub = cleanDataSub[cleanDataSub$gid %in% rowID_range[1]:rowID_range[2],]
  }

  #####################################################################
  # colouring
  cleanDataSub$col = ""
  for (i in 1:length(cleanDataSub$col)) {
    if (cleanDataSub$ae_grade[i]>0) {
      cleanDataSub$col[i] = cols[cleanDataSub$ae_grade[i]]
    }
  }

  xlim = c(toxDB@options@plotxMin, toxDB@options@plotxMax)
  ylim = c(min(cleanDataSub$gid)-0.5, max(cleanDataSub$gid) + 0.5)

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
  mtext("Days from start of treatment",side = 1, line = 2.5, cex = par("cex"))




  axis(1, labels = -10:100 * 7, at = -10:100*7, pos = ylim[1])
  abline(v = -10:100*7, lty = 2, col = "lightgrey")
  abline(h = 1:1000-0.5, lty = 2, col = "lightgrey", lwd = 0.5)
  if (toxDB@options@plotCycleLength > 0) {
    abline(v = 0:toxDB@options@plotCycles * toxDB@options@plotCycleLength, col="grey")
  }


  #########################################################
  ## get label positions for treatment and patid
  a = sort(c(min(cleanDataSub$gid)-1, by(cleanDataSub$gid, cleanDataSub$patid, max)) + 0.5)
  b = sort(c(min(cleanDataSub$gid)-1, by(cleanDataSub$gid, cleanDataSub$treatment, max)) + 0.5)
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
  ## add toxicities
  ud = 0.3 # up down size of polygons 0.5 would fill the rows

  for (i in 1:dim(cleanDataSub)[1]) {
    if(!is.na(cleanDataSub$rel_start[i]) & cleanDataSub$col[i] != "") {
      if (cleanDataSub$rel_end[i]-cleanDataSub$rel_start[i]<1) {
        points(cleanDataSub$rel_start[i],cleanDataSub$gid[i],pch=19,col=cleanDataSub$col[i],cex=2.5)
      } else {
        polygon(c(cleanDataSub$rel_start[i],cleanDataSub$rel_start[i],cleanDataSub$rel_end[i],cleanDataSub$rel_end[i]),cleanDataSub$gid[i]+c(-ud,ud,ud,-ud),col=cleanDataSub$col[i],border =cleanDataSub$col[i])
      }

      # end of assessment vertical line
      segments(cleanDataSub$rel_end_assessment[i],cleanDataSub$gid[i]-0.5,cleanDataSub$rel_end_assessment[i],cleanDataSub$gid[i]+0.5,lwd=3,col="grey")
      # end of treatment vertical line
      segments(cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]-0.5,cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]+0.5,lwd=3,col=1)

    }
  }

  #########################################################
  ## LHS patid and or treatment
  if(toxDB@options@plotLeftSideOption == "patid") {
    axis(2,labels = unique(cleanDataSub$patid), at = patid.lab, tick = FALSE)
  } else if(toxDB@options@plotLeftSideOption %in% c("treatment", "both")) {
    axis(2,labels = toxDB@treatmentLabels[unique(cleanDataSub$treatment)], at = treatment.lab, tick = FALSE)

    if(toxDB@options@plotLeftSideOption == "both") {
      text(x = toxDB@options@plotxMin + 0.25 , y = patid.lab, labels = unique(cleanDataSub$patid), pos = 4)
    }
  }

  #########################################################
  ## RHS toxicity names

  text(toxDB@options@plotxMax,cleanDataSub$gid,labels=cleanDataSub$ass_toxicity_disp,pos = 2,offset=0.25)
  box(lwd=2)

}


.toxPlot_cycle = function(toxDB, cleanDataSub, rowID_range = NULL, plot = TRUE, cols) {

  names_cycle = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "occur_in_cycle_")]
  names_present = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "present_in_cycle_")]
  names_cycle_stub = as.numeric(sub("occur_in_cycle_", "", names_cycle))

  cleanDataSub$rel_start   = apply(cleanDataSub[,names_cycle], 1, function(x) min(which(x > 0)) - 1)
  cleanDataSub$rel_end     = apply(cleanDataSub[,names_cycle], 1, function(x) max(which(x > 0)))
  cleanDataSub$rel_ent_trt = apply(cleanDataSub[,names_present], 1, function(x) max(which(x > 0)))
  cleanDataSub = cleanDataSub[order(cleanDataSub$treatment,cleanDataSub$patid,cleanDataSub$ass_category,cleanDataSub$ass_toxicity_disp,cleanDataSub$ae_start_date),]
  un = unique(cleanDataSub[, c("patid", "ass_category", "ass_toxicity_disp")])

  cleanDataSub$gid = 0
  for (i in 1:length(cleanDataSub$gid)) {
    st = which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
    cleanDataSub$gid[i]=which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
  }

  if (!plot) {
    return(max(cleanDataSub$gid))
  }

  # reduce to only the rows needed this time
  if(!is.null(rowID_range)){
    cleanDataSub = cleanDataSub[cleanDataSub$gid %in% rowID_range[1]:rowID_range[2],]
  }

  ######################################################
  # colouring
  cleanDataSub$col = ""
  for (i in 1:length(cleanDataSub$col)) {
    if(cleanDataSub$ae_grade[i] > 0){
      cleanDataSub$col[i]=cols[cleanDataSub$ae_grade[i]]
    }
  }

  xlim = c(toxDB@options@plotxMin, toxDB@options@plotxMax)
  ylim = c(min(cleanDataSub$gid)-0.5, max(cleanDataSub$gid) + 0.5)

  ##############################################################
  # get plot region size and split-screen
  size = dev.size("in")

  sizeBase = ifelse(size[1] < 9, 1, 0.6)
  ratioBase = sizeBase/size[2]
  par(mar=c(3.5,3,0.75,0.75))
  split.screen(figs = matrix(c(0,1,ratioBase,1,
                               0,1,0,ratioBase),ncol=4, byrow =TRUE))

  ##############################################################
  # Main plot
  screen(1)
  plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  mtext("Cycle",side = 1, line = 2.5, cex = par("cex"))




  axis(1, labels = toxDB@cycleLabels, at = 1:length(toxDB@cycleLabels)-0.5, pos = ylim[1])
  abline(v = -10:100, lty = 2, col = "lightgrey")
  abline(h = 1:1000-0.5, lty = 2, col = "lightgrey", lwd = 0.5)
  if (toxDB@options@plotCycleLength > 0) {
    abline(v = 0:toxDB@options@plotCycles * toxDB@options@plotCycleLength, col="grey")
  }


  abline(h = 1:1000-0.5, lty = 2, col = "grey", lwd = 0.5)

  #########################################################
  ## get label positions for treatment and patid
  a = sort(c(min(cleanDataSub$gid)-1, by(cleanDataSub$gid, cleanDataSub$patid, max)) + 0.5)
  b = sort(c(min(cleanDataSub$gid)-1, by(cleanDataSub$gid, cleanDataSub$treatment, max)) + 0.5)
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
  ## add toxicities
  ud = 0.3 # up down size of polygons 0.5 would fill the rows



  for (i in 1:dim(cleanDataSub)[1]) {
    if(!is.na(cleanDataSub$rel_start[i]) & !is.na(cleanDataSub$rel_end[i]) & cleanDataSub$col[i] !="") {
      polygon(c(cleanDataSub$rel_start[i],cleanDataSub$rel_start[i],cleanDataSub$rel_end[i],cleanDataSub$rel_end[i]),cleanDataSub$gid[i]+c(-ud,ud,ud,-ud),col = max(cleanDataSub$col[cleanDataSub$gid == cleanDataSub$gid[i]]),border = max(cleanDataSub$col[cleanDataSub$gid == cleanDataSub$gid[i]]))
    }
    segments(cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]-0.5,cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]+0.5,lwd=3,col=1)
  }

  #########################################################
  ## LHS patid and or treatment
  if(toxDB@options@plotLeftSideOption == "patid") {
    axis(2,labels = unique(cleanDataSub$patid), at = patid.lab, tick = FALSE)
  } else if(toxDB@options@plotLeftSideOption %in% c("treatment", "both")) {
    axis(2,labels = toxDB@treatmentLabels, at = treatment.lab, tick = FALSE)

    if(toxDB@options@plotLeftSideOption == "both") {
      text(x = toxDB@options@plotxMin + 0.25 , y = patid.lab, labels = unique(cleanDataSub$patid), pos = 4)
    }
  }

  #########################################################
  ## RHS toxicity names

  text(toxDB@options@plotxMax,cleanDataSub$gid,labels=cleanDataSub$ass_toxicity_disp,pos = 2,offset=0.25)
  box(lwd=2)


}
