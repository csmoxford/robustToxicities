#' Plot toxicities by cycle
#'
#' This function plots all the toxicities provided over time
#'
#' @param toxDB an object of class robustToxicities
#' @param patients Build in patient subsetting by a list if not ""
#' @param plot TRUE/FALSE plot the graph.
#'
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.

#' @export toxPlot_cycle

toxPlot_cycle = function(toxDB, patients = character(0), plot=TRUE) {

  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class robustToxicities")
  }

  # subset to specific patient if required
  cleanDataSub = toxDB@cleanData[toxDB@cleanData$ass_TRUE == TRUE, ]




  if (length(patients)) {
    cleanDataSub = cleanDataSub[cleanDataSub$patid %in% patients, ]
  }

  names_cycle = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "occur_in_cycle_")]
  names_present = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "present_in_cycle_")]
  names_cycle_stub = as.numeric(sub("occur_in_cycle_", "", names_cycle))



  cleanDataSub             = cleanDataSub[cleanDataSub$ae_grade > 0, ]
  cleanDataSub$rel_start   = apply(cleanDataSub[,names_cycle], 1, function(x) min(which(x > 0)) - 1)
  cleanDataSub$rel_end     = apply(cleanDataSub[,names_cycle], 1, function(x) max(which(x > 0)))
  cleanDataSub$rel_ent_trt = apply(cleanDataSub[,names_present], 1, function(x) max(which(x > 0)))
print(cleanDataSub[,c("patid",names_present)])
  cleanDataSub = cleanDataSub[order(cleanDataSub$treatment,cleanDataSub$patid,cleanDataSub$ass_category,cleanDataSub$ass_toxicity_disp,cleanDataSub$ae_start_date),]
  un = unique(cleanDataSub[, c("patid", "ass_category", "ass_toxicity_disp")])

  cleanDataSub$gid = 0
  for (i in 1:length(cleanDataSub$gid)) {
    st = which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
    cleanDataSub$gid[i]=which(cleanDataSub$patid[i]==un$patid & cleanDataSub$ass_category[i]==un$ass_category & cleanDataSub$ass_toxicity_disp[i]==un$ass_toxicity_disp)
  }

  if (!plot) {
    return(max(cleanDataSub$gid[i]))
  }


  # colouring
  cols = c("#00CC00","#FF9900","red","black","black")
  cleanDataSub$col = ""
  for (i in 1:length(cleanDataSub$col)) {
    cleanDataSub$col[i]=cols[cleanDataSub$ae_grade[i]]
  }


  xlim = c(toxDB@options@plotxMin, toxDB@options@plotxMax)
  ylim = c(0.5, max(cleanDataSub$gid) + 0.5)

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
  mtext("Cycle",side = 1, line = 2.5)




  axis(1, labels = toxDB@cycleLabels, at = 1:length(toxDB@cycleLabels)-0.5, pos = ylim[1])
  abline(v = -10:100*7, lty = 2, col = "lightgrey")
  abline(h = 1:1000-0.5, lty = 2, col = "lightgrey", lwd = 0.5)
  if (toxDB@options@plotCycleLength > 0) {
    abline(v = 0:toxDB@options@plotCycles * toxDB@options@plotCycleLength, col="grey")
  }


  abline(h = 1:1000-0.5, lty = 2, col = "grey", lwd = 0.5)

  #########################################################
  ## get label positions for treatment and patid
  a = sort(c(0, by(cleanDataSub$gid, cleanDataSub$patid, max)) + 0.5)
  b = sort(c(0, by(cleanDataSub$gid, cleanDataSub$treatment, max)) + 0.5)
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



  for (i in 1:length(cleanDataSub$col)) {
    if(!is.na(cleanDataSub$rel_start[i]) & !is.na(cleanDataSub$rel_end[i])) {
      polygon(c(cleanDataSub$rel_start[i],cleanDataSub$rel_start[i],cleanDataSub$rel_end[i],cleanDataSub$rel_end[i]),cleanDataSub$gid[i]+c(-ud,ud,ud,-ud),col = max(cleanDataSub$col[cleanDataSub$gid == cleanDataSub$gid[i]]))
    }
    segments(cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]-0.5,cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]+0.5,lwd=3,col=1)
  }

  print(cleanDataSub[,c("patid","rel_ent_trt")])

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

  #############################################################
  ## legend
  screen(2)

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

  return(max(cleanDataSub$gid))
}
