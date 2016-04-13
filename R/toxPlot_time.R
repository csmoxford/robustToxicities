#' Plot toxicities over time
#'
#' This function plots all the toxicities provided over time
#'
#' @inheritParams prepareToxicity
#' @param patients Build in patient subsetting by a list if not ""
#' @param plot TRUE/FALSE plot the graph.
#'
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.

#' @export toxPlot_time

toxPlot_time = function(toxDB, patients = character(0), plot=TRUE) {
  # subset to specific patient if required
  cleanDataSub = toxDB@cleanData[toxDB@cleanData$ass_TRUE == TRUE, ]


  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class robustToxicities")
  }

  if (length(patients)) {
    cleanDataSub = cleanDataSub[cleanDataSub$patid %in% patients, ]
  }

  cleanDataSub             = cleanDataSub[cleanDataSub$ae_grade>0,]
  cleanDataSub$rel_start   = cleanDataSub$ae_start_date - cleanDataSub[,toxDB@options@plotStartTreatment]
  cleanDataSub$rel_end     = cleanDataSub$ae_end_date   - cleanDataSub[,toxDB@options@plotStartTreatment]
  cleanDataSub$rel_ent_trt = cleanDataSub$date_stopped_treatment  - cleanDataSub[,toxDB@options@plotStartTreatment]

  cleanDataSub = cleanDataSub[order(cleanDataSub$patid,cleanDataSub$ass_category,cleanDataSub$ass_toxicity_disp,cleanDataSub$ae_start_date),]
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

  par(mai = 0.25 * c(8,3,2,2), mfrow = c(1,1), cex = 1)
  plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, xlab = "Days from start of treatment", ylab = "", xaxs = "i", yaxs = "i")

  ud = 0.3


  axis(1, label = 0:100 * 7, at = 0:100*7, pos = ylim[1])
  abline(v = 1:100*7, lty = 2, col = "grey")
  abline(h = 1:1000-0.5, lty = 2, col = "grey", lwd = 0.5)
  if (toxDB@options@plotCycleLength > 0) {
    abline(v = 0:20 * toxDB@options@plotCycleLength)
  }
  a = c(0, by(cleanDataSub$gid, cleanDataSub$patid, max)) + 0.5
  patid.lab = rep(0, length(a) - 1)
  for (i in 1:length(patid.lab)) {
    patid.lab[i] = (a[i] + a[i + 1]) / 2
  }
  abline(h = a,lwd = 2)
  axis(2,label = unique(cleanDataSub$patid), at = patid.lab, tick = FALSE)



  for (i in 1:length(cleanDataSub$col)) {
    if(!is.na(cleanDataSub$rel_start[i])) {
      if (cleanDataSub$rel_end[i]-cleanDataSub$rel_start[i]<1) {
        points(cleanDataSub$rel_start[i],cleanDataSub$gid[i],pch=19,col=cleanDataSub$col[i],cex=2.5)
      } else {
        polygon(c(cleanDataSub$rel_start[i],cleanDataSub$rel_start[i],cleanDataSub$rel_end[i],cleanDataSub$rel_end[i]),cleanDataSub$gid[i]+c(-ud,ud,ud,-ud),col=cleanDataSub$col[i],border =cleanDataSub$col[i])
      }
      segments(cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]-0.5,cleanDataSub$rel_ent_trt[i],cleanDataSub$gid[i]+0.5,lwd=3,col=1)
    }
  }

  text(toxDB@options@plotxMax,cleanDataSub$gid,labels=cleanDataSub$ass_toxicity_disp,pos = 2,offset=0.25)
  box()
  par(xpd=TRUE)
  xlow=xlim[1]+0.25*(xlim[2]-xlim[1])
  xrange=0.5*(xlim[2]-xlim[1])
  ylow=-7
  yrange=4
  polygon(xlow+c(0,0,xrange,xrange),ylow+c(0,yrange,yrange,0),border=1)

  text(xlow+xrange*0.2,ylow+yrange*0.75,labels="Grade 1")
  text(xlow+xrange*0.7,ylow+yrange*0.75,labels="Grade 2")
  text(xlow+xrange*0.2,ylow+yrange*0.25,labels="Grade 3")
  text(xlow+xrange*0.7,ylow+yrange*0.25,labels="Grade 4 or 5")

  polygon(xlow+xrange*c(0.3,0.3,0.4,0.4),ylow+yrange*c(0.65,0.85,0.85,0.65),col=cols[1],border=cols[1])
  polygon(xlow+xrange*c(0.8,0.8,0.9,0.9),ylow+yrange*c(0.65,0.85,0.85,0.65),col=cols[2],border=cols[2])
  polygon(xlow+xrange*c(0.3,0.3,0.4,0.4),ylow+yrange*c(0.15,0.35,0.35,0.15),col=cols[3],border=cols[3])
  polygon(xlow+xrange*c(0.8,0.8,0.9,0.9),ylow+yrange*c(0.15,0.35,0.35,0.15),col=cols[4],border=cols[4])

  par(xpd=FALSE)

  return(max(cleanDataSub$gid))
}
