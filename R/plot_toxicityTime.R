#' Plot toxicities over time
#'
#' This function plots all the toxicities provided over time
#'
#' @inheritParams prepareToxicity
#' @param patid Build in patient subsetting by a list if not ""
#' @param dayRange The range of time in days to display on the graph
#' @param omit
#' @param plot TRUE/FALSE plot the graph.
#' @param plotCycleLength
#'
#'
#' @return
#' This plot function return the number of row of unique toxicities * patients. This assists in computing optimal size for saved graphs.

#' @export plot_toxicityTime

plot_toxicityTime=function(toxDB,patid=character(0),dayRange=c(-7,60),omit=1,plot=TRUE,plotCycleLength=21) {
  # subset to specific patient if required

  if(class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }

  if (length(patid)){
    toxDB@cleanData=toxDB@cleanData[toxDB@cleanData$patid %in% patid,]
  }


  # tell the user about patients with no toxicities
  if (omit==1 & length(toxDB@cleanData$patid[toxDB@cleanData$ae_ctcae_grade==0])>0) {
    message("Patients with no toxicities omitted: ", toxDB@cleanData$patid[toxDB@cleanData$ae_ctcae_grade==0])
  }

  toxDB@cleanData=toxDB@cleanData[toxDB@cleanData$ae_ctcae_grade>0,]
  toxDB@cleanData$rel_start=toxDB@cleanData$ae_start_date- toxDB@cleanData$registration_date
  toxDB@cleanData$rel_end  =toxDB@cleanData$ae_end_date  - toxDB@cleanData$registration_date
  toxDB@cleanData$rel_ent_trt = toxDB@cleanData$date_stopped_treatment  - toxDB@cleanData$registration_date

  toxDB@cleanData=toxDB@cleanData[order(toxDB@cleanData$patid,toxDB@cleanData$ass_category,toxDB@cleanData$ass_toxicity_disp,toxDB@cleanData$ae_start_date),]
  un=unique(toxDB@cleanData[,c("patid","ass_category","ass_toxicity_disp")])

  toxDB@cleanData$gid=0
  for (i in 1:length(toxDB@cleanData$gid)) {
    st = which(toxDB@cleanData$patid[i]==un$patid & toxDB@cleanData$ass_category[i]==un$ass_category & toxDB@cleanData$ass_toxicity_disp[i]==un$ass_toxicity_disp)
    toxDB@cleanData$gid[i]=which(toxDB@cleanData$patid[i]==un$patid & toxDB@cleanData$ass_category[i]==un$ass_category & toxDB@cleanData$ass_toxicity_disp[i]==un$ass_toxicity_disp)
  }

  if (!plot) {
    return(max(toxDB@cleanData$gid[i]))
  }


  # colouring
  cols=c("#00CC00","#FF9900","red","black","black")
  toxDB@cleanData$col=""
  for (i in 1:length(toxDB@cleanData$col)) {
    toxDB@cleanData$col[i]=cols[toxDB@cleanData$ae_ctcae_grade[i]]
  }


  xlim=c(dayRange[1],dayRange[2])
  ylim=c(0.5,max(toxDB@cleanData$gid)+0.5)

  par(mai=0.25*c(8,3,2,2),mfrow=c(1,1),cex=1)
  plot(0,0,xlim=xlim,ylim=ylim,type="n",axes=FALSE,xlab="Days from start of treatment",ylab="",xaxs="i",yaxs="i")

  ud=0.3


  axis(1,label=0:100*7,at=0:100*7,pos=ylim[1])
  abline(v=1:100*7,lty=2,col="grey")
  abline(h=1:1000-0.5,lty=2,col="grey",lwd=0.5)
  abline(v=0:20*plotCycleLength)
  a=c(0,by(toxDB@cleanData$gid,toxDB@cleanData$patid,max))+0.5
  patid.lab=rep(0,length(a)-1)
  for (i in 1:length(patid.lab)) {
    patid.lab[i]=(a[i]+a[i+1])/2
  }
  abline(h=a,lwd=2)
  axis(2,label=unique(toxDB@cleanData$patid),at=patid.lab,tick = FALSE)



  for (i in 1:length(toxDB@cleanData$col)){
    segments(toxDB@cleanData$rel_ent_trt[i],toxDB@cleanData$gid[i]-0.5,toxDB@cleanData$rel_ent_trt[i],toxDB@cleanData$gid[i]+0.5,lwd=3,col=1)
    if (toxDB@cleanData$rel_end[i]-toxDB@cleanData$rel_start[i]<1) {
      points(toxDB@cleanData$rel_start[i],toxDB@cleanData$gid[i],pch=19,col=toxDB@cleanData$col[i],cex=2.5)
    } else {
      polygon(c(toxDB@cleanData$rel_start[i],toxDB@cleanData$rel_start[i],toxDB@cleanData$rel_end[i],toxDB@cleanData$rel_end[i]),toxDB@cleanData$gid[i]+c(-ud,ud,ud,-ud),col=toxDB@cleanData$col[i],border =toxDB@cleanData$col[i])
    }
  }

  text(dayRange[2],toxDB@cleanData$gid,labels=toxDB@cleanData$ass_toxicity_disp,pos = 2,offset=0.25)
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

  return(max(toxDB@cleanData$gid))
}
