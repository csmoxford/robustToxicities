## data currently used
# patid
# registration_date
# date_stopped_treatment
# ae_start_date
# ae_end_date
# ae_term
plot.toxicity.time=function(data,patid="",min.day=-7,max.day=60){
  # subset to specific patient if required
  if(length(patid)>1 & patid!=""){
    data=data[data$patid %in% patid,]
  }
  # subset only matched data
  data=data[data$ass_TRUE==TRUE,]

  data$rel_start=data$ae_start_date-data$registration_date
  data$rel_end  =data$ae_end_date  -data$registration_date
  data$rel_ent_trt = data$date_stopped_treatment  -data$registration_date

  data=data[order(data$patid,data$ass_category,data$ass_toxicity_disp,data$ae_start_date),]

  un=unique(data[,c("patid","ass_category","ass_toxicity_disp")])

  data$gid=0
  for(i in 1:length(data$gid)){
    data$gid[i]=which(data$patid[i]==un$patid & data$ass_category[i]==un$ass_category & data$ass_toxicity_disp[i]==un$ass_toxicity_disp)
  }


  # colouring
  cols=c("#00CC00","#FF9900","red","black","black")
  data$col=""
  for(i in 1:length(data$col)){
    data$col[i]=cols[data$ae_ctcae_grade[i]]
  }

  xlim=c(min.day,max.day)
  ylim=c(0.5,max(data$gid)+0.5)

  par(mai=0.25*c(8,3,2,2),mfrow=c(1,1),cex=1)
  plot(0,0,xlim=xlim,ylim=ylim,type="n",axes=FALSE,xlab="Days from start of treatment",ylab="",xaxs="i",yaxs="i")

  ud=0.3


  axis(1,label=0:55*7,at=0:55*7,pos=ylim[1])
  abline(v=1:55*7,lty=2,col="grey")
  abline(h=1:1000-0.5,lty=2,col="grey")
  abline(v=0:20*21)
  a=c(0,by(data$gid,data$patid,max))+0.5
  patid.lab=rep(0,length(a)-1)
  for(i in 1:length(patid.lab)){patid.lab[i]=(a[i]+a[i+1])/2}
  abline(h=a)
  axis(2,label=unique(data$patid),at=patid.lab,tick = FALSE)



  for(i in 1:length(data$col)){
    segments(data$rel_ent_trt[i],data$gid[i]-0.5,data$rel_ent_trt[i],data$gid[i]+0.5,lwd=3,col=1)
    if(data$rel_end[i]-data$rel_start[i]<1){
      points(data$rel_start[i],data$gid[i],pch=3,col=data$col[i],cex=1)
    } else {
      polygon(c(data$rel_start[i],data$rel_start[i],data$rel_end[i],data$rel_end[i]),data$gid[i]+c(-ud,ud,ud,-ud),col=data$col[i],border =data$col[i])
    }
  }

  text(max.day,data$gid,labels=data$ass_toxicity_disp,pos = 2,offset=0.25)
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

  return(max(data$gid))
}
