
# Note that the data subset for a patients toxicities is dt and should be used in requirement.
# requriement should be a quote quote(dt$ae_grade[j] >= 3) returning true or false for a condition in the data subset dt using j to index the row. This enables it to be run within the loop with a custom argument.
getGradeCyclesByAssTRUE = function(toxDB, dta = NULL, minGrade = 3){

  # dta should be a data frame with every patient and treatment as a variable
  # will generate from toxDB if not provided.
  if(is.null(dta)){
    dta = data.frame(patid = unique(toxDB@cleanData$patid))
    dta$treatment = sapply(dta$patid, function(x) toxDB@cleanData$treatment[toxDB@cleanData$patid == x][1])
    }

  res = list()
  for(j in 1:length(toxDB@treatmentLabels)){
    res[[paste0("n_",j)]] = rep(NA,length(toxDB@cycleLabels))
    res[[paste0("t_",j)]] = rep(NA,length(toxDB@cycleLabels))
  }

  for(i in 1:length(toxDB@cycleLabels)){

    dta$present = toxDB@cleanData[,paste0("present_in_cycle_",i)][1]

    dta$tox = sapply(1:dim(dta)[1], function(x){
      id = dta$patid[x]
      if(dta$present[x]) {
        return(sum(toxDB@cleanData$patid == dta$patid[x] & toxDB@cleanData[,paste0("occur_in_cycle_",i)] >= minGrade & toxDB@cleanData$ass_TRUE))

      } else {
        return(NA)
      }
    })

    for(j in 1:length(toxDB@treatmentLabels)){
      res[[paste0("n_",j)]][i] = sum(dta$present & dta$treatment == j, na.rm = TRUE)
      res[[paste0("t_",j)]][i] = sum(dta$tox & dta$treatment == j, na.rm = TRUE)
    }

  }
  return(res)
}



toxCyclePlot = function(toxDB,res,cycles = NULL,ylim = NULL, xlim = NULL, col = 2, lwd = lwd, plotN = FALSE){

  if(is.null(cycles)){
    cycles = 1:toxDB@cycleLabels
  }

  cex.lab = 1.5 * par("cex")
  cex.axis = 1.5
  cex.leg = 1.5
  lwd = 2
  if(is.null(ylim)){
    if(!plotN){
      ylim = range(pretty(c(0, sapply(1:length(toxDB@treatmentLabels), function(x) 100 * res[[paste0("t_", x)]] / res[[paste0("t_", x)]]))))
    } else {
      ylim = range(pretty(c(0, sapply(1:length(toxDB@treatmentLabels), function(x) res[[paste0("n_", x)]]))))
    }
  }

  plot(0,0,type = "n", xlab = "", ylab = "", xlim = c(1,length(cycles)), xaxs = "i", yaxs = "i", ylim = ylim, axes = FALSE)

  if(!plotN){
  for(i in 1:length(toxDB@treatmentLabels)){

    if(i == 1){
      lines(1:length(cycles),100 * res[[paste0("t_",i)]][cycles] / res[[paste0("n_",i)]][cycles], col = col[i], lwd = lwd * 0.75)
      polygon(c(1,1:length(cycles),length(cycles)),c(0,100 * res[[paste0("t_",i)]][cycles] / res[[paste0("n_",i)]][cycles],0), border = NA, col = adjustcolor(col[i], alpha = 0.25))
    } else {
      lines(1:length(cycles),100 * res[[paste0("t_",i)]][cycles] / res[[paste0("n_",i)]][cycles], col = col[i], lwd = lwd * 1.5)
    }
  }
    mtext(2,text = "Percentage of patients", line = 3.5, cex = cex.lab)
  } else {
    for(i in 1:length(toxDB@treatmentLabels)){

      if(i == 1){
        lines(1:length(cycles),res[[paste0("n_",i)]][cycles], col = col[i], lwd = lwd * 0.75)
        polygon(c(1,1:length(cycles),length(cycles)),c(0, res[[paste0("n_",i)]][cycles],0), border = NA, col = adjustcolor(col[i], alpha = 0.25))
      } else {
        lines(1:length(cycles),100 * res[[paste0("t_",i)]][cycles] / res[[paste0("n_",i)]][cycles], col = col[i], lwd = lwd * 1.5)
      }
    }
    mtext(2,text = "Number of patients", line = 3.5, cex = cex.lab)
  }

  axis(1, at = 1:length(cycles), labels = toxDB@cycleLabels[cycles], cex.axis = cex.axis, lwd = lwd)
  axis(2, cex.axis = cex.axis, las = 2, lwd = lwd)
  mtext(1,text = "Cycles", line = 2.5, cex = cex.lab)

}
