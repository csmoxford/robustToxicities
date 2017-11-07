.subSetAEToCycle = function(rt, toxDataSub = NULL, cycles) {

  if(is.null(toxDataSub)) {
    toxDataSub = rt@toxData
  }

  if(cycles[1] != "all") {

    if(is.numeric(cycles)) {
      cycles = rt@periodDividerCols[cycles]
    } else {
      for(cycle in cycles){
        if(!cycle %in% rt@periodDividerCols) {
          stop("Cycle with name ", cycle, " not defined in rt@periodDividerCols")
        }
      }
    }

    # toxicities in requested cycles
    toxDataSub$inCycle = FALSE
    for(i in 1:length(cycles)) {
      cycleCol = cycles[i]
      j = which(cycleCol == rt@periodDividerCols)
      # start time of cycle
      toxDataSub$t0 = sapply(toxDataSub[,rt@patidCol], function(x) rt@patientData[rt@patientData[,rt@patidCol] == x,cycleCol])

      # possible end time columns (we take the smallest value from among them)
      # This means that if a patient withdraws the dateOfEndOfToxWindow is used instead
      if(j < length(rt@periodDividerCols)) {
        cycleCols = c(rt@periodDividerCols[(j+1):length(rt@periodDividerCols)],rt@dateOfEndOfToxWindow)
      } else {
        cycleCols = rt@dateOfEndOfToxWindow
      }
      # Get out end of cycle time
      toxDataSub$t1 = sapply(toxDataSub[,rt@patidCol], function(x) min(as.numeric(rt@patientData[rt@patientData[,rt@patidCol] == x,cycleCols]), na.rm = TRUE))



      # check if toxicity occuring at least in part within time period.
      toxDataSub$inCycle = toxDataSub$inCycle | sapply(1:dim(toxDataSub)[1], function(x){
        if(is.na(toxDataSub$t0[x])) {
          return(FALSE)
        } else if(toxDataSub[x,rt@dateOfStartTox] >= toxDataSub$t1[x]) {
          return(FALSE)
        } else if(is.na(toxDataSub[x,rt@dateOfEndTox])){
          return(TRUE)
        } else if(toxDataSub[x,rt@dateOfEndTox] <= toxDataSub$t0[x]) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      })

      # keep if and only if t0 is less than or equal to t1
      toxDataSub =  toxDataSub[toxDataSub$t0 <= toxDataSub$t1 & toxDataSub$inCycle,]
    }
  }


  return(toxDataSub)

}
