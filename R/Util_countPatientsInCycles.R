

.countPatientsInCycles = function(rt, cycles) {

  if(cycles[1] == "all") {
    cycles = rt@dateOfStartOfToxWindow
  } else if(is.numeric(cycles)) {
    cycles = rt@periodDividerCols[cycles]
  } else {
    for(cycle in cycles){
      if(!cycle %in% rt@periodDividerCols) {
        stop("Cycle with name ", cycle, " not defined in rt@periodDividerCols")
      }

    }
  }

  nPatients = c()
  for(trt in rt@treatmentCodes){
    patids = c()
    for(cycleCol in cycles) {
      patientSub = rt@patientData[!is.na(rt@patientData[,cycleCol]) & rt@patientData[,rt@treatmentCol] == trt, ]
      patids = unique(c(patids, patientSub[patientSub[,cycleCol] <= patientSub[,rt@dateOfEndOfToxWindow], rt@patidCol]))
    }
    nPatients = c(nPatients,length(patids))
  }

  return(nPatients)
}
