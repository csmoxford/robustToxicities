#' @importFrom stringr str_detect

.toxTable_summary = function(rt) {


  #######################################################
  # create table to populate
  toxTable = data.frame(cycle.number = c(rt@periodDividerLabels,"All"), stringsAsFactors = FALSE)
  # for each treatment generate space to count these.
  treats = 1:length(rt@treatmentCodes)
  for (treatment in treats) {
    toxTable[paste0("tox.", treatment,".n", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".0", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".1", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".2", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".3", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".4", sep = "")] = 0
    toxTable[paste0("tox.", treatment,".5", sep = "")] = 0
  }
  #######################################################

  # For each cycle get summary
  if(length(rt@periodDividerCols) > 0) {
    for(cycle.no in 1:length(rt@periodDividerCols)) {

      nPatients = .countPatientsInCycles(rt, cycle.no)

      toxDataSub =  .subSetAEToCycle(rt, rt@toxData, cycle.no)
      patientData = rt@patientData

      patientData$worstGrade = worstGradeByPatient(rt, toxDataSub)

      # if merging more than one cycle need to apply to merge columns
      cleanDataSub = toxDataSub[toxDataSub$ass_TRUE  ==  TRUE,]


      for(treatmentID in 1:length(rt@treatmentCodes)) {
        trtCode = rt@treatmentCodes[treatmentID]

        toxTable[cycle.no, 7 * treatmentID - 5] = nPatients[treatmentID]

        patientDataSub = patientData[patientData[,rt@treatmentCol] == trtCode,]

        toxTable[cycle.no,3:8 + (treatmentID - 1) * 7] = c(sum(patientDataSub$worstGrade == 0),sum(patientDataSub$worstGrade == 1),sum(patientDataSub$worstGrade == 2),sum(patientDataSub$worstGrade == 3),sum(patientDataSub$worstGrade == 4),sum(patientDataSub$worstGrade == 5))
      }
    }
  } else {
    cycle.no = 0
  }
  # for all cycles get summary
  nPatients = .countPatientsInCycles(rt, "all")
  toxDataSub =  .subSetAEToCycle(rt, rt@toxData, "all")
  patientData = rt@patientData
  patientData$worstGrade = worstGradeByPatient(rt, toxDataSub)

  # if merging more than one cycle need to apply to merge columns
  cleanDataSub = toxDataSub[toxDataSub$ass_TRUE  ==  TRUE,]

  for(treatmentID in 1:length(rt@treatmentCodes)) {
    trtCode = rt@treatmentCodes[treatmentID]

    toxTable[cycle.no+1, 7 * treatmentID - 5] = nPatients[treatmentID]

    patientDataSub = patientData[patientData[,rt@treatmentCol] == trtCode,]

    toxTable[cycle.no+1,3:8 + (treatmentID - 1) * 7] = c(sum(patientDataSub$worstGrade == 0),sum(patientDataSub$worstGrade == 1),sum(patientDataSub$worstGrade == 2),sum(patientDataSub$worstGrade == 3),sum(patientDataSub$worstGrade == 4),sum(patientDataSub$worstGrade == 5))
  }
  toxTable[cycle.no+1,1] = "All"


  # Perform the column merge for toxicities
  if(is.null(rt@options@toxTable_mergeGrades) == FALSE){
    colMerge = strsplit(rt@options@toxTable_mergeGrades, "[|]")[[1]]
    toxTableClean = data.frame(toxTable$cycle, stringsAsFactors = FALSE)
    for(side in 1:length(treats)){
      for(col in colMerge){
        if(col == "n") {

          cols = paste0("tox.", side, ".n")
          toxTableClean[, cols] = toxTable[, cols]
        } else if(rt@options@toxTable_cumulativeGrades) {

          composition = min(as.numeric(strsplit(col, ",")[[1]])):5
          cols = paste0("tox.", side, ".", composition)
          cname = paste0("tox.", side, ".", composition[1])
          if(length(composition) > 1) {
            toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
          } else {
            toxTableClean[, cname] = toxTable[, cols]
          }

        } else {

          composition = as.numeric(strsplit(col, ",")[[1]])
          cols = paste0("tox.", side, ".", composition)
          cname = paste0("tox.", side, ".", paste0(composition, collapse = ""))
          if(length(composition) > 1) {
            toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
          } else {
            toxTableClean[, cols] = toxTable[, cols]
          }
        }
      }
    }
    toxTable = toxTableClean
  }

  # renaming
  toxTable[,1] = c(rt@periodDividerLabels, "Overall")

  # return the table to the app
  return(toxTable)

}
