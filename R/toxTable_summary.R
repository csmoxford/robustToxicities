
#' Summary table of toxicities
#'
#' Returns a summary toxicity table with the requested data according to the ass_TRUE column.
#'
#' @param rt an object of class robustToxicities
#'
#' @seealso \code{\link{toxicityOptions-class}}
#'
#' @return data.frame
#'
#' @example inst/HelpExamples/toxTable_summary_example.R
#'
#'
#'
#' @export ToxTable_summary
ToxTable_summary = function(rt) {

  if(!rt@wasQueried){
    message("Warning: QueryRobustToxicities has not been applied to this object")
  }

  validObject(rt)

  if (class(rt) != "robustToxicitiesClass") {
    stop("rt must be of class rt")
  }


  toxTable = .toxTable_summary(rt)

  ############################################
  # create column names
  treatment = suppressWarnings(as.integer(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][2])))
  grade     = suppressWarnings(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][3]))

  for (i in 1:length(grade)) {
    if (!is.na(grade[i])) {
      if (grade[i] == "n") {
        grade[i] = "Patients"
      } else if (str_length(grade[i]) == 1) {
        if (rt@options@toxTable_cumulativeGrades) {
          if(grade[i] != "5"){
            grade[i] = paste0(grade[i],"+")
          } else {
            grade[i] = paste0(grade[i])
          }
        } else {
          grade[i] = paste(grade[i])
        }
      } else {
        if (str_length(grade[i]) == 2) {
          grade[i] = paste(paste0(strsplit(grade[i],"")[[1]], collapse = " and "))
        } else {
          num = as.numeric(strsplit(grade[i],"")[[1]])
          grade[i] = paste(min(num),"-",max(num))
        }
      }
    } else {
      grade[i] = "Time period"
    }
  }

  nColTrt = (dim(toxTable)[2]-1)/length(rt@treatmentLabels)
  colnames(toxTable) = grade

  ############################################
  # Add percentages if required
  # Note: This only adds percentages if greater than zero
  if(rt@options@toxTable_tabulationPercent) {
    nColTrt = (dim(toxTable)[2]-1)/length(rt@treatmentLabels)
    dm = dim(toxTable)
    for(i in 1:length(rt@treatmentLabels)) {
      for(j in 1:dm[1]){
        counts = as.numeric(toxTable[j,2 + 1:(nColTrt-1) + (nColTrt)*(i-1)])
        percent = round(100 * counts / toxTable[j,2 + (nColTrt)*(i-1)], 0)
        value = sapply(1:length(counts), function(x) paste0(counts[x], ifelse(counts[x] > 0,paste0(" (", percent[x], "%)"),"")))
        toxTable[j,2 + 1:(nColTrt-1) + (nColTrt)*(i-1)] = value
      }
    }
  }

  ############################################
  # Remove zeros if necessary
  if(!rt@options@toxTable_tabulationZeros) {
    dm = dim(toxTable)
    for(i in 1:dm[1]){
      for(j in 1:dm[2]){
        if(toxTable[i,j] == 0){
          toxTable[i,j] = ""
        }
      }
    }
  }

  return(toxTable)
}


