
#' Toxicity tables by cycle/s
#'
#' Returns a toxicity table with the requested data according to the ass_TRUE column, for the cycles requested.
#'
#' @param rt an object of class robustToxicities
#' @param cycles The cycle column names, or index in rt@periodDividerCols of the cycles to tabulate. May also be "all" to use all cycles
#'
#' @importFrom stringr str_length
#' @export ToxTable_cycle
ToxTable_cycle = function(rt, cycles = "all") {

  if(!rt@wasQueried){
    stop("Warning: QueryRobustToxicities has not been applied to this object")
  }

  validObject(rt)

  if (class(rt) != "robustToxicitiesClass") {
    stop("rt must be of class rt")
  }

  nPatients = .countPatientsInCycles(rt, cycles)

  # Get the base table!
  toxTble = .toxTable_cycle(rt, cycles)
  ############################################
  if(dim(toxTble)[1] == 0){
    return(toxTble)
  }
  ############################################
  treatment = as.integer(sapply(colnames(toxTble), function(x) strsplit(x,"[.]")[[1]][2]))
  grade     = suppressWarnings(sapply(colnames(toxTble), function(x) strsplit(x,"[.]")[[1]][3]))

  # create column names
  grade[1:2] = c("Category", "Event Term")
  for (i in 3:length(grade)) {
    if (!is.na(grade[i])) {
      if (str_length(grade[i]) == 1) {
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
    }
  }
  nColTrt = (dim(toxTble)[2]-2)/length(rt@treatmentLabels)
  colnames(toxTble) = grade

  ############################################
  # Add percentages if required
  # Note: This only adds percentages if greater than zero
  if(rt@options@toxTable_tabulationPercent) {
    dm = dim(toxTble)
    for(i in 1:length(rt@treatmentLabels)) {
      for(j in 1:dim(toxTble)[1]){
        counts = as.numeric(toxTble[j,2 + 1:nColTrt + nColTrt*(i-1)])
        percent = round(100 * counts / nPatients[i], 0)
        value = sapply(1:length(counts), function(x) paste0(counts[x], ifelse(counts[x] > 0,paste0(" (", percent[x], "%)"),"")))
        toxTble[j,2 + 1:nColTrt + nColTrt*(i-1)] = value
      }
    }
  }

  ############################################
  # Remove zeros if necessary
  if(!rt@options@toxTable_tabulationZeros) {
    dm = dim(toxTble)
    for(i in 1:dm[1]){
      for(j in 1:dm[2]){
        if(toxTble[i,j] == 0){
          toxTble[i,j] = ""
        }
      }
    }
  }

  return(toxTble)
}
