
#' Toxicity tables by cycle/s
#'
#' Returns a toxicity table with the requested data according to the ass_TRUE column, for the cycles requested.
#'
#' @param toxDB an object of class robustToxicities
#' @param cycles The cycle number or numbers of the cycles to tabulate. May also be "all" to use all cycles
#' @param printMethod One of "print", "table" or "latex"
#'
#' @details
#' The latex option details requires you to use the \code{array} and \code{multirow} packages in the .tex file using \code{\\usepackage{array, multirow}}.
#'
#' @import xtable
#' @export print_toxTable_cycle

print_toxTable_cycle = function(toxDB, cycles = 1, printMethod = "table") {

  validObject(toxDB)

  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }

  if(!printMethod %in% c("table", "latex")) {
    stop("Print method not defined for method: ",printMethod)
  }

  if(length(cycles) == 1){
    if(cycles == "all"){
      cycles = 1:length(toxDB@cycleLabels)
    }
  }

  toxTableList = .toxTable_cycle(toxDB, cycles)
  toxTable  = toxTableList$toxTable
  nPatients = toxTableList$nPatients

  treatment = as.integer(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][2]))
  grade     = suppressWarnings(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][3]))
  grade[1:2] = c("Category", "Event Term")
  for (i in 3:length(grade)) {
    if (!is.na(grade[i])) {
      if (grade[i] == "total") {
        grade[i] = "Total"
      } else if (str_length(grade[i]) == 1) {
        if (toxDB@options@cumulativeGrades) {
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

  nColTrt = (dim(toxTable)[2]-2)/length(toxDB@treatmentLabels)

  colnames(toxTable) = grade

  # change to percentages
  if(toxDB@options@tabulationPercent) {
    message("This table is based on percentages")
    dm = dim(toxTable)
    for(i in 1:length(toxDB@treatmentLabels)) {
      toxTable[,2 + 1:nColTrt + nColTrt*(i-1)] = round(100 * toxTable[,2 + 1:nColTrt + nColTrt*(i-1)] / nPatients[i], 0)
    }
  }

  if(!toxDB@options@tabulationZeros) {
    dm = dim(toxTable)
    for(i in 1:dm[1]){
      for(j in 1:dm[2]){
        if(toxTable[i,j] == 0){
          toxTable[i,j] = ""
        }
      }
    }
  }


  toxTable = rbind(rep("",dim(toxTable)[1]),toxTable)
  toxTable[1,] = ""
  toxTable[1,1:2] = c("","Number of patients")

  for(i in 1:length(toxDB@treatmentLabels)){
    toxTable[1,nColTrt*(i-1)+3] = nPatients[i]
  }

  if( printMethod == "print") {
    print(toxTable, row.names = FALSE)
  } else if( printMethod == "table") {
    return(toxTable)
  } else if( printMethod == "latex") {

    align = c("l","|p{4cm}",">{\\raggedleft\\arraybackslash}p{5cm}|",rep(c(rep("c",nColTrt-1),"c|"),length(toxDB@treatmentLabels)))

    # define appearance of \multirow
    fullRow = which(toxTable[[1]] != "")
    if(length(fullRow) > 0){
      row.lengths = c(fullRow[2:length(fullRow)],nrow(toxTable)+1)-fullRow[1:(length(fullRow))]
      for(i in 1:length(fullRow)){
        if(row.lengths[i]>1){
          toxTable[[1]][fullRow[i]] = paste0("\\multirow{", row.lengths[i], "}{*}{\\parbox{4cm}{", toxTable[[1]][fullRow[i]], "}}")
        }
      }
    }

    # add multicolumn for treatments and return
    xtab = xtable(toxTable, digits = 0, align=align)
    addtorow <- list()
    addtorow$pos <- list(-1)
    addtorow$command <- paste0("\\hline\n &",paste0('& \\multicolumn{',nColTrt , '}{c|}{', toxDB@treatmentLabels, " (n=", nPatients, ")", '}', collapse=''), '\\\\\n')
    return(print(xtab, add.to.row=addtorow, include.rownames=FALSE, hline.after = c(0,fullRow-1,nrow(xtab)), sanitize.text.function = force, comment = FALSE))
  }
}
