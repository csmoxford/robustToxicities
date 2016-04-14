
#' Toxicity table summary
#'
#' Returns a summary toxicity table with the requested data according to the ass_TRUE column.
#'
#' @param toxDB an object of class robustToxicities
#' @param printMethod One of "print" "table" or "latex"
#'
#' @details
#' The latex option details requires you to use the \code{array} and \code{multirow} packages in the .tex file using \code{\\usepackage{array, multirow}}.
#'
#' @export print_toxTable_summary

print_toxTable_summary = function(toxDB, printMethod = "table") {

  validObject(toxDB)

  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }

  if(!printMethod %in% c("print","table", "latex")) {
    stop("Print method not defined for method: ",printMethod)
  }

  toxTable = .toxTable_summary(toxDB)

  treatment = as.integer(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][2]))
  grade     = suppressWarnings(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][3]))

  for (i in 1:length(grade)) {
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
    } else {
      grade[i] = "Time period"
    }
  }

  nColTrt = (dim(toxTable)[2]-1)/length(toxDB@treatmentLabels)

  colnames(toxTable) = grade

  # change to percentages
  # change to percentages
  if(toxDB@options@tabulationPercent) {
    message("This table is based on percentages")
    dm = dim(toxTable)
    for(i in 1:length(toxDB@treatmentLabels)) {
      for(j in 1:dm[1]){
        toxTable[j,2 + 1:(nColTrt-1) + nColTrt*(i-1)] = round(100 * toxTable[j,2 + 1:(nColTrt-1) + nColTrt*(i-1)] / toxTable[j,2 + nColTrt*(i-1)], 0)
      }
    }
  }

  if( printMethod == "print") {
    print(toxTable, row.names = FALSE)
  } else if( printMethod == "table") {
    return(toxTable)
  } else if( printMethod == "latex") {

    align = c("l","|r|",rep(c(rep("c",nColTrt-1),"c|"),length(toxDB@treatmentLabels)))

    xtab = xtable(toxTable, digits = 0, align=align)
    addtorow <- list()
    addtorow$pos <- list(-1)
    addtorow$command <- paste0("\\hline\n",paste0('& \\multicolumn{',nColTrt , '}{c|}{', toxDB@treatmentLabels, '}', collapse=''), '\\\\\n')
    return(print(xtab, add.to.row=addtorow, include.rownames=FALSE, hline.after = c(0,nrow(xtab))))
  }

}


