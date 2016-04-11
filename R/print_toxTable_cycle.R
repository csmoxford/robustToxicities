
#' Format the toxTable_summary to an output medium

#' @inheritParams toxTable_cycle
#' @param printMethod One of "print" "rtf" or "latex"
#' @param rtfDoc the name of the rtf document to output to

#' @export print_toxTable_cycle

print_toxTable_cycle = function(toxDB, cycles, printMethod = "print", rtfDoc = NULL) {

  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }

  if(!printMethod %in% c("print", "rtf", "latex")) {
    stop("Print method not defined for method: ",printMethod)
  }

  toxTable = toxTable_cycle(toxDB, cycles)

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



  colnames(toxTable) = grade

  if( printMethod == "print") {
    return(toxTable)
  } else if( printMethod == "latex") {

    nColTrt = (dim(toxTable)[2]-2)/length(toxDB@treatmentLabels)
    align = c("l","|p{5cm}",">{\\raggedleft\\arraybackslash}p{5cm}|",rep(c(rep("c",nColTrt-1),"c|"),length(toxDB@treatmentLabels)))

    # define appearance of \multirow
    fullRow = which(toxTable[[1]] != "")
    row.lengths = c(fullRow[2:length(fullRow)],nrow(toxTable)+1)-fullRow[1:(length(fullRow))]
    for(i in 1:length(fullRow)){
      if(row.lengths[i]>1){
        toxTable[[1]][fullRow[i]] = paste0("\\multirow{", row.lengths[i], "}{*}{\\parbox{5cm}{", toxTable[[1]][fullRow[i]], "}}")
      }
    }

    # add multicolumn for treatments and return
    if(length(toxDB@treatmentLabels) > 1){
    xtab = xtable(toxTable, digits = 0, align=align)
    addtorow <- list()
    addtorow$pos <- list(-1)
    addtorow$command <- paste0("\\hline\n &",paste0('& \\multicolumn{',nColTrt , '}{c|}{', toxDB@treatmentLabels, '}', collapse=''), '\\\\\n')
    return(print(xtab, add.to.row=addtorow, include.rownames=FALSE, hline.after = c(0,fullRow-1,nrow(xtab)), sanitize.text.function = force))
    } else {
      xtab = xtable(toxTable, digits = 0, align=align)
      return(print(xtab,include.rownames=FALSE, hline.after = c(-1,0,fullRow-1,nrow(xtab)), sanitize.text.function = force))
    }

  } else if( printMethod == "rtf") {

  }

}
