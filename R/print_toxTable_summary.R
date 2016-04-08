
#' Format the toxTable_summary to an output medium

#' @inheritParams prepareToxicity
#' @param printMethod One of "rft" or "latex"

#' @export print_toxTable_summary

print_toxTable_summary = function(toxDB, printMethod) {

  toxTable = toxTable_summary(toxDB)

  treatment = as.integer(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][2]))
  grade     = suppressWarnings(sapply(colnames(toxTable), function(x) strsplit(x,"[.]")[[1]][3]))

  for (i in 1:length(grade)) {
    if (!is.na(grade[i])) {
      if (grade[i] == "total") {
        grade[i] = "Total patients"
      } else if (str_length(grade[i]) == 1) {
        if (toxDB@options@cumulativeGrades) {
          if(grade[i] != "5"){
            grade[i] = paste0("Grade ", grade[i],"+")
          } else {
            grade[i] = paste0("Grade ", grade[i])
          }
        } else {
          grade[i] = paste("Grade", grade[i])
        }
      } else {
        if (str_length(grade[i]) == 2) {
        grade[i] = paste("Grades", paste0(strsplit(grade[i],"")[[1]], collapse = " and "))
        } else {
          num = as.numeric(strsplit(grade[i],"")[[1]])
        grade[i] = paste("Grades", min(num),"-",max(num))
        }
      }
    } else {
      grade[i] = "Time period"
    }
  }

  if (length(toxDB@treatmentLabels) == 1) {
    colnames(toxTable) = grade
  } else {
    # not yet done
  }

  return(toxTable)
}


