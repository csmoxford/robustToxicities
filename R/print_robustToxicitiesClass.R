


.print_toxicityOptions = function(x, ...) {

  message("Class toxicityOptions slots:")
  for(slt in slotNames(x)) {

    dta = slot(x, slt)
    if(length(dta) == 1) {
      prnt = dta
    } else {
      prnt = paste0("[",length(dta),"] ", dta)
    }

    message(slt, " = ", prnt)
  }

}

.show_toxicityOptions = function(object) {
  print(object)
}

setMethod(f = "show", "toxicityOptions", definition = .show_toxicityOptions)
setMethod(f = "print", signature = c(x = "toxicityOptions"), definition = .print_toxicityOptions)


.print_robustToxicitiesClass = function(x, ...) {

  patients = unique(x@patientData[,x@patidCol])
  patientsTox = unique(x@toxData[,x@patidCol])
  toxicities = dim(x@toxData)[1]

  message("Class robustToxicitiesClass\n")

  message("There are ", length(patients), " in the patientData dataset")
  message("There are ", sum(patientsTox %in% patients), " with toxicities in the toxData dataset")
  message("There are ", toxicities, " rows in the toxData dataset")

  message("\nQueries")
  if(!x@wasQueried) {
    message("This dataset still needs to be sent through QueryRobustToxicities")
  } else {
    message("There are ",sum(x@queries$problem_type == "Note")," notes in the query data.frame")
    message("There are ",sum(x@queries$problem_type == "Missing data")," missing data in the query data.frame")
  }

  message("\nOther Slots")

  for(slt in c("patidCol","treatmentCol","toxCategoryCol","toxNameCol","toxGradeCol","dateOfStartOfToxWindow","dateOfStartTox","dateOfEndTox","dateOfEndOfToxWindow")) {
    dta = slot(x,slt)
    message(slt, " = ", toString(dta))
  }

  if(!is.null(x@periodDividerCols)){
    width1 = max(nchar(c(x@periodDividerCols,"Columns")))
    width2 = max(nchar(c(x@periodDividerLabels, "Labels"))) + 2
    message("\nperiodDivider")
    message(sprintf(paste0("%",width1,"s"), "Columns"),sprintf(paste0("%",width2,"s"), "Labels"))
    for(i in 1:length(x@periodDividerCols)) {
      message(sprintf(paste0("%",width1,"s"), x@periodDividerCols[i]), sprintf(paste0("%",width2,"s"), x@periodDividerLabels[i]))
    }
  }

  if(!is.null(x@treatmentCodes)){
    width1 = max(nchar(c(x@treatmentCodes,"Treatment Codes")))
    width2 = max(nchar(c(x@treatmentLabels, "Labels"))) + 2
    message("\nTreatment coding")
    message(sprintf(paste0("%",width1,"s"), "Treatment Codes"),sprintf(paste0("%",width2,"s"), "Labels"))
    for(i in 1:length(x@treatmentCodes)) {
      message(sprintf(paste0("%",width1,"s"), x@treatmentCodes[i]), sprintf(paste0("%",width2,"s"), x@treatmentLabels[i]))
    }
  }

  message()
  print(x@options)

}

.show_robustToxicitiesClass = function(object) {
  print(object)
}

setMethod(f = "show", "robustToxicitiesClass", definition = .show_robustToxicitiesClass)
setMethod(f = "print", signature = c(x = "robustToxicitiesClass"), definition = .print_robustToxicitiesClass)
