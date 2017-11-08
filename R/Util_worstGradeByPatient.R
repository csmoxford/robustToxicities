
#' Generate worst grade by patient
#'
#' Returns a patient level data.frame containing the worst grade for each patient. The ass_TRUE is used as a filter. Optionally a subset of rt@toxData can be passed in as the second variable. In this case ass_TRUE is still used as a filter.
#'
#' @param rt an object of class robustToxicities
#' @param toxData A data.frame subset of rt@toxData if not all data should be used
#'
#' @details
#' worstGradeByPatientCategory does the same thing but wraps over all categories.
#'
#' @return Returns a data.frame
#'
#' @export worstGradeByPatient
worstGradeByPatient = function(rt, toxData = NULL) {

  if(!rt@wasQueried){
    message("Warning: QueryRobustToxicities has not been applied to this object")
  }

  if(is.null(toxData)) {
    toxData = rt@toxData
  }

  return(sapply(rt@patientData[,rt@patidCol], function(patid) max(-1,toxData[toxData[,rt@patidCol] == patid & toxData$ass_TRUE, rt@toxGradeCol], na.rm = TRUE)))

}

#' @export worstGradeByPatientCategory
#' @rdname worstGradeByPatient
worstGradeByPatientCategory = function(rt, categoryList = NULL){

  if(!rt@wasQueried){
    message("Warning: QueryRobustToxicities has not been applied to this object")
  }

  if(is.null(categoryList)){
    categories = unique(rt@toxData[rt@toxData$ass_TRUE,rt@toxCategoryCol])
  }

  for(i in categoryList){
    rt@patientData[[i]] = worstGradeByPatient(rt, rt@toxData[rt@toxData[,rt@toxCategoryCol] == i,])
  }

  return(rt)
}



