
#' Generate worst grade by patient
#'
#' Returns a patient level data.frame containing the worst grade for each patient. The ass_TRUE is used as a filter.
#'
#' @param toxDB an object of class robustToxicities
#' @param colName The name of the column created
#' @param df (optional) an existing data.frame to add the column to. If noy provided this is created from the data.
#'
#' @export worstGradeByPatient

worstGradeByPatient = function(toxDB, colName, df = NULL){

  if(is.null(df)){
    df = data.frame(patid = unique(toxDB@cleanData$patid))
  }
  print(table(toxDB@cleanData$ass_TRUE, useNA = "always"))

  df[,colName] = sapply(df$patid, function(patid) max(0,toxDB@cleanData$ae_grade[toxDB@cleanData$patid == patid & toxDB@cleanData$ass_TRUE], na.rm = TRUE))

  return(df)

}
