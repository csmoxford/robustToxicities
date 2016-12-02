
#' Generate worst grade by patient
#'
#' Returns a patient level data.frame containing the worst grade for each patient. The ass_TRUE is used as a filter.
#'
#' @param toxDB an object of class robustToxicities
#' @param colName The name of the column created
#' @param df (optional) an existing data.frame to add the column to. If noy provided this is created from the data.
#'
#' @details
#' worstGradeByPatientSystem does the same thing but wraps over all categories.
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

#' @export worstGradeByPatientSystem
#' @rdname worstGradeByPatient
worstGradeByPatientSystem = function(toxDB, df = NULL){

  categoryList = c(
    "Blood and lymphatic system disorders",
    "Cardiac disorders",
    "Congenital, familial and genetic disorders",
    "Ear and labyrinth disorders",
    "Endocrine disorders",
    "Eye disorders",
    "Gastrointestinal disorders",
    "General disorders and administration site conditions",
    "Hepatobiliary disorders",
    "Immune system disorders",
    "Infections and infestations",
    "Injury, poisoning and procedural complications",
    "Investigations",
    "Metabolism and nutrition disorders",
    "Musculoskeletal and connective tissue disorders",
    "Neoplasms benign, malignant and unspecified (incl cysts and polyps)",
    "Nervous system disorders",
    "Pregnancy, puerperium and perinatal conditions",
    "Psychiatric disorders",
    "Renal and urinary disorders",
    "Reproductive system and breast disorders",
    "Respiratory, thoracic and mediastinal disorders",
    "Skin and subcutaneous tissue disorders",
    "Social circumstances",
    "Surgical and medical procedures",
    "Vascular disorders",
    "Other",
    "")

  for(i in categoryList){
    toxDB@cleanData$ass_TRUE = toxDB@cleanData$ae_system == i
    df=worstGradeByPatient(toxDB, i, df)
  }
}
