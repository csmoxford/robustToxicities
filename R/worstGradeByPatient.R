
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
#' @return Returns a data.frame
#'
#' @export worstGradeByPatient


worstGradeByPatient = function(toxDB, colName, df = NULL){

  if(colName == ""){
    colName = "Missing"
  }
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

  return(df)
}


#' Generate a prettyTable of worst toxicity by category.
#'
#' This is a wrapper for the table_values function in prettyTables for worst grade by system.
#'
#' @param data A data.frame This should be the output from \code{\link{worstGradeByPatientSystem}}
#' @param categoryList A list of categories. Default is all. A subset is selected by changing this value.
#' @param type A vector of the method to summarise the var
#' @param strata A string of the column to used to stratify on
#' @param strata.names Otional. A named vector of alternative strata names
#' @param strata.count TRUE/FALSE for displaying strata counts at the top of each column
#' @param overall TRUE/FALSE for including an overall column
#' @param count "n","miss" or "none" providing the counts, missing values or omitting for each column for numeric variables
#' @param round A value or vector for the number of significant figures to report the data to
#'
#' categoryList can be replaced by other column names if the standard categorisation is not being used. For more flexibility you can view the code by typing table_tox_categories.
#'
#' Available methods and values for \strong{Type}:
#' \tabular{cc}{ "miqr" \tab median (Q25,Q75) \cr "miqrr" \tab median (Q25,Q75)[min,max] \cr "mrng" \tab median (Q0,Q100) \cr "avsd" \tab mean (sd) \cr "avci" \tab mean (confidence interval) \cr "st" \tab count \cr "str" \tab count/total \cr "stp" \tab count (percent) \cr "strp" \tab count/total (percent)
#' }
#' @return Returns a data.frame
#' @export table_tox_categories
#' @importFrom prettyTables table_values
table_tox_categories = function(data, categoryList = "all", strata = NULL, strata.names = NULL, strata.count = TRUE, overall = TRUE, count = "n", round = 3){

  if(categoryList == "all"){
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
      "Missing"
    )
  }

  var.order = list()
  for(i in categoryList){
    var.order[[i]] = c(0,1,2,3,4,5)
  }


  tble = table_values(
    data,
    var = categoryList,
    var.names = categoryList,
    var.order = var.order,
    strata = strata,
    strata.names = strata.names,
    strata.count = strata.count,
    type = rep("stp",28),
    round = 1,
    overall = overall,
    count = count
  )

  names(tble)[1:2] = c("Category", "Grade")
}
