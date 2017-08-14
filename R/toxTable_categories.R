#' Generate a prettyTable of worst toxicity by category.
#'
#' This is a wrapper for the table_values function in prettyTables for worst grade by category.
#'
#' @param rt RobustToxicitiesClass object
#' @param categoryList A list of categories. Default is all. A subset is selected by changing this value.
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
#' @export ToxTable_categories
#' @importFrom prettyTables table_values
ToxTable_categories = function(rt, categoryList = NULL, strata.count = TRUE, overall = TRUE, count = "n", round = 0){

  if(!rt@wasQueried){
    message("Warning: QueryRobustToxicities has not been applied to this object")
  }


  if(is.null(categoryList)) {
    categoryList = unique(rt@toxData[rt@toxData$ass_TRUE,rt@toxCategoryCol])
  }

  if(categoryList[1] == "all"){
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

  rt = worstGradeByPatientSystem(rt,categoryList)

  var.order = list()
  for(i in categoryList){
    var.order[[i]] = c(0,1,2,3,4,5)
  }


  treatmentLabels = rt@treatmentLabels
  names(treatmentLabels) = rt@treatmentCodes

  if(rt@options@toxTable_tabulationPercent){
    type = rep("stp",length(categoryList))
  } else {
    type = rep("st",length(categoryList))
  }

  tble = table_values(
    rt@patientData,
    var = categoryList,
    var.names = categoryList,
    var.order = var.order,
    strata = rt@treatmentCol,
    strata.names = treatmentLabels,
    strata.count = strata.count,
    type = type,
    round = round,
    overall = overall,
    count = count
  )

  names(tble)[1:2] = c("Category", "Grade")

  return(tble)
}
