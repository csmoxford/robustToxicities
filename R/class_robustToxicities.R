#' @exportClass toxicityOptions

.toxicityOptions = setClass("toxicityOptions", slots = c(
  trialName = "character",
  folderPath = "character",
  fileName = "character",
  timeType = "character",
  displayNotes = "logical",
  tabulationMethod = "character",
  cumulativeGrades = "logical",
  discardBaseline = "logical",
  plotxMin = "numeric",
  plotxMax = "numeric",
  plotCycleLength = "numeric",
  plotPxHeight = "numeric",
  plotPxWidth = "numeric",
  sumCycleMerge = "character",
  sumColumnMerge = "character",
  cycleCycleMerge = "character",
  cycleColumnMerge = "character",
  cycleCategoryMerge = "character", # what is this supposed to be?
  outputFolder = "character"
))

#' @export defaultToxicityOptions

defaultToxicityOptions = function(trialName, folderPath = NULL, fileName = "", timeType="time", outputFolder = NULL) {

  if(!timeType %in% c("time", "cycle")){
    stop("timeType must be one of time or cycle")
  }


  .toxicityOptions(
    trialName = trialName,
    folderPath = folderPath,
    fileName = fileName,
    timeType = timeType,
    displayNotes = TRUE,
    tabulationMethod = "worst",
    cumulativeGrades = FALSE,
    discardBaseline = FALSE,
    plotxMin = -7,
    plotxMax = 60,
    plotCycleLength = 21,
    plotPxHeight = 0,
    plotPxWidth = 1100,
    sumCycleMerge = "0|1|2|3",
    sumColumnMerge = "total|1|2|3|4,5",
    cycleColumnMerge = "1|2|3|4,5",
    cycleCycleMerge = "0|1|2|3",
    cycleCategoryMerge = "", # collapse CTCAE categories in table
    outputFolder = outputFolder
  )
}


#' @exportClass robustToxicities
.robustToxicities = setClass("robustToxicities",slots = c(data = "data.frame", cleanData = "data.frame", treatmentLabels = "character", cycleLabels = "data.frame", queries = "data.frame", options = "toxicityOptions"))

#' @export robustToxicities
robustToxicities = function(data, cycleLabels, options, treatmentLabels = NULL) {

  if(class(data) != "data.frame") {
    stop("data must be of class data.frame")
  }

  ################################################################################
  # Check fields are provided (all need these fields)
  requiredData = c("patid", "ae_term", "ae_system", "ae_ctcae_grade")
  name = names(data)
  # generic names
  stp = 0
  for (colName in requiredData) {
    if (!colName %in% name) {
      message("Column with name ",colName, " was not found in the data and is required.")
      stp = 1
    }
  }

  ################################################################################
  # warn about missing ass_TRUE
  if(!"ass_TRUE" %in% name){
    data$ass_TRUE = TRUE
    message("ass_TRUE not in database assume true for all entries (note this is used for patients with no toxicities and subsetting data)")
  }

  ################################################################################
  # treaments
  if(length(treatmentLabels) > 1 & !"treatment" %in% name){
    stop("treatment column not found in database")
  } else if(length(treatmentLabels) == 1 & !"treatment" %in% name){
    message("Only one treatment found, creating treatment column")
    data$treatment = 1
  }

  if(is.null(treatmentLabels)) {
    message("No treatment labels provided generating from the data")
    if(sum(is.na(data$treatment))){
      stop("There must be no missing treatment allocations")
    }
    treatmentLabels = levels(as.factor(data$treatment))
    data$treatment = sapply(data$treatment, function(x) which(x == treatmentLabels))

  }
  data$treatment = as.integer(data$treatment)
  if(!class(data$treatment) == "integer"){
    message("data$treatment must be of class integer")
    stp = 1
  }
  if(length(unique(data$treatment)) < max(data$treatment)) {
    message("data$treatment must be integer values and not have gaps")
    stp = 1
  }

  ################################################################################
  # time data only checks
  if(options@timeType  == "time") {
    requiredData = c("ae_start_date", "ae_end_date", "ae_cont_end_study", "date_stopped_treatment", paste0("cycle_start_date_",cycleLabels$index))
    # time data names
    for (colName in requiredData) {
      if (!colName %in% name) {
        message("Column with name",colName, "was not found in the data and is required.")
        stp = 1
      }
    }



  } else if(options@timeType  == "cycle") {
    ################################################################################
    # require either ae_cycle_occured or occur_in_cycle_
    if (!"ae_cycle_occured" %in% name & sum(grepl("occur_in_cycle_", name)) == 0) {
      message("Column with name ", colName, " was not found in the data and is required.")
      stp = 1
    }

    ################################################################################
    # Must provide present in cycle
    cycleLabels$index
    requiredData = paste0("present_in_cycle_", cycleLabels$index)
    # time data names
    for (colName in requiredData) {
      if (!colName %in% name) {
        message("Column with name",colName, "was not found in the data and is required.")
        stp = 1
      }
    }

  } else {
    message("Option timeType must be one of time, and cycle was: ", options@timeType)
  }

  if(stp){
    message("Something is broken")
  }

  ################################################################################
  # Create the empty query database
  queryNames = c("patid", "ae", "ae_cycle_occured", "problem_type", "message")
  queries = data.frame(matrix("",nrow = 0,ncol = length(queryNames)),stringsAsFactors = FALSE)
  names(queries) = queryNames

  return(.robustToxicities(data = data, queries = queries, cycleLabels = cycleLabels, options = options))
}


