
#' The robustToxicities class
#'
#' This is core object of this package. This object stores the original dataset as well as the automatically cleaned dataset and a list of notes and queries generated when cleaning the dataset. A list of options is also provided to store plot and tabulation options and provide additional metadata. Finally treatment and cycle labels are also required.
#'
#' @slot data The original dataset
#' @slot cleanData The cleaned dataset
#' @slot treatmentLabels A vector of treatment labels
#' @slot cycleLabels A vector of cycle / time perior labels
#' @slot options An s4 object of class \code{\link{toxicityOptions}} containing options and metadata for the files.


#' @exportClass robustToxicities
.robustToxicities = setClass("robustToxicities",slots = c(data = "data.frame", cleanData = "data.frame", treatmentLabels = "character", cycleLabels = "character", queries = "data.frame", options = "toxicityOptions"))






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
  if(length(treatmentLabels) < max(data$treatment)) {
    message("data$treatment must be integer valued and correspond to the labels in treatmentLabels")
    stp = 1
  }

  ################################################################################
  # time data only checks
  if(options@timeType  == "time") {
    requiredData = c("ae_start_date", "ae_end_date", "ae_cont_end_study", "date_stopped_treatment", paste0("cycle_start_date_",1:length(cycleLabels)))
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
    requiredData = paste0("present_in_cycle_", length(cycleLabels))
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


  # set options based on data:
  if (options@sumCycleMerge == "") {
    options@sumCycleMerge = paste0(1:length(cycleLabels), collapse = "|")
  }
  if (options@cycleCycleMerge == "") {
    options@cycleCycleMerge = paste0(1:length(cycleLabels), collapse = "|")
  }



  return(.robustToxicities(data = data, queries = queries, treatmentLabels = treatmentLabels, cycleLabels = cycleLabels, options = options))
}


