
#' Renaming function to generate the correct names for the robustToxicities class
#'
#' This function provides an efficient way of renaming the column names in the toxicity database to be compliant with the function \code{\link{robustToxicities}}.
#'
#'
#' @param data The toxicity database
#' @param patid Patient ID
#' @param treatment Treatment arms
#' @param ae_term The adverse event name / term
#' @param ae_system The adverse event system
#' @param ae_grade The adverse event grade
#' @param ae_start_date The adverse event start date (time based only)
#' @param ae_end_date The adverse event end date (time based only)
#' @param ae_cont_end_study Was the adverse event coninuing at the end of the study (time based only)
#' @param dateColumnNames The start date of each cycle / time period (time based only)
#' @param date_stopped_treatment Date the patients stopped treatment (time based only)
#' @param patientInCycle Was a patient enrolled for this cycle / time period (cycle based, can be auto genrated for time based data)
#' @param occurInCycle Was the toxicity present for this cycle / time period (cycle based, can be auto genrated for time based data)
#' @param ass_TRUE An indicator to determine if the toxicity should be counted in the analysis

#' @export nameDatabase

nameDatabase = function (data, patid = NULL, treatment = NULL, ae_term = NULL, ae_system = NULL, ae_grade = NULL, ae_start_date = NULL, ae_end_date = NULL, ae_cont_end_study = NULL, date_stopped_treatment = NULL, dateColumnNames = NULL, patientInCycle = NULL, occurInCycle = NULL, ass_TRUE = NULL) {


  if(!is.null(patid)){
    id=which(colnames(data) == patid)
    colnames(data)[id] = "patid"
  }

  if(!is.null(treatment)){
    id=which(colnames(data) == treatment)
    colnames(data)[id] = "treatment"
  }

  if(!is.null(ae_term)){
    id=which(colnames(data) == ae_term)
    colnames(data)[id] = "ae_term"
  }

  if(!is.null(ae_system)){
    id=which(colnames(data) == ae_term)
    colnames(data)[id] = "ae_system"
  }

  if(!is.null(ae_grade)){
    id=which(colnames(data) == ae_grade)
    colnames(data)[id] = "ae_grade"
  }

  if(!is.null(ae_start_date)){
    id=which(colnames(data) == ae_start_date)
    colnames(data)[id] = "ae_start_date"
  }

  if(!is.null(ae_end_date)){
    id=which(colnames(data) == ae_end_date)
    colnames(data)[id] = "ae_end_date"
  }

  if(!is.null(dateColumnNames)) {
    for (i in 1:length(dateColumnNames)) {
      id=which(colnames(data) == dateColumnNames[i])
      colnames(data)[id] = paste0("cycle_start_date_",i)
    }
  }

  if(!is.null(patientInCycle)) {
    for (i in 1:length(patientInCycle)) {
      id=which(colnames(data) == patientInCycle[i])
      colnames(data)[id] = paste0("present_in_cycle_",i)
    }
  }

  if(!is.null(occurInCycle)) {
    for (i in 1:length(occurInCycle)) {
      id=which(colnames(data) == occurInCycle[i])
      colnames(data)[id] = paste0("occur_in_cycle_",i)
    }
  }

  if(!is.null(date_stopped_treatment)){
    id=which(colnames(data) == date_stopped_treatment)
    colnames(data)[id] = "date_stopped_treatment"
  }

  if(!is.null(ass_TRUE)){
    id=which(colnames(data) == ass_TRUE)
    colnames(data)[id] = "ass_TRUE"
  }



  return(data)
}

#names(data)

#dta=nameDatabase(data, ae_grade = "ae_ctcae_grade", dateColumnNames = c("registration_date", "cycle_start_date1", "cycle_start_date2", "cycle_start_date3"))

#names(dta)
