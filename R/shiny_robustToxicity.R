#' Robust Toxicities
#'
#' Runs the shiny app to create customisable tables and graphs from toxicity data.
#'
#'
#' @details
#' The toxicity file should have the following column heading names in a csv file.
#' (Version 0.7 onwards)
#'
#' \itemize{
#'  \item patid
#'  \item registration_date
#'  \item treatment (set to zero if single arm)
#'  \item cycle_start_* (these should be dates for the start of cycles)
#'  \item date_stopped_treatment
#'  \item ae_term
#'  \item ae_grade
#'  \item ae_cycle_occured
#'  \item ae_start_date
#'  \item ae_end_date
#'  \item ae_cont_end_study
#'  \item ass_category
#'  \item ass_toxicity_disp
#'  \item ass_TRUE
#'  }
#'
#' @import shiny
#' @import rtf
#' @export shiny_robustToxicities
shiny_robustToxicities=function(){
shiny::runApp(file.path(system.file("Shiny/robustToxicities",package = "robustToxicities")))
}



