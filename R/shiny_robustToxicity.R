
## Robust toxicities run file
# Can be run with or without a log file


#' Robust Toxicities
#'
#' Runs the shiny app to create customisable tables and graphs from toxicity data.
#'
#' @param log.file The path and name to the log file to be created
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
#'  \item ae_ctcae_grade
#'  \item ae_cycle_occured
#'  \item ae_start_date
#'  \item ae_end_date
#'  \item ae_cont_end_study
#'  \item ass_category
#'  \item ass_toxicity_disp
#'  \item ass_TRUE
#'  }
#'
#' @export shiny.robustToxicities
shiny.robustToxicities=function(){
shiny::runApp(file.path(system.file("Shiny/robust.toxicities",package = "robust.toxicities")))
}

#' @export shiny.robustToxicities.log
shiny.robustToxicities.log=function(log.file){
  library(TeachingDemos)
  txtStart(paste0(log.file,".log"))
  shiny::runApp(file.path(system.file("Shiny/robust.toxicities",package = "robust.toxicities")))
  txtStop()
}




