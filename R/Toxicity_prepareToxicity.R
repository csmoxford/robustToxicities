#' Prepare toxicities
#'
#' Performs a number of data cleaning actions to ensure the data is appropriate for tabulation and plotting
#'
#' @param toxDB Main toxicity database.

#' @export prepareToxicity

prepareToxicity=function(toxDB){

  ################################################################################
  # checks and balances
  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }
  notes = toxDB@options@displayNotes

  if (dim(toxDB@queries)[1]>0) {
    warning("Queries database was non empty. Appending to existing queries database")
  }

  ################################################################################
  ################################################################################
  # internal functions

  # append the query to the query data.frame and spit out a message.
  query=function(toxDB,msg,problem_type,notes,aff=FALSE){
    if(!(problem_type == "Note" & !notes)){
      message(msg)
    }
    if(!aff){
    toxDB@queries[dim(toxDB@queries)[1]+1,]=c(toxDB@cleanData$patid[i], toxDB@cleanData$ae[i], toxDB@cleanData$ae_cycle_occured[i], problem_type, msg)
    } else {
      toxDB@queries[dim(toxDB@queries)[1]+1,]=c("", "", "", problem_type, msg)
    }
    return(toxDB@queries)
  }
  ################################################################################
  # set the data to cleanData keeping copy of the original!!
  # get the dimention for reference
  toxDB@cleanData = toxDB@data
  dm = dim(toxDB@cleanData)

  ################################################################################
  msg = paste("Number of patients:", length(unique(toxDB@cleanData$patid)), "in the provided database")
  toxDB@queries = query(toxDB, msg, "Affirmation",notes ,TRUE)

  noToxicities = 0
  for (i in 1:dm[1]) {
    if (!toxDB@cleanData$ass_TRUE[i]) {
      # toxicity free patient counter
      if (sum(toxDB@cleanData$patid == toxDB@cleanData$patid[i]) == 1 & (toxDB@cleanData$ae_term[i] == "" | is.na(toxDB@cleanData$ae_term[i]))) {
        noToxicities = noToxicities + 1
        toxDB@cleanData$ae_ctcae_grade[i] = 0
        toxDB@cleanData$ae_cycle_occured[i] = 0
        toxDB@cleanData$ae_term[i] = ""
        toxDB@cleanData$ae_start_date[i] = 0
      }
    }
  }
  # number of patients without toxicities
  if (noToxicities > 0) {
    msg = paste("There were", noToxicities, "patients with no eligible toxicities")
    toxDB@queries = query(toxDB, msg, "Affirmation",notes , TRUE)
  }

  ############################################################################################
  # patid complete?
  for (i in 1:dm[1]) {
    if (toxDB@cleanData$patid[i] == "") {
      msg = paste("Missing patid on row", i)
      toxDB@queries = query(toxDB, msg, "Missing data",notes)
    }
  }

  ############################################################################################
  # set ctcae grade to zero if missing
  for (i in 1:dm[1]) {
    if (toxDB@cleanData$ass_TRUE[i]) {
      if (is.na(toxDB@cleanData$ae_ctcae_grade[i])) {
        msg = paste("Patient", toxDB@cleanData$patid[i], "is missing toxicity grade for",toxDB@cleanData$ae_term , "line", i, "(currently set to zero)")
        toxDB@queries = query(toxDB, msg, "Missing data",notes)
      }
    }
  }

  ############################################################################################
  # missing ae_term
  for (i in 1:dm[1]) {
    if (toxDB@cleanData$ass_TRUE[i]) {
      if (is.na(toxDB@cleanData$ae_term[i])) {
        msg = paste("Patient", toxDB@cleanData$patid[i], "is missing ae_term (toxicity term), line", i)
        toxDB@queries = query(toxDB, msg, "Missing data",notes)
      }
    }
  }

  ############################################################################################
  # missing ae_system
  for (i in 1:dm[1]) {
    if (toxDB@cleanData$ass_TRUE[i]) {
      if (is.na(toxDB@cleanData$ae_system[i])) {
        msg = paste("Patient", toxDB@cleanData$patid[i], "is missing ae_system (ctcae category), line", i)
        toxDB@queries = query(toxDB, msg, "Missing data",notes)
      }
    }
  }

  # time based stuff now
  if (toxDB@options@timeType == "time") {

    ############################################################################################
    # if missing ae_cycle_occured, generate this
    if (is.null(toxDB@cleanData$ae_cycle_occured)) {
      message("ae_cycle_occured not provided, creating column and will populate it")
      toxDB@cleanData$ae_cycle_occured = NA
    }
    # if missing ae_cont_end_study, for time data generate this
    if (is.null(toxDB@cleanData$ae_cont_end_study)) {
      toxDB@cleanData$ae_cont_end_study = NA
      message("ae_cont_end_study not provided, creating column and will populate it")
    }

    ############################################################################################
    # format dates
    for (col in grep("date",names(toxDB@cleanData))) {
      test = as.numeric(as.Date(toxDB@cleanData[, col], format="%Y-%m-%d", origin="1970-01-01"))
      if (all(is.na(test))) {
        test = as.numeric(as.Date(toxDB@cleanData[, col], format="%d%b%Y", origin="1970-01-01"))
      }
      if(!all(is.na(test))){
        toxDB@cleanData[,col] = test
      }
    }

    ############################################################################################
    # maximum number of cycles of any patient
    no_cycles = sum(str_detect(names(toxDB@cleanData), "cycle_start_date"))
    ############################################################################################
    # location of the dates for those cycles
    names_cycle = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "cycle_start_date")]
    names_cycle_stub = sub("cycle_start_date", "", names_cycle)

    ############################################################################################
    # Missing registration date
    for(i in 1:dm[1]){
      if(toxDB@cleanData$ass_TRUE[i]){
        if(is.na(toxDB@cleanData$registration_date[i])){
          msg = paste("Patient ",toxDB@cleanData$patid[i], "is missing the registration date for:", toxDB@cleanData$ae_term[i], "line", i)
          toxDB@queries = query(toxDB, msg, "Missing data",notes)
        }
      }
    }
    ############################################################################################
    # If baseline toxicity without start date assign registration date -7. Tell user.
    for(i in 1:dm[1]){
      if(toxDB@cleanData$ass_TRUE[i]){
        if(is.na(toxDB@cleanData$ae_start_date[i])){
          msg = paste("Patient", toxDB@cleanData$patid[i], "is missing the date of start of toxicity for:", toxDB@cleanData$ae_term[i], "line", i, "(setting to 7 days prior to registration)")
          toxDB@queries = query(toxDB, msg, "Missing data",notes)
          toxDB@cleanData$ae_start_date[i]=toxDB@cleanData$registration_date[i]-7
        }
      }
    }

    ############################################################################################
    # Check date ordering and missing internal dates
    dates = grep("cycle_start_date",names(toxDB@cleanData))
    for (j in 2:length(dates)) {
      for (i in 1:dm[1]) {
        d1 = toxDB@cleanData[i,dates[j - 1]]
        d2 = toxDB@cleanData[i,dates[j]]
        if(!is.na(d2)) {
          # second date exists
          if(!is.na(d1)) {
            # first date exists
            if( d1 > d2) {
              # first date before second date
              msg = paste0("Patient ", toxDB@cleanData$patid[i], " date for cycle ", dates[j - 1], "(", d1, ") is before date for ", dates[j], "(", d2, ")")
              toxDB@queries = query(toxDB, msg, "Wrong data", notes)
            }
          } else {
            # first date missing by second date available
            msg = paste0("Patient ", toxDB@cleanData$patid[i], " date for ", dates[j - 1], " is missing but the future date for",  dates[j], "(", d2, ") is not")
            toxDB@queries = query(toxDB, msg, "Missing data", notes)
          }
        }
      }
    }


    ############################################################################################
    # Missing end of treatment date
    noEndTreatment=0
    noEndTreatmentPatid=c()
    for(i in 1:dm[1]){
      if(toxDB@cleanData$ass_TRUE[i]){
        if(is.na(toxDB@cleanData$date_stopped_treatment[i])){
          if(!toxDB@cleanData$patid[i] %in% noEndTreatmentPatid){
            noEndTreatmentPatid = c(noEndTreatmentPatid, toxDB@cleanData$patid[i])
            noEndTreatment = noEndTreatment + 1
          }
        }
      }
    }
    if(noEndTreatment > 0){
      msg = paste("Patients missing date of end of treatment (includes those still on study):", noEndTreatment)
      toxDB@queries = query(toxDB, msg, "Affirmation",notes, TRUE)
    }

    ############################################################################################
    # if missing end date and continuing at end of study assign end of treatment date + 30
    for(i in 1:dm[1]){
      if(toxDB@cleanData$ass_TRUE[i]){
        if(toxDB@cleanData$ae_cont_end_study[i]=="yes" & is.na(toxDB@cleanData$ae_end_date[i])){
          if(!is.na(toxDB@cleanData$date_stopped_treatment[i])){
            toxDB@cleanData$ae_end_date[i]=toxDB@cleanData$date_stopped_treatment[i] + 30
            msg = paste("Note: Patient:", toxDB@cleanData$patid[i], "toxicity:", toxDB@cleanData$ae_term[i], "line:", i, "is continueing at end of study, setting the ae_end_date to 30 days after date_stopped_treatment")
            toxDB@queries = query(toxDB, msg, "Note",notes)
          } else {
            msg = paste("Patient:", toxDB@cleanData$patid[i], "toxicity:", toxDB@cleanData$ae_term[i], "line:", i, "is continueing at end of study but the date_stopped_treatment is missing (setting ae_end_date to a large value)")
            toxDB@queries = query(toxDB, msg, "Missing data",notes)
            toxDB@cleanData$ae_end_date[i]=30000
          }
        } else if(is.na(toxDB@cleanData$ae_end_date[i])){
          msg = paste("Patient:", toxDB@cleanData$patid[i], "toxicity:", toxDB@cleanData$ae_term[i], "line:", i, "has no end date for toxicity (setting ae_end_date to a large value)")
          toxDB@queries = query(toxDB, msg, "Missing data",notes)
          toxDB@cleanData$ae_end_date[i] = 30000
        }
      }
    }

    ############################################################################################
    # Missing ae_end_date
    for(i in 1:dm[1]){
      if(toxDB@cleanData$ass_TRUE[i]){
        if(toxDB@cleanData$ae_cont_end_study[i]=="no" & is.na(toxDB@cleanData$ae_end_date[i])){
          msg = paste("Patient:", toxDB@cleanData$patid[i], "toxicity:", toxDB@cleanData$ae_term[i], "line:", i, "is missing the date_stopped_treatment (setting ae_end_date to a large value)")
          toxDB@queries = query(toxDB, msg, "Missing data",notes)
          toxDB@cleanData$ae_end_date[i]=30000
        }
      }
    }

    # mark if ae present in cycle
    dates = c(names_cycle, "date_stopped_treatment")
    for (j in 1:length(names_cycle)) {
      c_sd=dates[j]
      c_ed=dates[j+1]
      occur=paste0("occur_in_cycle_",names_cycle_stub[j])
      toxDB@cleanData[,occur]=0
      for (i in 1:dm[1]) {
        if (toxDB@cleanData$ass_TRUE[i]) {
          if (!is.na(toxDB@cleanData[i,c_sd]) & !is.na(toxDB@cleanData[i,c_ed])) {
            if (toxDB@cleanData[i,c_sd] <= toxDB@cleanData$ae_start_date[i] & toxDB@cleanData$ae_start_date[i] < toxDB@cleanData[i,c_ed] |
               toxDB@cleanData[i,c_sd]<=toxDB@cleanData$ae_end_date[i]   & toxDB@cleanData$ae_end_date[i]<toxDB@cleanData[i,c_ed]   |
               toxDB@cleanData$ae_start_date[i]<=toxDB@cleanData[i,c_sd] & toxDB@cleanData[i,c_sd]<=toxDB@cleanData$ae_end_date[i]   |
               toxDB@cleanData$ae_start_date[i]<toxDB@cleanData[i,c_ed] & toxDB@cleanData[i,c_ed]<toxDB@cleanData$ae_start_date[i] ){
               toxDB@cleanData[i,occur]=toxDB@cleanData$ae_ctcae_grade[i]
            }
          } else if(!is.na(toxDB@cleanData[i,c_sd]) & is.na(toxDB@cleanData[i,c_ed]) & !is.na(toxDB@cleanData[i,"date_stopped_treatment"])){
            if(toxDB@cleanData[i,c_sd] <= toxDB@cleanData$ae_start_date[i] & toxDB@cleanData$ae_start_date[i] < toxDB@cleanData[i,"date_stopped_treatment"] |
               toxDB@cleanData[i,c_sd] <= toxDB@cleanData$ae_end_date[i]   & toxDB@cleanData$ae_end_date[i] < toxDB@cleanData[i,"date_stopped_treatment"]   |
               toxDB@cleanData$ae_start_date[i] <= toxDB@cleanData[i, c_sd] & toxDB@cleanData[i, c_sd] <= toxDB@cleanData$ae_end_date[i]   |
               toxDB@cleanData$ae_start_date[i] < toxDB@cleanData[i, "date_stopped_treatment"] & toxDB@cleanData[i, "date_stopped_treatment"] < toxDB@cleanData$ae_start_date[i] ){
              toxDB@cleanData[i, occur] = toxDB@cleanData$ae_ctcae_grade[i]
            }
          } else if(!is.na(toxDB@cleanData[i,c_sd]) & is.na(toxDB@cleanData[i,c_ed]) & is.na(toxDB@cleanData[i,"date_stopped_treatment"])){
            #if the cycle start date is the last recorded date it must be in this cycle
            toxDB@cleanData[i,occur] = toxDB@cleanData$ae_ctcae_grade[i]
          }
        }
      }
    }

    toxDB@cleanData$occur_in_cycle_0  = toxDB@cleanData$ae_ctcae_grade * (toxDB@cleanData$ae_cycle_occured == 0)
    toxDB@cleanData$cycle_start_date0 = toxDB@cleanData$registration_date

  }

  ################################################################################
  # Summarise the preparation

  message("\n#############################################################")
  message("# Summary of preparation")
  message("Number of patients: ", length(unique(toxDB@cleanData$patid)))
  message("Number of patients with no toxicities: ", noToxicities)
  message("Patients missing date of end of treatment: ", noEndTreatment)
  message("Number of notes: ", sum(toxDB@queries$problem_type == "Note"))
  message("Number of missing data problems: ", sum(toxDB@queries$problem_type == "Missing data"))

  ################################################################################
  # number Toxicities
  toxDB@cleanData = toxDB@cleanData[order(toxDB@cleanData$ass_category, toxDB@cleanData$ass_toxicity_disp), ]

  toxDB@cleanData$ass_toxID    = 0
  toxDB@cleanData$ass_toxID[1] = 1
  j=1
  for (i in 2:length( toxDB@cleanData$ass_toxID)) {
    if (toxDB@cleanData$ass_toxicity_disp[i] != toxDB@cleanData$ass_toxicity_disp[i-1]) {
      j = j + 1
    }
    toxDB@cleanData$ass_toxID[i] = j
  }
  ################################################################################

  return(toxDB)
}



