
#' Query RobustToxicities
#'
#' A function which checks the provided data
#'
#' @param rt The robustToxicitiesClass object
#'
#' @return An S4 object of class robustToxicitiesClass. The queries are stored as a data.frame in the slot queries (rt@queires)
#'
#'
#' @export QueryRobustToxicities
QueryRobustToxicities = function(rt) {

  if(rt@wasQueried){
    stop("This function was already applied to this object")
  }

  # Check that the rt object is valid.
  validObject(rt)

  # for readability take the datasets and options out of their slots.
  toxData = rt@toxData
  patientData = rt@patientData
  dm = dim(toxData)
  notes = rt@options@displayNotes
  queries = rt@queries
  ################################################################################
  ################################################################################
  ### internal function
  # append the query to the query data.frame and spit out a message.
  query = function(clearnData, i, msg, problem_type, notes, aff = FALSE){
    if(!(problem_type == "Note" & !notes)) {
      message(msg)
    }
    if(!aff) {
      queries[dim(queries)[1]+1, ] = c(toxData[i, rt@patidCol], toxData[i, rt@toxNameCol], problem_type, msg)
    } else {
      queries[dim(queries)[1]+1, ] = c("", "", problem_type, msg)
    }
    return(queries)
  }

  ############################################################################################
  ############################################################################################
  # patid complete?
  for (i in 1:dm[1]) {
    if (toxData[i, rt@patidCol] == "") {
      msg = paste("Missing patid on row", i, "(toxData)")
      queries = query(toxData, i, msg, "Missing data", notes)
    }
  }

  ############################################################################################
  ############################################################################################
  # patid complete?
  for (i in 1:dim(patientData)[1]) {
    if (patientData[i, rt@patidCol] == "") {
      stop("Missing patid on row", i, "(patientData)")
      # queries = query(toxData, i, msg, "Missing data", notes)
    }
  }

  ############################################################################################
  ############################################################################################
  # patid in toxData and  patientData
  for (pt in unique(toxData[, rt@patidCol] )) {
    k = sum(pt== patientData[,rt@patidCol])
    if (k == 0) {
      msg = paste0("Error: Patient (", pt, ") does not appear in (toxData)")
      i = which(toxData[, rt@patidCol] == pt)[1]
      queries = query(toxData, i, msg, "Missing data", notes)
    }
  }

  ############################################################################################
  ############################################################################################
  # start tox window exists
  for (i in 1:dim(patientData)[1]) {
    if (is.na(patientData[i, rt@dateOfStartOfToxWindow])) {
      msg = paste0("Missing (",rt@dateOfStartOfToxWindow, ") on row ", i, " (patientData), Setting to 01/01/1970")
      patientData[i, rt@dateOfStartOfToxWindow] = as.Date("01/01/1970","%d/%m/%Y")
      queries = query(toxData, i, msg, "Missing data", notes)
    }
  }


  ############################################################################################
  # set ctcae grade to zero if missing
  for (i in 1:dm[1]) {
    if (is.na(toxData[i, rt@toxGradeCol])) {
      msg = paste("Patient", toxData[i, rt@patidCol], "is missing toxicity grade for",toxData[i, rt@toxNameCol] , "line", i, "(currently set to zero)")
      toxData[i,rt@toxGradeCol] = 0
      queries = query(toxData, i, msg, "Note", notes)
    }
  }

  ############################################################################################
  # If toxicity without start date assign study entry date -7. Tell user.
  for(i in 1:dm[1]){
    if(toxData[i, rt@toxGradeCol] > 0){
      if(is.na(toxData[i, rt@dateOfStartTox])){
        msg = paste("Patient", toxData[i, rt@patidCol], "is missing the date of start of toxicity for:", toxData[i, rt@toxNameCol], "line", i, "(setting to 7 days prior to earlist known date)")
        queries = query(toxData, i, msg, "Missing data",notes)
        patid = toxData[i,rt@patidCol]
        studyEntry = patientData[patientData[,rt@patidCol] == patid, rt@dateOfStartOfToxWindow]
        toxData[i, rt@dateOfStartTox] = studyEntry - 7
      }
    }
  }



  ############################################################################################
  # if missing end of toxicity date set to start date + 3000
  for(i in 1:dm[1]){
    if(toxData[i, rt@toxGradeCol] > 0){
      if(is.na(toxData[i, rt@dateOfEndTox])) {
        msg = paste("Note: Patient:", toxData[i, rt@patidCol], "toxicity:", toxData[i, rt@toxNameCol], "line:", i, "is missing an end date, setting to a large value")
        queries = query(toxData, i, msg, "Note",notes)
        patid = toxData[i,rt@patidCol]
        studyEntry = patientData[patientData[,rt@patidCol] == patid, rt@dateOfStartOfToxWindow]
        toxData[i,rt@dateOfEndTox] = studyEntry + 3000
      }
    }
  }


  ############################################################################################
  numPatients = dim(patientData)[1]
  msg = paste("Number of patients:", numPatients, "in the provided database")
  queries = query(toxData, i, msg, "Affirmation", notes ,TRUE)

  patientData$worstGrade = sapply(patientData[,rt@patidCol], function(x) max(-1,toxData[toxData[,rt@patidCol] == x,rt@toxGradeCol], na.rm = TRUE))

  noToxicities = sum(patientData$worstGrade <= 0)
  # number of patients without toxicities
  if (noToxicities > 0) {
    if(noToxicities == 1) {
      msg = paste("There was 1 patient with no eligible toxicities")
    } else {
    msg = paste("There were", noToxicities, "patient/s with no eligible toxicities")
    }
    queries = query(toxData, i, msg, "Affirmation",notes , TRUE)
  }

  ############################################################################################
  # missing ae_term
  for (i in 1:dm[1]) {
    if (toxData[i, rt@toxGradeCol] > 0) {
      if (is.na(toxData[i, rt@toxNameCol])) {
        msg = paste("Patient", toxData[i, rt@patidCol], "is missing ae_term (toxicity term), line", i)
        queries = query(toxData, i, msg, "Missing data",notes)
      }
    }
  }

  ############################################################################################
  # missing category
  for (i in 1:dm[1]) {
    if (toxData[i, rt@toxCategoryCol] == "") {
      msg = paste("Patient", toxData[i, rt@patidCol], "is missing category for the adverse event", toxData[i, rt@toxNameCol], ", line", i)
      queries = query(toxData, i, msg, "Missing data",notes)
    }
  }

  ############################################################################################
  # number Toxicities
  toxData[is.na(toxData[,rt@toxNameCol]), rt@toxNameCol] = ""
  toxData$order = 1:dm[1]
  toxData = toxData[order(toxData[,rt@toxCategoryCol], toxData[rt@toxNameCol]), ]

  toxData$ass_toxID    = 0
  toxData$ass_toxID[1] = 1
  j = 1
  for (i in 2:length(toxData$ass_toxID)) {
    if (toxData[i, rt@toxNameCol] != toxData[i - 1, rt@toxNameCol]) {
      j = j + 1
    }
    toxData$ass_toxID[i] = j
  }
  toxData = toxData[toxData$order,] # return to provided order
  toxData$order = NULL


  ############################################################################################
  # Generate toxStart from registration and toxEnd from registration here.
  toxData[ ,rt@dateOfStartOfToxWindow] = sapply(toxData[,rt@patidCol], function(x) patientData[patientData[,rt@patidCol] == x,rt@dateOfStartOfToxWindow])
  toxData[ ,rt@dateOfEndOfToxWindow] = sapply(toxData[,rt@patidCol], function(x) patientData[patientData[,rt@patidCol] == x,rt@dateOfEndOfToxWindow])

  toxData$rel_ae_start   = as.numeric(toxData[,rt@dateOfStartTox] - toxData[, rt@dateOfStartOfToxWindow])
  toxData$rel_ae_end     = as.numeric(toxData[,rt@dateOfEndTox]   - toxData[, rt@dateOfStartOfToxWindow])
  toxData$rel_ae_ent_assessment = as.numeric(toxData[, rt@dateOfEndOfToxWindow]  - toxData[, rt@dateOfStartOfToxWindow])

  ############################################################################################
  # Summarise the preparation
  message("############################################################################################")
  message("# Summary of preparation")
  message("Number of patients: ", numPatients)
  message("Number of patients with no toxicities: ", noToxicities)
  message("Number of notes: ", sum(queries$problem_type == "Note"))
  message("Number of missing data problems: ", sum(queries$problem_type == "Missing data"))
  # message("Number of incorrect data problems: ", sum(queries$problem_type == "Wrong data"))

  if(sum(is.na(patientData[,rt@dateOfStartOfToxWindow]))){
    warning("Missing date of study entry for ", sum(is.na(patientData[,rt@dateOfStartOfToxWindow])), " patient")
  }
  if(sum(is.na(patientData[,rt@dateOfEndOfToxWindow]))){
    warning("Missing date pf end of tox window for ", sum(is.na(patientData[,rt@dateOfEndOfToxWindow])), " patients")
  }

  rt@toxData = toxData
  rt@queries = queries

  if(rt@wasQueried){
    message("Warning: this function was already applied to this object")
  }
  rt@wasQueried = TRUE
  return(rt)
}
