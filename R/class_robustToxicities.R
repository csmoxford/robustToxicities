
#' The robustToxicities class
#'
#' This is core object of this package. This object stores the original dataset as well as the automatically cleaned dataset and a list of notes and queries generated when cleaning the dataset. A list of options is also provided to store plot and tabulation options and provide additional metadata. Finally treatment and cycle labels are also required.
#'
#' @slot data The original dataset
#' @slot cleanData The cleaned dataset
#' @slot treatmentLabels A vector of treatment labels
#' @slot cycleLabels A vector of cycle / time perior labels
#' @slot queries A data.frame containing all the queries and note generated when loading the data
#' @slot options An s4 object of class \code{\link{toxicityOptions-class}} containing options and metadata for the files.


#' @exportClass robustToxicities
.robustToxicities = setClass("robustToxicities",slots = c(data = "data.frame", cleanData = "data.frame", treatmentLabels = "character", cycleLabels = "character", queries = "data.frame", options = "toxicityOptions"), validity = function(object){

  validObject(object@options)

  cnames = colnames(object@cleanData)

  if (object@options@timeType == "time") {
    required_list = c("patid", "treatment", "ae_term", "ae_system", "ae_grade",
                      "ae_start_date", "ae_end_date", "ae_cont_end_study",
                      paste0("cycle_start_date_",1:length(object@cycleLabels)),
                      paste0("occur_in_cycle_",1:length(object@cycleLabels)),
                      paste0("present_in_cycle_",1:length(object@cycleLabels)),
                      "date_stopped_treatment", "ass_TRUE")
  } else if (object@options@timeType == "cycle") {
    required_list = c("patid", "treatment", "ae_term", "ae_system", "ae_grade",
                      paste0("occur_in_cycle_",1:length(object@cycleLabels)),
                      paste0("present_in_cycle_",1:length(object@cycleLabels)),
                      "ass_TRUE")
  }

  for (col in required_list) {
    if (!col %in% cnames) {
      return(paste0("variable ",col, " is not in cleanData and is a required field"))
    }
  }

  if(length(treatmentLabels) < max(object@cleanData$treatment)) {
    return(paste0("Number of treatments is fewer than the highest value in cleanData$treatment"))
  }

  if (sum(object@cleanData$ass_TRUE) == 0) {
    return(paste0("cleanData$ass_TRUE cannot all be zero / FALSE"))
  }

  return(TRUE)
})






#' @export robustToxicities
robustToxicities = function(data, cycleLabels, options, treatmentLabels = NULL) {

  if(class(data) != "data.frame") {
    stop("data must be of class data.frame")
  }

  cleanData = data
  dm = dim(cleanData)

  ################################################################################
  # Check fields are provided (all need these fields)
  requiredData = c("patid", "ae_term", "ae_system", "ae_grade")
  name = names(cleanData)
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
    cleanData$ass_TRUE = TRUE
    message("ass_TRUE not in database assume true for all entries (note this is used for patients with no toxicities and subsetting data)")
  }

  ################################################################################
  # treaments
  if(length(treatmentLabels) > 1 & !"treatment" %in% name){
    stop("treatment column not found in database")
  } else if(length(treatmentLabels) == 1 & !"treatment" %in% name){
    message("Only one treatment found, creating treatment column")
    cleanData$treatment = 1
  }

  if(is.null(treatmentLabels)) {
    message("No treatment labels provided generating from the data")
    if(sum(is.na(cleanData$treatment))){
      stop("There must be no missing treatment allocations")
    }
    treatmentLabels = levels(as.factor(cleanData$treatment))
    cleanData$treatment = as.integer(sapply(cleanData$treatment, function(x) which(x == treatmentLabels)))

  }

  if(!is.numeric(cleanData$treatment)) {
    treat = rep(0,length(cleanData$treatment))
    for(i in 1:length(treatmentLabels)) {
      treat[cleanData$treatment == treatmentLabels[i]] = i
    }
    if(sum(treat == 0) > 0) {
      stop("Not all treatments in treatmentLabels matched data in data$treatment")
    }
    cleanData$treatment = treat
  }

  if(length(treatmentLabels) < max(cleanData$treatment)) {
    message("data$treatment must be integer valued and correspond to the labels in treatmentLabels")
    stp = 1
  }
  if(sum(is.na(cleanData$treatment))){
    stop("There must be no missing treatment allocations")
  }
  print(cleanData$treatment)

  ################################################################################
  # time data only checks
  if(options@timeType  == "time") {

    ################################################################################
    # convert the columns which



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

  notes = options@displayNotes
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

  ################################################################################
  ################################################################################
  # internal function

  # append the query to the query data.frame and spit out a message.
  query=function(clearnData,i,msg,problem_type,notes,aff=FALSE){
    if(!(problem_type == "Note" & !notes)){
      message(msg)
    }
    if(!aff){
      queries[dim(queries)[1]+1,]=c(cleanData$patid[i], cleanData$ae_term[i], cleanData$ae_cycle_occured[i], problem_type, msg)
    } else {
      queries[dim(queries)[1]+1,]=c("", "", "", problem_type, msg)
    }
    return(queries)
  }

  # list of formatted categories for CTCAE classification
  categoryList=c(
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
    "Renal and urinary disorder",
    "Reproductive system and breast disorders",
    "Respiratory, thoracic and mediastinal disorders",
    "Skin and subcutaneous tissue disorders",
    "Social circumstances",
    "Surgical and medical procedures",
    "Vascular disorders",
    "Other",
    "")
  categoryListMatch = tolower(gsub("[[:punct:]]", "",word(categoryList)))
  categoryListCompare = tolower(gsub(" ","",gsub("[[:punct:]]","",gsub("&","and",categoryList))))
  ################################################################################
  # set the data to cleanData keeping copy of the original!!
  # get the dimention for reference

  ################################################################################
  msg = paste("Number of patients:", length(unique(cleanData$patid)), "in the provided database")
  queries = query(cleanData, i, msg, "Affirmation",notes ,TRUE)

  noToxicities = 0
  for (i in 1:dm[1]) {
    if (!cleanData$ass_TRUE[i]) {
      # toxicity free patient counter
      if (sum(cleanData$patid == cleanData$patid[i]) == 1 & (cleanData$ae_term[i] == "" | is.na(cleanData$ae_term[i]))) {
        noToxicities = noToxicities + 1
        cleanData$ae_grade[i] = 0
        cleanData$ae_term[i] = ""
        cleanData$ae_start_date[i] = 0
      }
    }
  }
  # number of patients without toxicities
  if (noToxicities > 0) {
    msg = paste("There were", noToxicities, "patients with no eligible toxicities")
    queries = query(cleanData, i, msg, "Affirmation",notes , TRUE)
  }

  ############################################################################################
  # if missing ae_cycle_occured, generate this
  if (is.null(cleanData$ae_cycle_occured)) {
    message("ae_cycle_occured not provided, creating column and will populate it")
    cleanData$ae_cycle_occured = NA
  }

  ############################################################################################
  # patid complete?
  for (i in 1:dm[1]) {
    if (cleanData$patid[i] == "") {
      msg = paste("Missing patid on row", i)
      queries = query(cleanData, i, msg, "Missing data",notes)
    }
  }

  ################################################################################
  # tidy the toxicity term ?
  if(is.null(cleanData$ass_toxicity_disp)){
    cleanData$ass_toxicity_disp = cleanData$ae_term
  }

  ################################################################################
  # tidy the categories
  cleanData$ass_category = ""
  for ( i in 1:dm[1]) {
    clean = tolower(gsub(" ","",gsub("[[:punct:]]", "", gsub("&", "and", cleanData$ae_system[i]))))
    j = which(clean == categoryListCompare)
    if(length(j)) {
      cleanData$ass_category[i] = categoryList[j]
    } else {
      m = tolower(gsub("[[:punct:]]", "",word(cleanData$ae_system[i])))
      j = which(m == categoryListMatch)
      if (length(j)) {
        msg = paste("Note: Category partial match for patient:", cleanData$patid[i], "Category:", cleanData$ae_system[i], "Matched to:", categoryList[j])
        queries = query(cleanData, i, msg, "Note", notes)
        cleanData$ass_category[i] = categoryList[j]
      } else {
        msg = paste("Category not matched for patient:", cleanData$patid[i], "Category:", cleanData$ae_system[i])
        queries = query(cleanData, i, msg, "Wrong data", notes)
      }
    }
  }

  ############################################################################################
  # set ctcae grade to zero if missing
  for (i in 1:dm[1]) {
    if (cleanData$ass_TRUE[i]) {
      if (is.na(cleanData$ae_grade[i])) {
        msg = paste("Patient", cleanData$patid[i], "is missing toxicity grade for",cleanData$ae_term[i] , "line", i, "(currently set to zero)")
        queries = query(cleanData, i, msg, "Missing data",notes)
      }
    }
  }

  ############################################################################################
  # missing ae_term
  for (i in 1:dm[1]) {
    if (cleanData$ass_TRUE[i]) {
      if (is.na(cleanData$ae_term[i])) {
        msg = paste("Patient", cleanData$patid[i], "is missing ae_term (toxicity term), line", i)
        queries = query(cleanData, i, msg, "Missing data",notes)
      }
    }
  }

  ############################################################################################
  # missing ae_system
  for (i in 1:dm[1]) {
    if (cleanData$ass_TRUE[i]) {
      if (is.na(cleanData$ae_system[i])) {
        msg = paste("Patient", cleanData$patid[i], "is missing ae_system (ctcae category), line", i)
        queries = query(cleanData, i, msg, "Missing data",notes)
      }
    }
  }

  # time based stuff now
  if (options@timeType == "time") {


    # if missing ae_cont_end_study, for time data generate this
    if (is.null(cleanData$ae_cont_end_study)) {
      cleanData$ae_cont_end_study = NA
      message("ae_cont_end_study not provided, creating column and will populate it")
    }

    ############################################################################################
    # format dates
    for (col in grep("date",names(cleanData))) {
      test = as.numeric(as.Date(cleanData[, col], format="%Y-%m-%d", origin="1970-01-01"))
      if (all(is.na(test))) {
        test = as.numeric(as.Date(cleanData[, col], format="%d%b%Y", origin="1970-01-01"))
      }
      if (all(is.na(test))) {
        test = as.numeric(as.Date(cleanData[, col], format="%d/%m/%Y", origin="1970-01-01"))
      }
      if(!all(is.na(test))){
        cleanData[,col] = test
      }
    }

    ############################################################################################
    # maximum number of cycles of any patient
    no_cycles = sum(str_detect(names(cleanData), "cycle_start_date_"))
    ############################################################################################
    # location of the dates for those cycles
    names_cycle = names(cleanData)[str_detect(names(cleanData), "cycle_start_date_")]
    names_cycle_stub = sub("cycle_start_date_", "", names_cycle)

    # generate present_for_cycle if not provided:
    no_present = sum(str_detect(names(cleanData), "present_in_cycle_"))
    if (no_present < no_cycles) {
      for (stub in names_cycle_stub) {
        cleanData[,paste0("present_in_cycle_",stub)] = !is.na(cleanData[,paste0("cycle_start_date_",stub)])
      }
    }


    ############################################################################################
    # If baseline toxicity without start date assign registration date -7. Tell user.
    for(i in 1:dm[1]){
      if(cleanData$ass_TRUE[i]){
        if(is.na(cleanData$ae_start_date[i])){
          msg = paste("Patient", cleanData$patid[i], "is missing the date of start of toxicity for:", cleanData$ae_term[i], "line", i, "(setting to 7 days prior to earlist known date)")
          queries = query(cleanData, i, msg, "Missing data",notes)
          cycle_dates=names(cleanData)[grepl("cycle_start_date_", names(cleanData))]
          cleanData$ae_start_date[i]=min(as.numeric(cleanData[i,cycle_dates]), na.rm = TRUE) - 7
        }
      }
    }

    ############################################################################################
    # Check date ordering and missing internal dates
    dates = grep("cycle_start_date_",names(cleanData))
    for (j in 2:length(dates)) {
      for (i in 1:dm[1]) {
        d1 = cleanData[i,dates[j - 1]]
        d2 = cleanData[i,dates[j]]
        if(!is.na(d2)) {
          # second date exists
          if(!is.na(d1)) {
            # first date exists
            if( d1 > d2) {
              # first date before second date
              msg = paste0("Patient ", cleanData$patid[i], " date for cycle ", dates[j - 1], "(", d1, ") is before date for ", dates[j], "(", d2, ")")
              queries = query(cleanData, i, msg, "Wrong data", notes)
            }
          } else {
            # first date missing by second date available
            msg = paste0("Patient ", cleanData$patid[i], " date for ", dates[j - 1], " is missing but the future date for",  dates[j], "(", d2, ") is not")
            queries = query(cleanData, i, msg, "Missing data", notes)
          }
        }
      }
    }


    ############################################################################################
    # Missing end of treatment date
    noEndTreatment=0
    noEndTreatmentPatid=c()
    for(i in 1:dm[1]){
      if(cleanData$ass_TRUE[i]){
        if(is.na(cleanData$date_stopped_treatment[i])){
          if(!cleanData$patid[i] %in% noEndTreatmentPatid){
            noEndTreatmentPatid = c(noEndTreatmentPatid, cleanData$patid[i])
            noEndTreatment = noEndTreatment + 1
          }
        }
      }
    }
    if(noEndTreatment > 0){
      msg = paste("Patients missing date of end of treatment (includes those still on study):", noEndTreatment)
      queries = query(cleanData, i, msg, "Affirmation",notes, TRUE)
    }

    ############################################################################################
    # if missing end date and continuing at end of study assign end of treatment date + 30
    for(i in 1:dm[1]){
      if(cleanData$ass_TRUE[i]){
        if(cleanData$ae_cont_end_study[i]=="yes" & is.na(cleanData$ae_end_date[i])){
          if(!is.na(cleanData$date_stopped_treatment[i])){
            cleanData$ae_end_date[i]=cleanData$date_stopped_treatment[i] + 30
            msg = paste("Note: Patient:", cleanData$patid[i], "toxicity:", cleanData$ae_term[i], "line:", i, "is continueing at end of study, setting the ae_end_date to 30 days after date_stopped_treatment")
            queries = query(cleanData, i, msg, "Note",notes)
          } else {
            msg = paste("Patient:", cleanData$patid[i], "toxicity:", cleanData$ae_term[i], "line:", i, "is continueing at end of study but the date_stopped_treatment is missing (setting ae_end_date to a large value)")
            queries = query(cleanData, i, msg, "Missing data",notes)
            cleanData$ae_end_date[i]=30000
          }
        } else if(is.na(cleanData$ae_end_date[i])){
          msg = paste("Patient:", cleanData$patid[i], "toxicity:", cleanData$ae_term[i], "line:", i, "has no end date for toxicity (setting ae_end_date to a large value)")
          queries = query(cleanData, i, msg, "Missing data",notes)
          cleanData$ae_end_date[i] = 30000
        }
      }
    }

    ############################################################################################
    # Missing ae_end_date
    for(i in 1:dm[1]){
      if(cleanData$ass_TRUE[i]){
        if(cleanData$ae_cont_end_study[i]=="no" & is.na(cleanData$ae_end_date[i])){
          msg = paste("Patient:", cleanData$patid[i], "toxicity:", cleanData$ae_term[i], "line:", i, "is missing the date_stopped_treatment (setting ae_end_date to a large value)")
          queries = query(cleanData, i, msg, "Missing data",notes)
          cleanData$ae_end_date[i]=30000
        }
      }
    }

    # mark if ae present in cycle
    dates = c(names_cycle, "date_stopped_treatment")
    for (j in 1:length(names_cycle)) {
      c_sd=dates[j]
      c_ed=dates[j+1]
      occur=paste0("occur_in_cycle_",names_cycle_stub[j])
      cleanData[,occur]=0
      for (i in 1:dm[1]) {
        if (cleanData$ass_TRUE[i]) {
          if (!is.na(cleanData[i,c_sd]) & !is.na(cleanData[i,c_ed])) {
            if (cleanData[i,c_sd] <= cleanData$ae_start_date[i] & cleanData$ae_start_date[i] < cleanData[i,c_ed] |
                cleanData[i,c_sd]<=cleanData$ae_end_date[i]   & cleanData$ae_end_date[i]<cleanData[i,c_ed]   |
                cleanData$ae_start_date[i]<=cleanData[i,c_sd] & cleanData[i,c_sd]<=cleanData$ae_end_date[i]   |
                cleanData$ae_start_date[i]<cleanData[i,c_ed] & cleanData[i,c_ed]<cleanData$ae_start_date[i] ){
              cleanData[i,occur]=cleanData$ae_grade[i]
            }
          } else if(!is.na(cleanData[i,c_sd]) & is.na(cleanData[i,c_ed]) & !is.na(cleanData[i,"date_stopped_treatment"])){
            if(cleanData[i,c_sd] <= cleanData$ae_start_date[i] & cleanData$ae_start_date[i] < cleanData[i,"date_stopped_treatment"] |
               cleanData[i,c_sd] <= cleanData$ae_end_date[i]   & cleanData$ae_end_date[i] < cleanData[i,"date_stopped_treatment"]   |
               cleanData$ae_start_date[i] <= cleanData[i, c_sd] & cleanData[i, c_sd] <= cleanData$ae_end_date[i]   |
               cleanData$ae_start_date[i] < cleanData[i, "date_stopped_treatment"] & cleanData[i, "date_stopped_treatment"] < cleanData$ae_start_date[i] ){
              cleanData[i, occur] = cleanData$ae_grade[i]
            }
          } else if(!is.na(cleanData[i,c_sd]) & is.na(cleanData[i,c_ed]) & is.na(cleanData[i,"date_stopped_treatment"])){
            #if the cycle start date is the last recorded date it must be in this cycle
            cleanData[i,occur] = cleanData$ae_grade[i]
          }
        }
      }
    }
  }


  ################################################################################
  # number Toxicities
  cleanData$order = 1:dm[1]
  cleanData = cleanData[order(cleanData$ass_category, cleanData$ass_toxicity_disp), ]

  cleanData$ass_toxID    = 0
  cleanData$ass_toxID[1] = 1
  j=1
  for (i in 2:length( cleanData$ass_toxID)) {
    if (cleanData$ass_toxicity_disp[i] != cleanData$ass_toxicity_disp[i-1]) {
      j = j + 1
    }
    cleanData$ass_toxID[i] = j
  }
  cleanData = cleanData[cleanData$order,]
  cleanData$order = NULL
  ################################################################################
  # Summarise the preparation


  message("\n#############################################################")
  message("# Summary of preparation")
  message("Number of patients: ", length(unique(cleanData$patid)))
  message("Number of patients with no toxicities: ", noToxicities)
  if(options@timeType == "time"){
    message("Patients missing date of end of treatment: ", noEndTreatment)
  }
  message("Number of notes: ", sum(queries$problem_type == "Note"))
  message("Number of missing data problems: ", sum(queries$problem_type == "Missing data"))
  message("Number of incorrect data problems: ", sum(queries$problem_type == "Wrong data"))

  return(.robustToxicities(data = data, queries = queries, treatmentLabels = treatmentLabels, cycleLabels = cycleLabels, cleanData = cleanData, options = options))
}


