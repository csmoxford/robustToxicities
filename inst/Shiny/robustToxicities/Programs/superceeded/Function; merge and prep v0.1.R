




prepare=function(tox.db){

  ############################################################################################
  cat("\nDatabase validation of cycles and dates")

  library(stringr)
  ############################################################################################
  # maximum number of cycles of any patient
  no_cycles=sum(str_detect(names(tox.db), "cycle_start_date"))

  ############################################################################################
  # location of the dates for those cycles
  names_cycle=names(tox.db)[str_detect(names(tox.db), "cycle_start_date")]
  names_cycle_stub=sub("cycle_start_date","",names_cycle)

  ############################################################################################
  # format dates
  tox.db$registration_date=as.numeric(as.Date(tox.db$registration_date,"%d%b%Y"))
  tox.db$date_stopped_treatment=as.numeric(as.Date(tox.db$date_stopped_treatment,"%d%b%Y"))
  tox.db$ae_start_date=as.numeric(as.Date(tox.db$ae_start_date,"%d%b%Y"))
  tox.db$ae_end_date=as.numeric(as.Date(tox.db$ae_end_date,"%d%b%Y"))
  for(i in names_cycle){
    tox.db[,i]=as.numeric(as.Date(tox.db[,i],"%d%b%Y"))
  }

  ############################################################################################
  # If baseline toxicity without start date assign registration date -7. Tell user.
  for(i in 1:dim(tox.db)[1]){
    if(is.na(tox.db$ae_start_date[i])){
      if(tox.db$ae_term[i]=="" & is.na(tox.db$ae_cycle_occured[i])){
        cat("\nPatient",tox.db$patid[i],"has no toxicities")
      } else {
        cat("\nPatient",tox.db$patid[i],"is missing date of start of AE for:", tox.db$ae_term[i], "line", i)
      }
      tox.db$ae_start_date[i]=tox.db$registration_date[i]-7
    }
  }

  # if missing end date and continuing at end of study assign end of treatment date
  for(i in 1:dim(tox.db)[1]){
    if(tox.db$ae_cont_end_study[i]=="yes" & is.na(tox.db$ae_end_date[i])){
      tox.db$ae_end_date[i]=tox.db$date_stopped_treatment[i]
    }
    # if still missing assign large date
    if(is.na(tox.db$ae_end_date[i])){
      if(!(tox.db$ae_term[i]=="" & is.na(tox.db$ae_cycle_occured[i]))){
        cat("\nPatient",tox.db$patid[i],"is missing date of end of AE for:",tox.db$ae_term[i], "line", i)
        tox.db$ae_end_date[i]=30000
      }
    }
  }

  # mark if ae present in cycle
  for(j in 1:length(names_cycle)){
    c_sd=c(names_cycle,"date_stopped_treatment")[j]
    c_ed=c(names_cycle,"date_stopped_treatment")[j+1]
    tox.db[,paste0("occur_in_cycle_",names_cycle_stub[j])]=0
    for(i in 1:dim(tox.db)[1]){

      if(is.na(tox.db[i,c_sd])==FALSE & is.na(tox.db[i,c_ed])==FALSE){
        if(tox.db[i,c_sd]<=tox.db$ae_start_date[i] & tox.db$ae_start_date[i]<tox.db[i,c_ed] |
           tox.db[i,c_sd]<=tox.db$ae_end_date[i]   & tox.db$ae_end_date[i]<tox.db[i,c_ed]   |
           tox.db$ae_start_date[i]<=tox.db[i,c_sd] & tox.db[i,c_sd]<=tox.db$ae_end_date[i]   |
           tox.db$ae_start_date[i]<tox.db[i,c_ed] & tox.db[i,c_ed]<tox.db$ae_start_date[i] ){
          tox.db[i,paste0("occur_in_cycle_",names_cycle_stub[j])]=tox.db$ae_ctcae_grade[i]
        }
      } else if(is.na(tox.db[i,c_sd])==FALSE & is.na(tox.db[i,c_ed])==TRUE & is.na(tox.db[i,"date_stopped_treatment"])==FALSE){
        if(tox.db[i,c_sd]<=tox.db$ae_start_date[i] & tox.db$ae_start_date[i]<tox.db[i,"date_stopped_treatment"] |
             tox.db[i,c_sd]<=tox.db$ae_end_date[i]   & tox.db$ae_end_date[i]<tox.db[i,"date_stopped_treatment"]   |
             tox.db$ae_start_date[i]<=tox.db[i,c_sd] & tox.db[i,c_sd]<=tox.db$ae_end_date[i]   |
             tox.db$ae_start_date[i]<tox.db[i,"date_stopped_treatment"] & tox.db[i,"date_stopped_treatment"]<tox.db$ae_start_date[i] ){
          tox.db[i,paste0("occur_in_cycle_",names_cycle_stub[j])]=tox.db$ae_ctcae_grade[i]
        }
      } else if(is.na(tox.db[i,c_sd])==FALSE & is.na(tox.db[i,c_ed])==TRUE & is.na(tox.db[i,"date_stopped_treatment"])==TRUE){
        #if the cycle start date is the last recorded date it must be in this cycle
        tox.db[i,paste0("occur_in_cycle_",names_cycle_stub[j])]=tox.db$ae_ctcae_grade[i]
      }
    }
  }

  tox.db$occur_in_cycle_0=1*tox.db$ae_cycle_occured==0
  tox.db$cycle_start_date0=tox.db$registration_date

  cat("\n\nFile updated with assigned category and toxicity\n################################################################\n")
  return(tox.db)
}



