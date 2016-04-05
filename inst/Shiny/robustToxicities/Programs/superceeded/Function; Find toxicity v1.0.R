############################################################################################
### Looks up toxicity in well defined tables to correct to standardised toxicities
# main.db is the shared toxicity database
# trial.correction.db is corrections trial specific additions
# key.word.db is keyword acceptable matching
# tox.db is the database to clean, must have field called toxicity
find.toxicity=function(main.db,trial.corrections.db,key.word.db,tox.db){
  # number of rows to check over
  a=dim(tox.db)[1]

  # add columns to store results

  if(length(tox.db$ass_TRUE)==0){
    tox.db$ass_category=0
    tox.db$ass_toxID=0
    tox.db$ass_toxicity_disp=""
    tox.db$ass_TRUE=FALSE
    tox.db$ass_keyword=FALSE
  }
  cat("\nSearching databases for standardised toxicity names.")

  for(i in 1:a){
    if(tox.db$ass_TRUE[i]==FALSE){
      clean.tox=clean.string(tox.db$ae_term[i])
      row.num=which(clean.tox$string==main.db$tox_clean)
      if(length(row.num)==1){
        tox.db$ass_category[i]      = main.db$category[row.num]
        tox.db$ass_toxID[i]         = main.db$toxID[row.num]
        tox.db$ass_toxicity_disp[i] = main.db$toxicity_disp[row.num]
        tox.db$ass_TRUE[i]          = TRUE
      } else if(clean.tox$string==""){
        if(!is.na(tox.db$ae_cycle_occured[i])){
          # If this is the entry for a patient with no toxicities this is reported later
          cat("\nMissing ae_term for patient",tox.db$patid[i],"line",i )
        } else {
          tox.db$ass_TRUE[i]        = TRUE
          tox.db$ae_ctcae_grade [i] = 0
        }
      } else {
        # look for match in exact corrections database
        cat("\nMismatch for patient",tox.db$patid[i],", AE:",clean.tox$string,", looking for match in exact corrections database:")

        match=which(clean.tox$string==trial.corrections.db$toxicity)
        staticID=trial.corrections.db$staticID[match]
        row.num=which(main.db$staticID==staticID)
        if(length(row.num)==1){
          cat(main.db$toxicity_disp[row.num])
          tox.db$ass_category[i]      =main.db$category[row.num]
          tox.db$ass_toxID[i]         =main.db$toxID[row.num]
          tox.db$ass_toxicity_disp[i] =main.db$toxicity_disp[row.num]
          tox.db$ass_TRUE[i]          =T
        } else{
          # look for key words to match to toxicity
          cat(" no match found.\n Looking for match in keyword database:")

          j=0
          while(j<length(key.word.db$keyword)){
            j=j+1
            if(grepl(key.word.db$keyword[j],clean.tox$string)){
              match=j
            }
          }
          staticID=key.word.db$staticID[match]
          row.num=which(main.db$staticID==staticID)


          if(length(row.num)==1){
            cat(" keyword", key.word.db$keyword[match], "Adverse event:",main.db$toxicity_disp[row.num])
            tox.db$ass_category[i]      =main.db$category[row.num]
            tox.db$ass_toxID[i]         =main.db$toxID[row.num]
            tox.db$ass_toxicity_disp[i] =main.db$toxicity_disp[row.num]
            tox.db$ass_TRUE[i]          =T
            tox.db$ass_keyword[i]       =T
          } else {
            cat("no match found\n")
            cat("No match for patient",tox.db$patid[i],", AE:", clean.tox$string, ", line:", i)
          }
        }
      }


    }
  }
  cat("\n################################################################\n")
  return(tox.db)

}
