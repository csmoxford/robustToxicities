
# Returns worst reported toxicity for a cycle.
# $treatment will divide the patients in sets depending on their treatment
##############################################################
## tox.full.db dataframe must have the following column names
# patid,
# treatment if more than 1 arm (will add a variable if missing assuming 1 arm),
# ae_ctcae_grade,
# ae_cycle_occured: cycles or time periods should be labeled from 0,1,2... for baseline, cycle 1, 2 etc.
# ensure these can be relabeled easily in the output table.
# max_cycle should reflect the maximum time period/cycle the patient reaches or has reached
# max_cycle_followup should reflect the last follow up cycle reached by the patient.
## Must also be at least 1 line per patient otherwise won't count them (grade 0 toxicity won't be counted as toxicity anywhere else so is safe to be in the main toxicity database)
############################################################################################

worst.toxicity=function(tox.full.db,input){




  cat("\nSummary: worst recorded toxicity by time period")
  cat("\n################################################################\n")

  # Check stuff is named ok
  a=0
  if(length(tox.full.db$patid)==0){
    a=1
    cat("\nError: patid not named correctly in database, no table created")
  }
  if(length(tox.full.db$ae_cycle_occured)==0){
    a=1
    cat("\nError: ae_cycle_occured not named correctly in database, no table created")
  }
  if(length(tox.full.db$ae_ctcae_grade)==0){
    a=1
    cat("\nError: ae_ctcae_grade not named correctly in database, no table created")
  }

  if(a==0){

    no_cycles=sum(str_detect(names(tox.full.db), "cycle_start_date"))
    names_cycle=names(tox.full.db)[str_detect(names(tox.full.db), "cycle_start_date")]
    names_occur=names(tox.full.db)[str_detect(names(tox.full.db), "occur_in_cycle")]


    # load column merge and category merge values if they are provided
    if(is.null(input$sum.col.merge)==FALSE){
      col.merge=input$sum.col.merge
    }

    cycle.lists=strsplit(input$sum.cycle.merge,"[|]")[[1]]

    # create table to populate
    tox.table=data.frame(cycle.number=cycle.lists,stringsAsFactors =F)
    # for each treatment generate space to count these.
    treats=unique(tox.full.db$treatment)
    for(treatment in treats){
      tox.table[paste0("tox.", treatment,".total", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".0", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".1", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".2", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".3", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".4", sep = "")]= 0
      tox.table[paste0("tox.", treatment,".5", sep = "")]= 0
    }



    for(treatment in 1:length(treats)){

      #sub set by treatment
      tox.db.sub=tox.full.db[tox.full.db$treatment==treats[treatment],]

      for(cycle.no in 1:length(cycle.lists)){
        cycles=strsplit(cycle.lists[cycle.no],"[,]")[[1]]
        # if merging more than one cycle need to apply to merge columns
        if(length(cycles)>1){
          tox.table[cycle.no,7*treatment-5]=length(unique(tox.db.sub$patid[apply(tox.db.sub[,paste0("cycle_start_date",cycles)],1,function(x) sum(!is.na(x))>0)]))
          x=aggregate(apply(tox.db.sub[,paste0("occur_in_cycle_",cycles)],1,max),by=list(tox.db.sub$patid),FUN=max)$x
        } else {
          tox.table[cycle.no,7*treatment-5]=length(unique(tox.db.sub$patid[is.na(tox.db.sub[,paste0("cycle_start_date",cycles)])==FALSE]))
          x=aggregate(tox.db.sub[,paste0("occur_in_cycle_",cycles)],by=list(tox.db.sub$patid),FUN=max)$x
        }
        tox.table[cycle.no,4:8+(treatment-1)*7]=c(sum(x==1),sum(x==2),sum(x==3),sum(x==4),sum(x==5))
        tox.table[cycle.no,3+(treatment-1)*7]=tox.table[cycle.no,2+(treatment-1)*7]-sum(tox.table[cycle.no,4:8+(treatment-1)*7])
      }
    }

    # Perform the column merge for toxicities
    if(is.null(col.merge)==FALSE){
      col.merge.cols=strsplit(col.merge,"[|]")[[1]]
      tox.table.2=data.frame(cycle.number=tox.table$cycle.number)
      for(treatment in treats){
        for(col in col.merge.cols){
          composition=as.numeric(strsplit(col,",")[[1]])
          if(length(composition)>1){
            tox.table.2[,paste0("tox.",treatment,".",paste0(composition,collapse=""))]=apply(tox.table[,paste0("tox.", treatment,".",composition)],1,function(x) {sum(as.numeric(x))} )
          } else {
            tox.table.2[,paste0("tox.", treatment,".",composition)]=tox.table[,paste0("tox.", treatment,".",composition)]
          }
        }
      }
      tox.table=tox.table.2
    }

    # print the resulting table to R
    print(tox.table,row.names = FALSE)
    cat("\n################################################################\n")
    # return the table to the app
    return(tox.table)
  } else {
    cat("\n################################################################\n")
  }
}
