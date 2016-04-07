#' Table: Summary of worst toxicity by cycle
#'
#' Summarises toxicities by treatment and cycle
#'
#' @inheritParams prepareToxicity
#'
#' @export toxTable_summary

toxTable_summary = function(toxDB) {

  ################################################################################
  # checks and balances
  if (class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }


  a = 0
  if (length(toxDB@cleanData$patid) == 0) {
    a = 1
    message("Error: patid not named correctly in database, no table created")
  }
  if (length(toxDB@cleanData$ae_cycle_occured)  ==  0) {
    a = 1
    message("Error: ae_cycle_occured not named correctly in database, no table created")
  }
  if (length(toxDB@cleanData$ae_ctcae_grade)  ==  0) {
    a = 1
    message("Error: ae_ctcae_grade not named correctly in database, no table created")
  }
  ################################################################################
  # continue if everything is ok
  if(a == 0){

    no_cycles = sum(str_detect(names(toxDB@cleanData), "cycle_start_date"))
    names_cycle = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "cycle_start_date")]
    names_occur = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "occur_in_cycle")]


    cycle.lists=strsplit(toxDB@options@sumCycleMerge,"[|]")[[1]]

    # create table to populate
    toxTable = data.frame(cycle.number = cycle.lists, stringsAsFactors = FALSE)
    # for each treatment generate space to count these.
    treats = unique(toxDB@cleanData$treatment)
    for (treatment in treats) {
      toxTable[paste0("tox.", treatment,".total", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".0", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".1", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".2", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".3", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".4", sep = "")]= 0
      toxTable[paste0("tox.", treatment,".5", sep = "")]= 0
    }


    for (treatment in 1:length(treats)) {

      #sub set by treatment
      cleanDataAll=toxDB@cleanData[toxDB@cleanData$treatment  ==  treats[treatment],]

      for(cycle.no in 1:length(cycle.lists)){
        cycles=strsplit(cycle.lists[cycle.no],"[,]")[[1]]
        # if merging more than one cycle need to apply to merge columns
        cleanDataSub = cleanDataAll[cleanDataAll$ass_TRUE  ==  TRUE,]
        if(length(cycles)>1){
          # cycle totals
          toxTable[cycle.no, 7 * treatment - 5] = length(unique(cleanDataAll$patid[apply(cleanDataAll[, paste0("cycle_start_date", cycles)], 1, function(x) sum(!is.na(x)) > 0)]))


          x=aggregate(apply(cleanDataAll[, paste0("occur_in_cycle_",cycles)],1,max),by=list(cleanDataAll$patid),FUN=max)$x
        } else {
          toxTable[cycle.no, 7 * treatment - 5 ] = length(unique(cleanDataAll$patid[is.na(cleanDataAll[, paste0("cycle_start_date", cycles)]) == FALSE]))

          x=aggregate(cleanDataSub[,paste0("occur_in_cycle_",cycles)],by=list(cleanDataSub$patid),FUN=max)$x
        }
        print(x)
        toxTable[cycle.no,3:8 + (treatment - 1) * 7] = c(sum(x == 0),sum(x == 1),sum(x == 2),sum(x == 3),sum(x == 4),sum(x == 5))
      }
    }

    # Perform the column merge for toxicities
    if(is.null(toxDB@options@sumColumnMerge) == FALSE){
      cols = strsplit(toxDB@options@sumColumnMerge,"[|]")[[1]]
      toxTable.2 = data.frame(cycle.number=toxTable$cycle.number)
      for (treatment in treats) {
        for (col in cols) {
          composition=strsplit(col,",")[[1]]
          if (length(composition)>1) {

            toxTable.2[, paste0("tox.",treatment, ".", paste0(composition, collapse = ""))] = apply(toxTable[, paste0("tox.", treatment,".",composition)],1,function(x) {sum(as.numeric(x))} )
          } else {
            toxTable.2[, paste0("tox.", treatment, ".", composition)] = toxTable[, paste0("tox.", treatment, ".", composition)]
          }
        }
      }
      toxTable = toxTable.2
    }

    # renaming
    colnames(toxTable)[1] = c("Cycle")

    # return the table to the app
    return(toxTable)
  }
}
