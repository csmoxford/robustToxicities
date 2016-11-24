#' @import stringr

.toxTable_summary = function(toxDB) {

  no_cycles = sum(str_detect(names(toxDB@cleanData), "present_in_cycle_"))
  names_cycle = names(toxDB@cleanData)[str_detect(names(toxDB@cleanData), "present_in_cycle_")]

  cycle.lists=strsplit(toxDB@options@sumCycleMerge,"[|]")[[1]]

  # create table to populate
  toxTable = data.frame(cycle.number = cycle.lists, stringsAsFactors = FALSE)
  # for each treatment generate space to count these.
  treats = sort(unique(toxDB@cleanData$treatment))
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
        toxTable[cycle.no, 7 * treatment - 5] = length(unique(cleanDataAll$patid[apply(cleanDataAll[, paste0("present_in_cycle_", cycles)], 1, function(x) sum(x) > 0)]))
        x=aggregate(apply(cleanDataAll[, paste0("occur_in_cycle_",cycles)],1,max),by=list(cleanDataAll$patid),FUN=max)$x
      } else {
        toxTable[cycle.no, 7 * treatment - 5 ] = length(unique(cleanDataAll$patid[cleanDataAll[, paste0("present_in_cycle_", cycles)]]))
        x=aggregate(cleanDataSub[,paste0("occur_in_cycle_",cycles)],by=list(cleanDataSub$patid),FUN=max)$x
      }
      toxTable[cycle.no,3:8 + (treatment - 1) * 7] = c(sum(x == 0),sum(x == 1),sum(x == 2),sum(x == 3),sum(x == 4),sum(x == 5))
    }
  }

  # Perform the column merge for toxicities
  if(is.null(toxDB@options@sumColumnMerge) == FALSE){
    colMerge = strsplit(toxDB@options@sumColumnMerge, "[|]")[[1]]
    toxTableClean = data.frame(toxTable$cycle, stringsAsFactors = FALSE)
    for(side in 1:length(treats)){
      for(col in colMerge){
        if(col == "total") {

          cols = paste0("tox.", side, ".total")
          toxTableClean[, cols] = toxTable[, cols]
        } else if(toxDB@options@cumulativeGrades) {

          composition = min(as.numeric(strsplit(col, ",")[[1]])):5
          cols = paste0("tox.", side, ".", composition)
          cname = paste0("tox.", side, ".", composition[1])
          if(length(composition) > 1) {
            toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
          } else {
            toxTableClean[, cname] = toxTable[, cols]
          }

        } else {

          composition = as.numeric(strsplit(col, ",")[[1]])
          cols = paste0("tox.", side, ".", composition)
          cname = paste0("tox.", side, ".", paste0(composition, collapse = ""))
          if(length(composition) > 1) {
            toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
          } else {
            toxTableClean[, cols] = toxTable[, cols]
          }
        }
      }
    }
    toxTable = toxTableClean
  }

  # renaming
  colnames(toxTable)[1] = c("Cycle")
  toxTable[,1] = toxDB@cycleLabels

  # return the table to the app
  return(toxTable)

}
