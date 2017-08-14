.subSetToTimePeriodFromStart = function(rt, from, to, toxDataSub = NULL) {

  if(is.null(toxDataSub)) {
    toxDataSub = rt@toxData
  }

  # check if toxicity occuring at least in part within time period.
  toxDataSub$inTime = sapply(1:dim(toxDataSub)[1], function(x){
    if(toxDataSub$rel_ae_start[x] > to) {
      return(FALSE)
    } else if(is.na(toxDataSub$rel_ae_end[x])){
      return(TRUE)
    } else if(toxDataSub$rel_ae_end[x] < from) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })

  toxDataSub = toxDataSub[toxDataSub$inTime,]

  return(toxDataSub)

}
