


.toxTableSetup = function(noTreatments) {

  # create table to populate
  toxTable=data.frame(toxID=rep(0,1), category=rep("",1), toxicity=rep("",1), stringsAsFactors = FALSE)
  for(side in 1:noTreatments){
    toxTable[paste0("tox.", side,".1", sep = "")]= 0
    toxTable[paste0("tox.", side,".2", sep = "")]= 0
    toxTable[paste0("tox.", side,".3", sep = "")]= 0
    toxTable[paste0("tox.", side,".4", sep = "")]= 0
    toxTable[paste0("tox.", side,".5", sep = "")]= 0
  }
  return(toxTable[0,])
}
