
#' @export numberToxicities
numberToxicities=function(database){
  database=database[order(database$ass_category,database$ass_toxicity_disp),]

  database$ass_toxID=0
  database$ass_toxID[1]=1
  j=1
  for(i in 2:length( database$ass_toxID)){
    if(database$ass_toxicity_disp[i] != database$ass_toxicity_disp[i-1]){
      j=j+1
    }
    database$ass_toxID[i]=j
  }
  return(database)
}
