#' @export toxTable_all

toxTable_all=function(toxDB,treatments){

  tox.id=sort(unique(toxDB$ass_toxID))

  # create table to populate
  toxTable = .toxTableSetup(length(treatments))


  i=1
  for (tox in tox.id) {
    toxDB_2 = toxDB[toxDB$ass_toxID == tox, ]
    i = i + 1
    # ID, category and name of toxicity
    toxTable[i, 1:3] = c(tox, toxDB_2$ass_category[1], toxDB_2$ass_toxicity_disp[1])
    toxTable[i, 4 + 1:length(treatments) * 5] = rep(0, length(treatments) * 5)
    # by treatment add data
    for (treatment in 1:length(treatments)) {
      toxDB_3 = toxDB_2[toxDB_2$treatment == treatments[treatment], ]
      # aggregate for each patient taking the maximum grade
      if (dim(toxDB_3)[1] > 0) {
      # add to table
      toxTable[i, 4:8 + (treatment - 1) * 5]=c(sum(toxDB_3$x == 1), sum(toxDB_3$x == 2), sum(toxDB_3$x == 3), sum(toxDB_3$x == 4), sum(toxDB_3$x == 5))
      } else {
        toxTable[i, 4:8 + (treatment-1) * 5]= rep(0, 5)
      }
    }
  }

  return(toxTable)
}
