.legendGetPosition = function(index, itemsOnRow, totalItems) {

  pos = list(x = rep(NA,length(index)),y = rep(NA,length(index)))

  fullMod = function(i, mod) {
    ret = i %% mod
    if(ret == 0) {return(mod)}
    return(ret)
  }

  for(j in 1:length(index)) {
    i = index[j]
    numberOfRows = ceiling(totalItems / itemsOnRow)
    areOnRow = ceiling(i / itemsOnRow)

    if(areOnRow < numberOfRows || totalItems == itemsOnRow * numberOfRows) {
      pos$x[j] = fullMod(i,itemsOnRow) / (itemsOnRow) - 0.5/itemsOnRow
    } else {
      itemsOnThisRow = fullMod(totalItems,itemsOnRow)
      pos$x[j] = fullMod(i,itemsOnRow) / (itemsOnRow) - 0.5/itemsOnRow + (itemsOnRow - itemsOnThisRow)/(2*itemsOnRow)
    }
    pos$y[j] = (numberOfRows + 1 - areOnRow)  / (numberOfRows + 1)

  }
  return(pos)
}
