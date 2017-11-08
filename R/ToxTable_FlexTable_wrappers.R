
#' FlexTable wrapper for toxicity tables
#'
#' These functions act as wrapper functions to create ConstructFlexTable objects.
#'
#' @param rt robustToxicity object.
#' @param cycles The cycle column names, or index in rt@periodDividerCols of the cycles to tabulate. May also be "all" to use all cycles
#' @param tble Optionally pass a pre computed table into the wrapper instead of computing it here. Note if columns don't match what is expected this is likely to fail. May be usefull for dropping some rows, or saving time rerunning the table generator function for large data sets.
#'
#' This functions return the ConstructFlexTable object from the prettyTables package. To create the FlexTable you can call object$GetTable() on the returned object. This can then be added to a word document using \code{\link{addFlexTable}} from the ReporteRs package.
#'
#' @seealso \code{\link{ToxTable_summary}}, \code{\link{ToxTable_cycle}}, \code{\link{ToxTable_category}}
#'
#'
#' @example  inst/HelpExamples/FT_ToxTable_example.R
#'
#' @importFrom prettyTables ConstructFlexTable
#' @importFrom stringr str_count
#' @name FT_ToxTable
NULL

#' @describeIn FT_ToxTable A wrapper for ToxTable_cycle
#' @export FT_ToxTable_summary
FT_ToxTable_summary = function(rt, tble = NULL) {

  if(is.null(tble)) {
    tble = ToxTable_summary(rt)
  }

  dm = dim(tble)
  each = (dm[2] - 1) / length(rt@treatmentLabels)

  ft = ConstructFlexTable(tble, header = TRUE)

  if(length(rt@treatmentCodes) > 1){
    ft$InsertHeaderRow(0, c("",rep(rt@treatmentLabels, each = each)))
    ft$SetSpanColumnByRow(1)
    ft$SetBorderWidthHorizontalByRow(2, c(0,2,ft$numRow-1,ft$numRow))
    ft$SetColorRowByIndex("#cccccc",c(1,2,ft$numRow))
    ft$SetBorderWidthVerticalByMatchingOnRow(2,1)
  } else {
    ft$SetBorderWidthHorizontalByRow(2, c(0,1,ft$numRow-1,ft$numRow))
    ft$SetColorRowByIndex("#cccccc",c(1,ft$numRow))
    ft$SetBorderWidthVerticalByColumn(2,c(0,1,ft$numCol))
  }



  ft$SetTextAlignColByIndex("center",2:ft$numCol)

  # ft$getTable()
  return(ft)

}


#' @export FT_ToxTable_cycle
#' @importFrom stringr fixed
#' @describeIn FT_ToxTable A wrapper for ToxTable_cycle
FT_ToxTable_cycle = function(rt, cycles = "all", tble = NULL) {

  if(is.null(tble)) {
    tble = ToxTable_cycle(rt, cycles)
  }
  nPatients = .countPatientsInCycles(rt, cycles)

  # print(nPatients)
  if(dim(tble)[1] == 0){
    message("Table has no rows table not returned")

  } else {
    ft = ConstructFlexTable(tble, header = TRUE)
    ###################################################################
    # header rows
    if(length(rt@treatmentCodes) > 1){
      toSpan = str_count(rt@options@toxTable_mergeGrades, pattern = fixed("|"))
      # add count to treatment labels for this cycle
      treatmentLabels = paste0(rt@treatmentLabels, " (n=", nPatients,")")
      ft$InsertHeaderRow(0,c("","",rep(treatmentLabels, each = toSpan)))
      ft$SetSpanColumnByRow(1)
    }
    ###################################################################

    ft$SetSpanRowByColumn(1)
    ft$SetBorderWidthHorizontalByMatchingOnColumn(2,1)
    ft$SetTextAlign("center")
    if(length(rt@treatmentCodes) > 1){
      ft$SetColorRowAlternatingByColumn(c("#ffffff","#cccccc"),1)
      ft$SetColorRowByIndex("#cccccc",1)
      ft$SetTextAlignColByIndex("left",c(1,2))
      ft$SetBorderWidthVerticalByMatchingOnRow(2,1)
    } else {
      ft$SetColorRowAlternatingByColumn(c("#cccccc","#ffffff"),1)
      ft$SetTextAlignColByIndex("left",c(1))
      ft$SetBorderWidthVerticalByColumn(2,c(0,1,ft$numCol))
    }

    return(ft)

  }
}

#' @export FT_ToxTable_category
#' @describeIn FT_ToxTable A wrapper for ToxTable_category
FT_ToxTable_category = function(rt, cycles = "all", tble = NULL) {

  if(is.null(tble)) {
    tble = ToxTable_category(rt, cycles)
  }
  nPatients = .countPatientsInCycles(rt, cycles)

  # print(nPatients)
  if(dim(tble)[1] == 0){
    message("Table has no rows table not returned")

  } else {
    ft = ConstructFlexTable(tble, header = TRUE)
    ###################################################################
    # header rows
    if(length(rt@treatmentCodes) > 1){
      toSpan = (dim(tble)[2] - 1) / length(rt@treatmentCodes)
      # add count to treatment labels for this cycle
      treatmentLabels = paste0(rt@treatmentLabels, " (n=", nPatients,")")
      ft$InsertHeaderRow(0,c("",rep(treatmentLabels, each = toSpan)))
      ft$SetSpanColumnByRow(1)
    }
    ###################################################################

    ft$SetSpanRowByColumn(1)
    ft$SetBorderWidthHorizontalByMatchingOnColumn(2,1)

    ft$SetTextAlign("center")
    if(length(rt@treatmentCodes) > 1){
      ft$SetColorRowAlternatingByColumn(c("#ffffff","#cccccc"),1)
      ft$SetColorRowByIndex("#cccccc",1)
      ft$SetTextAlignColByIndex("left",c(1,2))
      ft$SetBorderWidthVerticalByMatchingOnRow(2,1)
    } else {
      ft$SetColorRowAlternatingByColumn(c("#cccccc","#ffffff"),1)
      ft$SetTextAlignColByIndex("left",c(1))
      ft$SetBorderWidthVerticalByColumn(2,c(0,1,ft$numCol))
    }

    return(ft)

  }
}
