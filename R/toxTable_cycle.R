#' Table: Toxicity worst in cycle
#'
#' Generates a table of all toxicities for a certain cycle or cycles. There are options to report all changes in toxicities or just the worst per patient. CTCAE grades and categories can be merged for more sucinct tables.
#'
#' @inheritParams prepareToxicity
#' @param cycles The cycle or cycles to include toxicities from
#'
#'
#' #' @details
#' The output relies on the following options:
#' \item cycleCycleMerge
#' \item cycleColumnMerge
#' \item cumulativeGrades
#' \item cycleCategoryMerge
#'  The output also uses the cycleNames in the first column of the table. If cycles are to be merged the name for the first one will be used.
#'
#' @seealso \link{\code{print_toxTable_cycle}}
#'
#' @export toxTable_cycle

toxTable_cycle = function(toxDB , cycles){

  if(class(toxDB) != "robustToxicities") {
    stop("toxDB must be of class toxDB")
  }

  ###################################################
  ## Check stuff is named ok
  a = 0
  if (!length(toxDB@cleanData$treatment)) {
    message("Note: treatment not named correctly in database, assumed only 1 treatment arm\n")
  }
  if (!length(toxDB@cleanData$ae_cycle_occured)) {
    a = 1
    message("Error: ae_cycle_occured not named correctly in database, no table created\n")
  }
  if (!length(toxDB@cleanData$ae_ctcae_grade)) {
    a = 1
    message("Error: ae_ctcae_grade not named correctly in database, no table created\n")
  }

  if (toxDB@options@discardBaseline) {
    toxDB@cleanData = toxDB@cleanData[toxDB@cleanData$ae_cycle_occured != 0, ]
  }


  if (a == 0) {
    # subset database with toxicities in requested cycles cycles
    toxDB@cleanData$x = apply(toxDB@cleanData[paste0("occur_in_cycle_", cycles)] , 1 , function(y) max(as.numeric(y)))
    cleanDataSub = toxDB@cleanData[toxDB@cleanData$x > 0 & toxDB@cleanData$ass_TRUE, ]

    # maximum cycle must go up to
    max.cycle = sum(str_detect(names(cleanDataSub), "occur_in_cycle_"))
    # names of the cycles
    names_occur = names(cleanDataSub)[str_detect(names(cleanDataSub), "occur_in_cycle_")]


    treats = unique(toxDB@cleanData$treatment)


    # generate initial table
    if(toxDB@options@tabulationMethod == "worst"){
      toxTable = toxTable_worst(cleanDataSub, treats)
    } else if(toxDB@options@tabulationMethod == "all"){
      toxTable = toxTable_all(cleanDataSub, treats)
    }

    npatients = rep(0, length(treats))
    # count and record number of patients having at least some of the required time period (saved in table and in npatients)
    for(treatment in 1:length(treats)){
      # number of patients at this cycle
      tox_s = toxDB@cleanData[toxDB@cleanData$treatment == treats[treatment], ]
      if(length(cycles)>1){
        npatients[treatment] = length(unique(tox_s$patid[apply(tox_s[, paste0("present_in_cycle_", cycles)], 1, function(x) sum(x) > 0)]))
        toxTable[1, 5*treatment-1] = npatients[treatment]
      } else {
        npatients[treatment] = length(unique(tox_s$patid[tox_s[, paste0("present_in_cycle_", cycles)]]))
        toxTable[1, 5*treatment-1] = npatients[treatment]
      }
    }

    # Perform the column merge for toxicities
    if(is.null(toxDB@options@cycleColumnMerge) == FALSE){
      colMerge = strsplit(toxDB@options@cycleColumnMerge, "[|]")[[1]]
      toxTableClean = data.frame(toxID = toxTable$toxID, category = toxTable$category, toxicity = toxTable$toxicity, stringsAsFactors = FALSE)
      for(side in 1:length(treats)){
        for(col in colMerge){
          if(toxDB@options@cumulativeGrades) {

            composition = min(as.numeric(strsplit(col, ",")[[1]])):5
            cols = paste0("tox.", side, ".", composition)
            cname = paste0("tox.", side, ".", composition[1])
            if(length(composition) > 1) {
              toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
              if(!1 %in% composition) {
                toxTableClean[1, cname] = NA
              }
            } else {
              toxTableClean[, cname] = toxTable[, cols]
            }

          } else {

            composition = as.numeric(strsplit(col, ",")[[1]])
            cols = paste0("tox.", side, ".", composition)
            cname = paste0("tox.", side, ".", paste0(composition, collapse = ""))
            if(length(composition) > 1) {
              toxTableClean[, cname] = apply(toxTable[, cols], 1, function(x) {sum(as.numeric(x), na.rm = TRUE)})
              if(!1 %in% composition) {
                toxTableClean[1, cname] = NA
              }
            } else {
              toxTableClean[, cols] = toxTable[, cols]
            }
          }
        }
      }
      toxTable = toxTableClean
    }


    # Order according to toxID (ensuring that the toxicities are ordered alphabetically first by category and then by toxicity)
    toxTable = toxTable[order(as.numeric(as.character(toxTable$toxID)), decreasing = FALSE), ]
    toxTable = toxTable[which(toxTable$toxicity!= ""), ]
    # Perform the row merge for categories
    if(length(toxDB@options@cycleCategoryMerge)){
      for(category in toxDB@options@cycleCategoryMerge){
        if(category != ""){
          if(category %in% toxTable$category){
            merge.rows = which(category == toxTable$category)
            di = dim(toxTable)
            toxTable[merge.rows[1], 4:di[2]] = apply(toxTable[merge.rows, 4:di[2]], 2, function(x) {sum(as.numeric(x))})
            toxTable$toxicity[merge.rows[1]] = ""
            if(length(merge.rows)>1){
              toxTable = toxTable[-merge.rows[2:length(merge.rows)], ]
            }
          }
        }
      }
    }


    # drop toxID as not needed after ordering
    toxTable$toxID = NULL

    # remove duplication of writing categories (one under the other before)
    a = dim(toxTable)[1]
    if(a >= 3){
      for(i in a:3){
        if(toxTable$category[i-1] == toxTable$category[i]){
          toxTable$category[i] = ""
        }
      }
    }

    # renaming
    colnames(toxTable)[1:2] = c("Category", "Toxicity")

    # return the table to the app
    return(toxTable)
  }
}
# end





