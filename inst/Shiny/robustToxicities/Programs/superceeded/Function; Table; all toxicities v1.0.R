

# toxicity tabulation function by cycle and treatment side by side
##############################################################
# tox.full.db is the database, must contain:
# treatment if more than 1 arm (will add a variable if missing assuming 1 arm),
# ae_ctcae_grade
# ae_cycle_occured: cycles or time periods should be labeled from 0,1,2... for baseline, cycle 1, 2 etc.
# filepath is the folder to store tables in
# filestub is the base part of the filename (_subgroup name will be added)
##############################################################

toxicity.listing=function(tox.full.db,input,cycles){

  ###################################################
  ## Check stuff is named ok
  cat("toxicity tables by cycle or time period\n")
  cat("\n################################################################\n")
  a=0
  if(length(tox.full.db$treatment)==0){
    cat("Note: treatment not named correctly in database, assumed only 1 treatment arm\n")
  }
  if(length(tox.full.db$ae_cycle_occured)==0){
    a=1
    cat("Error: ae_cycle_occured not named correctly in database, no table created\n")
  }
  if(length(tox.full.db$ae_ctcae_grade)==0){
    a=1
    cat("Error: ae_ctcae_grade not named correctly in database, no table created\n")
  }

  if(a==0){


  #print(names(tox.full.db))


    # subset database with toxicities in requested cycles cycles
    tox.full.db$x=apply(tox.full.db[paste0("occur_in_cycle_",cycles)],1,function(y) max(as.numeric(y)))
    tox_sub=tox.full.db[tox.full.db$x>0,]

    # load column merge and category merge values if they are provided
    if(is.null(input$col.merge)==FALSE){
      col.merge=input$col.merge
    }
    if(length(input$merge.categories)>0){
      category.merge=input$merge.categories
    } else {
      category.merge=NULL
    }





    # maximum cycle must go up to
    max.cycle=sum(str_detect(names(tox_sub), "cycle_start_date"))
    # names of the cycles
    names_cycle=names(tox_sub)[str_detect(names(tox_sub), "cycle_start_date")]
    names_occur=names(tox_sub)[str_detect(names(tox_sub), "occur_in_cycle")]

    ## old code no longer needed
    #  tox_sub$occur_in_cycle_baseline=0
    #  for(i in 1:dim(tox.full.db)[1]){
    #    if(tox.full.db$ae_cycle_occured[i]==0){
    #      tox.full.db$occur_in_cycle_baseline[i]=tox.full.db$ae_ctcae_grade[i]
    #    }
    #  }


    treats=unique(tox.full.db$treatment)


    # generate initial table
    tox.table=tox.table.listing.worst(tox_sub,treats)

    # count and record number of patients having at least some of the required time period
    for(treatment in 1:length(treats)){
      # number of patients at this cycle
      tox_s=tox.full.db[tox.full.db$treatment==treats[treatment],]
      if(length(cycles)>1){
        tox.table[1,5*treatment-1]=length(unique(tox_s$patid[apply(tox_s[,paste0("cycle_start_date",cycles)],1,function(x) sum(is.na(x)==FALSE)>0)]))
      } else {
        tox.table[1,5*treatment-1]=length(unique(tox_s$patid[!is.na(tox_s[,paste0("cycle_start_date",cycles)])]))
      }
    }


    # Perform the column merge for toxicities
    if(is.null(col.merge)==FALSE){
      col.merge.cols=strsplit(col.merge,"[|]")[[1]]
      tox.table.2=data.frame(toxID=tox.table$toxID,category=tox.table$category,toxicity=tox.table$toxicity)
      for(side in 1:length(treats)){
        for(col in col.merge.cols){
          composition=as.numeric(strsplit(col,",")[[1]])
          if(length(composition)>1){
            tox.table.2[,paste0("tox.",side,".",paste0(composition,collapse=""))]=apply(tox.table[,paste0("tox.", side,".",composition)],1,function(x) {sum(as.numeric(x))} )
          } else {
            tox.table.2[,paste0("tox.", side,".",composition)]=tox.table[,paste0("tox.", side,".",composition)]
          }
        }
      }
      tox.table=tox.table.2
    }

    # Perform the row merge for categories
    if(is.null(category.merge)==FALSE){
      for(category in category.merge){
        if(category %in% tox.table$category){
          merge.rows=which(category==tox.table$category)
          di=dim(tox.table)
          tox.table[merge.rows[1],4:di[2]]=apply(tox.table[merge.rows,4:di[2]],2,function(x) {sum(as.numeric(x))})
          tox.table$toxicity[merge.rows[1]]=""
          tox.table=tox.table[-merge.rows[2:length(merge.rows)],]

        }
      }
    }

    # Order according to toxID (ensuring that the toxicities are ordered alphabetically first by category and then by toxicity)
    tox.table=tox.table[order(as.numeric(as.character(tox.table$toxID)),decreasing = FALSE),]
    tox.table=tox.table[which(tox.table$toxicity!=""),]

    # drop toxID as not needed after ordering
    tox.table$toxID=NULL

    # remove duplication of writing categories (one under the other before)
    a=dim(tox.table)[1]
    if(a>=2){
      for(i in a:2){
        if(tox.table$category[i-1]==tox.table$category[i]){tox.table$category[i]=""}
      }
    }

    # print the resulting table to R
    cat("Cycle:",cycles,"\n")
    print(tox.table,row.names=F)
    cat("\n################################################################\n")

    # return the table to the app
    return(tox.table)
  } else {
    cat("\n################################################################\n")
  }
}
# end





