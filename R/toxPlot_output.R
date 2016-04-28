
#' Toxixicity plot output to png and pdf
#'
#' This function acts as a wrapper to output png and pdf plots for toxPlot.
#'
#'
#' @param toxDB The robustToxicities object to plot
#' @param pageLimit The max number of row to add per page
#' @param type One of "pdf" or "png"
#' @param width The width of the plot output. It is recommended to use 9 for pdf and 1100 for png
#' @param height The height of the plot output. If se to NULL this is automatically calculated based on the pageSize.
#'
#' @export toxPlot_output

toxPlot_output = function(toxDB, pageLimit = 20, type = "pdf", width = 9, height = NULL) {

  rows = toxPlot(toxDB, plot = FALSE)
  print(rows)
  pages = ceiling(rows/pageLimit)
  new_height = height
print(pages)
  if(type == "pdf") {
    for(i in 1:pages) {
      print(i)
      onPage = min(rows -(i-1)*pageLimit, pageLimit)
      if(is.null(height)){
        new_height = 1.5 + onPage*0.25
      }
      next.rows = (i-1)*pageLimit + c(1,pageLimit)
      print("hi")
      print( next.rows)
      pdf(paste0(toxDB@options@trialName,"_plot_pg",i,"_",Sys.Date(),".pdf"), width = width, height = new_height)
      toxPlot(toxDB, next.rows, plot = TRUE)
      dev.off()
    }
  } else if(type == "png") {
    for(i in 1:pages) {
      onPage = min(rows -(i-1)*pageLimit, pageLimit)
      if(is.null(height)){
        new_height = 45 + onPage*26
      }
      next.rows = (i-1)*pageLimit + c(1,pageLimit)
      print(new_height)
      print(onPage)
      print(width)
      png(paste0(toxDB@options@trialName,"_plot_pg",i,"_",Sys.Date(),".png"), width = width, height = new_height)
      par(cex=1.5, cex.axis = 1)
      toxPlot(toxDB, next.rows, plot = TRUE)
      dev.off()
    }
  }
}
