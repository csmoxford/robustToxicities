############################################################################################
# This file contains all server side code for the toxicities cleaning and analysis application.
# This also contains the bulk of the ui since defaults must be updated server side.
# Console Printouts will either read print() or message().
############################################################################################
# Required libraries for application to run
library(shiny)
library(stringr)
library(rtf)

source("Programs/toxicityFunctions.R")
source("defaults/LINES defaults.R")
source("Programs/uiFunctions.R")



############################################################################################
############################################################################################
############################################################################################
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

 ############################################################################################
 # reactive list to store all data to
 values = reactiveValues(defaults = defaults)

 ############################################################################################
 # reset app on close
 session$onSessionEnded(function() {
  stopApp()
 })


 ############################################################################################
 # update application defaults
 change.defaults = observe({
  input$defaults
  isolate({
   if (!is.null(input$defaults)) {
    source(input$defaults$datapath)
    values$defaults = defaults
    message("Defaults changed to:", input$defaults$name)
    message("################################################################")
   }
  })
 })

 ############################################################################################
 ############################################################################################
 ############################################################################################
 # Applications ui (it's in server so defaults work!)
 output$toxicities.UI = renderUI({
  div(
   tabsetPanel(
    tabPanel(
     "Load data and validation",
     fluidRow(
      column(
       width = 3,
       br(),
       wellPanel(
        div(
         class = "text-center",
         fileInput("defaults", "Load application defaults for trial", accept = c(".R")),
         textInputRow("trial.name", "Trial name (acronym)", value = values$defaults$trial.name),
         fileInput("trial.file", "Load Toxicity database for analysis", accept = c(".csv", ".txt")),
         p(),
         buttonInput(id = "toxicity.db.update", class = "btn action-button btn-large btn-success", 'load database')
        )
       )
      ),
      column(
       width = 9,
       h2("Trial toxicities database"),
       dataTableOutput('tox.db')
      )
     )
    ),
    tabPanel(
     "Tables and graphs",
     tabsetPanel(
      tabPanel(
       "Plot time data",
       fluidRow(
        column(
         width = 3,
         wellPanel(
          uiOutput("plotUI"),
          numericInput_small("plot.minday", "Minimum day on x-axis", value = values$defaults$plot.min.day),
          numericInput_small("plot.maxday", "Maximum day on x-axis", value = values$defaults$plot.max.day),
          numericInput_small("plot.cycle.length", "Cycle length", value = values$defaults$plot.cycle.length),
          numericInput_small("plot.height", "Plot pixel height", value = values$defaults$plot.px.height),
          numericInput_small("plot.width", "Plot pixel width", value = values$defaults$plot.px.width),
          buttonInput(id = "plot.update", class = "btn action-button btn-large btn-success", 'Update plot')
         )),
        column(
         width = 9,
         plotOutput("Toxicity")
        )
       )

      ),
      tabPanel(
       "Summary",
       fluidRow(
        column(
         width = 3,
         wellPanel(
          div(
           class = "text-center",
           uiOutput("sum.UI1"),
           p(textInput("sum.cycle.merge", "Cycles to merge", value = values$defaults$sum.cycle.merge)),
           p(textInput("sum.col.merge", "Column merge", value = values$defaults$sum.column.merge)),
           buttonInput(id = "sum.table.update", class = "btn action-button btn-large btn-success", 'Update table'),
           textInputRow("sum.path", "Folder to output tables to", value = values$defaults$sum.save.folder),
           buttonInput(id = "sum.table.save", class = "btn action-button btn-large btn-warning", 'Save tables')
          ))),
        column(
         width = 9,
         p(),
         tableOutput("summary")
        )
       )
      ),
      tabPanel(
       "By time period",
       fluidRow(
        column(
         width = 3,
         wellPanel(
          div(
           class = "text-center",
           uiOutput("listing.UI1"),
           p(textInput("cycle.merge", "Cycles to merge", value = values$defaults$cycle.cycle.merge)),
           uiOutput("listing.UI2"),
           p(selectInput("worst", label = "List by time period", choices = c("worst", "all"), selected = "worst")),
           p(selectInput("skipbase", label = "Discard baseline toxicities", choices = c(TRUE, FALSE), selected = FALSE)),
           p(textInput("col.merge", "Column merge", value = values$defaults$cycle.column.merge)),
           p(selectInput("merge.categories", label = "Merge Categories", choices = unique(ls.cat), selected = NULL, multiple = T)),
           buttonInput(id = "table.update", class = "btn action-button btn-large btn-success", 'Update table'),
           textInputRow("listing.path", "Folder to output tables to", value = values$defaults$cycle.save.folder),
           buttonInput(id = "listing.table.update", class = "btn action-button btn-large btn-warning", 'Save tables')
          ))),
        column(
         width = 9,
         p(),
         tableOutput("listing")
        )
       )
      )
     )
    )
   )
  )
 })



 ############################################################################################
 # load and visually display Toxicity database in app
 tox.db = observe({
  if (!is.null(input$toxicity.db.update)) {
   if (input$toxicity.db.update>0) {
    isolate({
     if (is.null(input$trial.file)) {
      return(NULL)
     } else {
      message("Adverse events database loaded from file:", input$trial.file$name)
      message("################################################################")
      values$file = input$trial.file
      # load database
      data = read.csv(input$trial.file$datapath, stringsAsFactors = F)
      # initial cleaning of database

      options = defaultToxicityOptions(trialName = "LINES", folderPath = "I:/Data/MSG Support/projects/EuroSarc/LINES/Data Management/Data/03Jul2015_testing/Open clinica data/Stats final/For R", outputFolder = "C:/Users/pdutton/Desktop/New folder/toxicities")
      timeType = "time"
      cycleLabels = labels(c("Baseline", "Cycle 1", "Cycle 2", "Cycle 3"), c(0, 1, 2, 3))
      treatmentLabels = labels(c("Linsitinib"), 1)

      values$toxDB = robustToxicities(data, cycleLabels, options, treatmentLabels)
      values$toxDB = prepareToxicity(values$toxDB)

      if (class(values$toxDB@cleanData$ass_TRUE) == "integer") {
       values$toxDB@cleanData$ass_TRUE = (values$toxDB@cleanData$ass_TRUE == 1)
      }

      if (input$toxicity.db.update>0) {
       output$plotUI = renderUI({

        vals.pat = unique(values$toxDB@cleanData$patid)
        vals.treat = unique(values$toxDB@cleanData$treatment)

        div(selectInput("plot.patient", label = "Patients", choices = vals.pat, selected = vals.pat, multiple = T),
          selectInput("plot.treat", label = "Treatments", choices = vals.treat, selected = vals.treat, multiple = T))

       })
      }
     }
    })
   }}
 })

 output$tox.db = renderDataTable({
  if (!is.null(values$toxDB)) {
   if ("ae_system" %in% names(values$toxDB@cleanData) & "ae" %in% names(values$toxDB@cleanData)) {
    return(values$toxDB@cleanData[c("patid", "ae_cycle_occured", "ae_system", "ae_term", "ae", "ass_category", "ass_toxicity_disp", "ass_TRUE")])
   } else {
    return(values$toxDB@cleanData[c("patid", "ae_cycle_occured", "ae_term", "ass_category", "ass_toxicity_disp", "ass_TRUE")])
   }
  }
 })


 ############################################################################################
 # ui for merging cycles and viewing certain plots
 listing.UI = observe({
  if (!is.null(input$toxicity.db.update)) {
   if (input$toxicity.db.update>0) {
    names_cycle = names(values$toxDB)[str_detect(names(values$toxDB), "cycle_start_date")]
    values$names_cycle_stub = sub("cycle_start_date", "", names_cycle)
    given_cycles = unique(values$toxDB@cleanData$ae_cycle_occured)
    output$list.cycles = renderText(unique(c(values$names_cycle_stub, given_cycles)))
    output$listing.UI1 = renderUI({
     div(
      p("Names of time points / cycles or time periods:"),
      textOutput("list.cycles")
     )
    })
    output$sum.cycles = renderText(unique(c(values$names_cycle_stub, given_cycles)))
    output$sum.UI1 = renderUI({
     div(
      p("Names of time points / cycles or time periods:"),
      textOutput("sum.cycles")
     )
    })
   }
  }
 })

 ############################################################################################
 # display choices for viewed table
 linked.listing.inputs = observe({
  if (is.null(input$cycle.merge) == FALSE) {
   values$plot.cycle.merge = strsplit(input$cycle.merge, "[|]")[[1]]
   output$listing.UI2 = renderUI({
    selectInput("view.tab", label = "View generated table", choices = 1:length(values$plot.cycle.merge), selected = 1)
   })
  }
 })

 ############################################################################################
 # display requesting by time period table
 table.listing.out = observe({
  if (!is.null(input$table.update)) {
   if (input$table.update>0) {
    isolate({
     # cycles to build table
     cycles = strsplit(values$plot.cycle.merge[as.numeric(input$view.tab)], ", ")[[1]]
     if (!is.null(values$toxDB)) {
      tox.table =  toxTable_cycle(values$toxDB, cycles=cycles)
      output$listing = renderTable({tox.table}, digits = 0, include.rownames = FALSE)
     } else {
      message("No matched data in database, table not created")
      message("################################################################")
     }
    })
   }
  }
 })

 ############################################################################################
 # save all by time period
 table.listing.save = observe({
  if (!is.null(input$listing.table.update)) {
   if (input$listing.table.update>0) {
    isolate({
     if (!is.null(values$toxDB)) {
      # save tables to csv
      if (file.exists(input$listing.path)) {
        message("Saving toxicity listing by cycle:")

        rtffile <- RTF(paste0(input$listing.path, "/", input$trial.name, "_toxity_cycles.doc")) # this can be an .rtf or a .doc

        for(i in 1:length(values$plot.cycle.merge)) {
         # cycles to build table
         cycles = strsplit(values$plot.cycle.merge[i], ", ")[[1]]
         tox.table =  toxTable_cycle(values$toxDB, cycles=cycles)
         cycles = paste0(cycles, collapse = "")
         write.csv(tox.table, paste0(input$listing.path, "/", input$trial.name, "_cycle_", cycles, ".csv"), row.names = F)
         message("Filename:", paste0(input$trial.name, "_cycle_", cycles, ".csv"))
         addParagraph(rtffile, paste("Toxicities Cycle:", cycles, ""))
         addTable(rtffile, tox.table, NA.string = "")
        }
        done(rtffile)

      } else {
       message("Folder path does not exist no files created")
      }
      message("################################################################")
     } else {
       message("No matched data in database, table not created")
     }
    })
   }
  }
 })

 ############################################################################################
 # display worst toxicity by time period table
 table.sum.out = observe({
   if (!is.null(input$sum.table.update)) {
     if (input$sum.table.update>0) {
       isolate({

         if (!is.null(values$toxDB)) {
           output$summary = renderTable({ toxTable_summary(values$toxDB)})
         } else {
           message("No matched data in database, table not created")
           message("################################################################")
         }
       })
     }
   }
 })

 ############################################################################################
 # Save all worst toxicity summary
 table.sum.save = observe({
   if (!is.null(input$sum.table.save)) {
     if (input$sum.table.save>0) {
       isolate({
         if (!is.null(values$toxDB)) {
           sum.table =  toxTable_summary(values$toxDB)
           if (file.exists(input$sum.path)) {
             rtffile <- RTF(paste0(input$sum.path, "/", input$trial.name, "_toxity_summary.doc")) # this can be an .rtf or a .doc

             write.csv(sum.table, paste0(input$sum.path, "/", input$trial.name, "_summary", ".csv"), row.names = F)
             addTable(rtffile, sum.table, NA.string = "")
             done(rtffile)
             message("Saving toxicity summary:")
             message("Filename:", paste0(input$trial.name, "_summary", ".csv"))
           } else {
             message("Folder path does not exist no files created")
           }
           message("################################################################")
         }
       })
     }
   }
 })

 ############################################################################################
 # generate the plot time data
 plot.toxicity = observe({
  if (!is.null(input$plot.update)) {
   if (input$plot.update>0) {
    isolate({
     if (is.null(values$toxDB)) {
      message("Database not loaded")
     } else {
      message("Update plot with parameters")

      if (input$plot.height == 0) {
       plot.height = toxTable_cycle(values$toxDB, dayRange = c(input$plot.minday, input$plot.maxday), plot = FALSE)*18+180

      }else{
       plot.height = input$plot.height
      }
      if (input$plot.width == 0) {
       plot.width = 1100
      }else{
       plot.width = input$plot.width
      }
      output$Toxicity = renderPlot({isolate({toxTable_cycle(values$toxDB, dayRange = c(input$plot.minday, input$plot.maxday), plotCycleLength = input$plot.cycle.length)})}, height = plot.height, width = plot.width)
     }
     message("################################################################")

    })
   }
  }
 })



# End of server program
})
# End of file. That wasn't so bad was it?
