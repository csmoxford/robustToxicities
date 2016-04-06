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
load("defaults/robustToxicities_LINES_defaults.rData")
source("Programs/uiFunctions.R")



############################################################################################
############################################################################################
############################################################################################
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ############################################################################################
  # reactive list to store all data to
  values = reactiveValues(
    defaultOptions = options,
    options = options
    )

  ############################################################################################
  # reset app on close
  session$onSessionEnded(function() {
    stopApp()
  })


  ############################################################################################
  # update application defaults
  change.defaults = observe({
    if (!is.null(input$options)) {
      isolate({
        load(input$options$folderPath)
        values$defaultOptions = options
        print(options)
        message("Options changed to:", input$options$name)
        message("################################################################")

      })
    }
  })

  ############################################################################################
  ############################################################################################
  ############################################################################################
  # Applications ui (it's in server so defaults work!)
  output$uiLoad = renderUI({
    wellPanel(
      div(
        class = "text-center",
        fileInput("options", "Load application defaults for trial", accept = c(".rData")),
        textInput("trialName", "Trial name (acronym)", value = values$defaultOptions@trialName),
        textInput("folderPath", "Path to file", value = values$defaultOptions@folderPath),
        uiOutput("chooseData"),
        p(),
        buttonInput(id = "toxicityDBUpdate", class = "btn action-button btn-large btn-success", 'load database')
      )
    )
  })

  output$uiPlot = renderUI({
    wellPanel(
      uiOutput("plotUI"),
      numericInput_small("plotxMin", "Minimum day on x-axis", value = values$defaultOptions@plotxMin),
      numericInput_small("plotxMax", "Maximum day on x-axis", value = values$defaultOptions@plotxMax),
      numericInput_small("plotCycleLength", "Cycle length", value = values$defaultOptions@plotCycleLength),
      numericInput_small("plotPxHeight", "Plot pixel height", value = values$defaultOptions@plotPxHeight),
      numericInput_small("plotPxWidth", "Plot pixel width", value = values$defaultOptions@plotPxWidth),
      buttonInput(id = "plotUpdate", class = "btn action-button btn-large btn-success", 'Update plot')
    )
  })

  output$uiTimePeriod = renderUI({
    wellPanel(
      div(
        class = "text-center",
        uiOutput("listingUI1"),
        p(textInput("cycle.merge", "Cycles to merge", value = values$defaultOptions@cycleCycleMerge)),
        uiOutput("listingUI2"),
        p(selectInput("worst", label = "List by time period", choices = c("worst", "all"), selected = "worst")),
        p(selectInput("skipbase", label = "Discard baseline toxicities", choices = c(TRUE, FALSE), selected = FALSE)),
        p(textInput("cycleColumnMerge", "Column merge", value = values$defaultOptions@cycleColumnMerge)),
        p(selectInput("cycleCategoryMerge", label = "Merge Categories", choices = unique(ls.cat), selected = values$defaultOptions@cycleCategoryMerge, multiple = T)),
        buttonInput(id = "table.update", class = "btn action-button btn-large btn-success", 'Update table'),
        textInputRow("listing.path", "Folder to output tables to", value = values$defaultOptions@outputFolder),
        buttonInput(id = "listing.table.update", class = "btn action-button btn-large btn-warning", 'Save tables')
      )
    )
  })

  output$uiSummary = renderUI({
    wellPanel(
      div(
        class = "text-center",
        uiOutput("sum.UI1"),
        p(textInput("sumCycleMerge", "Cycles to merge", value = values$defaultOptions@sumCycleMerge)),
        p(textInput("sumColumnMerge", "Column merge", value = values$defaultOptions@sumColumnMerge)),
        buttonInput(id = "sumTableUpdate", class = "btn action-button btn-large btn-success", 'Update table'),
        textInputRow("sum.path", "Folder to output tables to", value = values$defaultOptions@outputFolder),
        buttonInput(id = "sum.table.save", class = "btn action-button btn-large btn-warning", 'Save tables')
      )
    )
  })

  chooseData=observe({
    if(!is.null(input$folderPath)){
      if(input$folderPath!=""){
        isolate({
          values$fileList=list.files(path=input$folderPath)
          values$fileList=values$fileList[grepl(".csv",values$fileList) | grepl(".txt",values$fileList) | grepl(".dta",values$fileList)]
          output$chooseData=renderUI(selectInput("dataFile","Select data (.txt or .csv)", selected = values$defaultOptions@fileName,choices=values$fileList))
        })
      }
    }
  })

  updateoptions = observe({
    test=names(getSlots("toxicityOptions"))
    for(var in test){
      if(!is.null(input[[var]])){
        isolate({
          if(!is.null(values$options)){
            theClass = class(slot(values$options,var))
            if(!is.na(input[[var]])){
              if(slot(values$options,var) != input[[var]]) {
                slot(values$options,var) = input[[var]]
                if(!is.null(values$toxDB)){
                  slot(values$toxDB@options,var) = input[[var]]
                }
              }
            }
          }
        })
      }
    }
  })

  ############################################################################################
  # load and visually display Toxicity database in app
  loadData = observe({
    if (!is.null(input$toxicityDBUpdate)) {
      if (input$toxicityDBUpdate>0) {
        isolate({
          if (is.null(input$dataFile)) {
            return(NULL)
          } else {
            message("Adverse events database loaded from file:", input$dataFile)
            message("################################################################")
            values$file = input$dataFile
            # load database
            data = read.csv(paste0(input$folderPath,"\\",input$dataFile), stringsAsFactors = FALSE)
            # initial cleaning of database

            cycleLabels = data.frame(label=c("Baseline", "Cycle 1", "Cycle 2", "Cycle 3"), index=c(0, 1, 2, 3))

            values$toxDB = robustToxicities(data, cycleLabels, options = values$options)
            values$toxDB = prepareToxicity(values$toxDB)

            if (class(values$toxDB@cleanData$ass_TRUE) == "integer") {
              values$toxDB@cleanData$ass_TRUE = (values$toxDB@cleanData$ass_TRUE == 1)
            }
            if (input$toxicityDBUpdate>0) {
              vals.pat = unique(values$toxDB@cleanData$patid)
              vals.treat = unique(values$toxDB@cleanData$treatment)
              output$plotUI = renderUI({
                div(
                  selectInput("plot.patient", label = "Patients", choices = vals.pat, selected = vals.pat, multiple = T),
                  selectInput("plot.treat", label = "Treatments", choices = vals.treat, selected = vals.treat, multiple = T)
                )
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
    if (!is.null(input$toxicityDBUpdate)) {
      if (input$toxicityDBUpdate>0) {
        names_cycle = names(values$toxDB)[str_detect(names(values$toxDB), "cycle_start_date")]
        values$names_cycle_stub = sub("cycle_start_date", "", names_cycle)
        given_cycles = unique(values$toxDB@cleanData$ae_cycle_occured)
        output$list.cycles = renderText(unique(c(values$names_cycle_stub, given_cycles)))
        output$listingUI1 = renderUI({
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
      output$listingUI2 = renderUI({
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

              rtffile <- RTF(paste0(input$listing.path, "/", input$trialName, "_toxity_cycles.doc")) # this can be an .rtf or a .doc

              for(i in 1:length(values$plot.cycle.merge)) {
                # cycles to build table
                cycles = strsplit(values$plot.cycle.merge[i], ", ")[[1]]
                tox.table =  toxTable_cycle(values$toxDB, cycles=cycles)
                cycles = paste0(cycles, collapse = "")
                write.csv(tox.table, paste0(input$listing.path, "/", input$trialName, "_cycle_", cycles, ".csv"), row.names = F)
                message("Filename:", paste0(input$trialName, "_cycle_", cycles, ".csv"))
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
    if (!is.null(input$sumTableUpdate)) {
      if (input$sumTableUpdate>0) {
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
              rtffile <- RTF(paste0(input$sum.path, "/", input$trialName, "_toxity_summary.doc")) # this can be an .rtf or a .doc

              write.csv(sum.table, paste0(input$sum.path, "/", input$trialName, "_summary", ".csv"), row.names = F)
              addTable(rtffile, sum.table, NA.string = "")
              done(rtffile)
              message("Saving toxicity summary:")
              message("Filename:", paste0(input$trialName, "_summary", ".csv"))
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
    if (!is.null(input$plotUpdate)) {
      if (input$plotUpdate>0) {
        isolate({
          if (is.null(values$toxDB)) {
            message("Database not loaded")
          } else {
            message("Update plot with parameters")

            if (input$plotPxHeight == 0) {
              plotPxHeight = toxPlot_time(values$toxDB, patients = input$plot.patient, plot = FALSE)*18+180

            }else{
              plotPxHeight = input$plotPxHeight
            }
            if (input$plotPxWidth == 0) {
              plotPxWidth = 1100
            }else{
              plotPxWidth = input$plotPxWidth
            }
            output$Toxicity = renderPlot({isolate({toxPlot_time(values$toxDB, patients = input$plot.patient)})}, height = plotPxHeight, width = plotPxWidth)
          }
        })
      }
    }
  })



  # End of server program
})
# End of file. That wasn't so bad was it?
