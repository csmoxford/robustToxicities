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
      textInput("plotStartTreatment", "Variable containing date treatment started", value = values$defaultOptions@plotStartTreatment),
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
        p(textInput("cycleCycleMerge", "Cycles to merge", value = values$defaultOptions@cycleCycleMerge)),
        uiOutput("listingUI2"),
        p(selectInput("worst", label = "List by time period", choices = c("worst", "all"), selected = "worst")),
        p(selectInput("skipbase", label = "Discard baseline toxicities", choices = c(TRUE, FALSE), selected = FALSE)),
        p(textInput("cycleColumnMerge", "Column merge", value = values$defaultOptions@cycleColumnMerge)),
        p(selectInput("cycleCategoryMerge", label = "Merge Categories", choices = unique(ls.cat), selected = values$defaultOptions@cycleCategoryMerge, multiple = T)),
        buttonInput(id = "table.update", class = "btn action-button btn-large btn-success", 'Update table')
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
        buttonInput(id = "sumTableUpdate", class = "btn action-button btn-large btn-success", 'Update table')
      )
    )
  })

  output$uiSave = renderUI({
    wellPanel(
      div(
        class = "text-center",
        textInput("outputFolder", "Folder to output tables to", value = values$defaultOptions@outputFolder),
        buttonInput(id = "tableSave", class = "btn action-button btn-large btn-warning", 'Save all tables'),
        h3("Save options"),
        textInput("optionsFolder", "Folder to output options to", value = values$defaultOptions@outputFolder),
        textInput("optionsFileName", "File name of options file", value = paste0(values$defaultOptions@trialName,"_toxicityOtiopns.rData")),
        buttonInput(id="optionsSave", class = "btn action-button btn-large btn-success", 'Save options')
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
    # print(test)
    for(var in test){
      if(!is.null(input[[var]])){
        isolate({
          if(!is.null(values$options)){
            # print(var)
            # print(class(slot(values$options,var)))
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

  # ui for merging cycles and viewing certain plots
  listingUI = observe({
    if (!is.null(input$toxicityDBUpdate)) {
      if (input$toxicityDBUpdate>0) {
        if (!is.null(values$toxDB)) {
          isolate({
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
          })
        }
      }
    }
  })

  # display choices for viewed cycles table
  selectCycle = observe({
    if (is.null(input$cycleCycleMerge) == FALSE) {
      values$cycleMerge = strsplit(input$cycleCycleMerge, "[|]")[[1]]
      output$listingUI2 = renderUI({
        selectInput("view.tab", label = "View generated table", choices = 1:length(values$cycleMerge), selected = 1)
      })
    }
  })

  ############################################################################################
  # Render the database
  output$toxDB = renderDataTable({
    if (!is.null(values$toxDB)) {
      if ("ae_system" %in% names(values$toxDB@cleanData) & "ae" %in% names(values$toxDB@cleanData)) {
        return(values$toxDB@cleanData[c("patid", "ae_cycle_occured", "ae_system", "ae_term", "ae", "ass_category", "ass_toxicity_disp", "ass_TRUE")])
      } else {
        return(values$toxDB@cleanData[c("patid", "ae_cycle_occured", "ae_term", "ass_category", "ass_toxicity_disp", "ass_TRUE")])
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

            cycleLabels = cycleLabels=c("Baseline", "Cycle 1", "Cycle 2", "Cycle 3")

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
      }
    }
  })

  ############################################################################################
  # display requesting by time period table
  tableCycle = observe({
    if (!is.null(input$table.update)) {
      if (input$table.update>0) {
        isolate({
          # cycles to build table
          cycles = strsplit(values$cycleMerge[as.numeric(input$view.tab)], ", ")[[1]]
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
  # display summary toxicity data
  tableSummary = observe({
    if (!is.null(input$sumTableUpdate)) {
      if (input$sumTableUpdate>0) {
        isolate({
          if (!is.null(values$toxDB)) {
            output$summary = renderTable({ toxTable_summary(values$toxDB)}, digits = 0, include.rownames = FALSE)
          } else {
            message("No matched data in database, table not created")
            message("################################################################")
          }
        })
      }
    }
  })


  tableSave = observe({
    if (!is.null(input$tableSave)) {
      if (input$tableSave>0) {
        isolate({

          if (!is.null(values$toxDB)) {
            if (file.exists(input$outputFolder)) {
              rtfFile <- RTF(paste0(input$outputFolder, "/", input$trialName, "_toxity_summary.doc")) # this can be an .rtf or a .doc

              sumTable =  toxTable_summary(values$toxDB)


              fname = paste0(input$trialName, "_ToxicityTables", ".csv")
              write.csv(sumTable, paste0(input$outputFolder, "/", fname), row.names = F)
              addTable(rtfFile, sumTable, NA.string = "")

              message("Saving toxicity summary:")
              message("Filename: ", fname)

              cycleMerge = strsplit(values$toxDB@options@cycleCycleMerge, "[|]")[[1]]
              for(i in 1:length(cycleMerge)) {
                # cycles to build table
                cycles = strsplit(cycleMerge[i], ", ")[[1]]
                tox.table =  toxTable_cycle(values$toxDB, cycles=cycles)
                cycles = paste0(cycles, collapse = "")
                write.csv(tox.table, paste0(input$outputFolder, "/", input$trialName, "_cycle_", cycles, ".csv"), row.names = F)
                message("Filename:", paste0(input$trialName, "_cycle_", cycles, ".csv"))
                addNewLine(rtfFile, n=1)
                addParagraph(rtfFile, paste("Toxicities Cycle:", cycles, ""))
                addTable(rtfFile, tox.table, NA.string = "")
              }

              done(rtfFile)
              message("################################################################")
            } else {
              message("Folder path does not exist no files created")
            }
          }

        })
      }
    }
  })

  ############################################################################################
  # generate the plot time data
  plotToxicity = observe({
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

  saveOptions = observe({
    if (!is.null(input$optionsSave)) {
      if(input$optionsSave > 0) {
        isolate({
          print(input$optionsFolder)
          print(file.exists(input$optionsFolder))
          print(paste0(input$optionsFolder,"/",input$optionsFileName))
          if(file.exists(input$optionsFolder)) {
            options = values$toxDB@options
            print(options)
            save(options, file=paste0(input$optionsFolder,"/",input$optionsFileName))
            message("Toxicity options saved to file: ", input$optionsFileName)
            message("Folder: ", input$optionsFolder)
          }
        })
      }
    }
  })


  # End of server program
})
# End of file. That wasn't so bad was it?
