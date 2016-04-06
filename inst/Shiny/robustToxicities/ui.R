library(shiny)
options(shiny.maxRequestSize = 20*1024^2)

shinyUI(fluidPage(
  theme="slate_bootstrap.css",
  div(
    style="text-align:center;",
    titlePanel("Toxicity cleaning, and presentation of data")
  ),
  div(
    tabsetPanel(
      tabPanel(
        "Load data and validation",
        fluidRow(
          column(
            width = 3,
            br(),
            uiOutput("uiLoad")

          ),
          column(
            width = 9,
            h2("Trial toxicities database"),
            dataTableOutput('toxDB')
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
                uiOutput("uiPlot")
                ),
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
                uiOutput("uiSummary")
                ),
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
                uiOutput("uiTimePeriod")
                ),
              column(
                width = 9,
                p(),
                tableOutput("listing")
              )
            )
          ),
          tabPanel(
            "Save tables",
            fluidRow(
              column(
                width = 3,
                uiOutput("uiSave")
              )
            )
          )
        )
      )
    )
  )
))

