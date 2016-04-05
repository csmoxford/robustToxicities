library(shiny)
options(shiny.maxRequestSize = 20*1024^2)

shinyUI(fluidPage(theme="slate_bootstrap.css",
                  div(
                    style="text-align:center;",
                    titlePanel("Toxicity cleaning, and presentation of data")
                  ),
                  uiOutput("toxicities.UI")
))

