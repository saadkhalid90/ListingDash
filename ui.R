library(shiny)
library(shinythemes)

## Define UI for rlisitng dashboard app
fluidPage(
  ## choosing a theme for shiny app
  theme = shinytheme("cerulean"),
  ## Application title
  titlePanel("Listing Report Generator"),
  
  ## UI for main panel (Tabs at the top)
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Cluster Level",
                         br(),
                         h4("Summary Download Tabs"),
                         bootstrapPage(
                           div(style="display:inline-block", downloadButton("ClustDwnld", label = "Consolidated report", width = 10)),
                           div(style="display:inline-block", downloadButton("CompClustDwnld", label = "Completed Clusters", width = 10)),
                           div(style="display:inline-block", downloadButton("ErrorClustDwnld", label = "Clusters (QA review)", width = 10))
                         ),
                         br(),
                         br(),
                         DT::dataTableOutput("tableClust")),
                tabPanel("District Level",
                         br(),
                         downloadButton("DistDwnld", label = "Download as .csv"),
                         br(),
                         DT::dataTableOutput("tableDist")),
                tabPanel("Cluster Level data",
                         br(),
                         uiOutput("ClustSelect"),
                         br(),
                         downloadButton("ClustDataDwnld", label = "Download as .csv"),
                         br(),
                         fluidRow(column(3, verbatimTextOutput("value"))),
                         br(),
                         DT::dataTableOutput("tableDataClust")),
                tabPanel("Cluster data (date-wise)",
                         br(),
                         dateInput("date_select", label = "Select Date", value = Sys.Date()),
                         br(),
                         DT::dataTableOutput("tabledateClust")),
                tabPanel("Eligible HHs",
                         br(),
                         uiOutput("ClustSelect1"),
                         br(),
                         DT::dataTableOutput("eligHH"))
    )
  )
)