library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Ambr Data Importer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("file"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fileInput("folder", "Choose ZIP File",
                        accept = c('application/zip', 'application/x-zip-compressed',
                                   'multipart/x-zip', 'application/x-compress', 'application/x-compressed',
                                   'application/gzip')),
              actionButton("submit", "Submit"),
              verbatimTextOutput("messages"),
              tableOutput("contents"),
              downloadButton("downloadData", "Download as Excel"),
              plotlyOutput("plot")
      )
    )
  )
)

