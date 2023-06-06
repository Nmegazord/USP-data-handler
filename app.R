# UI
ui <- dashboardPage(
  dashboardHeader(title = "USP Data Handler"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Ambr Data", tabName = "upload", icon = icon("file"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              shinyalert::useShinyalert(),
              fileInput("folder", "Choose ZIP File containing the AuditData folder",
                        accept = c('application/zip', 'application/x-zip-compressed',
                                   'multipart/x-zip', 'application/x-compress', 'application/x-compressed',
                                   'application/gzip')),
              actionButton("submit", "Submit"),
              selectInput("data_to_view", "Select data to preview", choices = ""),
              tableOutput("contents"),
              downloadButton("downloadData", "Download as Excel"),
              plotlyOutput("plot")
      )
    )
  )
)

options(shiny.maxRequestSize = 300*1024^2)

# Server
server <- function(input, output, session) {
  my_data <- reactiveVal(list())

  observeEvent(input$submit, {
    req(input$folder)

    shinyjs::runjs("Shiny.setInputValue('showSpinner', true)")

    withProgress(message = 'Importing Data...', {
      tmpdir <- tempdir()
      unzip(input$folder$datapath, exdir = tmpdir)

      subdirs <- list.dirs(tmpdir, recursive = FALSE)
      while (length(subdirs) > 0 && length(list.files(subdirs[1])) == 0) {
        tmpdir <- subdirs[1]
        subdirs <- list.dirs(tmpdir, recursive = FALSE)
      }

      if (length(list.files(tmpdir)) == 0) {
        shinyalert::shinyalert("Error", "No files found in the provided directory.", type = "error")
        return()
      }

      imported_data <- tryCatch(
        import_ambr_audit_data(tmpdir),
        error = function(e) {
          shinyalert::shinyalert("Error", paste("Error: ", e$message), type = "error")
          return(NULL)
        }
      )

      if (is.null(imported_data)) {
        shinyalert::shinyalert("Error", "Failed to import data.", type = "error")
        return()
      }

      shinyjs::runjs("Shiny.setInputValue('showSpinner', false)")

      shinyalert::shinyalert("Success", "Data imported successfully.", type = "success")

      my_data(imported_data)

      # Update the selectInput choices
      updateSelectInput(session, "data_to_view", choices = names(imported_data))
    })
  })

  output$contents <- renderTable({
    # Display selected data frame
    req(input$data_to_view)
    head(my_data()[[input$data_to_view]], n = 5)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      withProgress(message = 'Exporting Data...', {
        write_to_excel(my_data(), file)
      })
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
