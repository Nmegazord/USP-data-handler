library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(shinyalert)
library(AmbrDataImporter)
library(shinyWidgets)
library(openxlsx)

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
              pickerInput("data_to_view", "Select data to preview", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
              pickerInput("culture_station_filter", "Filter by culture station", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
              pickerInput("vessel_number_filter", "Filter by vessel number", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
              pickerInput("vessel_id_filter", "Filter by vessel ID", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
              downloadButton("downloadData", "Download as Excel"),
              uiOutput("plot")
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

      # Update the selectizeInput choices
      updatePickerInput(session, "data_to_view", choices = names(imported_data))
      updatePickerInput(session, "culture_station_filter", choices = unique(unlist(lapply(imported_data, function(x) unique(x$culture_station)))), selected = NULL)
      updatePickerInput(session, "vessel_number_filter", choices = unique(unlist(lapply(imported_data, function(x) unique(x$vessel_number)))), selected = NULL)
      updatePickerInput(session, "vessel_id_filter", choices = unique(unlist(lapply(imported_data, function(x) unique(x$vessel_id)))), selected = NULL)
    })
  })

  output$plot <- renderUI({
    req(input$data_to_view)

    plotly::plotlyOutput(paste0("plot_", input$data_to_view))
  })

  observe({
    req(input$data_to_view)

    for (i in seq_along(input$data_to_view)) {
      local({
        j <- i
        output[[paste0("plot_", input$data_to_view[j])]] <- renderPlotly({
          df <- my_data()[[input$data_to_view[j]]]

          # Apply filters
          if (!is.null(input$culture_station_filter) && length(input$culture_station_filter) > 0) {
            df <- df[df$culture_station %in% input$culture_station_filter,]
          }
          if (!is.null(input$vessel_number_filter) && length(input$vessel_number_filter) > 0) {
            df <- df[df$vessel_number %in% input$vessel_number_filter,]
          }
          if (!is.null(input$vessel_id_filter) && length(input$vessel_id_filter) > 0) {
            df <- df[df$vessel_id %in% input$vessel_id_filter,]
          }

          plotly::plot_ly(df, x = ~time_from_inoculation, y = ~value, color = ~vessel_id, type = "scatter", mode = "lines") %>%
            layout(title = input$data_to_view[j])
        })
      })
    }
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

shinyApp(ui, server)
