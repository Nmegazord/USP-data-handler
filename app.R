library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(shinyalert)
library(AmbrDataImporter)
library(shinyWidgets)
library(openxlsx)
library(devtools)

options(shiny.maxRequestSize = 300*1024^2)
shinyjs::useShinyjs()

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
              div(class = "well",
                  helpText("Choose a ZIP file containing the AuditData folder to upload and click Upload Data"),
                  fileInput("folder", "Choose ZIP File",
                            accept = c('application/zip', 'application/x-zip-compressed',
                                       'multipart/x-zip', 'application/x-compress', 'application/x-compressed',
                                       'application/gzip')),
                  actionButton("submit", "Analyze Data")),
              div(class = "well",
                  helpText("After submitting your file, select the data you want to preview. A graph will appear below."),
                  pickerInput("data_to_view", "Select data to preview", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
                  helpText("Filter the data by culture station, vessel number, or vessel ID"),
                  pickerInput("culture_station_filter", "Filter by culture station", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
                  pickerInput("vessel_number_filter", "Filter by vessel number", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
                  pickerInput("vessel_id_filter", "Filter by vessel ID", choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
              helpText("Check the box to display time in days instead of hours"),
              checkboxInput("time_in_days", "Display time in days", value = FALSE)),
              uiOutput("plot"),
              div(class = "well",
                helpText("Once you are done, you can download the entire dataset as an Excel file"),
                div(style = "text-align: right", # Align to the right
                  downloadButton("downloadData", "Download as Excel")))
      )
    )
  )
)





# Server
server <- function(input, output, session) {
  my_data <- reactiveValues(data = list())
  loading_data <- reactiveVal(FALSE)

  observeEvent(input$submit, {
    req(input$folder)

    loading_data(TRUE)
    shinyjs::disable("submit")

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

      my_data$data <- imported_data
      loading_data(FALSE)
      shinyjs::enable("submit")

      # Update the selectizeInput choices
      # Update the selectizeInput choices
      tryCatch({
        updatePickerInput(session, "data_to_view", choices = names(imported_data))

        culture_station_choices <- unique(unlist(lapply(imported_data, function(x) levels(x$culture_station))))
        updatePickerInput(session, "culture_station_filter", choices = culture_station_choices, selected = NULL)

        vessel_number_choices <- unique(unlist(lapply(imported_data, function(x) as.character(x$vessel_number))))
        updatePickerInput(session, "vessel_number_filter", choices = vessel_number_choices, selected = NULL)

        vessel_id_choices <- unique(unlist(lapply(imported_data, function(x) as.character(x$vessel_id))))
        updatePickerInput(session, "vessel_id_filter", choices = vessel_id_choices, selected = NULL)
      }, error = function(e) {
        print(paste("Error updating picker inputs:", e$message))
      })

    })
  })

  filtered_data <- reactive({
    req(input$data_to_view, my_data$data)
    apply_filters <- function(df, filter_values, column) {
      if (!is.null(filter_values) && length(filter_values) > 0) {
        df <- df[df[[column]] %in% filter_values,]
      }
      df
    }
    lapply(my_data$data[input$data_to_view], function(df) {
      df <- apply_filters(df, input$culture_station_filter, "culture_station")
      df <- apply_filters(df, input$vessel_number_filter, "vessel_number")
      df <- apply_filters(df, input$vessel_id_filter, "vessel_id")
      df
    })
  })

  output$plot <- renderUI({
    req(input$data_to_view)
    lapply(input$data_to_view, function(data_name) {
      plotly::plotlyOutput(paste0("plot_", data_name))
    })
  })

  observe({
    req(input$data_to_view, filtered_data())
    for (i in seq_along(input$data_to_view)) {
      local({
        j <- i
        output[[paste0("plot_", input$data_to_view[j])]] <- renderPlotly({
          df <- filtered_data()[[j]]

          # Add new column for labels
          df$label <- paste(df$culture_station, df$vessel_number)

          # Convert time to days if checkbox is checked
          if (input$time_in_days) {
            df$time_from_inoculation <- df$time_from_inoculation / 24
          }

          plotly::plot_ly(df, x = ~time_from_inoculation, y = ~value, color = ~label, type = "scatter", mode = "lines") %>%
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
        write_to_excel(my_data$data, file)
      })
    }
  )

  observe({
    req(loading_data())
    if (loading_data()) {
      shinyjs::disable(c("culture_station_filter", "vessel_number_filter", "vessel_id_filter", "data_to_view", "downloadData", "time_in_days"))
    } else {
      shinyjs::enable(c("culture_station_filter", "vessel_number_filter", "vessel_id_filter", "data_to_view", "downloadData", "time_in_days"))
    }
  })
}

shinyApp(ui, server)
