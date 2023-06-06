library(shiny)
library(AmbrDataImporter)
library(plotly)

options(shiny.maxRequestSize = 300*1024^2)  # This sets the maximum file upload size to 300MB

server <- function(input, output, session) {
  my_data <- reactiveVal(NULL)
  messages <- reactiveVal(character())

  observeEvent(input$submit, {
    req(input$folder)

    # Unzip the uploaded file to a temp directory
    tmpdir <- tempdir()
    unzip(input$folder$datapath, exdir = tmpdir)

    # Check for subdirectories
    subdirs <- list.dirs(tmpdir, recursive = FALSE)

    # Keep looking for subdirectories until we find a directory with files
    while (length(subdirs) > 0 && length(list.files(subdirs[1])) == 0) {
      tmpdir <- subdirs[1]
      subdirs <- list.dirs(tmpdir, recursive = FALSE)
    }

    # If there are still no files, throw an error
    if (length(list.files(tmpdir)) == 0) {
      messages("No files found in the provided directory.")
      return()
    }

    # Pass the directory path to your function
    imported_data <- tryCatch(
      import_ambr_audit_data(tmpdir),
      error = function(e) {
        messages(paste("Error: ", e$message))
        return(NULL)
      }
    )

    if (is.null(imported_data)) {
      messages("Failed to import data.")
      return()
    }

    # Print the structure of the imported data to the console
    str(imported_data)
    messages("Data imported successfully.")

    my_data(imported_data)
  })

  output$messages <- renderPrint({
    messages()
  })

  # output$plot <- renderPlotly({
  #   req(my_data())
  #   plot_ly(my_data()$yourDataFrame, x = ~x_var, y = ~y_var, type = 'scatter', mode = 'markers')  # Replace with your actual data frame and variables
  # })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Call your function to write the data to an Excel file
      write_to_excel(my_data(), file)
    }
  )
}
