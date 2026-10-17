library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(base64enc)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('download_xlsx', function(message) {
        const bytes = Uint8Array.from(
          atob(message.data),
          c => c.charCodeAt(0)
        );

        const blob = new Blob([bytes], {
          type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
        });

        const url = URL.createObjectURL(blob);
        const link = document.createElement('a');

        link.href = url;
        link.download = message.filename;

        document.body.appendChild(link);
        link.click();
        link.remove();

        setTimeout(() => URL.revokeObjectURL(url), 10000);
      });
    "))
  ),
  
  titlePanel("Student Attendance Pivot Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "upload_file",
        label = "Upload Excel (.xlsx / .xls)",
        accept = c(".xlsx", ".xls")
      ),
      
      helpText("Data reads starting from row 4 of the Excel file."),
      
      hr(),
      
      numericInput(
        inputId = "param_a",
        label = 'The number of "L" should be considered as an "E"',
        value = 3,
        min = 0.1,
        max = 100,
        step = 0.1
      ),
      
      helpText("Formula: Total = E + floor(L / a)"),
      
      hr(),
      
      actionButton(
        "download_result",
        "Download Result (.xlsx)",
        icon = icon("download")
      )
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
)

server <- function(input, output, session) {
  
  raw_attendance <- reactive({
    req(input$upload_file)
    
    read_excel(
      path = input$upload_file$datapath,
      skip = 3
    )
  })
  
  pivot_data <- reactive({
    df <- raw_attendance()
    
    validate(
      need(
        all(c("Formal Name", "Code1") %in% colnames(df)),
        'The uploaded file must contain "Formal Name" and "Code1" columns.'
      )
    )
    
    req(input$param_a)
    validate(
      need(input$param_a > 0, "Please enter a number greater than zero.")
    )
    
    df %>%
      group_by(`Formal Name`) %>%
      summarise(
        E = sum(Code1 == "E", na.rm = TRUE),
        L = sum(Code1 == "L", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Total = E + floor(L / input$param_a)
      ) %>%
      arrange(desc(Total)) %>%
      rename(Name = `Formal Name`)
  })
  
  output$result_table <- renderTable({
    pivot_data()
  })
  
  observeEvent(input$download_result, {
    if (is.null(input$upload_file)) {
      showNotification(
        "Please upload an Excel file first.",
        type = "warning"
      )
      return()
    }
    
    data <- pivot_data()
    
    file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(file), add = TRUE)
    
    tryCatch(
      {
        writexl::write_xlsx(
          list(Attendance = as.data.frame(data)),
          path = file
        )
        
        session$sendCustomMessage(
          "download_xlsx",
          list(
            filename = paste0(
              "attendance_pivot_result_",
              Sys.Date(),
              ".xlsx"
            ),
            data = base64enc::base64encode(file)
          )
        )
      },
      error = function(e) {
        showNotification(
          paste("Excel download failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      }
    )
  })
}

shinyApp(ui = ui, server = server)