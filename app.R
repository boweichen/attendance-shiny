library(shiny)
library(readxl)
library(writexl)
library(dplyr)

ui <- fluidPage(
  titlePanel("Student Attendance Pivot Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file",
                label = "Upload Excel (.xlsx / .xls)",
                accept = c(".xlsx", ".xls")),
      helpText("Data reads starting from row 4 of the Excel file."),
      hr(),
      numericInput(
        inputId = "param_a",
        label = "The number of \"L\" should be considered as an \"E\"",
        value = 3,
        min = 0.1,
        max = 100,
        step = 0.1
      ),
      helpText("Formula: Total = E + floor(L / a)"),
      hr(),
      downloadButton("download_result", "Download Result (.xlsx)")
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
    req(df, "Formal Name" %in% colnames(df), "Code1" %in% colnames(df))
    
    res <- df %>%
      group_by(`Formal Name`) %>%
      summarise(
        E = sum(Code1 == "E", na.rm = TRUE),
        L = sum(Code1 == "L", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        floor_L_div_a = floor(L / input$param_a),
        Total = E + floor_L_div_a
      ) %>%
      select(-floor_L_div_a) %>%
      arrange(desc(Total)) %>%
      rename(Name = `Formal Name`)
    
    res
  })
  
  output$result_table <- renderTable({
    req(pivot_data())
    pivot_data()
  })
  
  output$download_result <- downloadHandler(
    filename = function() {
      paste0("attendance_pivot_result_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(pivot_data(), path = file)
    }
  )
}

shinyApp(ui = ui, server = server)

# shinylive::export(appdir = ".", destdir = "docs")
