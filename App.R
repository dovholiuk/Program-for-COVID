library(shiny)
source("kod27.08.R")

ui <- fluidPage(
  titlePanel("Analize Predictors"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose BootSTR file", accept = ".csv"),
      downloadButton("download", "Save results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("LD50", tableOutput("table_ld50")),
        tabPanel("False Negatives", uiOutput("fn_tabs")),
        tabPanel("False Positives", uiOutput("fp_tabs"))
      )
    )
  )
)

server <- function(input, output) {
  results <- reactive({
    req(input$file)
    process_data(input$file$datapath)
  })

  # Predictors table
  output$table_ld50 <- renderTable({
    results()$ld50
  })

  # Save predictors button
  output$download <- downloadHandler(
    filename = function() {
      paste0("predictors_results-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )

# False Negatives
output$fn_tabs <- renderUI({
  fn <- results()$false_negatives
  tabs <- lapply(names(fn), function(pred) {
    tabPanel(pred, renderTable({
      
      data.frame(
        Iteration = seq_along(fn[[pred]]),
        Indices = sapply(fn[[pred]], function(x) paste(x, collapse = ", "))
      )
    }))
  })
  do.call(tabsetPanel, tabs)
})

# False Positives
output$fp_tabs <- renderUI({
  fp <- results()$false_positives
  tabs <- lapply(names(fp), function(pred) {
    tabPanel(pred, renderTable({
      data.frame(
        Iteration = seq_along(fp[[pred]]),
        Indices = sapply(fp[[pred]], function(x) paste(x, collapse = ", "))
      )
    }))
  })
  do.call(tabsetPanel, tabs)
})
}

shinyApp(ui = ui, server = server)