ui <- fluidPage(
  titlePanel("Аналіз Predictors"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Оберіть CSV файл", accept = ".csv"),
      downloadButton("download_all", "Зберегти в Excel (.xlsx)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("LD50",           tableOutput("tbl_ld50")),
        tabPanel("False Negatives", mod_listtabs_ui("fn")),
        tabPanel("False Positives", mod_listtabs_ui("fp")),
        tabPanel("Test Indices",    mod_listtabs_ui("ti")),
        tabPanel("Test Samples",    mod_listtabs_ui("ts")),
        tabPanel("Error Counts",    tableOutput("tbl_err"))
      )
    )
  )
)
