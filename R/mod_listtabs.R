# UI модуля: повертає контейнер з підвкладками
mod_listtabs_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tabs"))
}

# Server модуля:
#  - r_list: reactive(), що повертає list[predictor] -> list[iteration] -> int[]
#  - predictors_order: вектор порядку підвкладок (наприклад, PREDICTOR_ORDER)
mod_listtabs_server <- function(id, r_list, predictors_order) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$tabs <- renderUI({
      x <- r_list()
      x <- ensure_names(x, predictors_order)
      preds <- intersect(predictors_order, names(x))
      tabs <- lapply(preds, function(pred) {
        tabPanel(pred, tableOutput(ns(paste0("tbl_", idify(pred)))))
      })
      if (length(tabs) == 0) return(tags$em("Немає даних"))
      do.call(tabsetPanel, tabs)
    })

    observe({
      x <- r_list()
      x <- ensure_names(x, predictors_order)
      preds <- intersect(predictors_order, names(x))
      for (pred in preds) local({
        p <- pred
        out_id <- paste0("tbl_", idify(p))
        output[[out_id]] <- renderTable({
            data.frame(
                Iteration = seq_along(x[[p]]),
                Indices   = sapply(x[[p]], collapse_indices),
                stringsAsFactors = FALSE
            )
        })
      })
    })
  })
}
