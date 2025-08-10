server <- function(input, output, session) {
  res <- reactive({
    req(input$file)
    # process_data(path) має бути в kod27.08.R
    process_data(input$file$datapath)
  })

  # LD50
  output$tbl_ld50 <- renderTable({
    out <- res()$ld50
    validate(need(!is.null(out) && nrow(out) > 0, "Немає даних LD50"))
    out
  })

  # Модулі з підвкладками
  mod_listtabs_server("fn", reactive(res()$false_negatives), PREDICTOR_ORDER)
  mod_listtabs_server("fp", reactive(res()$false_positives), PREDICTOR_ORDER)
  mod_listtabs_server("ti", reactive(res()$test_indices),    PREDICTOR_ORDER)
  mod_listtabs_server("ts", reactive(res()$test_samples),    PREDICTOR_ORDER)

  # Error Counts — зведемо у таблицю сумарних помилок на предиктор
  output$tbl_err <- renderTable({
    ec_raw <- res()$error_counts
    preds <- names(ec_raw)
    validate(need(length(preds) > 0, "Немає даних Error Counts"))
    data.frame(
      Predictor   = preds,
      Error_Count = sapply(ec_raw, function(v) {
        n <- length(v)
        if (n > 0) sum(as.numeric(v), na.rm = TRUE) / n else NA_real_
      }),

      stringsAsFactors = FALSE
    )
  })

  # Експорт в один CSV
  output$download_all <- downloadHandler(
    filename = function() paste0("all_results-", Sys.Date(), ".csv"),
    contentType = "text/csv",
    content = function(file) {
      r <- res()

      ld50 <- r$ld50
      names(ld50) <- c("Predictor", "LD50")

      preds <- unique(c(
        ld50$Predictor,
        names(r$false_negatives %||% list()),
        names(r$false_positives %||% list()),
        names(r$test_indices   %||% list()),
        names(r$test_samples   %||% list()),
        names(r$error_counts   %||% list())
      ))
      preds <- intersect(PREDICTOR_ORDER, preds)

      out <- data.frame(
        Predictor       = preds,
        LD50            = NA_real_,
        False_Negatives = NA_character_,
        False_Positives = NA_character_,
        Test_Indices    = NA_character_,
        Test_Samples    = NA_character_,
        Error_Count     = NA_real_,
        stringsAsFactors = FALSE
      )

      if (!is.null(ld50) && nrow(ld50) > 0) {
        out$LD50 <- ld50$LD50[match(out$Predictor, ld50$Predictor)]
      }
      out$False_Negatives <- sapply(out$Predictor, function(p) collapse_list_for_predictor(r$false_negatives, p))
      out$False_Positives <- sapply(out$Predictor, function(p) collapse_list_for_predictor(r$false_positives, p))
      out$Test_Indices    <- sapply(out$Predictor, function(p) collapse_list_for_predictor(r$test_indices,    p))
      out$Test_Samples    <- sapply(out$Predictor, function(p) collapse_list_for_predictor(r$test_samples,    p))
      out$Error_Count <- sapply(out$Predictor, function(p) {
        if (is.null(r$error_counts) || is.null(r$error_counts[[p]])) return(NA_real_)
        v <- as.numeric(r$error_counts[[p]])
        n <- length(v)
        if (n > 0) sum(v, na.rm = TRUE) / n else NA_real_
      })
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}
