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

 output$download_all <- downloadHandler(
  filename = function() paste0("all_results-", Sys.Date(), ".xlsx"),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    # Пакет для запису Excel
    library(writexl)

    r <- res()  # результат process_data()

    # ---- 1) LD50 ----
    ld50 <- r$ld50
    if (!is.null(ld50) && nrow(ld50) > 0) {
      names(ld50) <- c("Predictor","LD50")
    } else {
      ld50 <- data.frame(Predictor=character(), LD50=numeric())
    }

    # ---- 2) Error Counts — середнє за ітерацію ----
    ec_raw <- r$error_counts
    err_df <- if (!is.null(ec_raw) && length(ec_raw) > 0) {
      data.frame(
        Predictor   = names(ec_raw),
        Error_Count = sapply(ec_raw, function(v) {
          v <- as.numeric(v); n <- length(v); if (n > 0) sum(v, na.rm = TRUE)/n else NA_real_
        }),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Predictor=character(), Error_Count=numeric())
    }

    # ---- 3) Хелпер: list[predictor]->list[it]->indices -> довга таблиця ----
    list_to_long <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(data.frame(Predictor=character(), Iteration=integer(), Indices=character()))
      }
      preds <- names(x); if (is.null(preds)) preds <- rep("All", length(x))
      do.call(rbind, lapply(seq_along(x), function(i){
        elem <- x[[i]]
        if (is.list(elem)) {
          data.frame(
            Predictor = preds[i],
            Iteration = seq_along(elem),
            Indices   = sapply(elem, function(v) paste(as.character(v), collapse = ", ")),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Predictor = preds[i],
            Iteration = NA_integer_,
            Indices   = paste(as.character(elem), collapse = ", "),
            stringsAsFactors = FALSE
          )
        }
      }))
    }

    fn_long <- list_to_long(r$false_negatives)
    fp_long <- list_to_long(r$false_positives)
    ti_long <- list_to_long(r$test_indices)

    # ---- 4) Формуємо аркуші й пишемо в .xlsx ----
    sheets <- list(
      LD50            = ld50,
      False_Negatives = fn_long,
      False_Positives = fp_long,
      Test_Indices    = ti_long,
      Error_Counts    = err_df
    )

    writexl::write_xlsx(sheets, path = file)
  }
)}
