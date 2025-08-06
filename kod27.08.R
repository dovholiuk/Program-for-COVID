process_data <- function(path) {
  data <- read.csv(path)

  predictors <- c("NLR", "d.NLR", "IGLR.100", "PLR", "SII")
  ld50_all <- c()
  false_negatives <- list()
  false_positives <- list()

  n <- nrow(data)
  k <- round(0.6 * n)

  for (pred in predictors) {
    model <- glm(data$Gr ~ data[[pred]], family = binomial)
    b <- coef(model)
    ld <- -b[1] / b[2]
    ld50_all[pred] <- ld

    fn_list <- list()
    fp_list <- list()

    for (i in 1:100) {
      train_idx <- sample(1:n, k)
      train <- data[train_idx, ]
      test <- data[-train_idx, ]
      global_idx <- setdiff(1:n, train_idx)

      m <- glm(train$Gr ~ train[[pred]], family = binomial)
      b_iter <- coef(m)
      ld_iter <- -b_iter[1] / b_iter[2]

      pred_class <- ifelse(test[[pred]] > ld_iter, 1, 0)

      fn_local <- which(test$Gr == 1 & pred_class == 0)
      fp_local <- which(test$Gr == 0 & pred_class == 1)

      fn_list[[i]] <- global_idx[fn_local]
      fp_list[[i]] <- global_idx[fp_local]
    }

    false_negatives[[pred]] <- fn_list
    false_positives[[pred]] <- fp_list
  }

  return(list(
    ld50 = data.frame(Predictor = names(ld50_all), LD50 = as.numeric(ld50_all)),
    false_negatives = false_negatives,
    false_positives = false_positives
  ))
}
