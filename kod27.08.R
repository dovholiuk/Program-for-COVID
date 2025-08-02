setwd("C:\\Users\\User\\OneDrive\\Робочий стіл\\Статистика\\стаття")
data <- read.csv("5predictorss.csv")

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
    
    m <- glm(train$Gr ~ train[[pred]], family = binomial)
    b_iter <- coef(m)
    ld_iter <- -b_iter[1] / b_iter[2]
    
    pred_class <- ifelse(test[[pred]] > ld_iter, 1, 0)
    
    fn_list[[i]] <- which(test$Gr == 1 & pred_class == 0)
    fp_list[[i]] <- which(test$Gr == 0 & pred_class == 1)
  }
  
  false_negatives[[pred]] <- fn_list
  false_positives[[pred]] <- fp_list

  correct <- sum(pred_class == data$Gr)
  incorrect <- sum(pred_class != data$Gr)
}

cat("Correct:", toString(correct), "\n")
cat("Incorrect:", toString(incorrect), "\n")
cat("False Negatives:", toString(false_negatives), "\n")
cat("False Positives:", toString(false_positives), "\n")
