setwd("C:\\Users\\User\\OneDrive\\Робочий стіл\\Статистика\\стаття")
data=read.csv("5predictorss.csv")

pred1 = glm(Gr ~ NLR, family = binomial, data = data)
b1 = coef(pred1)
ld50_1 = -b1[1] / b1[2]

pred2 = glm(Gr ~ d.NLR, family = binomial, data = data)
b2 = coef(pred2)
ld50_2 = -b2[1] / b2[2]

pred3 = glm(Gr ~ IGLR.100, family = binomial, data = data)
b3 = coef(pred3)
ld50_3 = -b3[1] / b3[2]

pred4 = glm(Gr ~ PLR, family = binomial, data = data)
b4 = coef(pred4)
ld50_4 = -b4[1] / b4[2]

pred5 = glm(Gr ~ SII, family = binomial, data = data)
b5 = coef(pred5)
ld50_5 = -b5[1] / b5[2]

ld50_all = c(NLR = ld50_1, d.NLR = ld50_2, IGLR.100 = ld50_3, PLR = ld50_4, SII = ld50_5)
print(ld50_all)









false_negatives = list()
false_positives = list()

n = nrow(data)      
k = round(0.6 * n) 






setwd("C:\\Users\\User\\OneDrive\\Робочий стіл\\Статистика\\стаття")
data <- read.csv("5predictorss.csv")

predictors <- c("NLR", "d.NLR", "IGLR.100", "PLR", "SII")
ld50_all <- c()
false_negatives <- list()
false_positives <- list()

n <- nrow(data)
k <- round(0.7 * n)

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
}

print(ld50_all)
save(ld50_all, false_negatives, false_positives, file = "results.RData")
 