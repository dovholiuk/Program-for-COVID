setwd("C:\\Users\\User\\OneDrive\\Робочий стіл\\Статистика\\стаття")
data=read.csv("bootStr.csv")

full_model = glm(Gr~NLR, family=binomial, data=data)
b = coef(full_model)
ld50 = function(b) -b[1]/b[2]
ldl = ld50(b)
write(ldl, file = "ldl.txt")
cat("Основний предиктор моделі:", ldl, "\n")

for(i in 1:100){
	n = 200
	k = 140
	sample_data = sample(1:n, k)
	train = data[sample_data, ]
	test = data[-sample_data, ]

	train_model = glm(Gr~NLR, family=binomial, data=train)
	b_train = coef(train_model)
	ld50_train = ld50(b_train)
	cat("Предиктор на тренувальних даних:", ld50_train, "\n")
	
	test$newGroup = ifelse(test$NLR>ld50_train, 1, 0)

	test$error = test$Gr-test$newGroup
	count_error=which(test$error>0)
	cat("Кількість помилок:", length(count_error), "\n")
	write(ldl, file = "ldl.txt")
}