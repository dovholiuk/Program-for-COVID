library(shiny)

# Підтягнемо всі допоміжні файли
source("R/helpers.R", encoding = "UTF-8")
source("R/mod_listtabs.R", encoding = "UTF-8")
source("R/ui.R", encoding = "UTF-8")
source("R/server.R", encoding = "UTF-8")

# Підключаємо твій обчислювальний скрипт з process_data(path)
source("kod27.08.R", encoding = "UTF-8")

# Запуск
shinyApp(ui = ui, server = server)
