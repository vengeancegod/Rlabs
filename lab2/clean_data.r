# Скрипт для очистки данных - оставляем только вариант 4
library(dplyr)

# Читаем исходный файл
data <- read.csv("л.р.2.csv", sep=";", header=TRUE, fileEncoding="windows-1251")

cat("Исходные колонки:\n")
print(names(data))
cat("\nКоличество колонок:", ncol(data), "\n")
cat("Количество строк:", nrow(data), "\n")

# Оставляем только нужные колонки для варианта 4
clean_data <- data %>%
  select(год, месяц, t, вар.4)

# Переименовываем колонку для удобства
names(clean_data)[4] <- "зарплата"

# Сохраняем очищенный файл с правильными настройками
write.csv(clean_data, "data_clean_var4.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# Также сохраним версию с разделителем точка с запятой (для read.csv2)
write.csv2(clean_data, "data_clean_var4.csv", 
           row.names = FALSE, 
           fileEncoding = "UTF-8")

cat("\nФайлы сохранены!\n")
cat("- data_clean_var4.csv (стандартный формат с запятыми)\n")
cat("- data_clean_var4.csv (формат с точкой с запятой)\n\n")

cat("Оставшиеся колонки:\n")
print(names(clean_data))
cat("\nПервые 10 строк очищенных данных:\n")
print(head(clean_data, 10))

# Проверим структуру сохраненного файла
cat("\nПроверка сохраненного файла:\n")
test_read <- read.csv("data_clean_var4.csv")
cat("Структура прочитанного файла:\n")
print(str(test_read))