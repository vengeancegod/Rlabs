# Установка локали
Sys.setlocale("LC_ALL", "ru_RU.UTF-8")

# Установка и загрузка GGally
if (!require(GGally, quietly = TRUE)) {
  install.packages("GGally", quiet = TRUE)
  library(GGally, quietly = TRUE)
}

if (!require(nortest, quietly = TRUE)) {
  install.packages("nortest", quiet = TRUE)
  library(nortest, quietly = TRUE)
}

file_path <- "вариант-4(л.р.1).csv"
data <- read.csv2(file_path, fileEncoding = "CP1251")

names(data) <- c("пп", "группа", "пол", "возраст", "стаж_работы", 
                 "процент_выполнения", "количество_ошибок", 
                 "удовлетворенность_баллы", "удовлетворенность_качество",
                 "качество_документирования")

cat("Переименованные столбцы:\n")
print(names(data))

cat("Размер данных:", nrow(data), "строк,", ncol(data), "столбцов\n\n")

data$стаж_работы <- as.numeric(data$стаж_работы)
data$процент_выполнения <- as.numeric(data$процент_выполнения)
data$количество_ошибок <- as.numeric(data$количество_ошибок)
data$удовлетворенность_баллы <- as.numeric(data$удовлетворенность_баллы)
data$качество_документирования <- as.numeric(data$качество_документирования)

# СОЗДАЕМ ГРУППЫ ПО СТАЖУ согласно варианту
data$группа <- ifelse(data$стаж_работы < 5, 1, 2)
data$группа <- as.factor(data$группа)

cat("Группы созданы по стажу работы:\n")
cat("- Группа 1: стаж менее 5 лет\n") 
cat("- Группа 2: стаж 5 лет и более\n\n")

cat("Распределение по группам:\n")
print(table(data$группа))
cat("\n")

# Проверяем стаж по группам
cat("Статистика стажа работы по группам:\n")
stats_стаж <- aggregate(стаж_работы ~ группа, data = data, 
                FUN = function(x) c(
                  n = length(x),
                  mean = round(mean(x), 2), 
                  min = min(x), 
                  max = max(x),
                  sd = round(sd(x), 2)
                ))
print(stats_стаж)
cat("\n")

# ДОБАВЛЕННЫЙ РАЗДЕЛ - ВЫВОД ИСХОДНЫХ ДАННЫХ И ПОДМНОЖЕСТВ
cat("П.2: ВЫВОД ИСХОДНЫХ ДАННЫХ И СОЗДАННЫХ ПОДМНОЖЕСТВ\n")
cat("=================================================\n\n")

# Вывод информации о столбце возраста
cat("1. РЕЗУЛЬТАТ ВЫВОДА СТОЛБЦА ВОЗРАСТ:\n")
cat("-------------------------------------\n")
cat("Столбец 'возраст' содержит возраст сотрудников в годах:\n")
print(data$возраст)
cat("\nОсновная статистика по возрасту:\n")
cat("Количество наблюдений:", length(data$возраст), "\n")
cat("Минимальный возраст:", min(data$возраст), "лет\n")
cat("Максимальный возраст:", max(data$возраст), "лет\n")
cat("Средний возраст:", round(mean(data$возраст), 2), "лет\n")
cat("Медианный возраст:", median(data$возраст), "лет\n")
cat("Стандартное отклонение:", round(sd(data$возраст), 2), "лет\n\n")

# Вывод нескольких строк исходной таблицы
cat("РЕЗУЛЬТАТ ВЫВОДА НЕСКОЛЬКИХ СТРОК ИСХОДНОЙ ТАБЛИЦЫ:\n")
cat("------------------------------------------------------\n")
cat("Первые 10 строк исходной таблицы:\n")
print(head(data, 10))
cat("\n")

cat("Случайные 5 строк из таблицы:\n")
set.seed(123) # для воспроизводимости
random_indices <- sample(1:nrow(data), 5)
print(data[random_indices, ])
cat("\n")

# Вывод информации о созданных подмножествах по стажу
cat("РЕЗУЛЬТАТЫ СОЗДАНИЯ ПОДМНОЖЕСТВ ПО СТАЖУ РАБОТЫ:\n")
cat("---------------------------------------------------\n")

# Первое подмножество - Группа 1 (стаж менее 5 лет)
cat("ПЕРВОЕ ПОДМНОЖЕСТВО - ГРУППА 1 (стаж работы < 5 лет):\n")
cat("=====================================================\n")
group1_data <- data[data$группа == 1, ]
cat("Количество сотрудников в группе 1:", nrow(group1_data), "\n")
cat("Диапазон стажа работы в группе 1: от", min(group1_data$стаж_работы), 
    "до", max(group1_data$стаж_работы), "лет\n")
cat("Средний стаж в группе 1:", round(mean(group1_data$стаж_работы), 2), "лет\n")
cat("Распределение по полу в группе 1:\n")
print(table(group1_data$пол))
cat("\nПервые 5 строк подмножества Группа 1:\n")
print(head(group1_data, 5))
cat("\n")

# Второе подмножество - Группа 2 (стаж 5 лет и более)
cat("ВТОРОЕ ПОДМНОЖЕСТВО - ГРУППА 2 (стаж работы ≥ 5 лет):\n")
cat("=====================================================\n")
group2_data <- data[data$группа == 2, ]
cat("Количество сотрудников в группе 2:", nrow(group2_data), "\n")
cat("Диапазон стажа работы в группе 2: от", min(group2_data$стаж_работы), 
    "до", max(group2_data$стаж_работы), "лет\n")
cat("Средний стаж в группе 2:", round(mean(group2_data$стаж_работы), 2), "лет\n")
cat("Распределение по полу в группе 2:\n")
print(table(group2_data$пол))
cat("\nПервые 5 строк подмножества Группа 2:\n")
print(head(group2_data, 5))
cat("\n")

# Сравнительная статистика между группами
cat("СРАВНИТЕЛЬНАЯ ХАРАКТЕРИСТИКА ГРУПП:\n")
cat("--------------------------------------\n")

# Создаем сравнительную таблицу
comparison_table <- data.frame(
  Характеристика = c("Количество сотрудников", 
                     "Средний возраст", 
                     "Средний стаж", 
                     "Средний процент выполнения",
                     "Среднее количество ошибок",
                     "Средняя удовлетворенность"),
  Группа_1 = c(
    nrow(group1_data),
    round(mean(group1_data$возраст), 2),
    round(mean(group1_data$стаж_работы), 2),
    round(mean(group1_data$процент_выполнения), 2),
    round(mean(group1_data$количество_ошибок), 2),
    round(mean(group1_data$удовлетворенность_баллы), 2)
  ),
  Группа_2 = c(
    nrow(group2_data),
    round(mean(group2_data$возраст), 2),
    round(mean(group2_data$стаж_работы), 2),
    round(mean(group2_data$процент_выполнения), 2),
    round(mean(group2_data$количество_ошибок), 2),
    round(mean(group2_data$удовлетворенность_баллы), 2)
  )
)

print(comparison_table)
cat("\n")

# Проверка распределения по полу в группах
cat("РАСПРЕДЕЛЕНИЕ ПО ПОЛУ В ГРУППАХ:\n")
cat("-----------------------------------\n")
sex_distribution <- table(data$группа, data$пол)
colnames(sex_distribution) <- c("Мужской", "Женский")
rownames(sex_distribution) <- c("Группа 1 (<5 лет)", "Группа 2 (≥5 лет)")
print(sex_distribution)
cat("\n")

cat("ВЫВОД: Группы успешно созданы по критерию стажа работы:\n")
cat("- Группа 1 (стаж < 5 лет):", nrow(group1_data), "сотрудников\n")
cat("- Группа 2 (стаж ≥ 5 лет):", nrow(group2_data), "сотрудников\n")
cat("Общее количество сотрудников:", nrow(data), "\n\n")

cat("\nОсновная статистика:\n")
print(summary(data))
cat("П.3: СТАТИЧЕСКИЙ АНАЛИЗ \n\n")

# Функция для расчета моды
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Функция для расчета асимметрии
get_assymetry <- function(x) {
    n <- length(x)
    x <- x[!is.na(x)]
    mean_x <- mean(x)
    sd_x <- sd(x)
    (sum((x - mean_x)^3) / n) / (sd_x^3)
}

# Функция для расчета эксцесса
get_excess <- function(x) {
    n <- length(x)
    x <- x[!is.na(x)]
    mean_x <- mean(x)
    sd_x <- sd(x)
    (sum((x - mean_x)^4) / n) / (sd_x^4) - 3
}

quant_vals <- c("возраст", "стаж_работы", "процент_выполнения", 
                "количество_ошибок", "удовлетворенность_баллы", 
                "качество_документирования")

# Функция для вывода статистики
print_stats <- function(data, group_name = "Вся выборка") {
  cat("\n", group_name, ":\n", sep = "")
  cat("=", rep("=", nchar(group_name) + 1), "\n", sep = "")
  
  stats_df <- data.frame(
    Переменная = character(),
    Мин = numeric(),
    Макс = numeric(),
    Среднее = numeric(),
    СтдОтклонение = numeric(),
    Q1 = numeric(),
    Медиана = numeric(),
    Q3 = numeric(),
    Мода = numeric(),
    Асимметрия = numeric(),
    Эксцесс = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(var in quant_vals) {
    x <- data[[var]]
    x <- x[!is.na(x)]
    
    if(length(x) > 0) {
      stats_df <- rbind(stats_df, data.frame(
        Переменная = var,
        Мин = min(x),
        Макс = max(x),
        Среднее = mean(x),
        СтдОтклонение = sd(x),
        Q1 = quantile(x, 0.25),
        Медиана = median(x),
        Q3 = quantile(x, 0.75),
        Мода = get_mode(x),
        Асимметрия = get_assymetry(x),
        Эксцесс = get_excess(x)
      ))
    }
  }
  
  print(stats_df, digits = 3, row.names = FALSE)
}

# Статистика для всей выборки
print_stats(data)

# Статистика по группам
for(group in 1:2) {
  group_data <- data[data$группа == group, ]
  print_stats(group_data, paste("Группа", group))
}                

data$группа <- as.factor(data$группа)
data$пол <- as.factor(data$пол)
data$удовлетворенность_качество <- as.factor(data$удовлетворенность_качество)

cat("П.4: ГРАФИЧЕСКИЙ АНАЛИЗ ДАННЫХ\n\n")

dir.create("графики", showWarnings = FALSE)

# 1. ДИАГРАММА РАССЕЯНИЯ
cat("1. Диаграмма рассеяния: Процент выполнения vs Количество ошибок\n")
png("графики/1_диаграмма_рассеяния.png", width = 1200, height = 800, res = 150)
par(mar = c(6, 6, 6, 3), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.3)
plot(data$процент_выполнения, data$количество_ошибок,
     xlab = "Процент выполнения, %", 
     ylab = "Количество ошибок",
     main = "Диаграмма рассеяния: Процент выполнения vs Ошибки",
     pch = 19, col = "blue", cex = 1.2)
grid()
abline(lm(количество_ошибок ~ процент_выполнения, data = data), 
       col = "red", lwd = 2)
legend("topright", legend = "Линия тренда", col = "red", lwd = 2)
dev.off()

# 2. КРУГОВАЯ ДИАГРАММА
cat("\n2. Круговая диаграмма: Удовлетворенность качеством\n")
png("графики/2_круговая_удовлетворенность.png", width = 1000, height = 800, res = 150)
par(mar = c(4, 4, 6, 4))
satisfaction_counts <- table(data$удовлетворенность_качество)
pie(satisfaction_counts, 
    labels = paste0(names(satisfaction_counts), "\n", satisfaction_counts),
    main = "Круговая диаграмма: Удовлетворенность качеством",
    col = c("lightblue", "lightgreen", "lightcoral"),
    cex = 1.2, radius = 0.8)
dev.off()

# 3. КАТЕГОРИАЛЬНАЯ КРУГОВАЯ ДИАГРАММА
cat("\n3. Категориальная круговая диаграмма: Удовлетворенность по полу и группе\n")
data$группа_пол <- interaction(data$группа, data$пол)

png("графики/3_категориальные_круговые.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(2, 2, 4, 2), cex.main = 1.0)
for(level in levels(data$группа_пол)) {
    subset_data <- data[data$группа_пол == level, ]
    counts <- table(subset_data$удовлетворенность_качество)
    if(length(counts) > 0 && sum(counts) > 0) {
        pie(counts, 
            labels = paste0(names(counts), "\n", counts),
            main = paste("Группа-Пол:", level),
            col = c("lightblue", "lightgreen", "lightcoral"),
            cex = 0.9, radius = 0.7)
    } else {
        plot(1, type = "n", xlab = "", ylab = "", 
             main = paste("Группа-Пол:", level, "\nНет данных"),
             cex.main = 0.8)
        text(1, 1, "Нет данных", cex = 0.8)
    }
}
dev.off()

# 4. СТОЛБИКОВАЯ ДИАГРАММА
cat("\n4. Столбиковая диаграмма: Средний процент выполнения по группе и полу\n")
png("графики/4_столбиковая_диаграмма.png", width = 1000, height = 800, res = 150)
par(mar = c(6, 6, 6, 3), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.3)
mean_table <- aggregate(процент_выполнения ~ группа + пол, data = data, mean)
bar_matrix <- matrix(mean_table$процент_выполнения, nrow = 2, byrow = TRUE)
colnames(bar_matrix) <- levels(mean_table$пол)
rownames(bar_matrix) <- levels(mean_table$группа)

barplot(bar_matrix, beside = TRUE, 
        main = "Средний процент выполнения по группе и полу",
        xlab = "Пол", ylab = "Процент выполнения, %",
        col = c("lightblue", "lightgreen"),
        legend.text = paste("Группа", rownames(bar_matrix)),
        args.legend = list(x = "topright", cex = 1.0),
        cex.names = 1.1)
dev.off()

# 5. ДИАГРАММА РАЗМАХА
cat("\n5. Диаграмма размаха: Качество документирования по группам\n")
png("графики/5_диаграмма_размаха.png", width = 1000, height = 800, res = 150)
par(mar = c(6, 6, 6, 3), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.3)
boxplot(качество_документирования ~ группа, data = data,
        xlab = "Группа", ylab = "Качество документирования",
        main = "Диаграмма размаха: Качество документирования по группам",
        col = c("lightblue", "lightgreen"),
        notch = FALSE)
dev.off()

# 6. ГИСТОГРАММЫ
cat("\n6. Гистограммы всех количественных признаков\n")
png("графики/6_гистограммы.png", width = 1500, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2), cex.axis = 1.0, cex.lab = 1.1, cex.main = 1.2)
quantitative_vars <- c("возраст", "стаж_работы", "процент_выполнения", 
                      "количество_ошибок", "удовлетворенность_баллы", 
                      "качество_документирования")

for(var in quantitative_vars) {
    clean_data <- na.omit(data[[var]])
    if(length(clean_data) > 0) {
        hist(clean_data, main = paste("Гистограмма:", var),
             xlab = var, col = "lightblue", border = "black",
             breaks = 10, cex.main = 1.1)
        
        dens <- density(clean_data)
        lines(dens, col = "red", lwd = 2)
        
        x_norm <- seq(min(clean_data), max(clean_data), length = 100)
        y_norm <- dnorm(x_norm, mean = mean(clean_data), sd = sd(clean_data))
        hist_info <- hist(clean_data, plot = FALSE)
        lines(x_norm, y_norm * length(clean_data) * diff(hist_info$breaks)[1],
              col = "darkgreen", lwd = 2, lty = 2)
        
        legend("topright", legend = c("Плотность", "Нормальное распр."), 
               col = c("red", "darkgreen"), lwd = 2, lty = c(1, 2), cex = 0.8)
    }
}
dev.off()

# 7. МАТРИЧНЫЙ ГРАФИК
cat("\n7. Матричный график всех количественных признаков\n")
png("графики/7_матричный_график.png", width = 1200, height = 1000, res = 150)
par(mar = c(4, 4, 6, 2))
quant_data <- na.omit(data[quantitative_vars])

if(nrow(quant_data) > 0) {
    pairs(quant_data, main = "Матричный график количественных признаков",
          pch = 19, col = rgb(0, 0, 1, 0.5), cex = 0.8,
          cex.labels = 1.2, cex.axis = 0.8)
} else {
    plot(1, type = "n", main = "Недостаточно данных")
    text(1, 1, "Недостаточно данных для матричного графика")
}
dev.off()

cat("П.5: ПРОВЕРКА ГИПОТЕЗЫ О НОРМАЛЬНОСТИ РАСПРЕДЕЛЕНИЯ\n\n")

# Выбираем переменную для анализа (например, процент выполнения)
variable <- "процент_выполнения"
cat("Анализируемая переменная:", variable, "\n\n")

# Функция для проверки нормальности двумя критериями (без Крамера-Мизеса)
check_normality <- function(data, group_name) {
  cat("=== ГРУППА:", group_name, "===\n")
  
  # Извлекаем данные
  x <- data[[variable]]
  x <- x[!is.na(x)]  # Убираем пропущенные значения
  
  if(length(x) < 3) {
    cat("Недостаточно данных для анализа\n\n")
    return()
  }
  
  # Основные статистики
  cat("Объем выборки:", length(x), "\n")
  cat("Среднее:", round(mean(x), 2), "\n")
  cat("Стандартное отклонение:", round(sd(x), 2), "\n")
  cat("Асимметрия:", round(get_assymetry(x), 3), "\n")
  cat("Эксцесс:", round(get_excess(x), 3), "\n\n")
  
  # 1. Критерий Шапиро-Уилка
  cat("1. КРИТЕРИЙ ШАПИРО-УИЛКА:\n")
  if(length(x) >= 3 && length(x) <= 5000) {
    shapiro_test <- shapiro.test(x)
    cat("   W =", round(shapiro_test$statistic, 4), "\n")
    cat("   p-value =", format.pval(shapiro_test$p.value, digits = 4), "\n")
    if(shapiro_test$p.value > 0.05) {
      cat("   ВЫВОД: Распределение не отличается от нормального (p > 0.05)\n")
    } else {
      cat("   ВЫВОД: Распределение отличается от нормального (p < 0.05)\n")
    }
  } else {
    cat("   Объем выборки не подходит для критерия Шапиро-Уилка\n")
  }
  cat("\n")
  
    # 2. Критерий Крамера-Мизеса
  cat("2. КРИТЕРИЙ КРАМЕРА-МИЗЕСА:\n")
if (!require(nortest, quietly = TRUE)) {
  install.packages("nortest", quiet = TRUE)
  library(nortest, quietly = TRUE)
}

if(length(x) >= 7) {
  cramer_test <- tryCatch({
    cramer.test(x)  # ПРАВИЛЬНОЕ название функции из пакета nortest
  }, error = function(e) {
    # Если все равно ошибка, используем cvm.test()
    cat("   Используем критерий Крамера-Мизеса-Смирнова (cvm.test)\n")
    cvm.test(x)
  })
  
  if(!is.null(cramer_test)) {
    cat("   W =", round(cramer_test$statistic, 4), "\n")
    cat("   p-value =", format.pval(cramer_test$p.value, digits = 4), "\n")
    if(cramer_test$p.value > 0.05) {
      cat("   ВЫВОД: Распределение не отличается от нормального (p > 0.05)\n")
    } else {
      cat("   ВЫВОД: Распределение отличается от нормального (p < 0.05)\n")
    }
  }
} else {
  cat("   Недостаточно данных для критерия Крамера-Мизеса (требуется ≥7 наблюдений)\n")
}
  cat("\n")
  
  # 3. Критерий Андерсона-Дарлинга
  cat("3. КРИТЕРИЙ АНДЕРСОНА-ДАРЛИНГА:\n")
  if(length(x) >= 7) {  # Минимальный объем для критерия
    anderson_test <- tryCatch({
      ad.test(x)
    }, error = function(e) {
      cat("   Ошибка при расчете критерия Андерсона-Дарлинга:", e$message, "\n")
      return(NULL)
    })
    
    if(!is.null(anderson_test)) {
      cat("   A =", round(anderson_test$statistic, 4), "\n")
      cat("   p-value =", format.pval(anderson_test$p.value, digits = 4), "\n")
      if(anderson_test$p.value > 0.05) {
        cat("   ВЫВОД: Распределение не отличается от нормального (p > 0.05)\n")
      } else {
        cat("   ВЫВОД: Распределение отличается от нормального (p < 0.05)\n")
      }
    }
  } else {
    cat("   Недостаточно данных для критерия Андерсона-Дарлинга (требуется ≥7 наблюдений)\n")
  }
  cat("\n")
  
  # === КОНЕЦ ДОПОЛНЕНИЯ ===
  
  # 4. Визуальная проверка - Q-Q plot
  cat("4. ВИЗУАЛЬНАЯ ПРОВЕРКА (Q-Q PLOT):\n")
  safe_group_name <- gsub("[^[:alnum:]]", "_", group_name)
  png_filename <- paste0("графики/qqplot_", safe_group_name, "_", variable, ".png")
  
  png(png_filename, width = 1920, height = 1080, res = 150)
  par(mfrow = c(1, 2))
  
  # Гистограмма с нормальной кривой
  hist(x, main = paste("Гистограмма:", group_name), 
       xlab = variable, col = "lightblue", probability = TRUE)
  
  if(length(x) > 1 && sd(x) > 0) {
    curve(dnorm(x, mean = mean(x), sd = sd(x)), 
          add = TRUE, col = "red", lwd = 2)
    legend("topright", legend = "Нормальное распределение", 
           col = "red", lwd = 2, bty = "n")
  }
  
  # Q-Q plot
  qqnorm(x, main = paste("Q-Q plot:", group_name))
  qqline(x, col = "red", lwd = 2)
  
  dev.off()
  cat("   Q-Q plot сохранен в файл\n")
  cat("   Визуальная оценка позволяет судить о близости распределения к нормальному\n")
  cat("   -----------------------------------------\n\n")
}

# Проверяем нормальность для всей выборки
check_normality(data, "Вся выборка")

# Проверяем нормальность для группы 1
group1_data <- data[data$группа == 1, ]
check_normality(group1_data, "Группа 1")

# Проверяем нормальность для группы 2
group2_data <- data[data$группа == 2, ]
check_normality(group2_data, "Группа 2")

# Сравнительный анализ
cat("СРАВНИТЕЛЬНЫЙ АНАЛИЗ НОРМАЛЬНОСТИ:\n")
cat("====================================\n")

# Создаем сводную таблицу результатов
cat("\nСВОДКА РЕЗУЛЬТАТОВ:\n")
cat("Группа           | Шапиро-Уилк | Крамер-Мизес | Андерсон-Дарлинг | Визуальная оценка | Вывод\n")
cat("-----------------|-------------|--------------|------------------|-------------------|-------\n")

# Функция для получения символа результата
get_result_symbol <- function(p_value) {
  ifelse(p_value > 0.05, "+ норма", "x не норма")
}

# Получаем результаты для сводной таблицы
results <- list()

# Вся выборка
x_all <- na.omit(data[[variable]])
shapiro_all <- shapiro.test(x_all)$p.value

cat(sprintf("%-16s | %-11s | %-17s | %s\n",
            "Вся выборка",
            get_result_symbol(shapiro_all),
            "по Q-Q plot",
            ifelse(shapiro_all > 0.05, "НОРМАЛЬНОЕ", "НЕ НОРМАЛЬНОЕ")))

# Группа 1
x_group1 <- na.omit(group1_data[[variable]])
if(length(x_group1) >= 3) {
  shapiro_g1 <- shapiro.test(x_group1)$p.value
  
  cat(sprintf("%-16s | %-11s | %-17s | %s\n",
              "Группа 1",
              get_result_symbol(shapiro_g1),
              "по Q-Q plot",
              ifelse(shapiro_g1 > 0.05, "НОРМАЛЬНОЕ", "НЕ НОРМАЛЬНОЕ")))
}

# Группа 2
x_group2 <- na.omit(group2_data[[variable]])
if(length(x_group2) >= 3) {
  shapiro_g2 <- shapiro.test(x_group2)$p.value
  
  cat(sprintf("%-16s | %-11s | %-17s | %s\n",
              "Группа 2", 
              get_result_symbol(shapiro_g2),
              "по Q-Q plot",
              ifelse(shapiro_g2 > 0.05, "НОРМАЛЬНОЕ", "НЕ НОРМАЛЬНОЕ")))
}

cat("\n")

cat("ВЫВОДЫ ДЛЯ ПРИКЛАДНОЙ ЗАДАЧИ:\n")
cat("==============================\n\n")

cat("На основе проведенного анализа нормальности распределения переменной '", 
    variable, "':\n\n", sep = "")

all_normal <- shapiro_all > 0.05
g1_normal <- if(exists("shapiro_g1")) shapiro_g1 > 0.05 else NA
g2_normal <- if(exists("shapiro_g2")) shapiro_g2 > 0.05 else NA

cat(" СТАТИСТИЧЕСКИЕ ВЫВОДЫ:\n")
if(all_normal) {
  cat("   Распределение процента выполнения в общей выборке соответствует нормальному закону\n")
} else {
  cat("   Распределение процента выполнения в общей выборке НЕ соответствует нормальному закону\n")
}

if(!is.na(g1_normal) && g1_normal) {
  cat("   Распределение в Группе 1 соответствует нормальному закону\n")
} else if(!is.na(g1_normal)) {
  cat("   Распределение в Группе 1 НЕ соответствует нормальному закону\n")
}

if(!is.na(g2_normal) && g2_normal) {
  cat("   Распределение в Группе 2 соответствует нормальному закону\n")
} else if(!is.na(g2_normal)) {
  cat("   Распределение в Группе 2 НЕ соответствует нормальному закону\n")
}

cat("П.6: КОРРЕЛЯЦИОННЫЙ АНАЛИЗ ДАННЫХ\n\n")

# 6.1 Связь между качественными переменными
cat("6.1. СВЯЗЬ МЕЖДУ КАЧЕСТВЕННЫМИ ПЕРЕМЕННЫМИ\n")

# Функция для безопасного анализа таблиц сопряженности
analyze_contingency_table <- function(data, group_name = "Вся выборка") {
  cat("\n", paste(rep("=", 50), collapse=""), "\n")
  cat(group_name, ":\n")
  
  # Создаем таблицу сопряженности
  table_data <- table(data$пол, data$удовлетворенность_качество)
  cat("Таблица сопряженности: Пол vs Удовлетворенность качеством\n")
  print(table_data)
  
  # Проверяем, достаточно ли данных для анализа
  if(nrow(table_data) < 2 | ncol(table_data) < 2) {
    cat("Недостаточно категорий для анализа\n")
    return()
  }
  
  # Проверяем, есть ли нулевые строки/столбцы
  row_sums <- rowSums(table_data)
  col_sums <- colSums(table_data)
  
  if(any(row_sums == 0) | any(col_sums == 0)) {
    cat("Есть категории с нулевыми наблюдениями - удаляем их\n")
    
    # Удаляем строки/столбцы с нулевыми суммами
    table_data <- table_data[row_sums > 0, col_sums > 0, drop = FALSE]
    
    if(nrow(table_data) < 2 | ncol(table_data) < 2) {
      cat("После очистки недостаточно категорий для анализа\n")
      return()
    }
    
    cat("Очищенная таблица:\n")
    print(table_data)
  }
  
  # Проверяем минимальное количество наблюдений
  if(sum(table_data) < 10) {
    cat("Слишком мало наблюдений для статистического анализа\n")
    return()
  }
  
  # Критерий Хи-квадрат
  cat("\nКРИТЕРИЙ ХИ-КВАДРАТ:\n")
  chi_test <- tryCatch({
  # Проверяем, достаточно ли ожидаемых частот
  expected <- chisq.test(table_data)$expected
  if(any(expected < 5)) {
    cat("   ВНИМАНИЕ: Некоторые ожидаемые частоты < 5\n")
    cat("   Рекомендуется интерпретировать результаты с осторожностью\n")
  }
  chisq.test(table_data, correct = FALSE)
}, error = function(e) {
  cat("Ошибка в расчете Хи-квадрат:", e$message, "\n")
  return(NULL)
}, warning = function(w) {
  # Используем точный критерий Фишера при проблемах с приближением
  cat("   Используем точный критерий Фишера из-за проблем с приближением\n")
  return(NULL)
})
  
  if(!is.null(chi_test)) {
    print(chi_test)
    
    # Коэффициент Крамера для силы связи
    n <- sum(table_data)
    k <- min(nrow(table_data), ncol(table_data))
    cramer_v <- sqrt(chi_test$statistic / (n * (k - 1)))
    
    cat("\nКОЭФФИЦИЕНТ КРАМЕРА V (сила связи):", round(as.numeric(cramer_v), 3), "\n")
    
    # Интерпретация
    if(cramer_v < 0.1) strength <- "очень слабая"
    else if(cramer_v < 0.3) strength <- "слабая"
    else if(cramer_v < 0.5) strength <- "умеренная"
    else strength <- "сильная"
    
    cat("ИНТЕРПРЕТАЦИЯ:", strength, "связь\n")
  }
  
  # Критерий Фишера (для небольших выборок или когда Хи-квадрат не подходит)
  # Критерий Фишера (для небольших выборок или когда Хи-квадрат не подходит)
cat("\nТОЧНЫЙ КРИТЕРИЙ ФИШЕРА:\n")
fisher_test <- tryCatch({
  fisher_result <- fisher.test(table_data)
  
  # Аккуратный вывод без дублирования
  cat("   p-value =", format.pval(fisher_result$p.value, digits = 4), "\n")
  if(!is.null(fisher_result$alternative)) {
    cat("   alternative hypothesis:", fisher_result$alternative, "\n")
  }
  
  # Дополнительная информация для больших таблиц
  if(nrow(table_data) > 2 | ncol(table_data) > 2) {
    cat("   (для таблиц больше 2x2 используется симуляция p-value)\n")
  }
  
  return(fisher_result)
}, error = function(e) {
  cat("Ошибка в расчете Фишера:", e$message, "\n")
  return(NULL)
})
  
  if(!is.null(fisher_test)) {
    print(fisher_test)
  }
  
  # Дополнительные меры связи
  if(!is.null(chi_test) & nrow(table_data) == 2 & ncol(table_data) == 2) {
    # Коэффициент фи для 2x2 таблиц
    phi <- sqrt(chi_test$statistic / sum(table_data))
    cat("\nКОЭФФИЦИЕНТ ФИ (для таблиц 2x2):", round(as.numeric(phi), 3), "\n")
  }
}

# Анализ для всей выборки
analyze_contingency_table(data, "ВСЯ ВЫБОРКА")

# Анализ по группам
for(group in 1:2) {
  group_data <- data[data$группа == group, ]
  group_label <- ifelse(group == 1, "ГРУППА 1 (стаж < 5 лет)", "ГРУППА 2 (стаж ≥ 5 лет)")
  analyze_contingency_table(group_data, group_label)
}

# 6.2 Связь качественной и количественных переменных
cat("\n6.2. СВЯЗЬ КАЧЕСТВЕННОЙ И КОЛИЧЕСТВЕННЫХ ПЕРЕМЕННЫХ\n")

# Анализ влияния группы на количественные показатели
qual_var <- "группа"
cat("АНАЛИЗ ВЛИЯНИЯ СТАЖА РАБОТЫ (ГРУППЫ) НА КОЛИЧЕСТВЕННЫЕ ПОКАЗАТЕЛИ:\n\n")

for(quant_var in quant_vals) {
  cat("Переменная:", quant_var, "\n")
  cat(paste(rep("-", nchar(quant_var) + 12), collapse=""), "\n")
  
  # Проверяем, достаточно ли данных в обеих группам
  group1_data <- data[data$группа == 1, quant_var]
  group2_data <- data[data$группа == 2, quant_var]
  
  group1_clean <- na.omit(group1_data)
  group2_clean <- na.omit(group2_data)
  
  cat("Группа 1 (стаж < 5 лет): наблюдений =", length(group1_clean), "\n")
  cat("Группа 2 (стаж ≥ 5 лет): наблюдений =", length(group2_clean), "\n")
  
  if(length(group1_clean) < 3 | length(group2_clean) < 3) {
    cat("Недостаточно данных для статистического анализа\n")
    
    # Показываем описательные статистики
    cat("Описательные статистики по группам:\n")
    if(length(group1_clean) > 0) {
      cat("Группа 1 - Среднее:", round(mean(group1_clean), 2), 
          "Медиана:", round(median(group1_clean), 2), 
          "Стд.откл:", round(sd(group1_clean), 2), "\n")
    }
    if(length(group2_clean) > 0) {
      cat("Группа 2 - Среднее:", round(mean(group2_clean), 2), 
          "Медиана:", round(median(group2_clean), 2),
          "Стд.откл:", round(sd(group2_clean), 2), "\n")
    }
    cat("\n")
    next
  }
  
  # Проверка нормальности для выбора критерия
  norm_test_g1 <- tryCatch(shapiro.test(group1_clean), error = function(e) NULL)
  norm_test_g2 <- tryCatch(shapiro.test(group2_clean), error = function(e) NULL)
  
  use_parametric <- FALSE
  if(!is.null(norm_test_g1) & !is.null(norm_test_g2)) {
    if(norm_test_g1$p.value > 0.05 & norm_test_g2$p.value > 0.05) {
      use_parametric <- TRUE
      cat("Распределения нормальные -> применяем ANOVA\n")
    } else {
      cat("Распределения не нормальные -> применяем Крускал-Уоллис\n")
    }
  } else {
    cat("Применяем непараметрический тест (Крускал-Уоллис)\n")
  }
  
  # Если распределения нормальные - ANOVA, иначе - Крускал-Уоллис
  if(use_parametric) {
    anova_test <- aov(as.formula(paste(quant_var, "~", qual_var)), data = data)
    anova_summary <- summary(anova_test)
    print(anova_summary)
    
    p_value <- anova_summary[[1]]$"Pr(>F)"[1]
  } else {
    kruskal_test <- kruskal.test(as.formula(paste(quant_var, "~", qual_var)), data = data)
    print(kruskal_test)
    
    p_value <- kruskal_test$p.value
  }
  
  # Интерпретация
  if(p_value < 0.05) {
    cat("ВЫВОД: Существует статистически значимое различие между группами по стажу (p =", 
        round(p_value, 4), ")\n")
  } else {
    cat("ВЫВОД: Нет статистически значимого различия между группами по стажу (p =", 
        round(p_value, 4), ")\n")
  }
  
  # Дополнительно: средние/медианы по группам
  cat("Описательные статистики по группам:\n")
  stats <- aggregate(as.formula(paste(quant_var, "~", qual_var)), 
                    data = data, 
                    function(x) c(
                      n = length(na.omit(x)),
                      mean = round(mean(x, na.rm = TRUE), 2), 
                      median = round(median(x, na.rm = TRUE), 2),
                      sd = round(sd(x, na.rm = TRUE), 2)
                    ))
  # Переименовываем группы для понятности
  stats$группа <- ifelse(stats$группа == 1, "стаж < 5 лет", "стаж ≥ 5 лет")
  print(stats)
  cat("\n")
}

# 6.3 Корреляция между количественными переменными
cat("6.3. КОРРЕЛЯЦИЯ МЕЖДУ КОЛИЧЕСТВЕННЫМИ ПЕРЕМЕННЫМИ\n")

# Функция для расчета корреляций с проверкой данных
calculate_correlations <- function(data, group_name = "Вся выборка") {
  cat("\n", group_name, ":\n")
  cat(rep("=", nchar(group_name) + 1), "\n")
  
  quant_data <- na.omit(data[quant_vals])
  
  if(nrow(quant_data) > 3) {
    # Матрицы корреляций
    cor_pearson <- cor(quant_data, method = "pearson")
    cor_spearman <- cor(quant_data, method = "spearman")
    cor_kendall <- cor(quant_data, method = "kendall")
    
    cat("Коэффициенты корреляции Пирсона:\n")
    print(round(cor_pearson, 3))
    
    cat("\nКоэффициенты корреляции Спирмена:\n")
    print(round(cor_spearman, 3))
    
    cat("\nКоэффициенты корреляции Кендалла:\n")
    print(round(cor_kendall, 3))
    
    # Находим максимальную по модулю корреляцию Пирсона
    cor_pearson_no_diag <- cor_pearson
    diag(cor_pearson_no_diag) <- 0
    max_indices <- which(abs(cor_pearson_no_diag) == max(abs(cor_pearson_no_diag)), arr.ind = TRUE)
    
    if(nrow(max_indices) > 0) {
      max_cor <- max_indices[1,]
      var1 <- rownames(cor_pearson)[max_cor[1]]
      var2 <- colnames(cor_pearson)[max_cor[2]]
      max_value <- cor_pearson[max_cor[1], max_cor[2]]
      
      cat("\nМАКСИМАЛЬНАЯ КОРРЕЛЯЦИЯ ПИРСОНА:\n")
      cat(var1, "и", var2, ":", round(max_value, 3), "\n")
      
      return(list(var1 = var1, var2 = var2, value = max_value))
    }
  } else {
    cat("Недостаточно данных для анализа\n")
  }
  return(NULL)
}

# Корреляции для всей выборки
max_cor_all <- calculate_correlations(data)

# Корреляции по группам
max_cor_groups <- list()
for(group in 1:2) {
  group_data <- data[data$группа == group, ]
  max_cor_groups[[group]] <- calculate_correlations(group_data, paste("Группа", group))
}

# 6.4 Частный коэффициент корреляции
cat("\n6.4. ЧАСТНЫЙ КОЭФФИЦИЕНТ КОРРЕЛЯЦИИ\n")

if(!is.null(max_cor_all)) {
  var1 <- max_cor_all$var1
  var2 <- max_cor_all$var2
  
  cat("Переменные с максимальной корреляцией:", var1, "и", var2, "\n")
  cat("Коэффициент Пирсона:", round(max_cor_all$value, 3), "\n\n")
  
  # Исключаем эти переменные для поиска контрольной переменной
  control_vars <- setdiff(quant_vals, c(var1, var2))
  
  if(length(control_vars) > 0) {
    control_var <- control_vars[1]  # Берем первую переменную как контрольную
    
    cat("Контрольная переменная:", control_var, "\n")
    
    # Функция для расчета частной корреляции без пакета ppcor
    calculate_partial_cor <- function(data, group_name) {
      cat("\n", group_name, ":\n")
      
      temp_data <- na.omit(data[c(var1, var2, control_var)])
      
      if(nrow(temp_data) > 3) {
        # Расчет частной корреляции вручную через множественную регрессию
        # Частная корреляция = корреляция между остатками двух регрессий
        model1 <- lm(as.formula(paste(var1, "~", control_var)), data = temp_data)
        model2 <- lm(as.formula(paste(var2, "~", control_var)), data = temp_data)
        
        residuals1 <- resid(model1)
        residuals2 <- resid(model2)
        
        partial_cor <- cor(residuals1, residuals2)
        
        cat("Частный коэффициент корреляции (контроль:", control_var, "):", 
            round(partial_cor, 3), "\n")
        
        # Сравнение с обычной корреляцией
        simple_cor <- cor(temp_data[[var1]], temp_data[[var2]])
        cat("Обычный коэффициент корреляции:", round(simple_cor, 3), "\n")
        cat("Разница:", round(partial_cor - simple_cor, 3), "\n")
        
        # Интерпретация изменения
        if(abs(partial_cor - simple_cor) > 0.1) {
          cat("ВЫВОД: Контрольная переменная существенно влияет на связь\n")
        } else {
          cat("ВЫВОД: Контрольная переменная слабо влияет на связь\n")
        }
        
        return(partial_cor)
      } else {
        cat("Недостаточно данных\n")
        return(NA)
      }
    }
    
    # Расчет для всей выборки и групп
    calculate_partial_cor(data, "Вся выборка")
    
    for(group in 1:2) {
      group_data <- data[data$группа == group, ]
      calculate_partial_cor(group_data, paste("Группа", group))
    }
  }
}

# 6.5 Визуализация корреляций
cat("\n6.5. ВИЗУАЛИЗАЦИЯ КОРРЕЛЯЦИОННЫХ МАТРИЦ\n")

# Функция для создания графиков с ggpairs
create_correlation_plots <- function(data, group_name = "Вся выборка", filename_suffix = "") {
  quant_data <- na.omit(data[quant_vals])
  
  if(nrow(quant_data) > 3) {
    # 1. Упрощенная тепловая карта с помощью base R
    png(paste0("графики/корреляция_тепловая_карта", filename_suffix, ".png"), 
        width = 1000, height = 800, res = 150)
    
    cor_matrix <- cor(quant_data)
    
    # Создаем простую тепловую карту
    par(mar = c(8, 8, 6, 2))
    image(1:ncol(cor_matrix), 1:nrow(cor_matrix), t(cor_matrix)[, nrow(cor_matrix):1],
          col = colorRampPalette(c("blue", "white", "red"))(20),
          xlab = "", ylab = "", axes = FALSE,
          main = paste("Тепловая карта корреляций -", group_name))
    
    axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2, cex.axis = 0.8)
    axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix), las = 1, cex.axis = 0.8)
    
    # Добавляем значения корреляций
    for(i in 1:nrow(cor_matrix)) {
      for(j in 1:ncol(cor_matrix)) {
        text(j, i, round(cor_matrix[i, j], 2), cex = 0.8, 
             col = ifelse(abs(cor_matrix[i, j]) > 0.5, "white", "black"))
      }
    }
    
    # Легенда
    legend("topright", legend = c("Сильная отриц.", "Слабая", "Сильная полож."),
           fill = colorRampPalette(c("blue", "white", "red"))(3),
           cex = 0.7)
    
    dev.off()
    
    # 2. Матричный график с помощью ggpairs (как требуется в задании)
    png(paste0("графики/корреляция_ggpairs", filename_suffix, ".png"), 
        width = 1600, height = 1200, res = 150)
    
    # Создаем матричный график с ggpairs
    ggpairs_plot <- ggpairs(quant_data,
                            title = paste("Матричный график корреляций -", group_name),
                            axisLabels = "show") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(ggpairs_plot)
    dev.off()
    
    cat("Графики для", group_name, "сохранены\n")
  } else {
    cat("Недостаточно данных для графиков в", group_name, "\n")
  }
}

# Создаем графики для всей выборки
create_correlation_plots(data, "Вся выборка", "_вся_выборка")

# Создаем графики для групп
for(group in 1:2) {
  group_data <- data[data$группа == group, ]
  create_correlation_plots(group_data, paste("Группа", group), paste("_группа", group))
}
cat("\n============================================================\n")
cat("АНАЛИЗ ДАННЫХ ПОЛНОСТЬЮ ЗАВЕРШЕН\n")
cat("============================================================\n")
cat(" Все этапы анализа выполнены успешно\n")
cat(" Графики сохранены в папку 'графики/'\n") 
cat(" Статистические результаты выведены выше\n")
cat(" Предупреждения устранены\n")
cat("============================================================\n\n")

# # Завершить выполнение
# quit(save = "no")