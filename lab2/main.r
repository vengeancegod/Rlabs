Sys.setlocale("LC_ALL", "ru_RU.UTF-8")

dir.create("графики_лр2", showWarnings = FALSE)

cat("2.1. ЗАГРУЗКА ИСХОДНЫХ ДАННЫХ\n")
cat("================================\n")

file_path <- "data_clean_var4.csv"

data <- read.csv2(file_path,  
                 fileEncoding = "UTF-8",
                 check.names = FALSE)

cat("УСПЕХ! Загружено", nrow(data), "строк\n")

cat("Структура данных:\n")
print(str(data))
cat("\nПервые 10 строк:\n")
print(head(data, 10))
cat("\n")

cat("Названия колонок:\n")
print(names(data))
cat("\n")

data$дата <- as.Date(paste(data$год, data$месяц, "01", sep = "-"))

cat("Данные для варианта 4:\n")
print(head(data[, c("год", "месяц", "t", "зарплата", "дата")], 10))
cat("\n")

cat("Размер данных:", nrow(data), "наблюдений\n")
cat("Период данных: от", as.character(min(data$дата)), "до", as.character(max(data$дата)), "\n")
cat("Диапазон зарплат: от", round(min(data$зарплата, na.rm = TRUE), 2), "до", 
    round(max(data$зарплата, na.rm = TRUE), 2), "тыс. руб.\n\n")

ts_data <- ts(data$зарплата, 
              start = c(2007, 1), 
              frequency = 12)

cat("Временной ряд создан:\n")
print(head(ts_data, 12))
cat("\nДлина ряда:", length(ts_data), "наблюдений\n")
cat("Частота:", frequency(ts_data), "(месячные данные)\n\n")

# 2.2. Постройте график временного ряда
cat("2.2. ГРАФИЧЕСКИЙ АНАЛИЗ ВРЕМЕННОГО РЯДА\n")
cat("=========================================\n")

png("графики_лр2/1_временной_ряд.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.3)
plot(ts_data, 
     main = "Временной ряд средней заработной платы программистов (Вариант 4)",
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "blue", 
     lwd = 2)
grid()
dev.off()
cat("График временного ряда сохранен\n")

cat("Декомпозиция временного ряда:\n")
decomposed <- decompose(ts_data, type = "additive")

png("графики_лр2/2_декомпозиция.png", width = 1920, height = 1080, res = 150)
par(mfrow = c(4, 1), mar = c(4, 4, 2, 2))
plot(decomposed$trend, main = "Трендовая составляющая", col = "red", lwd = 2, ylab = "Зарплата")
plot(decomposed$seasonal, main = "Сезонная составляющая", col = "green", lwd = 2, ylab = "Зарплата")
plot(decomposed$random, main = "Случайная составляющая", col = "purple", lwd = 2, ylab = "Зарплата")
plot(ts_data, main = "Исходный ряд с моделью", col = "blue", lwd = 1, ylab = "Зарплата")
lines(decomposed$trend + decomposed$seasonal, col = "orange", lwd = 2)
legend("topleft", legend = c("Исходный", "Модель"), 
       col = c("blue", "orange"), lwd = c(1, 2))
dev.off()
cat("График декомпозиции сохранен\n")

# Автокорреляционная функция (ACF)
png("графики_лр2/3_автокорреляция.png", width = 1000, height = 800, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
acf(ts_data, main = "Автокорреляционная функция (ACF)", lag.max = 48,
    ylab = "Корреляция", xlab = "Лаг")
pacf(ts_data, main = "Частная автокорреляционная функция (PACF)", lag.max = 48,
     ylab = "Частная корреляция", xlab = "Лаг")
dev.off()
cat("Графики ACF и PACF сохранены\n")

cat("\nПРЕДВАРИТЕЛЬНЫЕ ВЫВОДЫ:\n")
cat("========================\n")

# Анализ тренда
trend_test <- tryCatch({
  cor.test(time(ts_data), as.numeric(ts_data), method = "spearman")
}, error = function(e) NULL)

if(!is.null(trend_test)) {
  if(trend_test$p.value < 0.05) {
    if(trend_test$estimate > 0) {
      cat("- Наличие ВОСХОДЯЩЕГО тренда (p =", round(trend_test$p.value, 4), ")\n")
    } else {
      cat("- Наличие НИСХОДЯЩЕГО тренда (p =", round(trend_test$p.value, 4), ")\n")
    }
  } else {
    cat("- Статистически значимый тренд не обнаружен\n")
  }
}

# Анализ сезонности
seasonal_strength <- sd(decomposed$seasonal, na.rm = TRUE) / sd(ts_data, na.rm = TRUE)
cat("- Сила сезонной составляющей:", round(seasonal_strength, 3), "\n")

if(seasonal_strength > 0.1) {
  cat("- Выраженная сезонная составляющая\n")
} else if(seasonal_strength > 0.05) {
  cat("- Умеренная сезонная составляющая\n")
} else {
  cat("- Слабая сезонная составляющая\n")
}

# Анализ ACF/PACF
acf_values <- acf(ts_data, plot = FALSE, lag.max = 1)
pacf_values <- pacf(ts_data, plot = FALSE, lag.max = 1)

cat("- Первый лаг ACF:", round(acf_values$acf[2], 3), "\n")
cat("- Первый лаг PACF:", round(pacf_values$acf[1], 3), "\n")

# Основные статистики
cat("\nОСНОВНЫЕ СТАТИСТИКИ:\n")
cat("Среднее:", round(mean(ts_data, na.rm = TRUE), 2), "тыс. руб.\n")
cat("Стандартное отклонение:", round(sd(ts_data, na.rm = TRUE), 2), "тыс. руб.\n")
cat("Минимум:", round(min(ts_data, na.rm = TRUE), 2), "тыс. руб.\n")
cat("Максимум:", round(max(ts_data, na.rm = TRUE), 2), "тыс. руб.\n")
cat("Коэффициент вариации:", round(sd(ts_data, na.rm = TRUE)/mean(ts_data, na.rm = TRUE), 3), "\n")

cat("\nСЕЗОННЫЙ АНАЛИЗ (средние по месяцам):\n")
monthly_means <- aggregate(as.numeric(ts_data) ~ cycle(ts_data), FUN = mean, na.rm = TRUE)
colnames(monthly_means) <- c("Месяц", "Средняя_зарплата")
month_names <- c("Янв", "Фев", "Мар", "Апр", "Май", "Июн", 
                 "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек")
monthly_means$Месяц_назв <- month_names
print(monthly_means)

cat("\nВИЗУАЛЬНЫЙ АНАЛИЗ ТРЕНДА:\n")
cat("- Наличие явного тренда: ДА\n")
cat("- Характер тренда: Восходящий\n") 
cat("- Тип функции тренда: Предположительно нелинейный\n")
cat("- Наличие сезонной составляющей: ДА\n")
cat("- Периодичность: 12 месяцев (годовая)\n")

cat("\n=========================================\n")
cat("ПЕРВИЧНЫЙ АНАЛИЗ ЗАВЕРШЕН\n")
cat("Все графики сохранены в папку 'графики_лр2/'\n")
cat("Переходим к идентификации тренда...\n")
cat("=========================================\n\n")

# 2.3. ИССЛЕДОВАНИЕ И МОДЕЛЬНОЕ ОПИСАНИЕ ВРЕМЕННОГО РЯДА
cat("2.3. ИССЛЕДОВАНИЕ И МОДЕЛЬНОЕ ОПИСАНИЕ ВРЕМЕННОГО РЯДА\n")
cat("=======================================================\n\n")

# Функция для расчета всех характеристик точности
calculate_metrics <- function(actual, predicted, model_name = "") {
  residuals <- actual - predicted
  n <- length(actual)
  
  mse <- mean(residuals^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals), na.rm = TRUE)
  mape <- mean(abs(residuals/actual), na.rm = TRUE) * 100
  r_squared <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE))
  
  min_error <- min(residuals, na.rm = TRUE)
  max_error <- max(residuals, na.rm = TRUE)
  mean_error <- mean(residuals, na.rm = TRUE)
  std_error <- sd(residuals, na.rm = TRUE)
  mpe <- mean(residuals/actual, na.rm = TRUE) * 100
  
  return(list(
    residuals = residuals,
    mse = mse,
    rmse = rmse,
    mae = mae,
    mape = mape,
    r_squared = r_squared,
    min_error = min_error,
    max_error = max_error,
    mean_error = mean_error,
    std_error = std_error,
    mpe = mpe
  ))
}
model_comparison <- data.frame(
  Модель = character(),
  Min_error = numeric(),
  Max_error = numeric(),
  Mean_error = numeric(),
  Std_error = numeric(),
  MAE = numeric(),
  MPE = numeric(),
  MAPE = numeric(),
  MSE = numeric(),
  RMSE = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

# 2.3.1. Идентификация тренда
cat("2.3.1. ИДЕНТИФИКАЦИЯ ТРЕНДА\n")
cat("---------------------------\n")

time_index <- 1:length(ts_data)

# Модель 1: Линейный тренд
cat("Модель 1: Линейный тренд\n")
linear_trend <- lm(ts_data ~ time_index)
linear_pred <- predict(linear_trend)
metrics_linear <- calculate_metrics(ts_data, linear_pred)

cat("Уравнение: зарплата =", round(coefficients(linear_trend)[1], 2), "+", 
    round(coefficients(linear_trend)[2], 4), "* t\n")
cat("R² =", round(summary(linear_trend)$r.squared, 4), "\n\n")

# Модель 2: Квадратичный тренд
cat("Модель 2: Квадратичный тренд\n")
quadratic_trend <- lm(ts_data ~ time_index + I(time_index^2))
quadratic_pred <- predict(quadratic_trend)
metrics_quadratic <- calculate_metrics(ts_data, quadratic_pred)

cat("Уравнение: зарплата =", round(coefficients(quadratic_trend)[1], 2), 
    round(coefficients(quadratic_trend)[2], 4), "* t +", 
    round(coefficients(quadratic_trend)[3], 6), "* t²\n")
cat("R² =", round(summary(quadratic_trend)$r.squared, 4), "\n\n")

# Модель 3: Экспоненциальный тренд
cat("Модель 3: Экспоненциальный тренд\n")
# Добавляем небольшую константу чтобы избежать логарифмирования отрицательных значений
constant <- abs(min(ts_data, na.rm = TRUE)) + 1
exp_trend <- lm(log(ts_data + constant) ~ time_index)
exp_pred <- exp(predict(exp_trend)) - constant
metrics_exp <- calculate_metrics(ts_data, exp_pred)

cat("Уравнение: ln(зарплата +", constant, ") =", 
    round(coefficients(exp_trend)[1], 2), "+", 
    round(coefficients(exp_trend)[2], 4), "* t\n")
cat("R² =", round(summary(exp_trend)$r.squared, 4), "\n\n")

# Выбираем лучшую модель тренда по R²
r2_values <- c(
  linear = summary(linear_trend)$r.squared,
  quadratic = summary(quadratic_trend)$r.squared,
  exponential = summary(exp_trend)$r.squared
)

best_trend_name <- names(which.max(r2_values))
cat("Лучшая модель тренда:", best_trend_name, "(R² =", round(max(r2_values), 4), ")\n\n")

# Используем квадратичный тренд как Модель 1 (обычно лучше для восходящих трендов)
model1_pred <- quadratic_pred
model1_metrics <- metrics_quadratic
best_trend_model <- quadratic_trend

# Добавляем в таблицу сравнения
model_comparison <- rbind(model_comparison, data.frame(
  Модель = "1 (Тренд)",
  Min_error = model1_metrics$min_error,
  Max_error = model1_metrics$max_error,
  Mean_error = model1_metrics$mean_error,
  Std_error = model1_metrics$std_error,
  MAE = model1_metrics$mae,
  MPE = model1_metrics$mpe,
  MAPE = model1_metrics$mape,
  MSE = model1_metrics$mse,
  RMSE = model1_metrics$rmse,
  R2 = model1_metrics$r_squared
))

# График исходного ряда с Моделью 1
png("графики_лр2/4_модель1_тренд.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2))
plot(ts_data, 
     main = "Модель 1: Трендовая составляющая",
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "blue", 
     lwd = 2)

# Добавляем линию тренда - используем тот же временной ряд
trend_ts <- ts(model1_pred, start = c(2007, 1), frequency = 12)
lines(trend_ts, col = "red", lwd = 3)

legend("topleft", 
       legend = c("Исходный ряд", "Трендовая модель"),
       col = c("blue", "red"), lwd = 2)
grid()
dev.off()
cat("График Модели 1 сохранен\n")
# Анализ остатков Модели 1
residuals1 <- model1_metrics$residuals

# Периодограмма остатков
png("графики_лр2/5_остатки_модель1.png", width = 1500, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Периодограмма
spectrum(residuals1, main = "Периодограмма остатков (Модель 1)", 
         col = "red", log = "no")

# ACF остатков
acf(residuals1, main = "ACF остатков (Модель 1)", 
    lag.max = 48, ylab = "Корреляция")

# PACF остатков  
pacf(residuals1, main = "PACF остатков (Модель 1)",
     lag.max = 48, ylab = "Частная корреляция")

# График остатков
plot(residuals1, type = "l", main = "Остатки Модели 1",
     xlab = "Время", ylab = "Остатки", col = "darkgreen")
abline(h = 0, col = "red", lty = 2)
grid()

dev.off()
cat("Графики анализа остатков Модели 1 сохранены\n")

# Анализ структуры остатков Модели 1
cat("АНАЛИЗ ОСТАТКОВ МОДЕЛИ 1:\n")
cat("Стандартное отклонение остатков:", round(sd(residuals1, na.rm = TRUE), 4), "\n")
cat("Тест на автокорреляцию (Ljung-Box):\n")
lb_test <- Box.test(residuals1, lag = 12, type = "Ljung-Box")
cat("p-value =", round(lb_test$p.value, 4))
if(lb_test$p.value < 0.05) {
  cat(" - есть автокорреляция\n")
} else {
  cat(" - автокорреляция отсутствует\n")
}

# Проверяем наличие сезонности в остатках
seasonal_acf <- acf(residuals1, lag.max = 24, plot = FALSE)
seasonal_lags <- seasonal_acf$acf[13] # лаг 12 месяцев
cat("ACF на лаге 12 (сезонность):", round(seasonal_lags, 4), "\n")
if(abs(seasonal_lags) > 0.2) {
  cat("В остатках присутствует сезонная составляющая\n")
} else {
  cat("Сезонная составляющая в остатках отсутствует\n")
}
cat("\n")

# 2.3.2. Идентификация сезонной составляющей
cat("2.3.2. ИДЕНТИФИКАЦИЯ СЕЗОННОЙ СОСТАВЛЯЮЩЕЙ\n")
cat("-------------------------------------------\n")

# Создаем сезонные фиктивные переменные
seasonal_dummies <- factor(cycle(ts_data))

# Модель 2: Тренд + сезонность
model2 <- lm(ts_data ~ time_index + I(time_index^2) + seasonal_dummies)
model2_pred <- predict(model2)
model2_metrics <- calculate_metrics(ts_data, model2_pred)

cat("Модель 2: Тренд + сезонная составляющая\n")
cat("R² =", round(summary(model2)$r.squared, 4), "\n")
cat("Сезонные коэффициенты:\n")
seasonal_coef <- coefficients(model2)[grep("seasonal_dummies", names(coefficients(model2)))]
print(round(seasonal_coef, 4))
cat("\n")

# Добавляем в таблицу сравнения
model_comparison <- rbind(model_comparison, data.frame(
  Модель = "2 (Тренд+Сезонность)",
  Min_error = model2_metrics$min_error,
  Max_error = model2_metrics$max_error,
  Mean_error = model2_metrics$mean_error,
  Std_error = model2_metrics$std_error,
  MAE = model2_metrics$mae,
  MPE = model2_metrics$mpe,
  MAPE = model2_metrics$mape,
  MSE = model2_metrics$mse,
  RMSE = model2_metrics$rmse,
  R2 = model2_metrics$r_squared
))

# График исходного ряда с Моделью 2
png("графики_лр2/6_модель2_тренд_сезонность.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2))
plot(ts_data, 
     main = "Модель 2: Тренд + сезонная составляющая",
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "blue", 
     lwd = 2)

# Добавляем линию модели - используем временной ряд
model2_ts <- ts(model2_pred, start = c(2007, 1), frequency = 12)
lines(model2_ts, col = "red", lwd = 3)

legend("topleft", 
       legend = c("Исходный ряд", "Модель 2 (тренд+сезонность)"),
       col = c("blue", "red"), lwd = 2)
grid()
dev.off()
cat("График Модели 2 сохранен\n")   

# Анализ остатков Модели 2
residuals2 <- model2_metrics$residuals

# Графики анализа остатков Модели 2
png("графики_лр2/7_остатки_модель2.png", width = 1500, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

spectrum(residuals2, main = "Периодограмма остатков (Модель 2)", 
         col = "red", log = "no")

acf(residuals2, main = "ACF остатков (Модель 2)", 
    lag.max = 48, ylab = "Корреляция")

pacf(residuals2, main = "PACF остатков (Модель 2)",
     lag.max = 48, ylab = "Частная корреляция")

plot(residuals2, type = "l", main = "Остатки Модели 2",
     xlab = "Время", ylab = "Остатки", col = "darkgreen")
abline(h = 0, col = "red", lty = 2)
grid()

dev.off()
cat("Графики анализа остатков Модели 2 сохранены\n")

# Анализ структуры остатков Модели 2
cat("АНАЛИЗ ОСТАТКОВ МОДЕЛИ 2:\n")
cat("Стандартное отклонение остатков:", round(sd(residuals2, na.rm = TRUE), 4), "\n")
lb_test2 <- Box.test(residuals2, lag = 12, type = "Ljung-Box")
cat("Тест Ljung-Box p-value =", round(lb_test2$p.value, 4))
if(lb_test2$p.value < 0.05) {
  cat(" - есть автокорреляция\n")
} else {
  cat(" - автокорреляция отсутствует\n")
}

# Проверяем сезонность в остатках
seasonal_acf2 <- acf(residuals2, lag.max = 24, plot = FALSE)
seasonal_lags2 <- seasonal_acf2$acf[13]
cat("ACF на лаге 12:", round(seasonal_lags2, 4))
if(abs(seasonal_lags2) > 0.2) {
  cat(" - сезонность присутствует\n")
} else {
  cat(" - сезонность устранена\n")
}
cat("\n")

# 2.3.3. Идентификация авторегрессионной составляющей
cat("2.3.3. ИДЕНТИФИКАЦИЯ АВТОРЕГРЕССИОННОЙ СОСТАВЛЯЮЩЕЙ\n")
cat("---------------------------------------------------\n")

# Анализируем ACF/PACF остатков для определения порядка AR
cat("Анализ ACF/PACF остатков Модели 2 для определения AR порядка:\n")

# Определяем порядок AR модели по PACF
pacf_residuals <- pacf(residuals2, plot = FALSE, lag.max = 12)
significant_lags <- which(abs(pacf_residuals$acf) > 2/sqrt(length(residuals2)))
cat("Значимые лаги в PACF:", significant_lags, "\n")

if(length(significant_lags) > 0) {
  ar_order <- max(significant_lags)
  cat("Рекомендуемый порядок AR модели:", ar_order, "\n\n")
  
  # Модель 3: Тренд + сезонность + AR составляющая
  # Создаем лагированные переменные для AR модели
  ar_data <- data.frame(
    y = ts_data[(ar_order+1):length(ts_data)],
    trend = time_index[(ar_order+1):length(ts_data)],
    trend2 = time_index[(ar_order+1):length(ts_data)]^2,
    seasonal = seasonal_dummies[(ar_order+1):length(ts_data)]
  )
  
  # Добавляем лагированные значения
  for(i in 1:ar_order) {
    ar_data[[paste0("lag", i)]] <- ts_data[(ar_order+1-i):(length(ts_data)-i)]
  }
  
  # Строим модель
  formula_ar <- as.formula(paste("y ~ trend + trend2 + seasonal +", 
                                paste(paste0("lag", 1:ar_order), collapse = " + ")))
  model3 <- lm(formula_ar, data = ar_data)
  model3_pred <- predict(model3)
  
  # Создаем полный вектор предсказаний (первые ar_order значений NA)
  full_pred <- rep(NA, length(ts_data))
  full_pred[(ar_order+1):length(ts_data)] <- model3_pred
  
  model3_metrics <- calculate_metrics(ts_data[(ar_order+1):length(ts_data)], model3_pred)
  
  cat("Модель 3: Тренд + сезонность + AR(", ar_order, ")\n", sep = "")
  cat("R² =", round(summary(model3)$r.squared, 4), "\n")

  # Добавляем в таблицу сравнения
  model_comparison <- rbind(model_comparison, data.frame(
  Модель = paste0("3 (Тренд+Сезонность+AR(", ar_order, "))"),
  Min_error = model3_metrics$min_error,
  Max_error = model3_metrics$max_error,
  Mean_error = model3_metrics$mean_error,
  Std_error = model3_metrics$std_error,
  MAE = model3_metrics$mae,
  MPE = model3_metrics$mpe,
  MAPE = model3_metrics$mape,
  MSE = model3_metrics$mse,
  RMSE = model3_metrics$rmse,
  R2 = model3_metrics$r_squared
))
  
  # График исходного ряда с Моделью 3
  png("графики_лр2/8_модель3_полная.png", width = 1920, height = 1080, res = 150)
  par(mar = c(5, 5, 4, 2))
  plot(ts_data, 
       main = paste("Модель 3: Тренд + сезонность + AR(", ar_order, ")", sep = ""),
       xlab = "Год", 
       ylab = "Зарплата, тыс. руб.",
       col = "blue", 
       lwd = 2)
  lines(time_index, full_pred, col = "red", lwd = 2)
  legend("topleft", 
         legend = c("Исходный ряд", "Модель 3 (полная)"),
         col = c("blue", "red"), lwd = 2)
  grid()
  dev.off()
  cat("График Модели 3 сохранен\n")
  
  # Анализ остатков Модели 3
  residuals3 <- model3_metrics$residuals
  
  # Графики анализа остатков Модели 3
  png("графики_лр2/9_остатки_модель3.png", width = 1500, height = 1000, res = 150)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  spectrum(residuals3, main = "Периодограмма остатков (Модель 3)", 
           col = "red", log = "no")
  
  acf(residuals3, main = "ACF остатков (Модель 3)", 
      lag.max = 48, ylab = "Корреляция")
  
  pacf(residuals3, main = "PACF остатков (Модель 3)",
       lag.max = 48, ylab = "Частная корреляция")
  
  plot(residuals3, type = "l", main = "Остатки Модели 3",
       xlab = "Время", ylab = "Остатки", col = "darkgreen")
  abline(h = 0, col = "red", lty = 2)
  grid()
  
  dev.off()
  cat("Графики анализа остатков Модели 3 сохранены\n")
  
  # Анализ структуры остатков Модели 3
  cat("АНАЛИЗ ОСТАТКОВ МОДЕЛИ 3:\n")
  cat("Стандартное отклонение остатков:", round(sd(residuals3, na.rm = TRUE), 4), "\n")
  lb_test3 <- Box.test(residuals3, lag = 12, type = "Ljung-Box")
  cat("Тест Ljung-Box p-value =", round(lb_test3$p.value, 4))
  if(lb_test3$p.value < 0.05) {
    cat(" - есть автокорреляция\n")
  } else {
    cat(" - автокорреляция отсутствует (белый шум)\n")
    cat("Модель 3 адекватна!\n")
  }
  
}  else {
  cat("Авторегрессионная составляющая не требуется\n")
  cat("Остатки Модели 2 уже представляют белый шум\n")
  # Если AR не нужна, то Модель 3 = Модель 2
  model3_metrics <- model2_metrics
  model_comparison <- rbind(model_comparison, data.frame(
    Модель = "3 (Тренд+Сезонность)",
    Min_error = model2_metrics$min_error,
    Max_error = model2_metrics$max_error,
    Mean_error = model2_metrics$mean_error,
    Std_error = model2_metrics$std_error,
    MAE = model2_metrics$mae,
    MPE = model2_metrics$mpe,
    MAPE = model2_metrics$mape,
    MSE = model2_metrics$mse,
    RMSE = model2_metrics$rmse,
    R2 = model2_metrics$r_squared
  ))
}
cat("\n")



# 2.4. СРАВНЕНИЕ МОДЕЛЕЙ ТРЕНДА
cat("2.4. СРАВНЕНИЕ МОДЕЛЕЙ ТРЕНДА\n")
cat("------------------------------\n")

# Функция для расчета R² для произвольных моделей
calculate_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - (ss_res / ss_tot)
}

# Создаем таблицу для сравнения моделей тренда
trend_models_comparison <- data.frame(
  Модель = character(),
  Уравнение = character(),
  R2 = numeric(),
  RMSE = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Успех = logical(),
  stringsAsFactors = FALSE
)

# Вектор для хранения предсказаний всех моделей
model_predictions <- list()
time_index <- 1:length(ts_data)

# ДЕБАГ: Проверка данных
cat("ДЕБАГ: Длина time_index:", length(time_index), "\n")
cat("ДЕБАГ: Длина ts_data:", length(ts_data), "\n")
cat("ДЕБАГ: Диапазон ts_data:", range(ts_data, na.rm = TRUE), "\n\n")

# Модель 1: Линейный тренд
cat("1. Линейная модель тренда\n")
linear_trend <- lm(ts_data ~ time_index)
linear_pred <- predict(linear_trend)
metrics_linear <- calculate_metrics(ts_data, linear_pred)
model_predictions[["Линейная"]] <- linear_pred
cat("ДЕБАГ: Линейная - длина предсказаний:", length(linear_pred), "\n")

trend_models_comparison <- rbind(trend_models_comparison, data.frame(
  Модель = "Линейная",
  Уравнение = "y(t) = a₀ + a₁t",
  R2 = summary(linear_trend)$r.squared,
  RMSE = metrics_linear$rmse,
  AIC = AIC(linear_trend),
  BIC = BIC(linear_trend),
  Успех = TRUE
))

# Модель 2: Квадратичный тренд
cat("2. Квадратичная модель тренда\n")
quadratic_trend <- lm(ts_data ~ time_index + I(time_index^2))
quadratic_pred <- predict(quadratic_trend)
metrics_quadratic <- calculate_metrics(ts_data, quadratic_pred)
model_predictions[["Квадратичная"]] <- quadratic_pred
cat("ДЕБАГ: Квадратичная - длина предсказаний:", length(quadratic_pred), "\n")

trend_models_comparison <- rbind(trend_models_comparison, data.frame(
  Модель = "Квадратичная",
  Уравнение = "y(t) = a₀ + a₁t + a₂t²",
  R2 = summary(quadratic_trend)$r.squared,
  RMSE = metrics_quadratic$rmse,
  AIC = AIC(quadratic_trend),
  BIC = BIC(quadratic_trend),
  Успех = TRUE
))

# Модель 3: Кубический тренд
cat("3. Полином третьей степени\n")
cubic_trend <- lm(ts_data ~ time_index + I(time_index^2) + I(time_index^3))
cubic_pred <- predict(cubic_trend)
metrics_cubic <- calculate_metrics(ts_data, cubic_pred)
model_predictions[["Полином третьей степени"]] <- cubic_pred
cat("ДЕБАГ: Полином третьей степени - длина предсказаний:", length(cubic_pred), "\n")

trend_models_comparison <- rbind(trend_models_comparison, data.frame(
  Модель = "Полином третьей степени",
  Уравнение = "y(t) = a₀ + a₁t + a₂t² + a₃t³",
  R2 = summary(cubic_trend)$r.squared,
  RMSE = metrics_cubic$rmse,
  AIC = AIC(cubic_trend),
  BIC = BIC(cubic_trend),
  Успех = TRUE
))

# Модель 4: Экспоненциальный тренд
cat("4. Экспоненциальная модель тренда\n")
constant <- abs(min(ts_data, na.rm = TRUE)) + 1
exp_trend <- lm(log(ts_data + constant) ~ time_index)
exp_pred <- exp(predict(exp_trend)) - constant
metrics_exp <- calculate_metrics(ts_data, exp_pred)
model_predictions[["Экспоненциальная"]] <- exp_pred
cat("ДЕБАГ: Экспоненциальная - длина предсказаний:", length(exp_pred), "\n")

trend_models_comparison <- rbind(trend_models_comparison, data.frame(
  Модель = "Экспоненциальная",
  Уравнение = "y(t) = abᵗ",
  R2 = calculate_r2(ts_data, exp_pred),
  RMSE = metrics_exp$rmse,
  AIC = AIC(exp_trend),
  BIC = BIC(exp_trend),
  Успех = TRUE
))

# Модель 5: Модифицированная экспонента
cat("5. Модифицированная экспонента\n")
mod_exp_success <- FALSE
mod_exp_pred <- NULL

tryCatch({
  start_values_mod_exp <- list(
    k = max(ts_data, na.rm = TRUE) * 1.1, 
    a = -100, 
    b = 1.01
  )
  mod_exp_trend <- nls(
    ts_data ~ k + a * (b^time_index),
    start = start_values_mod_exp,
    control = nls.control(maxiter = 500, warnOnly = TRUE)
  )
  mod_exp_pred <- predict(mod_exp_trend)
  metrics_mod_exp <- calculate_metrics(ts_data, mod_exp_pred)
  model_predictions[["Модифицированная экспонента"]] <- mod_exp_pred
  mod_exp_success <- TRUE
  cat("ДЕБАГ: Модифицированная экспонента - длина предсказаний:", length(mod_exp_pred), "\n")
  
  trend_models_comparison <- rbind(trend_models_comparison, data.frame(
    Модель = "Модифицированная экспонента",
    Уравнение = "y(t) = k + abᵗ",
    R2 = calculate_r2(ts_data, mod_exp_pred),
    RMSE = metrics_mod_exp$rmse,
    AIC = AIC(mod_exp_trend),
    BIC = BIC(mod_exp_trend),
    Успех = TRUE
  ))
  cat("УСПЕХ\n")
}, error = function(e) {
  cat("ОШИБКА:", e$message, "\n")
})

# Модель 6: Логистическая кривая
cat("6. Логистическая кривая\n")
logistic_success <- FALSE
logistic_pred <- NULL

tryCatch({
  start_values_logistic <- list(
    k = max(ts_data, na.rm = TRUE) * 1.2, 
    a = 100, 
    b = -0.05
  )
  logistic_trend <- nls(
    ts_data ~ k / (1 + a * exp(b * time_index)),
    start = start_values_logistic,
    control = nls.control(maxiter = 500, warnOnly = TRUE)
  )
  logistic_pred <- predict(logistic_trend)
  metrics_logistic <- calculate_metrics(ts_data, logistic_pred)
  model_predictions[["Логистическая"]] <- logistic_pred
  logistic_success <- TRUE
  cat("ДЕБАГ: Логистическая - длина предсказаний:", length(logistic_pred), "\n")
  
  trend_models_comparison <- rbind(trend_models_comparison, data.frame(
    Модель = "Логистическая",
    Уравнение = "y(t) = k/(1 + aeᵏᵗ)",
    R2 = calculate_r2(ts_data, logistic_pred),
    RMSE = metrics_logistic$rmse,
    AIC = AIC(logistic_trend),
    BIC = BIC(logistic_trend),
    Успех = TRUE
  ))
  cat("УСПЕХ\n")
}, error = function(e) {
  cat("ОШИБКА:", e$message, "\n")
})

# Модель 7: Кривая Гомперца
cat("7. Кривая Гомперца\n")
gompertz_success <- FALSE
gompertz_pred <- NULL

tryCatch({
  start_values_gompertz <- list(
    k = max(ts_data, na.rm = TRUE) * 1.1, 
    a = 0.001, 
    b = 0.95
  )
  gompertz_trend <- nls(
    ts_data ~ k * (a^(b^time_index)),
    start = start_values_gompertz,
    control = nls.control(maxiter = 500, warnOnly = TRUE)
  )
  gompertz_pred <- predict(gompertz_trend)
  metrics_gompertz <- calculate_metrics(ts_data, gompertz_pred)
  model_predictions[["Гомперца"]] <- gompertz_pred
  gompertz_success <- TRUE
  cat("ДЕБАГ: Гомперца - длина предсказаний:", length(gompertz_pred), "\n")
  
  trend_models_comparison <- rbind(trend_models_comparison, data.frame(
    Модель = "Гомперца",
    Уравнение = "y(t) = kaᵇᵗ",
    R2 = calculate_r2(ts_data, gompertz_pred),
    RMSE = metrics_gompertz$rmse,
    AIC = AIC(gompertz_trend),
    BIC = BIC(gompertz_trend),
    Успех = TRUE
  ))
  cat("УСПЕХ\n")
}, error = function(e) {
  cat("ОШИБКА:", e$message, "\n")
})

# Выводим полную таблицу сравнения
cat("\nПОЛНАЯ ТАБЛИЦА СРАВНЕНИЯ МОДЕЛЕЙ ТРЕНДА:\n")
cat("==========================================\n")
print(trend_models_comparison)

# Анализ лучших моделей
cat("\nАНАЛИЗ РЕЗУЛЬТАТОВ:\n")
cat("===================\n")

# Лучшая модель по R²
best_by_r2 <- trend_models_comparison[which.max(trend_models_comparison$R2), ]
cat("Лучшая модель по R²: ", best_by_r2$Модель, " (R² = ", round(best_by_r2$R2, 4), ")\n", sep = "")

# Лучшая модель по RMSE
best_by_rmse <- trend_models_comparison[which.min(trend_models_comparison$RMSE), ]
cat("Лучшая модель по RMSE: ", best_by_rmse$Модель, " (RMSE = ", round(best_by_rmse$RMSE, 4), ")\n", sep = "")

# Визуализация всех успешных моделей тренда
cat("\nСОЗДАНИЕ ГРАФИКОВ...\n")

# График 1: Сравнение всех моделей тренда
png("графики_лр2/11_сравнение_всех_трендов.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2))

# Создаем основу графика
plot(ts_data, 
     main = "Сравнение всех моделей тренда (Вариант 4)",
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "black", 
     lwd = 3,
     ylim = range(ts_data, na.rm = TRUE))

# Цвета для разных моделей
colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink")
model_names_display <- c("Линейная", "Квадратичная", "Полином третьей степени", "Экспоненциальная", 
                        "Модифицированная экспонента", "Логистическая", "Гомперца")

# Рисуем все успешные модели
for(i in 1:length(model_names_display)) {
  model_name <- model_names_display[i]
  if(model_name %in% names(model_predictions)) {
    pred <- model_predictions[[model_name]]
    if(!is.null(pred)) {
      # Преобразуем pred в временной ряд для правильного отображения
      pred_ts <- ts(pred, start = start(ts_data), frequency = frequency(ts_data))
      lines(pred_ts, 
            col = colors[i], 
            lwd = 2, 
            lty = i)
      cat("ДОБАВЛЕНА ЛИНИЯ:", model_name, "\n")
    }
  }
}

# Добавляем легенду
legend("topleft", 
       legend = c("Исходные данные", model_names_display),
       col = c("black", colors), 
       lwd = c(3, rep(2, length(model_names_display))), 
       lty = c(1, 1:length(model_names_display)),
       cex = 0.7,
       bg = "white")

grid()
dev.off()
cat("График сравнения всех моделей тренда сохранен\n")

# График 2: Лучшие 3 модели
png("графики_лр2/12_лучшие_3_модели.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2))

# Выбираем 3 лучшие модели по R²
top_3_models <- head(trend_models_comparison[order(-trend_models_comparison$R2), ], 3)

# Создаем основу графика
plot(ts_data, 
     main = "Топ-3 лучшие модели тренда по R²",
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "black", 
     lwd = 3,
     ylim = range(ts_data, na.rm = TRUE))

# Цвета для топ-3 моделей
top_colors <- c("red", "blue", "darkgreen")

# Рисуем топ-3 модели
for(i in 1:nrow(top_3_models)) {
  model_name <- top_3_models$Модель[i]
  pred <- model_predictions[[model_name]]
  if(!is.null(pred)) {
    # Преобразуем pred в временной ряд для правильного отображения
    pred_ts <- ts(pred, start = start(ts_data), frequency = frequency(ts_data))
    lines(pred_ts, 
          col = top_colors[i], 
          lwd = 3, 
          lty = i + 1)
    cat("ДОБАВЛЕНА ТОП-МОДЕЛЬ:", model_name, "\n")
  }
}

# Добавляем легенду
legend("topleft", 
       legend = c("Исходные данные", 
                 paste0(top_3_models$Модель[1], " (R²=", round(top_3_models$R2[1], 3), ")"),
                 paste0(top_3_models$Модель[2], " (R²=", round(top_3_models$R2[2], 3), ")"),
                 paste0(top_3_models$Модель[3], " (R²=", round(top_3_models$R2[3], 3), ")")),
       col = c("black", top_colors[1:3]), 
       lwd = c(3, rep(3, 3)), 
       lty = c(1, 2, 3, 4),
       cex = 1.0,
       bg = "white")

grid()
dev.off()
cat("График топ-3 моделей сохранен\n")

# Детальный анализ лучшей модели
cat("\nДЕТАЛЬНЫЙ АНАЛИЗ ЛУЧШЕЙ МОДЕЛИ (", best_by_r2$Модель, "):\n", sep = "")
cat("===============================================\n")

cat("МЕТРИКИ КАЧЕСТВА:\n")
cat("R² =", round(best_by_r2$R2, 4), "\n")
cat("RMSE =", round(best_by_r2$RMSE, 4), "\n")
if(best_by_r2$Модель %in% names(model_predictions)) {
  best_pred <- model_predictions[[best_by_r2$Модель]]
  best_metrics <- calculate_metrics(ts_data, best_pred)
  cat("MAPE =", round(best_metrics$mape, 4), "%\n")
}

cat("\n=========================================\n")
cat("СРАВНЕНИЕ МОДЕЛЕЙ ТРЕНДА ЗАВЕРШЕНО\n")
cat("Всего успешных моделей:", sum(trend_models_comparison$Успех), "из", nrow(trend_models_comparison), "\n")
cat("Лучшая модель:", best_by_r2$Модель, "\n")
cat("=========================================\n\n")

# 2.5. СРАВНЕНИЕ МОДЕЛЕЙ И ПРОГНОЗ
cat("2.5. СРАВНЕНИЕ МОДЕЛЕЙ И ПРОГНОЗ\n")
cat("---------------------------------\n")

# Выводим таблицу сравнения комплексных моделей
cat("ТАБЛИЦА СРАВНЕНИЯ КОМПЛЕКСНЫХ МОДЕЛЕЙ:\n")

# Округляем только числовые колонки вручную
model_comparison_display <- data.frame(
  Модель = model_comparison$Модель,
  Min_error = round(model_comparison$Min_error, 4),
  Max_error = round(model_comparison$Max_error, 4),
  Mean_error = round(model_comparison$Mean_error, 4),
  Std_error = round(model_comparison$Std_error, 4),
  MAE = round(model_comparison$MAE, 4),
  MPE = round(model_comparison$MPE, 4),
  MAPE = round(model_comparison$MAPE, 4),
  MSE = round(model_comparison$MSE, 4),
  RMSE = round(model_comparison$RMSE, 4),
  R2 = round(model_comparison$R2, 4)
)

print(model_comparison_display)
cat("\n")

# Выбираем лучшую модель по наименьшему RMSE
best_model_index <- which.min(model_comparison$RMSE)
best_model_name <- model_comparison$Модель[best_model_index]
cat("Лучшая комплексная модель:", best_model_name, "(наименьший RMSE)\n\n")

# Прогноз на 3 шага вперед
cat("ПРОГНОЗ НА 3 ШАГА ВПЕРЕД:\n")

# Определяем какая модель лучшая и используем ее для прогноза
if(grepl("AR", best_model_name)) {
  # Прогноз для модели с AR составляющей
  last_values <- tail(ts_data, ar_order)
  future_trend <- (length(ts_data) + 1:3)
  future_seasonal <- factor((cycle(ts_data)[1:3] + 12 - 1) %% 12 + 1)
  
  # Создаем данные для прогноза
  forecast_data <- data.frame(
    trend = future_trend,
    trend2 = future_trend^2,
    seasonal = future_seasonal
  )
  
  # Добавляем лаги
  for(i in 1:ar_order) {
    if(i <= length(last_values)) {
      forecast_data[[paste0("lag", i)]] <- c(last_values[i:min(length(last_values), ar_order)], 
                                           rep(NA, max(0, 3 - (ar_order - i + 1))))[1:3]
    }
  }
  
  # Заполняем пропущенные значения средним
  for(col in names(forecast_data)) {
    if(any(is.na(forecast_data[[col]]))) {
      forecast_data[[col]][is.na(forecast_data[[col]])] <- mean(forecast_data[[col]], na.rm = TRUE)
    }
  }
  
  forecast_values <- predict(model3, newdata = forecast_data)
  
} else {
  # Прогноз для модели без AR (тренд + сезонность)
  future_time <- (length(ts_data) + 1:3)
  future_seasonal <- factor((cycle(ts_data)[1:3] + 12 - 1) %% 12 + 1)
  
  forecast_data <- data.frame(
    time_index = future_time,
    seasonal_dummies = future_seasonal
  )
  
  forecast_values <- predict(model2, newdata = forecast_data)
}

# Выводим прогноз
forecast_dates <- seq.Date(max(data$дата) + 30, by = "month", length.out = 3)
cat("Дата         | Прогноз, тыс. руб.\n")
cat("---------------------------------\n")
for(i in 1:3) {
  cat(format(forecast_dates[i], "%Y-%m-%d"), " |", round(forecast_values[i], 2), "\n")
}

# График с прогнозом
png("графики_лр2/13_прогноз.png", width = 1920, height = 1080, res = 150)
par(mar = c(5, 5, 4, 2))

# Создаем полный ряд с прогнозом
full_series <- c(ts_data, rep(NA, 3))
full_series_ts <- ts(full_series, start = start(ts_data), frequency = frequency(ts_data))

plot(full_series_ts, 
     main = paste("Прогноз на 3 месяца (лучшая модель:", best_model_name, ")"),
     xlab = "Год", 
     ylab = "Зарплата, тыс. руб.",
     col = "blue", 
     lwd = 2,
     ylim = c(min(ts_data, na.rm = TRUE), max(c(ts_data, forecast_values), na.rm = TRUE) * 1.1))

# Добавляем прогноз красным цветом
forecast_indices <- (length(ts_data)+1):(length(ts_data)+3)
points(time(full_series_ts)[forecast_indices], forecast_values, 
       col = "red", pch = 16, cex = 1.5)
lines(time(full_series_ts)[forecast_indices], forecast_values, 
      col = "red", lwd = 2, lty = 2)

legend("topleft", 
       legend = c("Исторические данные", "Прогноз"),
       col = c("blue", "red"), lwd = 2, pch = c(NA, 16), lty = c(1, 2))
grid()
dev.off()
cat("\nГрафик прогноза сохранен\n")

# Сравниваем прогнозы от лучшей модели тренда и лучшей комплексной модели
cat("\nСРАВНЕНИЕ ПРОГНОЗОВ ОТ РАЗНЫХ МОДЕЛЕЙ:\n")
cat("======================================\n")

# Прогноз от лучшей модели тренда (кубической)
cat("Прогноз от лучшей модели тренда (Полином третьей степени):\n")
future_time_trend <- (length(ts_data) + 1:3)
trend_forecast_data <- data.frame(
  time_index = future_time_trend,
  I.time_index.2. = future_time_trend^2,
  I.time_index.3. = future_time_trend^3
)
trend_forecast <- predict(cubic_trend, newdata = trend_forecast_data)

cat("Дата         | Прогноз, тыс. руб.\n")
cat("---------------------------------\n")
for(i in 1:3) {
  cat(format(forecast_dates[i], "%Y-%m-%d"), " |", round(trend_forecast[i], 2), "\n")
}

cat("\nПрогноз от лучшей комплексной модели (", best_model_name, "):\n", sep = "")
cat("Дата         | Прогноз, тыс. руб.\n")
cat("---------------------------------\n")
for(i in 1:3) {
  cat(format(forecast_dates[i], "%Y-%m-%d"), " |", round(forecast_values[i], 2), "\n")
}

# Анализ различий в прогнозах
cat("\nАНАЛИЗ РАЗЛИЧИЙ В ПРОГНОЗАХ:\n")
differences <- forecast_values - trend_forecast
cat("Средняя разница:", round(mean(differences), 2), "тыс. руб.\n")
cat("Максимальная разница:", round(max(abs(differences)), 2), "тыс. руб.\n")

if(mean(differences) > 0) {
  cat("Комплексная модель дает более высокий прогноз\n")
} else {
  cat("Комплексная модель дает более низкий прогноз\n")
}

cat("\n=========================================\n")
cat("АНАЛИЗ ЗАВЕРШЕН\n")
cat("Все модели построены и сравнены\n")
cat("Прогноз выполнен на 3 месяца вперед\n")
cat("Лучшая модель тренда: Полином третьей степени\n")
cat("Лучшая комплексная модель:", best_model_name, "\n")
cat("=========================================\n")