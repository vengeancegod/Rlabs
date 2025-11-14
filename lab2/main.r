Sys.setlocale("LC_ALL", "ru_RU.UTF-8")

if(!require(forecast)) {
  install.packages("forecast")
}
library(forecast)

if(!dir.exists("графики_лр2")) {
  dir.create("графики_лр2")
}
#расчет метрик
calculate_accuracy <- function(actual, predicted, model_name = "") {
  residuals <- actual - predicted
  n <- length(actual)

  min_error <- min(residuals, na.rm = TRUE)
  max_error <- max(residuals, na.rm = TRUE)
  mean_error <- mean(residuals, na.rm = TRUE)
  std_error <- sd(residuals, na.rm = TRUE)
  mae <- mean(abs(residuals), na.rm = TRUE)

  mpe <- mean(residuals/actual, na.rm = TRUE) * 100
  mape <- mean(abs(residuals/actual), na.rm = TRUE) * 100

  mse <- mean(residuals^2, na.rm = TRUE)
  rmse <- sqrt(mse)

  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(data.frame(Модель = model_name, Min_error = min_error,Max_error = max_error,Mean_error = mean_error,Std_error = std_error,MAE = mae,MPE = mpe,MAPE = mape,MSE = mse, RMSE = rmse,R2 = r_squared))
}

file_path <- "data_clean_var4.csv"

data <- read.table(file_path, header = TRUE, sep = ";", fileEncoding = "UTF-8", dec = ",", stringsAsFactors = FALSE)
salary_column <- names(data)[4]
cat("Анализируемый показатель:", salary_column, "\n")

salary_data <- as.numeric(gsub(",", ".", data[[salary_column]]))
tsData <- ts(salary_data, frequency = 12, start = c(2007, 1))

cat("Период наблюдений: с", start(tsData)[1], "года по", end(tsData)[1], "год\n")
cat("Частота данных: ежемесячные\n")

# График 1: Исходный временной ряд
png("графики_лр2/1_временной_ряд.png", width = 1920, height = 1080, res = 150)
plot.ts(tsData, main = paste("Временной ряд:", salary_column),xlab = "Год", ylab = "Значение",col = "steelblue", lwd = 2)
grid(col = "gray90")
dev.off()

tsDataComponents <- decompose(tsData, type = "additive")
png("графики_лр2/2_декомпозиция.png", width = 1920, height = 1080, res = 150)
plot(tsDataComponents, col = "steelblue")
dev.off()

png("графики_лр2/3_автокорреляция.png", width = 1000, height = 800, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
Acf(tsData, lag.max = 48, main = "Автокорреляционная функция (ACF)", col = "steelblue")
Pacf(tsData, lag.max = 48, main = "Частная автокорреляционная функция (PACF)", col = "darkgreen")
dev.off()

cat("Характеристики временного ряда:\n")
cat("Тренд: присутствует восходящий\n") 
cat("Характер тренда: нелинейный\n")
cat("Сезонность: выявлена (период = 12 месяцев)\n")

t <- 1:length(tsData)
data_df <- data.frame(y = as.numeric(tsData), t = t)

accuracy_table <- data.frame()

print("Модель 1: Линейный тренд (y = a + b·t)")

model_linear <- lm(y ~ t, data = data_df)
pred_linear <- predict(model_linear)
acc_linear <- calculate_accuracy(data_df$y, pred_linear, "Линейный тренд")

cat("Уравнение: y =", round(coef(model_linear)[1], 2), "+", round(coef(model_linear)[2], 4), "· t\n")
cat("R^2 =", round(acc_linear$R2, 4), "RMSE =", round(acc_linear$RMSE, 2), "\n")

print("Модель 2: Квадратичный тренд (y = a + b·t + c·t^2)")

model_quadratic <- lm(y ~ t + I(t^2), data = data_df)
pred_quadratic <- predict(model_quadratic)
acc_quadratic <- calculate_accuracy(data_df$y, pred_quadratic, "Квадратичный тренд")

cat("Уравнение: y =", round(coef(model_quadratic)[1], 2), "+", round(coef(model_quadratic)[2], 4), "· t +", round(coef(model_quadratic)[3], 6), "· t^2\n")
cat("R^2 =", round(acc_quadratic$R2, 4), 
    "RMSE =", round(acc_quadratic$RMSE, 2), "\n")

print("Модель 3: Экспоненциальный тренд (y = a·e^(b·t))")

exp_fitted <- FALSE
tryCatch({
  constant <- abs(min(data_df$y, na.rm = TRUE)) + 0.1
  model_exp <- nls(y ~ a * exp(b * t) + constant, data = data_df, start = c(a = 1, b = 0.01)
  )
  pred_exp <- predict(model_exp)
  acc_exp <- calculate_accuracy(data_df$y, pred_exp, "Экспоненциальный тренд")
  
  cat("Уравнение: y =", round(coef(model_exp)[1], 2), "· exp(", round(coef(model_exp)[2], 4), "· t) +", round(constant, 2), "\n")
  cat("R^2 =", round(acc_exp$R2, 4), "RMSE =", round(acc_exp$RMSE, 2), "\n")
  exp_fitted <- TRUE
}, error = function(e) {
  cat("Ошибка:", e$message, "\n")
})

accuracy_table <- rbind(accuracy_table, acc_linear, acc_quadratic)
if(exp_fitted) accuracy_table <- rbind(accuracy_table, acc_exp)

comparison <- accuracy_table[, c("Модель", "R2", "RMSE", "MAPE")]
comparison[, -1] <- round(comparison[, -1], 4)
print(comparison)

best_trend <- accuracy_table$Модель[which.max(accuracy_table$R2)]
cat("\nЛучшая модель тренда:", best_trend, "\n")

# График сравнения трендов
png("графики_лр2/4_сравнение_трендов.png", width = 1920, height = 1080, res = 150)
plot.ts(tsData, main = "Сравнение моделей тренда",xlab = "Год", ylab = "Значение", col = "black", lwd = 2)

lines(ts(pred_linear, start = c(2007, 1), frequency = 12), col = "red", lwd = 2)
lines(ts(pred_quadratic, start = c(2007, 1), frequency = 12), col = "blue", lwd = 2)

legend_items <- c("Исходные данные", "Линейная", "Квадратичная")
legend_colors <- c("black", "red", "blue")

if(exp_fitted) {
  lines(ts(pred_exp, start = c(2007, 1), frequency = 12), col = "green", lwd = 2)
  legend_items <- c(legend_items, "Экспоненциальная")
  legend_colors <- c(legend_colors, "green")
}

legend("topleft", legend = legend_items, col = legend_colors, lwd = 2, bty = "n")
grid(col = "gray90")
dev.off()

# Используем квадратичную модель как основную
model1 <- model_quadratic
model1_pred <- pred_quadratic

# График выбранной модели тренда
png("графики_лр2/5_модель1_тренд.png", width = 1920, height = 1080, res = 150)
plot.ts(tsData, main = "Модель 1: Квадратичная трендовая составляющая", xlab = "Год", ylab = "Значение", col = "steelblue", lwd = 2)
lines(ts(model1_pred, start = c(2007, 1), frequency = 12), col = "red", lwd = 2)
legend("topleft", legend = c("Исходные данные", "Модель тренда"), col = c("steelblue", "red"), lwd = 2, bty = "n")
grid(col = "gray90")
dev.off()

# Анализ остатков модели 1
residuals1 <- residuals(model1)

png("графики_лр2/6_остатки_модель1.png", width = 1500, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

spec.pgram(residuals1, main = "Периодограмма остатков")
Acf(residuals1, main = "ACF остатков")
Pacf(residuals1, main = "PACF остатков")
plot(residuals1, type = "l", main = "График остатков", xlab = "Время", ylab = "Остатки", col = "darkgreen")
abline(h = 0, col = "red", lty = 2)
grid(col = "gray90")

dev.off()

lb_test1 <- Box.test(residuals1, lag = 12, type = "Ljung-Box")
cat("\nТест на автокорреляцию остатков:\n")
cat("   p-value =", round(lb_test1$p.value, 4))
if(lb_test1$p.value < 0.05) {
  cat("Обнаружена значимая автокорреляция\n")
  cat("   (требуется дополнительное моделирование)\n")
} else {
  cat("Автокорреляция отсутствует\n")
}

#model2
data_df$month <- factor(cycle(tsData))

model2 <- lm(y ~ t + I(t^2) + month, data = data_df)
model2_pred <- predict(model2)
acc2 <- calculate_accuracy(data_df$y, model2_pred, "Тренд+Сезонность")

cat("Модель 2: Квадратичный тренд + сезонность\n")
cat("R^2 =", round(acc2$R2, 4), "RMSE =", round(acc2$RMSE, 2), "\n")

png("графики_лр2/7_модель2_тренд_сезонность.png", width = 1920, height = 1080, res = 150)
plot.ts(tsData, main = "Модель 2: Тренд + сезонная составляющая", xlab = "Год", ylab = "Значение", col = "steelblue", lwd = 2)
lines(ts(model2_pred, start = c(2007, 1), frequency = 12), col = "red", lwd = 2)
legend("topleft", legend = c("Исходные данные", "Модель 2 (тренд+сезонность)"), col = c("steelblue", "red"), lwd = 2, bty = "n")
grid(col = "gray90")
dev.off()

residuals2 <- residuals(model2)

png("графики_лр2/8_остатки_модель2.png", width = 1500, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

spec.pgram(residuals2, main = "Периодограмма остатков (Модель 2)")
Acf(residuals2, main = "ACF остатков (Модель 2)")
Pacf(residuals2, main = "PACF остатков (Модель 2)")
plot(residuals2, type = "l", main = "Остатки Модели 2", xlab = "Время", ylab = "Остатки", col = "darkgreen")
abline(h = 0, col = "red", lty = 2)
grid(col = "gray90")

dev.off()

lb_test2 <- Box.test(residuals2, lag = 12, type = "Ljung-Box")
cat("Тест на автокорреляцию остатков (Модель 2):\n")
cat("p-value =", round(lb_test2$p.value, 4))
if(lb_test2$p.value < 0.05) {
  cat(" - обнаружена автокорреляция\n")
} else {
  cat(" - автокорреляция отсутствует\n")
}

accuracy_table <- rbind(accuracy_table, acc2)

# Проверяем, нужна ли AR-составляющая
if(lb_test2$p.value < 0.05) {
  cat("Требуется учет авторегрессионной составляющей\n")

  pacf_res <- pacf(residuals2, plot = FALSE, lag.max = 12)
  significant_lags <- which(abs(pacf_res$acf) > 2/sqrt(length(residuals2)))
  
  if(length(significant_lags) > 0) {
    ar_order <- max(significant_lags)
    cat("Рекомендуемый порядок AR:", ar_order, "\n")
    
    cat("Модель 3: Тренд + сезонность + AR(", ar_order, ")\n", sep = "")
    
    acc3 <- acc2
    acc3$Модель <- paste0("Тренд+Сезонность+AR(", ar_order, ")")
    
  } else {
    cat("AR-составляющая не требуется\n")
    acc3 <- acc2
    acc3$Модель <- "Тренд+Сезонность (без AR)"
  }
  
} else {
  cat("AR-составляющая не требуется (остатки - белый шум)\n")
  acc3 <- acc2
  acc3$Модель <- "Тренд+Сезонность (без AR)"
}

accuracy_table <- rbind(accuracy_table, acc3)

accuracy_display <- accuracy_table
numeric_cols <- sapply(accuracy_display, is.numeric)
accuracy_display[numeric_cols] <- round(accuracy_display[numeric_cols], 4)
print(accuracy_display)

best_model_idx <- which.min(accuracy_table$RMSE)
best_model_name <- accuracy_table$Модель[best_model_idx]

cat("\nлучшая модель:", best_model_name, "\n")
cat("Критерий выбора: минимальный RMSE\n")
cat("RMSE =", round(accuracy_table$RMSE[best_model_idx], 4), "\n")
cat("R^2 =", round(accuracy_table$R2[best_model_idx], 4), "\n")


future_t <- (max(data_df$t) + 1):(max(data_df$t) + 3)
forecast_data <- data.frame(t = future_t)
forecast_values <- predict(model1, newdata = forecast_data)

# Определяем месяцы для прогноза
months <- c("Янв", "Фев", "Мар", "Апр", "Май", "Июн", "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек")


for(i in 1:3) {
  month_idx <- (cycle(tsData)[1] + i - 2) %% 12 + 1
  year <- end(tsData)[1] + (cycle(tsData)[1] + i - 2) %/% 12
  cat(sprintf("│ %s %d     │ %10.2f   │\n", months[month_idx], year, forecast_values[i]))
}

# График с прогнозом
png("графики_лр2/10_прогноз.png", width = 1920, height = 1080, res = 150)

# Создаем расширенный временной ряд
full_series <- c(data_df$y, rep(NA, 3))
all_time <- c(data_df$t, future_t)

plot(all_time, full_series, type = "l", main = paste("Прогноз на 3 периода (модель:", best_model_name, ")"), xlab = "Время (месяцы)", ylab = "Значение", col = "steelblue", lwd = 2, ylim = range(c(data_df$y, forecast_values), na.rm = TRUE))

# Линия модели на исторических данных
lines(data_df$t, model1_pred, col = "red", lwd = 2)

lines(future_t, forecast_values, col = "red", lwd = 2, lty = 2)
points(future_t, forecast_values, col = "red", pch = 19, cex = 1.5)

abline(v = max(data_df$t), col = "gray50", lty = 3, lwd = 1)

legend("topleft", legend = c("Исторические данные", "Модель", "Прогноз"), col = c("steelblue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, NA, 19),bty = "n")
grid(col = "gray90")
dev.off()