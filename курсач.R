library(tidyverse)
library(stringr)

df <- read_csv('D:/Downloads/email.csv')
head(df)

summary(df)

# проверка значений таргета
count(df, Category, sort = TRUE)

# обработка странного значения
df <- df %>% filter(Category %in% c("ham", "spam"))
count(df, Category, sort = TRUE)

# проверка пропусков
any(is.na(df))

# обработка дубликатов
sum(duplicated(df))
df <- df %>% distinct()
table(df$Category)
sum(duplicated(df))

# Подсчет количества ham и spam
counts <- table(df$Category)

# Доверительный интервал для доли спама
prop.test(counts["spam"], sum(counts), conf.level = 0.95)

# Добавляем столбец с длиной текста
df$text_length <- nchar(df$Message)

# Проверка нормальности (если p-value < 0.05 — распределение ненормальное)
shapiro.test(df$text_length[df$Category == "ham"])
shapiro.test(df$text_length[df$Category == "spam"])

# Распределение ненормальное — U-критерий Манна-Уитни
wilcox.test(text_length ~ Category, data = df)


# Создаем бинарный признак наличия слова "free"
df$has_free <- str_detect(df$Message, "free") * 1

# Таблица сопряженности
contingency_table <- table(df$Category, df$has_free)

# Тест хи-квадрат
chisq.test(contingency_table)

ggplot(df, aes(x = text_length, fill = Category)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Распределение количества слов в сообщениях",
       x = "Количество слов",
       y = "Частота") +
  scale_fill_manual(values = c("ham" = "blue", "spam" = "red")) +
  theme_minimal()

ggplot(df, aes(x = log(text_length + 1), fill = Category)) +
  geom_density(alpha = 0.5)

ggplot(df, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Добавляем числа над столбцами
  labs(title = "Распределение сообщений по категориям",
       subtitle = paste("Всего сообщений:", nrow(df)),
       x = "Тип сообщения",
       y = "Количество") +
  scale_fill_manual(values = c("ham" = "#4CAF50", "spam" = "#F44336")) +
  theme_minimal() +
  theme(legend.position = "none")  # Убираем легенду, так как подписи и так понятны

