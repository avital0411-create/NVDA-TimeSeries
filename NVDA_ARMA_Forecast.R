### מטלה מסכמת אקונומטריקה מתקדמת ###
### סדרות עתיות ###

# Installing and loading required packages
library(readr)
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(reshape2)
library(GGally)
library(readxl)
library(tidyr)
library(lubridate)
library(urca)
library(forecast)

# Reading the Excel dataset into R

data_nvda = read_excel("C:/Users/avita/OneDrive/שולחן העבודה/R/Data- NVDA.xlsx")
dim(data_nvda)
head(data_nvda)
tail(data_nvda)

# Convert the 'Date' column to Date format
data_nvda$Date <- as.Date(data_nvda$Date)

# Create a 'Year' column extracted from the 'Date' column
data_nvda$Year <- year(data_nvda$Date)

# Preview the 'Date' and 'Year' columns to verify the result
head(data_nvda[, c("Date", "Year")])

# Checking for missing values in each column
colSums(is.na(data_nvda))

# שלב 3: השמטת תצפיות חריגות במיוחד (3*IQR)
# מכל העמודות המספריות במסד הנתונים
# --------------------------------------------------------

num_cols <- c("Open", "High", "Low", "Close", "Adj Close", "Volume")

# פונקציה למציאת חריגים (3*IQR)
for (col in num_cols) 
  Q1 <- quantile(data_nvda[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data_nvda[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 3 * IQR_val
  upper_bound <- Q3 + 3 * IQR_val
  
  lower_outliers <- sum(data_nvda[[col]] < lower_bound, na.rm = TRUE)
  upper_outliers <- sum(data_nvda[[col]] > upper_bound, na.rm = TRUE)
  
  cat(paste0(col, ":\n",
             "  חריגים מתחת: ", lower_outliers, "\n",
             "  חריגים מעל: ", upper_outliers, "\n\n"))

data_long <- data_nvda %>%
select(Open, High, Low, Close, `Adj Close`, Volume) %>%
pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  
ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue", outlier.color = "black", outlier.size = 1.5) +
  labs(title = "Boxplot of All Variables (Before Filtering)",
  x = "Variable", y = "Value") +
  theme_minimal()
  
head(data_nvda[order(-data_nvda$Volume), "Volume"], 5)

data_nvda_filtered <- data_nvda[!data_nvda$Volume %in% c(154391100, 146368400), ]

data_nvda_filtered_long <- pivot_longer(data_nvda_filtered, 
                           cols = c(`Adj Close`, Close, High, Low, Open, Volume),
                           names_to = "Variable", 
                           values_to = "Value")

ggplot(data_nvda_filtered_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot of All Variables (After Removing Top 2 Volume Outliers)") +
  theme_minimal()

# נפריד את המשתנים
numeric_vars <- names(data_nvda_filtered)[sapply(data_nvda_filtered, is.numeric)]
categorical_vars <- names(data_nvda_filtered)[sapply(data_nvda_filtered, is.character) | sapply(data_nvda_filtered, is.factor)]

# חישוב מדדים סטטיסטיים כמותיים
numeric_summary <- data_nvda_filtered %>%
  select(all_of(numeric_vars)) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    var = ~var(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    Q1 = ~quantile(., 0.25, na.rm = TRUE),
    Q3 = ~quantile(., 0.75, na.rm = TRUE)
  ))

# הפיכה לפורמט ארוך וטבלה נוחה לקריאה
numeric_summary_long <- numeric_summary %>%
  pivot_longer(cols = everything(),
               names_to = "Measure",
               values_to = "Value") %>%
  separate(Measure, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value)

# הצגה
print(numeric_summary_long)

# סיכום למשתנים איכותיים (אם קיימים)
if (length(categorical_vars) > 0) {
  cat("\n\nמדדים סטטיסטיים - משתנים איכותיים:\n")
  for (var in categorical_vars) {
    cat("\n", var, ":\n")
    print(table(data_nvda_filtered[[var]]))
    print(round(prop.table(table(data_nvda_filtered[[var]])) * 100, 2))
  }
} else {
  cat("\nאין משתנים איכותיים בנתונים.\n")
}

data_nvda_filtered$Open_Binned <- cut(data_nvda_filtered$Open,breaks = 10, include.lowest = TRUE)

#### גרפים ####
ggplot(data_nvda_filtered, aes(x = Date, y = Open)) +
  geom_line(color = "steelblue") +
  labs(title = "מחיר פתיחה לאורך זמן",
       x = "תאריך",
       y = "מחיר פתיחה") +
  theme_minimal()

# גרף המחיר הגבוה
ggplot(data_nvda_filtered, aes(x = Date, y = High)) +
  geom_line(color = "darkgreen") +
  labs(title = "המחיר הגבוה ביותר לאורך זמן",
       x = "תאריך",
       y = "מחיר גבוה") +
  theme_minimal()

# גרף המחיר הנמוך
ggplot(data_nvda_filtered, aes(x = Date, y = Low)) +
  geom_line(color = "orange") +
  labs(title = "המחיר הנמוך ביותר לאורך זמן",
       x = "תאריך",
       y = "מחיר נמוך") +
  theme_minimal()

# גרף מחיר סגירה
ggplot(data_nvda_filtered, aes(x = Date, y = Close)) +
  geom_line(color = "purple") +
  labs(title = "מחיר סגירה לאורך זמן",
       x = "תאריך",
       y = "מחיר סגירה") +
  theme_minimal()

# גרף מחיר סגירה מתואם
ggplot(data_nvda_filtered, aes(x = Date, y = `Adj Close`)) +
  geom_line(color = "red") +
  labs(title = "מחיר סגירה מתואם לאורך זמן",
       x = "תאריך",
       y = "מחיר סגירה מתואם (Adj Close)") +
  theme_minimal()

# גרף נפח מסחר
ggplot(data_nvda_filtered, aes(x = Date, y = Volume)) +
  geom_line(color = "brown") +
  labs(title = "נפח מסחר לאורך זמן",
       x = "תאריך",
       y = "נפח מסחר") +
  theme_minimal()

ggplot(data_nvda_filtered, aes(x = High, y = Close)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "הקשר בין מחיר גבוה למחיר סגירה",
       x = "High (המחיר הגבוה)",
       y = "Close (מחיר הסגירה)") +
  theme_minimal()

# נניח ש-Adj Close הוא מחיר הסגירה שאת רוצה להשתמש בו
data_nvda_filtered$new_returns <- c(NA, diff(log(data_nvda_filtered$`Close`)))

# ADF test על התשואות היומיות
adf_result <- ur.df(na.omit(data_nvda_filtered$new_returns), type = "drift", selectlags = "AIC")

# הצגת תוצאות המבחן
summary(adf_result)

# הסרת NA מהתשואות
returns_clean <- na.omit(data_nvda_filtered$new_returns)

# חלוקה ל־train ול־test לפי הדרישה
train_returns <- head(returns_clean, -1)  # כל התצפיות חוץ מהאחרונה
test_return <- tail(returns_clean, 1)     # רק התצפית האחרונה

# הדפסה לבדיקה
length(train_returns)  # מספר תצפיות ב-train
test_return            # תצפית ה-test

# מציאת מודל ARIMA אופטימלי לנתוני האימון
best_model <- auto.arima(train_returns)
forecast_result <- forecast(best_model, h = 1)
y_hat <- forecast_result$mean[1]
MER <- abs((y_hat - test_return) / test_return)
print(MER)













