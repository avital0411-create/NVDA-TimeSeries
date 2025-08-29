# -----------------------------------------------
# Final Project: Advanced Econometrics with R
# Dataset Source: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
# -----------------------------------------------

# Load required libraries
library(stargazer)
library(plm) 
library(ggplot2)
library(corrplot)
library(GGally)
library(reshape2)
library(readxl)
library(caret)
library(dplyr)
library(plm) 

# Set working directory if needed
setwd("C:/Users/Lax/Downloads")

# Import dataset
df <- read_excel("C:/Users/avita/OneDrive/שולחן העבודה/R/Life_Expectancy_Data.xlsx")

# Basic structure
dim(df)
head(df)
tail(df)
names(df)

# Add row ID
df$Row_ID <- 1:nrow(df)

# Manual country to continent mapping
continent_map <- list(
  Africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Cote d'Ivoire", "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Togo", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe"),
  Asia = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Democratic People's Republic of Korea", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", "Republic of Korea", "Saudi Arabia", "Singapore", "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Viet Nam", "Yemen"),
  Europe = c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Republic of Moldova", "Romania", "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"),
  North_America = c("Canada", "United States of America", "Mexico", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Guatemala", "Haiti", "Honduras", "Jamaica", "Nicaragua", "Panama", "Trinidad and Tobago"),
  South_America = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"),
  Oceania = c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")
)

# Assign continent
get_continent <- function(country_name) {
  for (continent in names(continent_map)) {
    if (country_name %in% continent_map[[continent]]) {
      return(continent)
    }
  }
  return(NA)
}
df$Continent <- sapply(df$Country, get_continent)

# Remove rows with NA continent
df <- df %>% filter(!is.na(Continent))

# Mark observations with missing values
df$Status <- ifelse(!complete.cases(df), "Removed: NA", "Original")

# Remove rows with missing values
df_no_na <- na.omit(df)

# Detect and mark outliers with IQR
remove_outliers_iqr_with_status <- function(data) {
  data$Status <- ifelse(is.na(data$Status), "Original", data$Status)
  num_cols <- sapply(data, is.numeric)
  for (col in names(data)[num_cols]) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    outlier_index <- which(data[[col]] < lower | data[[col]] > upper)
    data$Status[outlier_index[data$Status[outlier_index] == "Original"]] <- "Removed: Outlier"
  }
  return(data)
}

# Apply function
df_status <- remove_outliers_iqr_with_status(df)
names(df_status)[names(df_status) == "Life expectancy"] <- "Life_Expectancy"
df_status$Continent <- df$Continent
# Summary statistics for numeric variables
numeric_vars <- df_status %>%
  select(where(is.numeric)) %>%
  select(-Row_ID)

numeric_summary <- data.frame(
  Variable = names(numeric_vars),
  Mean = sapply(numeric_vars, mean, na.rm = TRUE),
  Median = sapply(numeric_vars, median, na.rm = TRUE),
  Std_Dev = sapply(numeric_vars, sd, na.rm = TRUE),
  Min = sapply(numeric_vars, min, na.rm = TRUE),
  Max = sapply(numeric_vars, max, na.rm = TRUE)
)
print(numeric_summary)

# Frequency tables for categorical variables
categorical_vars <- df_status %>%
  select(where(~is.character(.x) || is.factor(.x))) %>%
  select(-Status)

for (var in names(categorical_vars)) {
  cat("\n\nVariable:", var, "\n")
  print(table(categorical_vars[[var]]))
}

# Histogram for numeric variables
for (col in names(numeric_vars)) {
  p <- ggplot(df_status, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", col),
         x = col, y = "Frequency") +
    theme_minimal()
  print(p)
}

# Bar plots for categorical variables
for (col in names(categorical_vars)) {
  p <- ggplot(df_status, aes_string(x = col)) +
    geom_bar(fill = "coral") +
    labs(title = paste("Bar Plot of", col),
         x = col, y = "Count") +
    theme_minimal()
  print(p)
}

# Boxplot of Life Expectancy by Status
ggplot(df_status, aes(x = Status, y = Life_Expectancy, fill = Status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Life Expectancy by Cleaning Status",
       x = "Observation Status",
       y = "Life Expectancy") +
  theme_minimal()

# Bar plot: count of observations per continent
ggplot(df_status, aes(x = Continent)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Observations by Continent",
       x = "Continent",
       y = "Count") +
  theme_minimal()
# Line plot of average life expectancy per year per continent
df_status %>%
  group_by(Year, Continent) %>%
  summarise(mean_life = mean(Life_Expectancy, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean_life, color = Continent)) +
  geom_line(size = 1.2) +
  labs(title = "Life Expectancy Over Time by Continent",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal()

# Declare pdata.frame for panel analysis
panel_df <- pdata.frame(df_status, index = c("Country", "Year"))

# Fixed Effects (Within) Model
fe_model <- plm(Life_Expectancy ~ Alcohol + GDP + Schooling, 
                data = panel_df, model = "within")
summary(fe_model)

# First Differences (FD) Model
fd_model <- plm(Life_Expectancy ~ Alcohol + GDP + Schooling, 
                data = panel_df, model = "fd")
summary(fd_model)

# Comparison using summary side-by-side
stargazer::stargazer(fe_model, fd_model, type = "text", title = "Model Comparison")

# Extract coefficient summaries from both models
coefs_fe <- summary(fe_model)$coefficients
coefs_fd <- summary(fd_model)$coefficients

# Identify shared variables between both models
shared_vars <- intersect(rownames(coefs_fe), rownames(coefs_fd))

# Create comparison data frame for only shared variables
coef_df <- data.frame(
  Variable = shared_vars,
  FE_Estimate = coefs_fe[shared_vars, 1],
  FE_SE = coefs_fe[shared_vars, 2],
  FD_Estimate = coefs_fd[shared_vars, 1],
  FD_SE = coefs_fd[shared_vars, 2]
)

# Reshape for plotting
library(tidyr)
coef_long <- coef_df %>%
  pivot_longer(cols = c(FE_Estimate, FD_Estimate, FE_SE, FD_SE),
               names_to = c("Model", ".value"),
               names_pattern = "(FE|FD)_(Estimate|SE)")

# Rename model labels
coef_long$Model <- recode(coef_long$Model,
                          "FE" = "Fixed Effects",
                          "FD" = "First Differences")

# Plot comparison
library(ggplot2)
ggplot(coef_long, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE,
                    ymax = Estimate + 1.96 * SE),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Coefficient Comparison: Fixed Effects vs First Differences",
       x = "Variable",
       y = "Estimate") +
  theme_minimal()
